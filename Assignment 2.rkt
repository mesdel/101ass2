;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Assignment 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rsound)
(require 2htdp/image)
(require 2htdp/universe)

;; a ws is
;; - (make-ws slider-frac end-frame)
;; where frac-x and frac-y are numbers between
;; zero and one that correspond to x and y values
; respectively, and end-frame is a frame
;; number
(define-struct ws [frac-x frac-y song-frame end-frame])

(define ps (make-pstream))

;for testing using check-within
(define acceptableError 0.00001)

;; examples:
(make-ws 0.5 0.5 0 2000)
(make-ws 0.7 0.7 4000 80000)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GUI constants and crosshair definition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define BOX-WIDTH 600)
(define BOX-HEIGHT 200)
(define CROSSHAIR-LENGTH 30)
(define CROSSHAIR-WIDTH 10)
(define crosshair (place-image (rectangle CROSSHAIR-WIDTH CROSSHAIR-LENGTH "solid" "purple")
                               (/ CROSSHAIR-LENGTH 2) (/ CROSSHAIR-LENGTH 2)
                               (place-image
                                (rectangle CROSSHAIR-LENGTH CROSSHAIR-WIDTH "solid" "purple")
                                (/ CROSSHAIR-LENGTH 2) (/ CROSSHAIR-LENGTH 2)
                                (rectangle CROSSHAIR-LENGTH CROSSHAIR-LENGTH "solid" "light blue"))))

(define SLIDER-BG (rectangle BOX-WIDTH
                             BOX-HEIGHT
                             "solid"
                             "light blue"))
;; draw the slider
;; ws -> image
(define (draw-slider ws)
  (place-image crosshair
               (* (ws-frac-x ws)
                  BOX-WIDTH)
               (* (ws-frac-y ws)
                  BOX-HEIGHT)
               SLIDER-BG))

;(check-expect
; (draw-slider (make-ws 0.5 0.5 1234 120 69))
; (place-image crosshair 600 600 SLIDER-BG))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SINE SOUNDER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define MAX-HEIGHT 200)
(define MIN-HEIGHT 0)
(define MAX-FREQ 3000)
(define MIN-FREQ 50)

;takes a fraction representing a y position and returns
;the correspoding sound frequencies,
;ranging from the global constants MAX-FREQ to MIN-FREQ 
;Number -> Number
(check-expect (y-freq 0.5) 1525)

(define (y-freq y)
  (+ (* (- 1 y) (- MAX-FREQ MIN-FREQ)) MIN-FREQ))

;Accepts a vertical fraction and uses it to
;create a tone with a frequency corresponding to the
;given fraction
;ws -> rsound
(define (tone-maker frac-y)
  (make-tone (y-freq frac-y) 0.5 (round (/ PLAY-FRAMES 0.5))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DRUM SOUNDER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Constants
(define NOTE
  (resample-to-rate
   FRAME-RATE
   kick))

(define NOTE-LENGTH
  (rs-frames NOTE))

(define PAUSE-SCALE (* 1 FRAME-RATE))

;code

(define (pause-len frac-x)
  (* PAUSE-SCALE frac-x))

(define (song-len frac-x)
  (+ NOTE-LENGTH (pause-len frac-x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GUI MOUSE HANDLER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; handle a mouse action; on a drag or
;; click, update the slider position
;; ws number number event -> ws
(define (handle-mouse ws x y event)
  (cond [(string=? event "button-down")
         (make-ws (/ x BOX-WIDTH) (/ y BOX-HEIGHT)
                  (ws-song-frame ws)(ws-end-frame ws))]
        [(string=? event "drag")
         (make-ws (/ x BOX-WIDTH) (/ y BOX-HEIGHT)
                  (ws-song-frame ws)(ws-end-frame ws))]
        [else ws]))

;(check-expect
; (handle-mouse (make-ws 0.6 0.6 1234 120 69)
;               150 44 "move")
; (make-ws 0.6 0.6 1234 120 69))
;(check-expect
; (handle-mouse (make-ws 0.6 0.6 4243 120 69)
;               150 44 "drag")
; (make-ws (/ 150 BOX-WIDTH) 0.6 4243 120 69))
;(check-expect
; (handle-mouse (make-ws 0.6 0.6 1123 120 69)
;               150 44 "button-down")
; (make-ws (/ 150 BOX-WIDTH) 0.6 1123 120 69))

;NEED-TO-CHANGE
;; if it's time, queue up the next section
;; of the song
; ws -> ws
(define (tick-fun ws)
  (cond [(time-to-play? (ws-end-frame ws)
                        (pstream-current-frame ps))
         (if (> (song-len (ws-frac-x ws)) (ws-song-frame ws))
             (both
              (queue-next-fragment
               (ws-frac-y ws)
               (ws-song-frame ws) ;;FIX THIS
               (ws-end-frame ws))
              (make-ws (ws-frac-x ws)
                       (ws-frac-y ws)
                       (+ (ws-song-frame ws) PLAY-FRAMES)
                       (+ (ws-end-frame ws) PLAY-FRAMES)))
             (both
              (queue-next-fragment
               (ws-frac-y ws)
               0
               (ws-end-frame ws))
              (make-ws (ws-frac-x ws)
                       (ws-frac-y ws)
                       PLAY-FRAMES
                       (+ (ws-end-frame ws) PLAY-FRAMES))))]
        [else ws]))


;; queue up the next fragment of drums and tone
(define (queue-next-fragment frac-y song-frame frame-to-play)
  (if (< (+ song-frame PLAY-FRAMES) (rs-frames kick))
      (pstream-queue ps
                     (rs-overlay
                      (tone-maker frac-y)
                      (clip NOTE song-frame (+ song-frame PLAY-FRAMES)))
                      frame-to-play)
      (pstream-queue ps
                     (rs-overlay
                      (tone-maker frac-y)
                      (silence PLAY-FRAMES))
                      frame-to-play)))



;; how long should each queued segment be, in seconds?
(define PLAY-SECONDS 1/20)
;; .. in frames?
(define PLAY-FRAMES (* PLAY-SECONDS FRAME-RATE))
;; how long should the big-bang ticks be?
(define TICK-LEN 1/40)
;; the longest lead time for which we'll queue the next sound
(define MAX-QUEUE-INTERVAL (* 3/80 FRAME-RATE))

;; is it time to play the next chunk, yet?
; number number -> boolean
(define (time-to-play? end-frame cur-frame)
  (< (- end-frame cur-frame) MAX-QUEUE-INTERVAL))

;function that executes function a and returns value b
(define (both a b) b)

;Startup state for program
(define INITIAL-STATE
  (make-ws 0.5 0.5 0 PLAY-FRAMES))

;main function
(big-bang INITIAL-STATE
          [to-draw draw-slider]
          [on-mouse handle-mouse]
          [on-tick tick-fun TICK-LEN])