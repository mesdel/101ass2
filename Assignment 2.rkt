;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Assignment 2|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require rsound)
(require 2htdp/image)
(require 2htdp/universe)

;; a ws is
;; - (make-ws slider-frac end-frame)
;; where hori-frac and vert-frac are numbers between
;; zero and one that correspond to x and y values
; respectively, and end-frame is a frame
;; number
(define-struct ws [hori-frac vert-frac end-frame])

(define ps (make-pstream))

;; examples:
(make-ws 0.5 0.5 2000)
(make-ws 0.7 0.7 80000)

(define INITIAL-STATE
  (make-ws 0.5 0.5 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GUI constants and crosshair definition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define BOX-WIDTH 1200)
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
               (* (ws-hori-frac ws)
                  BOX-WIDTH)
               (* (ws-vert-frac ws)
                  BOX-HEIGHT)
               SLIDER-BG))

;(check-expect
; (draw-slider (make-ws 0.5 0.5 1234 120 69))
; (place-image crosshair 600 600 SLIDER-BG))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GUI MOUSE HANDLER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; handle a mouse action; on a drag or
;; click, update the slider position
;; ws number number event -> ws
(define (handle-mouse ws x y event)
  (cond [(string=? event "button-down")
         (make-ws (/ x BOX-WIDTH) (/ y BOX-HEIGHT)
                  (ws-end-frame ws))]
        [(string=? event "drag")
         (make-ws (/ x BOX-WIDTH) (/ y BOX-HEIGHT)
                  (ws-end-frame ws))]
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
         (both
          (queue-next-fragment (ws-hori-frac ws)
           (ws-end-frame ws))
          (make-ws (ws-hori-frac ws) (ws-vert-frac ws)
                   (+ (ws-end-frame ws) PLAY-FRAMES))
          )]
        [else ws]))

;NEED-TO-CHANGE
;; queue up the next fragment
(define (queue-next-fragment tempo frame-to-play)
  (pstream-queue ps drumroll frame-to-play))

;; how long should each queued segment be, in seconds?
(define PLAY-SECONDS 1/20)
;; .. in frames?
(define PLAY-FRAMES (* PLAY-SECONDS FRAME-RATE))
;; how long should the big-bang ticks be?
(define TICK-LEN 1/40)
;; the longest lead time for which we'll queue the next sound
(define MAX-QUEUE-INTERVAL (* 3/80 FRAME-RATE))

(define drumroll (silence 1)) ;place-holder

;; is it time to play the next chunk, yet?
; number number -> boolean
(define (time-to-play? end-frame cur-frame)
  (< (- end-frame cur-frame) MAX-QUEUE-INTERVAL))

;function that executes function a and returns value b
(define (both a b) b)

(big-bang INITIAL-STATE
          [to-draw draw-slider]
          [on-mouse handle-mouse]
          [on-tick tick-fun TICK-LEN])

;Accepts y value of mouse and makes sine wave with freq
;proportional to the given y value (will be called to change pitch, starts at 440 Hz)
;Number -> Sine Wave

(define (Sin440 ws)
  (sin (* 2 pi 400 (/ ws 44100)))) ;;Math is wrong

(define (sinesnd ws)
  (signal->rsound 44100 (indexed-signal Sin440))) ;;Makes sine 
