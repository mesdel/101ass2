;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |GUI Component|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rsound)
(require 2htdp/image)
(require 2htdp/universe)

;; a ws is
;; - (make-ws slider-frac end-frame)
;; where slider-frac is a number between
;; zero and one, and end-frame is a frame
;; number
(define-struct ws [hori-frac vert-frac end-frame tempo pitch])

(define INITIAL-STATE
  (make-ws 0.5 0.5 0 120 69))

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

;; handle a mouse action; on a drag or
;; click, update the slider position
;; ws number number event -> ws
(define (handle-mouse ws x y event)
  (cond [(string=? event "button-down")
         (make-ws (/ x BOX-WIDTH) (/ y BOX-HEIGHT)
                  (ws-end-frame ws)(ws-tempo ws) (ws-pitch ws))]
        [(string=? event "drag")
         (make-ws (/ x BOX-WIDTH) (/ y BOX-HEIGHT)
                  (ws-end-frame ws) (ws-tempo ws) (ws-pitch ws))]
        [else ws]))

(define (tick-fun ws)
  (cond [(time-to-play? (ws-end-frame ws)
                        (pstream-current-frame ps))
         (both
          (queue-next-fragment (ws-tempo ws)
           (ws-end-frame ws))
          (make-ws (ws-hori-frac ws) (ws-vert-frac ws)
                   (+ (ws-end-frame ws) PLAY-FRAMES) (ws-tempo ws) (ws-pitch ws))
          )]
        [else ws]))