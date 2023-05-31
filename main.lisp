(defpackage #:clock
  (:use :cl))

(ql:quickload "sdl2")
(ql:quickload "sdl2kit")
(ql:quickload "cl-opengl")

(in-package #:clock)

(require :sdl2)
(require :sdl2kit)
(require :cl-opengl)

(defun draw-circle (draw-function x0 y0 radius)
  (labels ((f (x y)
             (funcall draw-function x y))
           (put (x y m)
             (let ((x+ (+ x0 x))
                   (x- (- x0 x))
                   (y+ (+ y0 y))
                   (y- (- y0 y))
                   (x0y+ (+ x0 y))
                   (x0y- (- x0 y))
                   (xy0+ (+ y0 x))
                   (xy0- (- y0 x)))
               (f x+ y+)
               (f x+ y-)
               (f x- y+)
               (f x- y-)
               (f x0y+ xy0+)
               (f x0y+ xy0-)
               (f x0y- xy0+)
               (f x0y- xy0-)
               (multiple-value-bind (y m) (if (plusp m)
                                              (values (1- y) (- m (* 8 y)))
                                              (values y m))
                 (when (<= x y)
                   (put (1+ x)
                        y
                        (+ m 4 (* 8 x))))))))
    (put 0 radius (- 5 (* 4 radius)))
    (values)))

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defparameter *clock-radius* 200)

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "SDL2 Tutorial 07"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
         ,@body))))

(defun run ()
  (with-window-renderer (window renderer)

    (defun draw-point (x y)
      (sdl2:render-draw-point renderer x y))
    
    (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)

    (sdl2:with-event-loop (:method :poll)
      (:quit () t)
      (:idle ()
	     (sdl2:set-render-draw-color renderer #x00 #x00 #x00 #x00)
	     (sdl2:render-clear renderer)

	     (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
	     (draw-circle #'draw-point (/ *screen-width* 2) (/ *screen-height* 2) *clock-radius*)
             (sdl2:render-draw-line renderer
                                    0
                                    (/ *screen-height* 2)
                                    *screen-width*
                                    (/ *screen-height* 2))
	     (sdl2:render-present renderer)))))
