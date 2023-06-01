(defpackage #:clock
  (:use :cl))

(ql:quickload "sdl2")
;(ql:quickload "sdl2kit")
(ql:quickload "cl-opengl")

(in-package #:clock)

(require :sdl2)
;(require :sdl2kit)
(require :cl-opengl)

;;; Vector stuff
(defstruct vec2 v1 v2)

(defun sqr (n)
  (expt n 2))

(defun magnitude (vec)
  (sqrt (+ (sqr (vec2-v1 vec))
	   (sqr (vec2-v2 vec)))))

(defun *vec (n vec)
  (make-vec2 :v1 (* n (vec2-v1 vec))
	     :v2 (* n (vec2-v2 vec))))

(defun -vec (v1 v2)
  (make-vec2 :v1 (- (vec2-v1 v1) (vec2-v1 v2))
             :v2 (- (vec2-v2 v1) (vec2-v2 v2))))

(defun normalize (vec)
  (*vec (/ 1 (magnitude vec)) vec))

;;; Draw functions
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

;;; Clock
(defparameter *1second* 1000)

(defun mark-at-angle (angle)
  (let* ((magnitude 10)
	 (radius (/ *screen-height* 2))
	 (offset (/ *screen-height* 2))
	 (x-start (+ offset (* radius (cos angle))))
	 (y-start (+ offset (* radius (sin angle))))
	 (origin (make-vec2 :v1 offset :v2 offset))
	 (dir (*vec magnitude
		    (normalize
		     (-vec origin (make-vec2 :v1 x-start :v2 y-start)))))
	 (x-end (+ x-start (vec2-v1 dir)))
	 (y-end (+ y-start (vec2-v2 dir))))
    (values (coerce x-start 'single-float)
	    (coerce y-start 'single-float)
	    (coerce x-end 'single-float)
	    (coerce y-end 'single-float))))

(defparameter *mark-angles*
  (loop for n from 0 to 12
	for a = (* n (/ pi 6))
	:collect a))

(defun drawline (renderer from to)
  (let ((x0 (vec2-v1 from))
	(y0 (vec2-v2 from))
	(x1 (vec2-v1 to))
	(y1 (vec2-v2 to)))
    (sdl2-ffi.functions:sdl-render-draw-line-f
     renderer
     (coerce x0 'single-float)
     (coerce y0 'single-float)
     (coerce (+ x0 x1) 'single-float)
     (coerce (+ y0 y1) 'single-float))))

(defun draw-mark-at-angle (renderer angle)
  (multiple-value-bind (x0 y0 x y)
      (mark-at-angle angle)
    (sdl2-ffi.functions:sdl-render-draw-line-f renderer x0 y0 x y)))

(defun draw-marks (renderer)
  (dolist (angle *mark-angles*)
    (draw-mark-at-angle renderer angle)))

(defun draw-hand (renderer len angle win-h)
  (let ((adjusted-angle (+ *zero-angle* angle))
	(x0 (/ win-h 2)))
    (drawline renderer
	      (make-vec2 :v1 x0 :v2 x0)
	      (make-vec2
	       :v1 (* len (cos adjusted-angle))
	       :v2 (* len (sin adjusted-angle))))))

(defun draw-min-hand (renderer angle)
  (draw-hand renderer (* 80 (/ *screen-height* 200)) *min-angle* *screen-height*))

(defun draw-hour-hand (renderer angle)
  (draw-hand renderer (* 50 (/ *screen-height* 200)) angle *screen-height*))

;;; Main
(defparameter *screen-width* 400)
(defparameter *screen-height* 400)
(defparameter *clock-radius* 200)
(defparameter *origin-x* 200)
(defparameter *origin-y* 200)
(defparameter *zero-angle* (- (/ pi 2)))
(defparameter *min-angle* (/ pi 4))

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "clock"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
         ,@body))))

(defun update-time ()
  (multiple-value-bind (sec min hour) (get-decoded-time))
  (setf *min-angle* (+ *min-angle* (/ pi 6))))

(defun start-timer ()
  (sb-ext:schedule-timer (sb-ext:make-timer #'update-time) 1.0 :repeat-interval 1.0))

(defun stop-timers ()
  (dolist (timer (sb-ext:list-all-timers))
    (sb-ext:unschedule-timer timer)))

(defun run ()
  (start-timer)
  (with-window-renderer (window renderer)
    (sdl2:with-event-loop (:method :poll)
      (:quit () t)
      (:wait () t)

      (:keydown (:keysym keysym)
		(case (sdl2:scancode keysym)
		  (:scancode-escape
		   (stop-timers)
		   ;; (sdl2:quit)
		   )))
      
      (:idle ()
	     (sdl2:set-render-draw-color renderer #x00 #x00 #x00 #x00)
	     (sdl2:render-clear renderer)
	     (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
	     
	     (draw-circle (lambda (x y)
			    (sdl2:render-draw-point renderer x y))
			  200 200 *clock-radius*)

	     (draw-marks renderer)
	     (draw-min-hand renderer (/ pi 4))
	     (draw-hour-hand renderer 0)
	     
	     (sdl2:delay 80)
	     (sdl2:render-present renderer)))))
