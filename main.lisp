(defpackage #:clock
  (:use :cl))

(ql:quickload "sdl2")
;;(ql:quickload "sdl2kit")
(ql:quickload "cl-opengl")

(in-package #:clock)

(require :sdl2)
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
(defvar *1second* 1000)
(defvar *1min-rad* (/ pi 30))
(defvar *1hour-rad* (/ pi 6))

(defparameter *clock-radius* (/ *window-height* 2))
(defparameter *zero-angle* (- (/ pi 2))) ; as in twelve-o-clock
(defparameter *min* 0)
(defparameter *hour* 0)

(defun mil-hour (hour)
  (cond
    ((zerop hour) 12)
    ((>= hour 13) (- hour 12))
    (t hour)))

(defun min->rad (min) (* min *1min-rad*))
(defun hour->rad (hour min)
  (if (>= min 60) hour
      (* (+ hour (/ min 60)) *1hour-rad*)))

(defun update-time ()
  (multiple-value-bind (_ min hour) (get-decoded-time)
    (setf *min* min
	  *hour* hour)))

(defun cis-sf (angle)
  (let ((c (cis angle)))
    (complex (coerce (realpart c) 'single-float)
	     (coerce (imagpart c) 'single-float))))

(defun values-of (type &rest args)
  (values-list
   (loop for a in args
	 collect (coerce a type))))

(defun drawline (renderer from to)
  (let ((x0 (vec2-v1 from))
	(y0 (vec2-v2 from))
	(x1 (vec2-v1 to))
	(y1 (vec2-v2 to)))
    (sdl2-ffi.functions:sdl-render-draw-line-f
     renderer x0 y0
     (+ x0 x1) (+ y0 y1))))

(defun mark-at-angle (angle)
  (let* ((magnitude 10)
	 (radius (- *padding* (/ *window-height* 2))) ; TODO: make window-height an argument
	 (offset (/ *window-height* 2))
	 (point (* radius (cis-sf angle)))
	 (x-start (+ offset (realpart point)))
	 (y-start (+ offset (imagpart point)))
	 (origin (make-vec2 :v1 offset :v2 offset))
	 (dir (*vec magnitude
		    (normalize
		     (-vec origin (make-vec2 :v1 x-start :v2 y-start)))))
	 (x-end (+ x-start (vec2-v1 dir)))
	 (y-end (+ y-start (vec2-v2 dir))))
    (values x-start y-start
	    x-end y-end)))

(defvar *mark-angles*
  (let ((step (/ pi 6)))
    (loop for n from 0 to 12
	  for a = (* n step)
	  :collect a)))

(defun draw-marks (renderer)
  (dolist (angle *mark-angles*)
    (multiple-value-bind (x0 y0 x y) (mark-at-angle angle)
      (sdl2-ffi.functions:sdl-render-draw-line-f
       renderer
       x0 y0
       x y))))

(defun draw-hand (renderer len angle win-h)
  (let* ((adjusted-angle (+ *zero-angle* angle))
	 (x0 (coerce (/ win-h 2) 'single-float))
	 (point (* (- len *padding*) (cis-sf adjusted-angle))))
    (drawline renderer
	      (make-vec2 :v1 x0 :v2 x0)
	      (make-vec2 :v1 (realpart point)
			 :v2 (imagpart point)))))

(defun draw-min-hand (renderer angle)
  (draw-hand renderer (* 80 (/ *window-height* 200)) angle *window-height*))

(defun draw-hour-hand (renderer angle)
  (draw-hand renderer (* 50 (/ *window-height* 200)) angle *window-height*))

;;; Main
(defparameter *window-width* 500)
(defparameter *window-height* 500)

(defparameter *padding* 0)
(defparameter *offset* (+ *padding* (/ *window-height* 2)))

(defun start-timer ()
  (let ((timer (sb-ext:make-timer #'update-time)))
    (sb-ext:schedule-timer timer 1.0 :repeat-interval 30.0)
    timer))

(defun stop-timers ()
  (dolist (timer (sb-ext:list-all-timers))
    (sb-ext:unschedule-timer timer)))

(defun printf (format-str &rest args)
  (destructuring-bind (a) args
      (print (format nil format-str a))))

;;; TODO: This is shit
(defun handle-window-event (w)
  (let ((size (sdl2:get-window-size w)))
    (setf *window-height* size
	  *window-width* size
	  *clock-radius* (round (/ size 2))
	  *offset* (+ *padding* (/ *window-height* 2)))))

(defun draw (wh min hour renderer)
  (let* ((h/2 (round (/ wh 2)))
	 (x0 h/2)
	 (y0 h/2)
	 (radius (- h/2 *padding*)))
    (draw-circle (lambda (x y) (sdl2:render-draw-point renderer x y)) x0 y0 radius)
    (draw-marks renderer)
    (draw-min-hand renderer (min->rad min))
    (draw-hour-hand renderer (hour->rad hour min))))

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "clock"
                        :w *window-width*
                        :h *window-height*
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
         ,@body))))

(defun run ()
  (update-time)
  ;; (start-timer)
  (with-window-renderer (window renderer)
    (sdl2:set-hint :render-scale-quality "1")
    (sdl2:with-event-loop (:method :poll)
      (:quit () t)
      (:wait () t)

      (:keydown (:keysym keysym)
		(case (sdl2:scancode keysym)
		  (:scancode-escape
		   (stop-timers)
		   ;; (sdl2:quit)
		   )))

      (:windowevent () (handle-window-event window))
      
      (:idle ()
	     (sdl2:set-render-draw-color renderer #x00 #x00 #x00 #x00)
	     (sdl2:render-clear renderer)
	     (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
	     (draw *window-height* *min* *hour* renderer)
	     (sdl2:delay 100)
	     (sdl2:render-present renderer)))))
