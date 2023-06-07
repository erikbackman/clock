(defpackage #:clock
  (:use :cl)
  (:export :run
	   :*padding*))

(in-package #:clock)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "sdl2")
  (ql:quickload "sdl2-ttf")
  (ql:quickload "cl-opengl"))

(require :sdl2)
(require :sdl2-ttf)
(require :cl-opengl)

;;;

(defun values-of (type &rest args)
  (values-list
   (loop for a in args
	 collect (coerce a type))))

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
(defun draw-circle-polar (draw-function x0 y0 radius)
  (loop for n from 0 to 360
	for angle = (/ (* n 3.142) 180)
	do (funcall draw-function
		    (+ x0 (* radius (cos angle)))
		    (+ y0 (* radius (sin angle))))))

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
(defvar *zero-angle* (- (/ pi 2))) ; as in twelve-o-clock

(defparameter *padding* 0)

(defun mil-hour (hour)
  (cond
    ((zerop hour) 12)
    ((>= hour 13) (- hour 12))
    (t hour)))

(defun min->rad (min) (* min *1min-rad*))
(defun hour->rad (hour min)
  (if (>= min 60) hour
      (* (+ hour (/ min 60)) *1hour-rad*)))

(defun cis-sf (angle)
  (let ((c (cis angle)))
    (complex (coerce (realpart c) 'single-float)
	     (coerce (imagpart c) 'single-float))))

(defun drawline (renderer from to)
  (let ((x0 (vec2-v1 from))
	(y0 (vec2-v2 from))
	(x1 (vec2-v1 to))
	(y1 (vec2-v2 to)))
    (sdl2-ffi.functions:sdl-render-draw-line-f
     renderer x0 y0
     (+ x0 x1) (+ y0 y1))))

(defun mark-at-angle (angle win-h)
  (let* ((magnitude 10)
	 (radius (- *padding* (/ win-h 2)))
	 (offset (/ win-h 2))
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

(defun draw-marks (renderer win-h)
  (dolist (angle *mark-angles*)
    (multiple-value-bind (x0 y0 x y) (mark-at-angle angle win-h)
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

(defun draw-min-hand (renderer angle win-h)
  (draw-hand renderer (* 80 (/ win-h 200)) angle win-h))

(defun draw-hour-hand (renderer angle win-h)
  (draw-hand renderer (* 50 (/ win-h 200)) angle win-h))

;;; Main
(defun timer-exists? ()
    (member 'clock-timer (sb-ext:list-all-timers) :test #'equal :key #'sb-ext:timer-name))

(defun start-timer (callback)
  (unless (timer-exists?)
    (let ((timer (sb-ext:make-timer callback :name 'clock-timer)))
      (sb-ext:schedule-timer timer 1.0 :repeat-interval 30.0)
      timer)))

(defun stop-timers ()
  (dolist (timer (sb-ext:list-all-timers))
    (when (equal (sb-ext:timer-name timer) 'clock-timer)
      (sb-ext:unschedule-timer timer))))

(defun printf (format-str &rest args)
  (destructuring-bind (a) args
    (print (format nil format-str a))))

(defun draw (wh min hour renderer)
  (let* ((h/2 (round (/ wh 2)))
	 (x0 h/2)
	 (y0 h/2)
	 (radius (- h/2 *padding*)))
    (draw-circle (lambda (x y) (sdl2:render-draw-point renderer x y)) x0 y0 radius)
    (draw-marks renderer wh)
    (draw-min-hand renderer (min->rad min) wh)
    (draw-hour-hand renderer (hour->rad hour min) wh)))

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:set-hint :render-scale-quality "1")
     (sdl2:with-window (,window
			:title "clock"
			:w 500
			:h 500
			:flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
	 ,@body))))

(defun start-clock ()
  (let ((min 0)
	(hour 0)
	(win-h 500))

    (flet ((update-time ()
	     (multiple-value-bind (_ m h) (get-decoded-time)
	       (declare (ignore _))
	       (setf min m
		     hour h))))
      (update-time)
      (start-timer #'update-time))

    (with-window-renderer (window renderer)
      (sdl2:with-event-loop (:method :poll)
	(:quit () t)
	(:wait () t)
	(:keydown (:keysym keysym)
		  (case (sdl2:scancode keysym)
		    (:scancode-escape
		     (stop-timers))))
	(:windowevent () (setf win-h (sdl2:get-window-size window)))
	(:idle ()
	       (sdl2:set-render-draw-color renderer #x00 #x00 #x00 #x00)
	       (sdl2:render-clear renderer)
	       (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
	       (draw win-h min hour renderer)
	       (sdl2:delay 100)
	       (sdl2:render-present renderer))))))

(defun run () (start-clock))
