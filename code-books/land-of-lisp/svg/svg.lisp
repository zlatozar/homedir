;;;; SVG library
(in-package #:chapter-17/svg)

(defmacro svg (&body body)
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
				   "xmlns:xlink" "http://www.w3.org/1999/xlink")
		,@body))

(defun brightness (color amount)
  (mapcar (lambda (x)
			(min 255 (max 0 (+ x amount))))
		  color))

(defun svg-style (color)
  (format nil
		  "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
		  (append color
				  (brightness color -100))))

(defun circle (centre radius color)
  (tag circle
	   (cx (car centre)
		   cy (cdr centre)
		   r radius
		   style (svg-style color))))

(defun polygon (points color)
  (tag polygon (points (format nil
							   "~{~a,~a ~}"
							   (mapcan (lambda (tp)
										 (list (car tp) (cdr tp)))
									   points))
					   style (svg-style color))))
