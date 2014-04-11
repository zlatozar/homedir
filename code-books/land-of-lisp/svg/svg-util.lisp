(in-package #:chapter-17/svg-util)

(defmacro let1 (var val &body body)
  `(let ((,var ,val))
	 ,@body))

(defmacro split (val yes no)
  (let1 g (gensym)
	`(let1 ,g ,val
	   (if ,g
		   (let ((head (car ,g))
				 (tail (cdr ,g)))
			 ,yes)
		   ,no))))

(defun pairs (lst)
  (labels ((f (lst acc)
			 (split lst
					(if tail
						(f (cdr tail) (cons (cons head (car tail)) acc))
						(reverse acc))
					(reverse acc))))
	(f lst nil)))

(defun print-tag (name alst closingp)
  (princ #\<)
  (when closingp
	(princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
		  (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
		alst)
  (princ #\>))

(defmacro tag (name atts &body body)
  `(progn (print-tag ',name
					 (list ,@(mapcar (lambda (x)
									   `(cons ',(car x) ,(cdr x)))
									 (pairs atts)))
					 nil)
		  ,@body
		  (print-tag ',name nil t)))
