(defpackage aliya
  (:use :cl :uiop :py4cl)
  (:export :hello))
(in-package :aliya)

(defun hello ()
  (format t "~a ~a" "Hello" (uiop:getenv "USER")))
