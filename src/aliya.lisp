(defpackage aliya
  (:use :cl :uiop)
  (:export :hello))
(in-package :aliya)

(defun hello ()
  (format t "~a ~a" "Hello" (uiop:getenv #+os-windows "USERNAME" #-os-windows "USER")))
