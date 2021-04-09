(defpackage aliya
  (:use :cl :uiop)
  (:export :hello :cli))
(in-package :aliya)

(defun hello ()
  (format t "~a ~a"
          "Hello"
          (uiop:getenv
           #+os-windows "USERNAME"
           #-os-windows "USER")))

(defun cli (command &rest rest)
  (print (format nil "Command: ~A" command)))
