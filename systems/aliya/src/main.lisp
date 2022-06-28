(defpackage aliya
  (:use :cl)
  (:export
   :cli
   :main))

(in-package aliya)

(defun main (&rest args)
  (declare (ignorable args))
  (format t "Hello Aliya!!!"))

(clish:defcli cli (:docs "Aliya")
  (clish #'clish:cli)
  (emacy #'emacy:cli)
  (loong #'loong:cli)
  (ust #'ust:cli)
  (nil #'main))
