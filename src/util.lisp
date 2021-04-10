(defpackage aliya.util
  (:use :cl))

(in-package :aliya.util)

(defun join (str list)
  (if (null list)
      ""
    (let ((result (first list)))
      (dolist (item (cdr list))
        (setf result (concatenate 'string result str item)))
      result)))

;; TODO
(defun split (str splitter))
