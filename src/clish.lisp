(defpackage :aliya.clish
  (:use :cl :uiop)
  (:export :repl))

(in-package :aliya.clish)

(defparameter *hist* 0)

(defun promot ()
  (format t "~A:[~D]>~%☰ " (uiop:getenv #+os-windows "USERNAME" #-os-windows "USER") *hist*)
  (setf *hist* (1+ *hist*)))

(defmacro handle-errors (&body body)
  `(handler-case (progn ,@body)
     (simple-condition (err)
       (format *error-output* "~&~A: ~%" (class-name (class-of err)))
       (apply (function format) *error-output*
              (simple-condition-format-control   err)
              (simple-condition-format-arguments err))
       (format *error-output* "~&"))
     (condition (err)
        (format *error-output* "~&~A: ~%  ~S~%"
               (class-name (class-of err)) err))))

(defun repl ()
  (promot)
  (loop for - = (read)
        while (not (position - '((quit) (exit)) :test #'equal))
        do (handle-errors
            (setf +++ ++ ++ + + -)
            (setf /// // // / / (multiple-value-list (eval -)))
            (setf *** ** ** * * (first /))
            (format t "~& --> ~{~S~^ ;~%     ~}~%" /)
            (promot))))

(defun read-shell-script (stream))

(defun shell-mode (stream char)
  (let ((script (read-line stream)));; FIXME need double #\NewLine
    (uiop:run-program script :ignore-error-status t :output :interactive)))

(set-macro-character #\! #'shell-mode)

(defun main ()
  (repl))
