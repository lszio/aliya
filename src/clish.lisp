(defpackage :aliya.clish
  (:use :cl :uiop)
  (:export :repl :provide-cli))

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
  (do ()
      (nil)
    (promot)
    (setf +++ ++ ++ + + - - (read))
    (when (position - '((quit) (exit)) :test #'equal)
      (format t "Exit...")
      (return-from repl))
    (setf /// // // / / (multiple-value-list (eval -)))
    (setf *** ** ** * * (first /))
    (format t "~& --> ~{~S~^ ;~%     ~}~%" /)))
    
(defun read-shell-script (stream)
  (loop for c = (read-char)
        while (not (char= c #\NewLine))
        collect c))

(defun shell-mode (stream char)
  (let ((script (read-line stream)));; FIXME need double #\NewLine
    (uiop:run-program script :ignore-error-status t :output :interactive)))

(set-macro-character #\! #'shell-mode)

(defun main ()
  (repl))
  
(defun parse-params (params)
  (mapcar (lambda (param) (if (position #\- param)
                            (intern (string-upcase (string-left-trim '(#\- ) param)) :keyword)
                            param))
          params))
         
(parse-params (list "aa" "-aa" "--aaa"))

(defun function-helper (fn)
  (let ((params (second (function-lambda-expression fn))))
    (pprint params)))

(defun provide-cli (command-alist argument-list &optional (name "aliya"))
  (let* ((command (first argument-list))
         (args (parse-params (cdr argument-list)))
         (fn (cdr (assoc command command-alist :test #'equal))))
    (when (or (equal command "help") (member :help args) (not fn))
      (let ((target (if (equal command "help") (first args) command)))
        (if (member target command-alist :test #'equal :key #'car)
          (function-helper fn)
          (progn
            (format t "Help for ~A~%  Commands: ~%" name)
            (dolist (item command-alist)
              (format t "    ~A" (car item))
              (function-helper (cdr item))
              (format t "~%")))))
      (return-from provide-cli))
    (apply fn args)))
