(ql:quickload :uiop)

(let ((home (or (uiop:getenv "ALIYA")
                "~/Aliya")))
  (asdf:load-asd (pathname (concatenate 'string
                                        home "/aliya.asd"))))
(ql:quickload :aliya)

(in-package :common-lisp-user)

(defun main()
  (aliya:cli))
