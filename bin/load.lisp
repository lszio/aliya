(ql:quickload :uiop)

(let ((home (or (uiop:getenv "ALIYA")
                #+os-windows "C:/Liszt/Aliya"
                #-os-windows "~/.aliya")))
  (asdf:load-asd (pathname (concatenate 'string
                                     home "/aliya.asd"))))
(ql:quickload :aliya)

(in-package :common-lisp-user)
(defun main()
  (aliya:hello))
