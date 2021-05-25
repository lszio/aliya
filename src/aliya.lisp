(in-package :aliya)

(defun hello ()
  (format t "~a ~a"
          "Hello"
          (uiop:getenv
           #+os-windows "USERNAME"
           #-os-windows "USER")))

(defun install-app (app)
  (dispatch (list app) '(:install)))

(defun remove-app (app)
  (dispatch (list app) '(:remove)))

(defun cli (&rest args)
  (aliya.clish:provide-cli
   (pairlis (list "install" "remove")
            (list #'install-app #'remove-app))
   args))


(defun main ())
