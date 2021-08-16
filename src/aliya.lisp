(in-package :aliya)

(defun hello ()
  (format t "~a ~a"
          "Hello"
          (uiop:getenv
           #+os-windows "USERNAME"
           #-os-windows "USER")))

(defun install-app (apps &key (list nil) (source nil))
  (when list
      (maphash #'(lambda (key value)
                   (print key)
                   (when source (pprint value))) *apps*)
      (return-from install-app))
  (dispatch apps '(:install)))

(defun remove-app (apps)
  (dispatch apps '(:remove)))

(defun cli (&rest args)
  (provide-cli
   (pairlis (list "remove" "install")
            (list #'remove-app #'install-app))
   args))


(defun main ())

