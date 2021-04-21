(in-package :aliya)

(defun hello ()
  (format t "~a ~a"
          "Hello"
          (uiop:getenv
           #+os-windows "USERNAME"
           #-os-windows "USER")))

(defun cli (&optional (command "help") &rest rest)
  (pprint (format nil "Command: ~A" command))
  (cond
    ((equalp command "install") 
     (let ((app (first rest)))
        (if (equal app "--list")
          (maphash #'(lambda (key value) (print key) (pprint value)) *apps*)
          (install-app app))))))

(defun install-app (name)
  (print (format nil "Installing ~A" name))
  (dispatch `(,name) '(:install)))

(defun main ())
