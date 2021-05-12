(in-package :aliya)

(defun hello ()
  (format t "~a ~a"
          "Hello"
          (uiop:getenv
           #+os-windows "USERNAME"
           #-os-windows "USER")))

(defun cli (&optional (command "help") &rest rest)
  (cond
    ((equalp command "install") 
     (let ((app (first rest)))
        (if (equal app "--list")
          (maphash #'(lambda (key value) (print key) (pprint value)) *apps*)
          (dispatch (list app) '(:install)))))
    ((equalp command "remove")
     (let ((app (first rest)))
       (if (equal app "--list")
           (pprint nil)
           (dispatch (list app) '(:remove)))))
    (t
     (format t "Help~%Commands:~%  install~%  remove~%"))))

(defun main ())
