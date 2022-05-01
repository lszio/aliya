(defpackage loong
 (:use :cl)
 (:export :cli))
(in-package :loong)

(defun find-all-asds (&optional (folder #p"./") (recursive t))
  (directory (merge-pathnames "**/*.asd" folder)))

(defparameter *system-asds* (find-all-asds))

(defun test-system-in-asd (file)
  (let ((name (pathname-name file)))
    (setf (gethash name asdf::*source-registry*) file)
    (format t "Testing ~A..." name)
    (let ((result
            (handler-case (asdf:test-system name)
              (error nil))))
      (remhash name asdf::*source-registry*)
      result)))

(defun test-asd-systems (asd-files)
  (loop for asd in asd-files
        collect (format nil "~20<System ~A:~; ~A~>"
                        (pathname-name asd)
                        (if (test-system-in-asd asd)
                          "Pass"
                          "Fail"))))

(defun name->path (name)
  (car (member name *system-asds* :key #'pathname-name :test #'equal)))

(defun test-systems (&rest args)
  (let ((systems (if args (mapcar #'name->path args) *system-asds*)))
    (format nil "~{~A~%~}" (test-asd-systems systems))))

(defun list-systems ()
  *system-asds*)

(clish:defcli cli
  (test #'test-systems)
  (list #'list-systems))
