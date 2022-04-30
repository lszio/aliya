(defpackage loong
 (:use :cl))
(in-package :loong)

(defun find-all-asds (&optional (folder #p"./") (recursive t))
  (directory (merge-pathnames "**/*.asd" folder)))

(defun test-asd-systems (asd-files)
  (loop for asd in asd-files
        do (test-sustem-in-asd asd)))

(defparameter *system-asds* (find-all-asds))

(defun test-system-in-asd (file)
  (let ((name (pathname-name file)))
    (setf (gethash name asdf::*source-registry*) file)
    (asdf:test-system name)
    (remhash name asdf::*source-registry*)))

(test-system-in-asd #p"/home/liszt/Projects/Aliya/systems/clish/clish.asd")

(test-system-in-asd #p"/home/liszt/Projects/Aliya/systems/lisql/lisql.asd")

(defun test (&rest systems)
  ())

(defun list-systems ()
  (print *system-asds*))

(clish:defcli cli
  (test #'test)
  (list #'list-systems))

(asdf:test-system "lisql")
