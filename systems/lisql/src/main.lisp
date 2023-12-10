(defpackage lisql
  (:use :cl :access :esrap))
(in-package :lisql)

(defun pathname-parents (path)
  "Get parents of path"
  (let ((directory '()))
    (reverse (loop for name in (pathname-directory path)
                   do (push name directory)
                   collect (make-pathname
                            :host (pathname-host path)
                            :device (pathname-device path)
                            :directory (reverse directory))))))

(defun detect-configs (&key (name ".clish") (folders '(#p"~/Sync/")))
  "Detect configs"
  (labels ((fn (path) (merge-pathnames name path)))
    (remove-if-not #'probe-file
                   (append (mapcar #'fn (pathname-parents (probe-file ".")))
                           (mapcar #'fn folders)
                           (mapcar #'fn '(#p"~/.config/" #p"~/Dotfiles/" #p"~/.dotfiles/"))))))

(defun load-configs (&key (name ".clish") folders)
  "Load configs"
  (loop for file in (detect-configs :name name :folders folders)
        for config = (with-open-file (in file)
                       (with-standard-io-syntax
                         (read in)))
        collect (cons (cons :path file) config)))

(defun get-config (key &key (name ".clish") test)
  "get all configs when key match"
  (let ((results
            (loop for config in (load-configs :name name)
                  for path = (cdr (assoc :path config))
                  for value = (cdr (assoc key config))
                  when value
                  collect (cons path value))))
    (values (if test (cdar (member-if test results :key #'car)) (cdar results)) results)))

(defun set-config (key value &rest args &key (section ".clish") (folder #p"."))
  "set config to current folder"
  (let* ((file (merge-pathnames section folder))
         (config (if (probe-file file) (with-open-file (in file) (read in)))))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (print (acons key value config) out))))

;; Readers
(defun read-lisp-file (path)
  (format nil "Lisp")
  (with-open-file (in path) (with-standard-io-syntax (read in))))

(defun read-yaml-file (path)
  (format nil "Yaml"))

(defun type-matched (type &rest types)
  (member type types :test #'equal))

(defun read-config-file (path)
  (cons path (let ((type (pathname-type path)))
               (cond
                 ((type-matched type "lisp") (read-lisp-file path))
                 ((type-matched type "yaml" "yml") (read-yaml-file path))))))

(defun load-file-metadata (file)
  (when (probe-file file)
    (pairlis '(:name :directory :path :type)
             (list (pathname-name file)
                   (pathname-directory file)
                   file
                   (pathname-type file)))))

;; locator
(defun detect-configs (&key (name "config.lisp") (folders '(#p"~/Sync/")))
  (labels ((fn (path) (merge-pathnames name path)))
    (remove-if-not #'probe-file
                   (append (mapcar #'fn (pathname-parents (probe-file ".")))
                           (mapcar #'fn folders)
                           (mapcar #'fn '(#p"~/.config/" #p"~/Dotfiles/" #p"~/.dotfiles/"))))))

(defun load-configs (&optional (files (detect-configs)))
  (loop for file in files collect (read-config-file file)))

(defparameter *db* '())

(setf *db* (acons :configs (load-configs) *db*))

*db*

(defun alist->vector (alist &key (key #'car) (value #'cdr))
  (make-array (length alist) :initial-contents
              (mapcar value alist)
              :adjustable t))

(defun vector->alist (vector &key key value)
  (let ((n -1))
    (flet ((fn (s i)
             (acons (if key (funcall key i) (incf n))
                    (if value (funcall value i) i)
                    s)))
      (reverse (reduce #'fn vector :initial-value '())))))

(vector->alist #(1 2 3 4))

(vector->alist (alist->vector (accesses *db* :configs)))

(alist->vector *db*)

(defparameter v (make-array 1 :adjustable t :fill-pointer 0))

