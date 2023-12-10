(defpackage cln
  (:use :cl :clish)
  (:export
   :cli
   :main))

(in-package :cln)

(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
        (reduce #'(lambda (v f) (funcall f v))
                rest
                :initial-value (apply fn1 args)))))

;; FIXME: in separate package
(defun load-rc-file (path)
  (when (probe-file path)
    (with-open-file (in path)
      (with-standard-io-syntax
        (format t "Load config from ~A~%" path))
      (do ((f :start (read in nil nil))
           (v nil (eval f)))
          ((null f) v)))))

(defun plistp (lst)
  "test plist or not"
  (and (listp lst) (evenp (length lst))))

(defun valid-config-p (config)
  (and (listp config) (every #'plistp config)))

(defun parse-config (config)
  (if (valid-config-p config)
      (progn 
        (pprint config)
        config)))

(defun load-config-file (path)
  ;; TODO:
  (load-lisp-config path))

(defun load-lisp-config (path)
  (when (probe-file path)
    (with-open-file (in path)
      (with-standard-io-syntax (read in)))))

(defun string-to-pathname (string)
  (if (char= (char string 0) #\~)
      (merge-pathnames (subseq string 2) (user-homedir-pathname))
      (parse-namestring string)))

(defun load-configs (&rest paths)
  (apply #'append
         (loop for path in paths
               for p = (string-to-pathname path)
               for data = (load-config-file p)
               for parsed = (parse-config data)
               when parsed
                 collect parsed)))

(defparameter *configs* (load-configs ".clns"))

(defun main (&rest args)
  (declare (ignorable args))
  (format t "Hello Cln!!"))

(defun do-sync ()
  (format t "Sync")
  (let ((config (load-config-file #P"./.clns" "~/.dotfiles")))
    (pprint config)))

(defun do-list ()
  (pprint *configs*)
  (format t "List: ~%index name source target~%")
  (loop for item in *configs*
        and index from 0
        do (format t "~<~5A~> ~A ~<~6A~> -> ~A ~%" index (getf item :name) (getf item :source) (getf item :target))))

(defun do-link (&rest ids)
  (dolist (id ids)
    (pprint id)))

(defcli cli (:docs "common link tool")
  (sync #'do-sync)
  (list #'do-list)
  (do #'do-link)
  (nil #'main))
