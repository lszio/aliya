(defpackage ust
  (:use :cl)
  (:export :cli))

(in-package ust)

(defun load-file (path &optional (default '()))
  (if (probe-file path)
      (with-open-file (in path)
        (with-standard-io-syntax
          (format t "Load config from ~A~%" path)
          (read in)))
      default))

(defun to-relative (path &optional (base (probe-file "")))
  (setf path (uiop:ensure-absolute-pathname path (probe-file "")))
  (when (equal (pathname-device path)
               (pathname-device base))
    (let ((src (pathname-directory base))
          (tgt (pathname-directory path)))
      (loop for a in src
            for b in tgt
            while (equal a b)
            do (progn (pop src) (pop tgt)))
      (make-pathname :directory (append '(:relative)
                                        (mapcar (lambda (n) (declare (ignorable n)) :up) src)
                                        tgt)
                    :name (pathname-name path)
                    :type (pathname-type path)))))

(defun load-config ()
  (load-file
    #p"~/.config/ust/config"
    (list (list :commands
                (cons :lisp "ros")
                (cons :py "python")
                (cons :js "node")
                (cons :ts "node")
                (cons :jl "julia")
                (cons :pl "perl")
                (cons :go "go run")
                (cons :lua "lua")
                (cons :php "php")
                (cons :clj "clj -M")
                (cons :scm "guile --no-auto-compile")
                #-os-windows (cons :sh "bash")
                #+os-windows (cons :cmd "cmd")
                #+os-windows (cons :reg "powershell")
                #+os-windows (cons :bat "powershell")
                #+os-windows (cons :ps1 "powershell"))
          (cons :repos
            (list #p"~/.dotfiles/scripts/"
                  #p"~/.config/ust/scripts/"
                  #p"~/.scripts/")))))

(defun load-cache () (load-file #p"~/.config/ust/cache" '((:scripts))))

(defun save-file (content path)
  (uiop:ensure-all-directories-exist (list path))
  (with-open-file (out path :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (print content out))))

(defun save-config () (save-file *config* #p"~/.config/ust/config"))
(defun save-cache () (save-file *cache* #p"~/.config/ust/cache"))

(defparameter *config* (load-config))
(defparameter *cache* (load-cache))

(defun shell (&rest cmds)
  (let ((command (str:join ";" cmds)))
    (third
     (multiple-value-list
      (uiop:run-program
       (format nil "~A~A"
               #+os-windows "powershell $OLDPWD=pwd;"
               #-os-windows ""
               command)
       :input :interactive
       :output :interactive
       :ignore-error-status t
       :error-output :interactive)))))

(defun update-cache (key value)
  (setf (cdr (assoc key *cache*)) value))

(defun valid-script-p (path)
  (member (intern (string-upcase (pathname-type path)) 'keyword) (cdr (assoc :commands *config*)) :key #'car))

(defun scan-scripts (folder)
  (when (probe-file folder)
    (format t "Scanning ~A ~%" folder)
    (loop for path in (directory (make-pathname :name :wild :type :wild :defaults folder))
          for name = (pathname-name path)
          when (not name)
          collect (cons (car (last (pathname-directory path))) (scan-scripts path))
          when (and name (valid-script-p path))
          collect (cons name path))))

(defun this-script-p (key item)
  (let ((name (car key))
        (type (cadr key)))
    (and (equal name (car item))
         (or (not type) (equal type (and (pathnamep (cdr item)) (pathname-type (cdr item))))))))

(defun eval-script (path args)
  (let* ((type (intern (string-upcase (pathname-type path)) 'keyword))
         (cmd (cdar (member type (cdr (assoc :commands *config*)) :key #'car))))
    (if cmd
        (let ((script (format nil "~A ~A~{ ~A~}" cmd (or (to-relative path) path) args)))
          (format t "Eval: ~A ~%" script)
          (shell script))
        (format t "No available command for script ~A~%" path))))

(defun run-script (arguments &optional defaults)
  (let ((name (car arguments))
        (args (cdr arguments))
        (scripts (or defaults (cdr (assoc :scripts *cache*)))))
    (if name
        (let ((item (cdar (member (str:split #\. name) scripts :test #'this-script-p))))
          (if item
              (if (pathnamep item)
                  (eval-script item args)
                  (run-script args item))
              (if (member "default" scripts :test #'equal :key #'car)
                  (run-script (cons "default" arguments) scripts)
                  (format t "Script ~A not founded~%" name))))
        (print scripts))))

(defun list-scripts ()
  (update-cache :scripts
                (apply #'append
                       (loop for folder in (cdr (assoc :repos *config*))
                             for scripts = (scan-scripts folder)
                             when scripts
                             collect scripts)))
  (print (cdr (assoc :scripts *cache*))))

(defun display-script-info (&rest scripts)
  ;;TODO
  scripts)

(defun dispatch (action &rest scripts)
  (dolist (script scripts)
    (run-script (list script action))))

(defun check (&rest args)
  (when (and (not (eq (intern "LIST" 'command) (car args)))
             (not (cdr (assoc :scripts *cache*))))
    (format t "Empty scripts, scaning...~%")
    (list-scripts)))

(clish:defcli cli (:docs "Ust cli")
  (:default (lambda (&rest args) (run-script args)))
  (:before #'check)
  (:after (lambda (&rest args) (declare (ignore args)) (save-cache)))
  (info #'display-script-info)
  (list #'list-scripts)
  (update (lambda (&rest scripts) (apply #'dispatch (cons "update" scripts))))
  (install (lambda (&rest scripts) (apply #'dispatch (cons "install" scripts))))
  (uninstall (lambda (&rest scripts) (apply #'dispatch (cons "uninstall" scripts)))))
