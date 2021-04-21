(in-package :aliya)

(defvar *home* (or (uiop:getenv "ALIYA")
                   "~/Aliya"))

(defparameter *apps* (make-hash-table :test #'equalp))

(defmacro defapp (name &key install remove start stop update cover)
  (let ((key (string-downcase (string name))))
    (print (format nil "Registering app ~A" key))
    (if (and (not cover) (gethash key *apps*))
      (print (format nil "App ~A already exists" key))
      `(setf (gethash ,key *apps*)
             (list :install ,install
                   :remove ,remove
                   :update ,update
                   :start ,start
                   :stop ,stop)))))
  

(defun dispatch (apps actions)
  (loop for name in apps
        for app = (gethash (string name) *apps*)
        do (if app
               (loop for action in actions
                     do (progn
                          (print (format nil "Eval ~A ~A..." name action))
                          (eval (getf app action))))
               (print (format nil "App ~A not found..." name)))))

(defun string->path (string)
  (let ((path (if (char= #\@ (char string 0))
                  (pathname (concatenate 'string *home* (subseq string 1)))
                  string)))
    path))


(defun remove-folder (path)
  (let ((folder (string->path path)))
    (if (uiop:directory-exists-p folder)
        (progn
          (print (format nil "Deleting ~A" (namestring folder)))
          (uiop:run-program (format nil "rm -rf ~A" (namestring folder))))
        (print (format nil "Directory ~A doesn't exists" (namestring folder))))))

(defun git-clone (repo path)
  (let ((dest (string->path path)))
    (if (uiop:directory-exists-p dest)
        (pprint (format nil "Directory ~A aliready exists..." path))
        (when (not
               (zerop
                (third
                 (multiple-value-list
                  (uiop:run-program (format nil "git clone ~A ~A" repo dest)
                                    :output *standard-output*
                                    :error-output *standard-output*
                                    :ignore-error-status t)))))
          (print "Cloning failed")
          (remove-folder path)))))

(defapp pyenv :install '(progn
                         (print "Install pyenv")
                         (git-clone "https://github.com/pyenv/pyenv" "@/app/pyenv"))
              :update '(print "Update pyenv")
              :remove '(progn
                        (print "Remove pyenv")
                        (remove-folder "@/app/pyenv"))
              :cover t)

(defapp nvm :install '(progn
                       (print "Install nvm")
                       (git-clone "https://gitee.com/mirrors/nvm.git" "@/app/nvm"))
  :update '(print "Update nvm")
  :remove '(progn
            (print "Remove nvm")
            (remove-folder "@/app/nvm"))
  :cover t)

(defun package-manager (action name &key scoop yay apt)
  (let ((mapper '(:yay (:install "-Ss" :remove "-Rcus")
                  :scoop (:install "install" :remove "uninstall")
                  :apt (:install "install" :remove "remove"))))
       (getf (getf mapper name) action)))