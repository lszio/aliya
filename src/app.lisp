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

;; #-os-windows
(defapp ohmyzsh :install '(progn
                       (print "Install ohmyzsh")
                       (uiop:run-program (format nil "~A;~A;~A"
                                                 "export ZSH=$ALIYA/app/ohmyzsh"
                                                 "export RUNZSH=no"
                                                 "sh -c \"$(curl -fsSL https://gitee.com/mirrors/oh-my-zsh/raw/master/tools/install.sh)\"")
                                         :output *standard-output*
                                         :error-output *standard-output*)
                       (let ((profile (merge-pathnames ".zshrc" (user-homedir-pathname))))
                         (when (uiop:directory-exists-p profile)
                            (rename-file profile (merge-pathnames ".zshrc.bak" (user-homedir-pathname))))
                         (with-open-file (out profile :direction :output :if-exists :supersede)
                          (with-standard-io-syntax
                            ;; TODO Optimise
                            (princ (format nil "source ~A" (concatenate 'string *home* "/etc/entry")) out)
                            )))
                       (git-clone "https://github.com/reobin/typewritten.git" "@/app/ohmyzsh/themes/typewritten")
                       (uiop:run-program (format nil "ln -s ~A ~A" (concatenate 'string
                                                                                *home*
                                                                                "/app/ohmyzsh/themes/typewritten/typewritten.zsh-theme")
                                                                   (concatenate 'string
                                                                                *home*
                                                                                "/app/ohmyzsh/themes/typewritten.zsh-theme")))
                       (git-clone "https://github.com/zsh-users/zsh-autosuggestions" "@/app/ohmyzsh/plugins/zsh-autosuggestions")
                       (git-clone "https://github.com/zsh-users/zsh-syntax-highlighting" "@/app/ohmyzsh/plugins/zsh-syntax-highlighting")
                        )
  :update '(print "Update ohmyzsh")
  :remove '(progn
            (print "Remove ohmyzsh")
            (remove-folder "@/app/ohmyzsh"))
  :cover t)

(defapp lemacs :install '(progn
                       (print "Install lemacs")
                       (git-clone "https://github.com/Liszt21/Lemacs" "@/app/lemacs"))
  :update '(print "Update lemacs")
  :remove '(progn
            (print "Remove lemacs")
            (remove-folder "@/app/lemacs"))
  :cover t)

(defun package-manager (action name &key scoop yay apt)
  (let ((mapper '(:yay (:install "-Ss" :remove "-Rcus")
                  :scoop (:install "install" :remove "uninstall")
                  :apt (:install "install" :remove "remove"))))
       (getf (getf mapper name) action)))

       