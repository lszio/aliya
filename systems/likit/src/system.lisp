(in-package :likit)

(defun shell (string &rest arguments)
  (third
   (multiple-value-list
    (uiop:run-program
     (apply #'format
            (cons nil
                  (cons (format nil
                                "~A~A"
                                #+os-windows "powershell $OLDPWD=pwd;"
                                #-os-windows "" string)
                        arguments)))
     :ignore-error-status t
     :output :interactive
     :input :interactive
     :error-output :interactive))))

(defun toggle-feature (feature &optional (action t action-supplied-p))
  (let* ((p (position feature cl:*features*))
         (on (and (not p) (or (not action-supplied-p)
                              (and action action-supplied-p))))
         (off (and p (or (not action-supplied-p)
                         (and (not action) action-supplied-p)))))
    (cond
      (on (setf cl:*features* (cons feature cl:*features*)))
      (off (setf cl:*features* (remove feature cl:*features*)))
      (t cl:*features*))))

(defun command-exists-p (command)
  (let ((code
         (third
          (multiple-value-list
           (uiop:run-program
            (format nil "~A ~A"
                    #+os-windows "powershell -Command Get-Command"
                    #-os-windows "command -v"
                    command)
            :ignore-error-status t)))))
    (zerop code)))
