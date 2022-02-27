(in-package :likit)

(defmacro when-match ((target &key (test #'equal)) &rest forms)
  `(progn ,@(apply #'append
                   (loop for form in forms
                         when (intersection (eval target) (car form) :test test)
                         collect (cdr form)))))

(defun get-system-info ()
  "Get system revelent infos"
  ;; set features
  (push :likit cl:*features*)

  #+os-unix
  (loop for distro in (run/lines "grep -Pho '[a-zA-Z ]*(?= Linux)' /etc/*-release | awk '!a[$0]++{print}'")
        for key = (intern (string-upcase distro) 'keyword)
        do (push key cl:*features*))

  #-os-windows
  (when (uiop:getenv "WSL_DISTRO_NAME")
    (push (intern "WSL" 'keyword) cl:*features*))

  #-os-windows
  (loop for name in (run/lines (format nil "cat /etc/passwd | grep -P '~A:' | grep -Pho '[a-z]*$'" (UIOP:getenv "USER")))
        for key = (intern (string-upcase name) 'keyword)
        do (push key cl:*features*))

  (when (equal "LISZT" (string-upcase (uiop:getenv "USER")))
    (push :me cl:*features*)))

(get-system-info)
