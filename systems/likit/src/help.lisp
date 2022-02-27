(in-package :likit)

(defun package-help (package)
  ;; TODO
  (format t "help"))

(defun package-symbols (package)
  (loop for s being the external-symbols of package
        collect s))

(defun helper (object)
  ;; TODO
  (format t ""))

