(in-package :clish)

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

(defcli config-cli
  (all #'load-configs)
  (get #'get-config)
  (set #'set-config))
