(defpackage likit
  (:use :cl :inferior-shell :alexandria)
  (:export
   ;;core
   :when-match
   ;; path
   :filep :folderp :folder-exists :path :map-child
   ;; system
   :shell :command-exists-p :toggle-feature))
