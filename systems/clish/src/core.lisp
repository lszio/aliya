(in-package :clish)

(defcli cli (:docs "Clish: command line interafce for common lisp")
  (config #'config-cli)
  (:default #'repl))
