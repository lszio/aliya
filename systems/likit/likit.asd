(defsystem "likit"
  :version "0.1.0"
  :author "Liszt21"
  :license ""
  :depends-on ("str" "inferior-shell" "alexandria")
  :serial t
  :components ((:module "src"
                :components
                ((:file "likit")
                 (:file "core")
                 (:file "path")
                 (:file "list")
                 (:file "help")
                 (:file "system"))))
  :description ""
  :in-order-to ((test-op (test-op "likit/tests"))))

(defsystem "likit/tests"
  :author "Liszt21"
  :license ""
  :depends-on ("likit"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for likit"
  :perform (test-op (op c) (symbol-call :rove :run c)))
