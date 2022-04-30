(defsystem "lisql"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "lisql/tests"))))

(defsystem "lisql/tests"
  :author ""
  :license ""
  :depends-on ("lisql"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for lisql"
  :perform (test-op (op c) (symbol-call :rove :run c)))
