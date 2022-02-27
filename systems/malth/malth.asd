(defsystem "malth"
  :version "0.0.0"
  :author "Liszt21"
  :license ""
  :depends-on ()
  :serial t
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "malth/tests"))))

(defsystem "malth/tests"
  :author "Liszt21"
  :license ""
  :depends-on ("malth"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for malth"
  :perform (test-op (op c) (symbol-call :fiveam :run! :malth)))
