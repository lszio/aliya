(defsystem "ust"
  :version "0.1.1"
  :author ""
  :license ""
  :depends-on ("clish" "str")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "ust/tests"))))

(defsystem "ust/tests"
  :author ""
  :license ""
  :depends-on ("ust"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for ust"
  :perform (test-op (op c) (symbol-call :rove :run c)))
