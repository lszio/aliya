(defsystem "cln"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ("likit" "clish" "log4cl")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cln/tests"))))

(defsystem "cln/tests"
  :author ""
  :license ""
  :depends-on ("cln"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cln"
  :perform (test-op (op c) (symbol-call :rove :run c)))
