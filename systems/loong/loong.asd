(defsystem "loong"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "loong/tests"))))

(defsystem "loong/tests"
  :author ""
  :license ""
  :depends-on ("loong"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for loong"
  :perform (test-op (op c) (symbol-call :rove :run c)))
