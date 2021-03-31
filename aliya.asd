(defsystem "aliya"
  :version "0.1.0"
  :author "Liszt21"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "aliya"))))
  :description ""
  :in-order-to ((test-op (test-op "aliya/tests"))))

(defsystem "aliya/tests"
  :author "Liszt21"
  :license ""
  :depends-on ("aliya"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "aliya"))))
  :description "Test system for aliya"
  :perform (test-op (op c) (symbol-call :rove :run c)))
