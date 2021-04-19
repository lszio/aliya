(defsystem "aliya"
  :version "0.1.0"
  :author "Liszt21"
  :license ""
  :depends-on ()
  :components ((:module "src"
                        :components
                        ((:file "package")
                         (:file "core" :depends-on ("package"))
                         (:file "app" :depends-on ("package" "core"))
                         (:file "aliya" :depends-on ("package" "app")))))
  :entry-point "aliya:cli"
  :description "Liszt's virtual assistant"
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
