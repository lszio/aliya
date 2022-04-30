(defsystem "loong"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("clish")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "loong/tests"))))

(defsystem "loong/tests"
  :author ""
  :license ""
  :depends-on ("loong"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for loong"
  ;; :perform (test-op (op c) (symbol-call :rove :run c))
  :perform (test-op (op c) (symbol-call :fiveam :run! :loong)))
