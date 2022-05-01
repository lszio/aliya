(defpackage clish/tests
  (:use :cl :fiveam))
(in-package :clish/tests)

(def-suite :clish)
(in-suite :clish)

(clish:defcli cli (:docs "test-cli")
  (:default (lambda (arg) arg))
  (keys (lambda (&key a b) (cons a b)))
  (rest (lambda (&rest args) args))
  (mixed (lambda (&rest args &key a b (c "c")) (cons args (list a b c))))
  (nested #'nested-cli))

(clish:defcli nested-cli (:docs "nested-cli")
  (nil (lambda () "Nil"))
  (add (lambda (a b) (+ a b))))

(test test-cli-keys
  "Test keys parameter"
  (is (equal (cli "keys" "-a" "1" "-b" "2") (cons "1" "2")))
  (is (equal (cli "keys" "-b" "1" "-a" "2") (cons "2" "1"))))

(test test-cli-mixed
  "Test mixed parameter"
  (is (equal (cli "mixed" "-a" "2") (cons (list :a "2") (list "2" nil "c")))))

(test test-cli-number
  "Test numbers"
  (is (equal (cli "rest" "1" "#n11" "#x111" "#b1111" "#c(1 2)" "#n1.234" "#n1/2") (list "1" 11 273 15 #c(1 2) 1.234 1/2))))

(test test-cli-lisp-code
  "Test lisp code"
  (is (equal (cli "(+ 1 2)" ) 3))
  (is (equal (cli "(cons 1 2)" ) (cons 1 2)))
  (is (equal (cli "(list 1 2 3)" ) (list 1 2 3))))

(test test-cli-help
  "Test help"
  (is (equal (cli "--help") nil)))

(test test-nested-cli
  "Test nested cli"
  (is (equal (nested-cli "add" "#n1" "#n2") 3))
  (is (equal (cli "nested") "Nil"))
  (is (equal (cli "nested" "add" "#n1" "#n2") 3)))
