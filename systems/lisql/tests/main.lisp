(defpackage lisql/tests/main
  (:use :cl
        :lisql
        :rove))
(in-package :lisql/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :lisql)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
