(defpackage cln/tests/main
  (:use :cl
        :cln
        :rove))
(in-package :cln/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cln)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
