(defpackage loong/tests/main
  (:use :cl
        :loong
        :rove))
(in-package :loong/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :loong)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
