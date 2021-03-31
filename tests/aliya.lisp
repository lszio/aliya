(defpackage aliya/tests/aliya
  (:use :cl
        :aliya
        :rove))
(in-package :aliya/tests/aliya)

;; NOTE: To run this test file, execute `(asdf:test-system :aliya)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
