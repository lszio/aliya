(defpackage ust/tests
  (:use :cl
        :ust
        :rove))
(in-package :ust/tests)

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
