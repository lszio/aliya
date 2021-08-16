(defpackage aliya/tests
  (:use
   :cl
   :aliya
   :fiveam))

(in-package :aliya/tests)

(def-suite :aliya)

(in-suite :aliya)

(test test-5am
  (is (equal 1 1))
  (is (not (equal 2 1))))
