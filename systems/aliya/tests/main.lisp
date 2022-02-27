(defpackage aliya/tests
  (:use :cl :fiveam :aliya))
(in-package :aliya/tests)

(def-suite :aliya)
(in-suite :aliya)

(test test-cli
      (is (equal (+ 1 1) 2)))

