(defpackage malth/tests
  (:use :cl :fiveam :malth))
(in-package :malth/tests)

(def-suite :malth)
(in-suite :malth)

(test test-5am
      (is (equal 1 1)))
