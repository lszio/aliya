(defpackage loong/tests
  (:use :cl :fiveam :loong))
(in-package :loong/tests)

(def-suite :loong)
(in-suite :loong)

(test test-base
      (is (equal (+ 1 2) 3)))
