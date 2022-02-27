(defpackage likit/tests
  (:use :cl
        :likit
        :rove))
(in-package :likit/tests)

;; (deftest test-string
;;   (testing "test split"
;;            (ok (equal (split "a b c") '("a" "b" "c")))
;;            (ok (equal (split "a;b;c" #\;) '("a" "b" "c"))))
;;   (testing "test join"
;;            (ok (equal (join " " '("a" "b" "c")) "a b c"))))

(deftest test-core
    (testing "core tester"
             (ok (zerop (when-match ((a b c))
                            ((a) 0)
                            ((b) 1))))))

(deftest test-path
  (testing "file tester"
           (ok (filep "Makefile"))
           (ng (filep "###"))
           (ng (filep "~")))
  (testing "folder tester"
           (ok (folderp "."))
           (ng (folderp "###"))
           (ng (folderp "Makefile"))))

(deftest test-system
    (testing "command tester"
             (ok (command-exists-p "echo"))
             (ng (command-exists-p "likit")))
    (testing "feature tester"
             (ng (member :testing *features*))
             (toggle-feature :testing t)
             (ok (member :testing *features*))))


