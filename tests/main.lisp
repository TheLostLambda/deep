(defpackage deep/tests
  (:use :cl
        :deep
        :rove))
(in-package :deep/tests)

;; NOTE: To run this test file, execute `(asdf:test-system :deep)' in your Lisp.

(deftest vector-addition
  (testing "Test binary scalar addition"
    (ok (= 4 (deep.linalg:+ 2 2))))

  (testing "Test multiple scalar addition"
    (ok (= 15 (deep.linalg:+ 1 2 3 4 5))))

  (testing "Test multiple, mixed-type scalar addition"
    (ok (= 157/50 (deep.linalg:+ 3 1/10 1/25)))
    (ok (= 10.20 (deep.linalg:+ #b1111 0.2 -5))))

  (testing "Test binary vector addition"
    (ok (equalp #(3 4 1) (deep.linalg:+ #(4 3 -1) #(-1 1 2)))))

  (testing "Test multiple vector addition"
    (ok (equalp #(5 5) (deep.linalg:+ #(1 2) #(2 1) #(2 2)))))

  (testing "Test multiple, mixed-type vector addition"
    (ok (equalp #(3.14d0 4/3) (deep.linalg:+ #(3 #b1) #(0.1d0 1/3) #(0.04d0 0))))))
