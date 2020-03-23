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

(deftest vector-multiplication
  (testing "Test binary scalar multiplication"
    (ok (= 4 (deep.linalg:* 2 2))))

  (testing "Test multiple scalar multiplication"
    (ok (= 120 (deep.linalg:* 1 2 3 4 5))))

  (testing "Test multiple, mixed-type scalar multiplication"
    (ok (= 3/250 (deep.linalg:* 3 1/10 1/25)))
    (ok (= -15.0 (deep.linalg:* #b1111 0.2 -5))))

  (testing "Test multiple, scalar-vector multiplication"
    (ok (equalp #(16 8 4) (deep.linalg:* 2 #(4 2 1) 2)))
    (ok (equalp #(16 8 4) (deep.linalg:* #(4 2 1) 2 2)))
    (ok (equalp #(16 8 4) (deep.linalg:* 2 2 #(4 2 1)))))

  (testing "Test binary vector multiplication"
    (ok (= -3 (deep.linalg:* #(4 3 -1) #(-1 1 2)))))

  (testing "Test multiple, mixed vector and scalar multiplication"
    (ok (equalp #(8 8) (deep.linalg:* #(1 2) #(2 1) #(2 2)))))

  (testing "Test multiple, mixed-type vector multiplication"
    (ok (equalp #(0.092d0 0.0d0) (deep.linalg:* #(3 #b1) #(0.1d0 2/1) #(0.04d0 0))))))

(deftest vector-magnitude
  (testing "Test scalar magnitudes"
    (ok (= 42 (deep.linalg:mag #(42)))))
  (testing "Test some special triangles"
    (ok (= (sqrt 2) (deep.linalg:mag #(1 1))))
    (ok (= 2 (deep.linalg:mag #(1 #.(sqrt 3)))))
    (ok (= 5 (deep.linalg:mag #(3 4)))))
  (testing "Test some 3D vectors"
    (ok (= 13 (deep.linalg:mag #(3 4 12))))))

(deftest normalise-vectors
  (testing "Normalise vectors"
    (ok (approx #(0.707 0.707) (deep.linalg:norm #(5 5))))
    (ok (approx #(3/13 4/13 12/13) (deep.linalg:norm #(3 4 12))))))

(defgeneric approx (a b &optional tol)
  (:documentation "Tests if a and b are approximately equal to each other"))

(defmethod approx ((a vector) (b vector) &optional (tol 0.001))
  (every (lambda (x y) (< (abs (- x y)) tol)) a b))
