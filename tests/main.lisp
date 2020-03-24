(defpackage deep/tests
  (:use :cl
        :deep
        :rove))
(in-package :deep/tests)

;; NOTE: To run this test file, execute `(asdf:test-system :deep)' in your Lisp.

(deftest vector-addition
  (testing "Binary scalar addition"
    (ok (= 4 (deep.linalg:+ 2 2))))

  (testing "Multiple scalar addition"
    (ok (= 15 (deep.linalg:+ 1 2 3 4 5))))

  (testing "Multiple, mixed-type scalar addition"
    (ok (= 157/50 (deep.linalg:+ 3 1/10 1/25)))
    (ok (= 10.20 (deep.linalg:+ #b1111 0.2 -5))))

  (testing "Binary vector addition"
    (ok (equalp #(3 4 1) (deep.linalg:+ #(4 3 -1) #(-1 1 2)))))

  (testing "Multiple vector addition"
    (ok (equalp #(5 5) (deep.linalg:+ #(1 2) #(2 1) #(2 2)))))

  (testing "Multiple, mixed-type vector addition"
    (ok (equalp #(3.14d0 4/3) (deep.linalg:+ #(3 #b1) #(0.1d0 1/3) #(0.04d0 0))))))

(deftest vector-multiplication
  (testing "Binary scalar multiplication"
    (ok (= 4 (deep.linalg:* 2 2))))

  (testing "Multiple scalar multiplication"
    (ok (= 120 (deep.linalg:* 1 2 3 4 5))))

  (testing "Multiple, mixed-type scalar multiplication"
    (ok (= 3/250 (deep.linalg:* 3 1/10 1/25)))
    (ok (= -15.0 (deep.linalg:* #b1111 0.2 -5))))

  (testing "Multiple, scalar-vector multiplication"
    (ok (equalp #(16 8 4) (deep.linalg:* 2 #(4 2 1) 2)))
    (ok (equalp #(16 8 4) (deep.linalg:* #(4 2 1) 2 2)))
    (ok (equalp #(16 8 4) (deep.linalg:* 2 2 #(4 2 1)))))

  (testing "Binary vector multiplication"
    (ok (= -3 (deep.linalg:* #(4 3 -1) #(-1 1 2)))))

  (testing "Multiple, mixed vector and scalar multiplication"
    (ok (equalp #(8 8) (deep.linalg:* #(1 2) #(2 1) #(2 2)))))

  (testing "Multiple, mixed-type vector multiplication"
    (ok (equalp #(0.092d0 0.0d0) (deep.linalg:* #(3 #b1) #(0.1d0 2/1) #(0.04d0 0))))))

(deftest vector-magnitude
  (testing "Scalar magnitudes"
    (ok (= 42 (deep.linalg:mag #(42)))))

  (testing "Special triangles"
    (ok (= (sqrt 2) (deep.linalg:mag #(1 1))))
    (ok (= 2 (deep.linalg:mag #(1 #.(sqrt 3)))))
    (ok (= 5 (deep.linalg:mag #(3 4)))))

  (testing "3D vectors"
    (ok (= 13 (deep.linalg:mag #(3 4 12))))))

(deftest normalise-vectors
  (testing "Vector normalisation"
    (ok (approx #(0.707 0.707) (deep.linalg:norm #(5 5))))
    (ok (approx #(3/13 4/13 12/13) (deep.linalg:norm #(3 4 12))))))

(deftest matrix-creation
  (testing "Construct a valid matrix"
    (ok #M((1 2 3) (4 5 6) (7 8 9))))

  (testing "Rows must be a sequence"
    (ok (signals (read-from-string "#M42") 'type-error)))

  (testing "Rows must contain at least one element"
    (ok (signals (read-from-string "#M()") 'type-error)))

  (testing "Rows must contain at least one sequence"
    (ok (signals (read-from-string "#M(42)") 'type-error)))

  (testing "Rows must contain at least one non-empty sequence"
    (ok (signals (read-from-string "#M(())") 'type-error)))

  (testing "Rows must be of the same size"
    (ok (signals (read-from-string "#M((1 2) (3))") 'type-error)))

  (testing "Rows must contain only numbers"
    (ok (signals (read-from-string "#M((1 2) (3 #\\4))") 'type-error))))

(deftest matrix-multiplication
  (testing "Multiplication by the identity matrix"
    (ok (approx #1=#M((1 2) (3 4)) (deep.linalg:* #1# #M((1 0) (0 1))))))

  (testing "Multiplication of inverse matrices"
    (ok (approx #M((1 0) (0 1)) (deep.linalg:* #M((-2 3) (3 -4))
                                               #M((4 3) (3 2))))))

  (testing "Multiplication of non-square matrices"
    (ok (approx #M((83 63 37 75)) (deep.linalg:* #M((3 4 2)) #M((13 9 7 15)
                                                                (8 7 4 6)
                                                                (6 4 0 3)))))))

(defgeneric approx (a b &optional tol)
  (:documentation "Tests if a and b are approximately equal to each other"))

(defmethod approx ((a vector) (b vector) &optional (tol 0.001))
  (every (lambda (x y) (< (abs (- x y)) tol)) a b))

(defmethod approx ((a deep.linalg:matrix) (b deep.linalg:matrix) &optional (tol 0.001))
  (every #'approx (deep.linalg:get-rows a) (deep.linalg:get-rows b)))
