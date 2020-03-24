;;;; Linear Algebra Utilities
(defpackage deep.linalg
  (:use :cl)
  (:shadow :+ :*)
  (:export :+ :* :mag :norm :make-matrix))
(in-package :deep.linalg)

;;; Some macro-magic for overriding operators
(defmacro make-op (op)
  (let ((bin-op (intern (format nil "BIN-~A" (string op)))))
    `(progn
       (defgeneric ,bin-op (a b))
       (defun ,op (&rest args)
         (reduce (function ,bin-op) (cdr args) :initial-value (car args))))))

;;; Overriding +
(make-op +)

(defmethod bin-+ ((a number) (b number))
  (cl:+ a b))

(defmethod bin-+ ((a vector) (b vector))
  (map 'vector #'cl:+ a b))

;;; Overriding *
(make-op *)

(defmethod bin-* ((a number) (b number))
  (cl:* a b))

(defmethod bin-* ((a number) (b vector))
  (map 'vector (lambda (x) (cl:* a x)) b))

(defmethod bin-* ((a vector) (b number))
  (bin-* b a))

(defmethod bin-* ((a vector) (b vector))
  (apply #'cl:+ (map 'list #'cl:* a b)))

;;; Finding vector magnitude
(defun mag (vec)
  (sqrt (loop for x across vec summing (expt x 2))))

;;; Normalise vector to magnitude 1
(defun norm (vec)
  (let ((scale (/ 1.0d0 (mag vec))))
    (* scale vec)))

;;; Matrices
(defun matrixp (obj)
  (and (arrayp obj)
       (= 2 (array-rank obj))
       (loop for i from 0 to (1- (array-total-size obj))
             always (numberp (row-major-aref obj i)))))

(defclass matrix (sequence standard-object)
  ((internal-array
    :initarg :internal-array
    :type (satisfies matrixp))))

(defun make-matrix (array)
  (check-type array (satisfies matrixp))
  (make-instance 'matrix :internal-array array))

;; This could use some improved pretty-printing
(defmethod print-object ((m matrix) stream)
  (let ((internal (write-to-string (slot-value m 'internal-array))))
    (format stream "#M~A" (string-trim "#2A" internal))))

(defun read-matrix (stream subchar arg)
  (declare (ignore subchar arg))
  (let* ((prefix (make-string-input-stream "#2A"))
         (stream (make-concatenated-stream prefix stream)))
    (make-matrix (read stream))))

(set-dispatch-macro-character #\# #\M #'read-matrix)

(defmethod shape ((m matrix))
  (array-dimensions (slot-value m 'internal-array)))

(defmethod get-rows ((m matrix))
  (destructuring-bind (rows columns) (shape m)
    (let ((array (slot-value m 'internal-array)))
      (loop for r from 0 to (1- rows)
            collect (loop for c from 0 to (1- columns)
                          collect (aref array r c))))))

(defmethod get-columns ((m matrix))
  (destructuring-bind (rows columns) (shape m)
    (let ((array (slot-value m 'internal-array)))
      (loop for c from 0 to (1- columns)
            collect (loop for r from 0 to (1- rows)
                          collect (aref array r c))))))
