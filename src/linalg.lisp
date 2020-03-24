;;;; Linear Algebra Utilities
(defpackage deep.linalg
  (:use :cl)
  (:shadow :+ :*)
  (:export :+ :* :mag :norm :shape))
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
  (and (vectorp obj)
       (/= (length obj) 0)
       (every #'vectorp obj)
       (/= (length (elt obj 0)) 0)
       (apply #'= (map 'list #'length obj))
       (every (lambda (v) (every #'numberp v)) obj)))

(defclass matrix (sequence standard-object)
  ((rows
    :initarg :rows
    :type (satisfies matrixp)
    :reader get-rows)))

(defun make-matrix (vector)
  (check-type vector (satisfies matrixp))
  (make-instance 'matrix :rows vector))

;; This could use some improved pretty-printing
(defmethod print-object ((m matrix) stream)
    (format stream "#M~A" (remove #\# (write-to-string (get-rows m)))))

(defun read-matrix (stream subchar arg)
  (declare (ignore subchar arg))
  (let* ((prefix (make-string-input-stream "#"))
         (stream (make-concatenated-stream prefix stream))
         (vector (map 'vector (lambda (v) (coerce v 'vector)) (read stream))))
    (make-matrix vector)))

(set-dispatch-macro-character #\# #\M #'read-matrix)

(defmethod shape ((m matrix))
  (with-slots (rows) m
      (list (length rows) (length (elt rows 0)))))

(defmethod get-columns ((m matrix))
  (with-slots (rows) m
    (apply #'map 'vector #'vector (coerce rows 'list))))
