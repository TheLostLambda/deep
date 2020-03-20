;;;; Linear Algebra Utilities
(defpackage deep.linalg
  (:use :cl)
  (:shadow :+ :*)
  (:export :+ :* :mag :norm))
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
