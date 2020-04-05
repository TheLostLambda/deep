;;;; Linear Algebra Utilities
(defpackage deep.linalg
  (:use :cl)
  (:shadow :+ :* :- :/)
  (:export :+ :* :- :/ :mag :norm :matrix :get-rows :get-columns :shape))
(in-package :deep.linalg)

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

(defun make-matrix (rows)
  (setf rows (map 'vector (lambda (v) (coerce v 'vector)) (coerce rows 'vector)))
  (check-type rows (satisfies matrixp))
  (make-instance 'matrix :rows rows))

;; This could use some improved pretty-printing
(defmethod print-object ((m matrix) stream)
    (format stream "#M~A" (remove #\# (write-to-string (get-rows m)))))

;; I need this so that matrix objects can be saved with compile-file
(defmethod make-load-form ((m matrix) &optional environment)
  (declare (ignore environment))
  `(make-instance 'matrix :rows ,(get-rows m)))

(defun read-matrix (stream subchar arg)
  (declare (ignore subchar arg))
  (let* ((prefix (make-string-input-stream ""))
         (stream (make-concatenated-stream prefix stream)))
    (make-matrix (read stream))))

(set-dispatch-macro-character #\# #\M #'read-matrix)

(defmethod shape ((m matrix))
  (with-slots (rows) m
      (list (length rows) (length (elt rows 0)))))

(defmethod get-columns ((m matrix))
  (with-slots (rows) m
    (apply #'map 'vector #'vector (coerce rows 'list))))

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
  (unless (= (length a) (length b))
    (error "Vectors are of incompatible dimensions for addition"))
  (map 'vector #'cl:+ a b))

;;; Overriding -
(make-op -)

(defmethod bin-- (a b)
  (+ a (* -1 b)))

;;; Overriding *
(make-op *)

(defmethod bin-* ((a number) (b number))
  (cl:* a b))

(defmethod bin-* ((a number) (b vector))
  (map 'vector (lambda (x) (cl:* a x)) b))

(defmethod bin-* ((a vector) (b number))
  (bin-* b a))

(defmethod bin-* ((a vector) (b vector))
  (unless (= (length a) (length b))
    (error "Vectors are of incompatible dimensions for multiplication"))
  (apply #'cl:+ (map 'list #'cl:* a b)))

(defmethod bin-* ((a matrix) (b matrix))
  (if (= (cadr (shape a)) (car (shape b)))
    (make-matrix
      (loop :for r :across (get-rows a)
            :collect (loop :for c :across (get-columns b)
                           :collect (* r c))))
    ; Make this a condition!
    (error "Matrices are of incompatible dimensions for multiplication")))

;;; Overriding /
(make-op /)

(defmethod bin-/ (a (b number))
  (* a (cl:/ b)))

;;; Finding vector magnitude
(defun mag (vec)
  (sqrt (loop for x across vec summing (expt x 2))))

;;; Normalise vector to magnitude 1
(defun norm (vec)
  (if (zerop (mag vec)) vec (/ vec (mag vec))))
