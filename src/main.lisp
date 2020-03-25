(defpackage deep
  (:use :cl)
  (:shadowing-import-from :deep.linalg :+ :*))
(in-package :deep)

(defun binary-classifier (x)
  (if (< x 0) -1 1))

(defclass perceptron ()
  ((weights
    :initarg :w
    :accessor get-weights
    :type vector)
   (bias
    :initarg :b
    :initform 0
    :accessor get-bias
    :type number)
   (activation
    :initarg :afun
    :initform #'identity
    :writer set-activation
    :type function)))

(defun make-perceptron (&rest args &key inputs w &allow-other-keys)
  (unless (or inputs w) (error "Input size could not be determined!"))
  (unless w
    (setf (getf args :w) (make-array inputs :initial-element 0))
    (remf args :inputs))
  (apply #'make-instance 'perceptron args))

(defmethod predict ((p perceptron) (x vector))
  (with-slots (weights bias activation) p
      (funcall activation (+ (* weights x) bias))))

;; Just make `method` optional and merge all of the loss methods?
(defmethod loss ((p perceptron) (x vector) (y vector) (method (eql :points)))
  (declare (ignore method)))

(defmethod loss ((p perceptron) (x vector) (y vector) (method (eql :distance)))
  (declare (ignore method)))
