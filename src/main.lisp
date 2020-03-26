(defpackage deep
  (:shadowing-import-from :deep.linalg :+ :- :* :/)
  (:use :cl :deep.linalg))
(in-package :deep)

(defun binary-classifier (x)
  (if (> x 0) 1 -1))

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
    :type function)
   (loss
    :initarg :lfun
    :initform #'point-loss
    :writer set-loss
    :type function)))

(defun make-perceptron (&rest args &key inputs w &allow-other-keys)
  (unless (or inputs w) (error "Input size could not be determined!"))
  (unless w
    (setf (getf args :w) (make-array inputs :initial-element 0))
    (remf args :inputs))
  (apply #'make-instance 'perceptron args))

(defmethod predict ((p perceptron) (xs vector))
  (with-slots (weights bias activation) p
      (funcall activation (+ (* weights xs) bias))))

(defmethod train ((p perceptron) (xs vector) (y number))
  (unless (= (predict p xs) y) ; (= (loss p xs y) 0)?
    (with-slots (weights bias) p
      (values
        (setf weights (+ weights (* y xs)))
        (incf bias y)))))

;; Could use some more polish
(defmethod loss ((p perceptron) (xs vector) (y number))
  (with-slots (loss) p
    (funcall loss p xs y)))

(defun point-loss (perceptron xs y)
  (if (= (predict perceptron xs) y) 0 1))

;; What is this actually doing?
(defun distance-loss (perceptron xs y)
  (let ((yp (predict perceptron xs))
        (w (norm (get-weights perceptron)))
        (b (get-bias perceptron))) ;; Not a fan of this let
    (if (/= yp y) (* (cl:- y) (+ (* w xs) b)) 0)))
