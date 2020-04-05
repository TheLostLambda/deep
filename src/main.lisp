(defpackage deep
  (:shadowing-import-from :deep.linalg :+ :- :* :/)
  (:use :cl :deep.linalg)
  (:export :make-perceptron :binary-classifier :learn :loss))
(in-package :deep)

(defun binary-classifier (x)
  (cond ((> x 0)  1)
        ((< x 0) -1)
        (t x)))

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

;; Add some restarts here that can be called in the error case
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

;; Could I get fancy with a loop here?
;; This needs to abort the loop at some point if things don't converge...
(defmethod learn ((p perceptron) (xss matrix) (ys vector))
  (do ((c 0 (1+ c))
       (i 0 (rem (1+ i) (length ys))))
      ((= c (length ys)))
    (when (train p (elt (get-rows xss) i) (elt ys i))
      (setf c -1)))
  (values (loss p xss ys) (get-weights p) (get-bias p)))

(defmethod loss ((p perceptron) (xss matrix) (ys vector))
  (with-slots (loss) p
    (loop :for xs :across (get-rows xss)
          :for y  :across ys
          :summing (funcall loss p xs y))))

(defun point-loss (perceptron xs y)
  (if (= (predict perceptron xs) y) 0 1))


;; -y_i(w*x_i+b)/|w|
;; y_i is the output of the classifier {1, -1}
;; w is a weights vector (also the normal vector to this hyperplane)
;; x_i is the input point (also a vector)
;; b is the d of the plane equation
;; |w| is a magnitude of the w vector
;; Overall, you are taking some vector from the plane to the point
;; and projecting it on to the normal vector of the plane (minimal distance)
;; See: https://mathworld.wolfram.com/Point-PlaneDistance.html
(defun distance-loss (perceptron xs y)
  (let ((yp (predict perceptron xs))
        (w (norm (get-weights perceptron)))
        (b (get-bias perceptron))) ;; Not a fan of this let
    (if (/= yp y) (* (cl:- y) (+ (* w xs) b)) 0)))
