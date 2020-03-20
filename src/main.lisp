(defpackage deep
  (:use :cl)
  (:shadowing-import-from :deep.linalg :+ :*))
(in-package :deep)

(+ #(4 3 -1) (* 3 #(-1 1 2)))
