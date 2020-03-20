(defsystem "deep"
  :version "0.1.0"
  :author "Brooks J Rady"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "linalg")
                 (:file "main" :depends-on ("linalg")))))
  :description "A home-rolled library for deep-learning"
  :in-order-to ((test-op (test-op "deep/tests"))))

(defsystem "deep/tests"
  :author "Brooks J Rady"
  :license ""
  :depends-on ("deep"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for deep"
  :perform (test-op (op c) (symbol-call :rove :run c)))
