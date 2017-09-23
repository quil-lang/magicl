;;;; magicl-tests.asd
;;;;
;;;; Author: Joseph Lin

(asdf:defsystem #:magicl-tests
  :description "Regression tests for MAGICL."
  :author "Rigetti Computing"
  :depends-on (#:uiop
               #:magicl
               #:magicl-examples
               #:fiasco)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :magicl-tests
                                           '#:run-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "packages")
               (:file "suite")
               (:file "high-level-tests")))
