;;;; magicl-tests.asd
;;;;
;;;; Author: Joseph Lin

(asdf:defsystem #:magicl-tests
  :description "Regression tests for MAGICL."
  :author "Joseph Lin <joe.lin@rigetti.com>"
  :depends-on (#:magicl
               #:magicl-examples
               #:fiasco)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :magicl-tests
                                           '#:run-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "packages")
               (:file "high-level-tests")))
