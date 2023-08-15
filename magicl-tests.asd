;;;; magicl-tests.asd
;;;;
;;;; Author: Joseph Lin

(asdf:defsystem #:magicl-tests
  :license "BSD 3-Clause (See LICENSE.txt)"
  :description "Regression tests for MAGICL."
  :author "Rigetti Computing"
  :depends-on (#:alexandria
               #:uiop
               #:magicl
               #:magicl-examples
               #:fiasco)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :magicl-tests
                                           '#:run-magicl-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "packages")
               (:file "suite")
               (:file "constants")
               (:file "util-tests")
               (:file "allocation-tests")
               (:file "abstract-tensor-tests")
               (:file "specialization-tests")
               (:file "constructor-tests")
               (:file "matrix-tests")
               (:file "high-level-tests")))
