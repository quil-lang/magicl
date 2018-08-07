;;;; magicl.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:magicl
  :license "BSD 3-Clause (See LICENSE.txt)"
  :maintainer "Rigetti Computing"
  :version (:read-file-form "VERSION.txt")
  :depends-on (#:alexandria
               #:cffi
               #:cffi-libffi)
  :in-order-to ((asdf:test-op (asdf:test-op #:magicl-tests)))
  :serial t
  :components
  ((:file "packages")
   (:file "load-libs")
   (:file "with-array-pointers")
   (:file "cffi-types")
   (:file "blas-cffi")
   (:file "lapack-cffi")
   (:file "high-level")
   (:file "random")
   (:file "magicl")))
