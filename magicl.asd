;;;; magicl.asd
;;;;
;;;; Author: Robert Smith

(asdf:defsystem #:magicl
  :license "BSD 3-Clause (See LICENSE.txt)"
  :description "Matrix Algebra proGrams In Common Lisp"
  :maintainer "Rigetti Computing"
  :author "Rigetti Computing"
  :version (:read-file-form "VERSION.txt")
  :depends-on (#:alexandria
               #:cffi
               #:cffi-libffi)
  :in-order-to ((asdf:test-op (asdf:test-op #:magicl-tests)))
  :serial t
  :pathname "src/"
  :components
  ((:file "packages")
   (:file "load-libs")
   (:file "with-array-pointers")
   (:file "cffi-types")
   (:module "bindings"
            :components
            ((:file "lapack00-cffi")
             (:file "lapack01-cffi")
             (:file "lapack02-cffi")
             (:file "lapack03-cffi")
             (:file "lapack04-cffi")
             (:file "lapack05-cffi")
             (:file "lapack06-cffi")
             (:file "lapack07-cffi")
             (:file "blas-cffi")))
   (:file "high-level/high-level")
   (:file "high-level/random")
   (:file "high-level/einsum")
   (:file "magicl")))
