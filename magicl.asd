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
               #:cffi-libffi
               #:abstract-classes)
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
   (:file "high-level/util")
   (:file "high-level/shape")
   (:file "high-level/abstract-tensor")
   (:file "high-level/specialize-tensor")
   (:file "high-level/tensor")
   (:file "high-level/matrix")
   (:file "high-level/vector")
   (:file "high-level/types/single-float")
   (:file "high-level/types/double-float")
   (:file "high-level/types/complex-single-float")
   (:file "high-level/types/complex-double-float")
   (:file "high-level/types/int32")
   (:file "high-level/constructors")
   (:file "high-level/specialize-constructor")
   (:file "magicl")))
