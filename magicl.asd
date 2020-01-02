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
               #:abstract-classes
               #:policy-cond)
  :in-order-to ((asdf:test-op (asdf:test-op #:magicl-tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t))
                      (funcall compile)))
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
   (:module "high-level"
    :serial t
    :components ((:file "util")
                 (:file "shape")
                 (:file "abstract-tensor")
                 (:file "specialize-tensor")
                 (:file "tensor")
                 (:file "matrix")
                 (:file "vector")
                 (:file "lapack-generics")
                 (:file "lapack-macros")
                 (:file "types/single-float")
                 (:file "types/double-float")
                 (:file "types/complex-single-float")
                 (:file "types/complex-double-float")
                 (:file "types/int32")
                 (:file "constructors")
                 (:file "specialize-constructor")))
   (:file "magicl")))
