;;;; magicl.asd
;;;;
;;;; Author: Robert Smith

;;; XXX: For now, load all extensions when loading MAGICL so as to not
;;; break users' code. In the future, however, we may make MAGICL a
;;; synonym for MAGICL/CORE.
(asdf:defsystem #:magicl
  :license "BSD 3-Clause (See LICENSE.txt)"
  :description "Matrix Algebra proGrams In Common Lisp"
  :maintainer "Rigetti Computing"
  :author "Rigetti Computing"
  :version (:read-file-form "VERSION.txt")
  :in-order-to ((asdf:test-op (asdf:test-op #:magicl-tests)))
  :depends-on (#:magicl/core
               #:magicl/ext-blas
               #:magicl/ext-lapack
               #:magicl/ext-expokit))

(asdf:defsystem #:magicl/core
  :license "BSD 3-Clause (See LICENSE.txt)"
  :description "Matrix Algebra proGrams In Common Lisp (pure Lisp core)"
  :maintainer "Rigetti Computing"
  :author "Rigetti Computing"
  :version (:read-file-form "VERSION.txt")
  :depends-on (#:alexandria
               #:abstract-classes
               #:policy-cond
               #:interface              ; for CALLING-FORM

               #:magicl/ext             ; Allow extensions
               )
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :pathname "src/"
  :serial t
  :components
  ((:file "packages")
   (:file "backend-function")
   (:file "benchmark-utilities")
   (:module "high-level"
    :serial t
    :components ((:file "util")
                 (:file "shape")
                 (:file "abstract-tensor")
                 (:file "specialize-tensor")
                 (:file "tensor")
                 (:file "matrix")
                 (:file "vector")
                 (:file "types/single-float")
                 (:file "types/double-float")
                 (:file "types/complex-single-float")
                 (:file "types/complex-double-float")
                 (:file "types/int32")
                 (:file "constructors")
                 (:file "specialize-constructor")
                 (:file "polynomial-solver")))
   (:file "magicl")))

;;; Extension common code

(asdf:defsystem #:magicl/ext
  :description "Common code for extending MAGICL with foreign libraries."
  :depends-on (#:cffi
               #:cffi-libffi)
  :serial t
  :pathname "src/extensions/common"
  :components
  ((:file "package")
   (:file "library-tracking")
   (:file "with-array-pointers")
   (:file "cffi-types")))

;;; BLAS

(asdf:defsystem #:magicl/ext-blas
  :description "Native BLAS routines in MAGICL."
  :depends-on (#:magicl/core
               #:magicl/ext
               #:cffi)
  :serial t
  :pathname "src/"
  :components
  ((:file "extensions/blas/package")
   (:file "extensions/blas/load-libs")
   (:module "bindings"
    :components ((:file "blas-cffi")))
   (:file "extensions/blas/arithmetic")))


;;; LAPACK

(asdf:defsystem #:magicl/ext-lapack
  :description "Native LAPACK routines in MAGICL."
  :depends-on (#:magicl/core
               #:magicl/ext
               #:magicl/ext-blas
               #:cffi
               #:policy-cond)
  :serial t
  :pathname "src/"
  :components
  ((:file "extensions/lapack/package")
   (:file "extensions/lapack/load-libs")
   (:module "bindings"
    :components ((:file "lapack00-cffi")
                 (:file "lapack01-cffi")
                 (:file "lapack02-cffi")
                 (:file "lapack03-cffi")
                 (:file "lapack04-cffi")
                 (:file "lapack05-cffi")
                 (:file "lapack06-cffi")
                 (:file "lapack07-cffi")))
   (:module "extensions/lapack"
    :components ((:file "lapack-generics")
                 (:file "lapack-templates")
                 (:file "lapack-bindings")
                 (:file "lapack-csd")))))


;;; EXPOKIT

;;; Adapted from commonqt's qt.asd
(defclass f->so (asdf:source-file)
  ()
  (:default-initargs
   :type "f"))

(defmethod output-files ((operation compile-op) (component f->so))
  (values (list (make-pathname :name "libexpokit"
                               :type #-darwin "so" #+darwin "dylib"
                               :defaults (component-pathname component)))
          t))

(defmethod perform ((operation load-op) (component f->so))
  t)

(defmethod perform ((operation compile-op) (component f->so))
  (flet ((nn (x) (uiop:native-namestring x)))
    (let* ((fortran-file (component-pathname component))
           (object-file (make-pathname :type "o" :defaults fortran-file))
           (shared-object (make-pathname :type #+darwin "dylib" #-darwin "so"
                                         :name "libexpokit"
                                         :defaults fortran-file)))
      (uiop:run-program
       (list "gfortran" "-fPIC" "-std=legacy"
             "-c"
             (nn fortran-file)
             "-o"
             (nn object-file)))
      (uiop:run-program
       (list "gfortran" #+darwin "-dynamiclib" #-darwin "-shared"
             "-o" (nn shared-object)
             (nn object-file)
             #+darwin "-lblas"
             #+darwin "-llapack"))
      (delete-file object-file))))


(asdf:defsystem #:magicl/ext-expokit
  :description "Expokit for MAGICL."
  :depends-on (#:alexandria
               #:cffi
               #:cffi-libffi
               #:magicl/core
               #:magicl/ext
               #:magicl/ext-blas
               #:magicl/ext-lapack)
  :serial t
  :components
  ((:module "expokit"
    :components ((f->so "expokit")))
   (:module "src/extensions/expokit"
    :components ((:file "package")
                 (:file "load-libs")))
   (:file "src/bindings/expokit-cffi")
   (:file "src/extensions/expokit/expm")))
