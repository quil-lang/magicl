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
               #:magicl/ext
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
               #:static-vectors
               #:trivial-garbage
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
                 (:file "allocation")
                 (:file "allocation-allegro" :if-feature :allegro)
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
                 (:file "types/specialized-vector")
                 (:file "constructors")
                 (:file "specialize-constructor")
                 (:file "polynomial-solver")
                 (:module "matrix-functions"
                  :serial t
                  :components ((:file "mult-definition")
                               (:file "mult-methods")
                               (:file "csd")))))
   (:file "magicl")))

;;; Extension common code

(asdf:defsystem #:magicl/ext
  :description "Common code for extending MAGICL with foreign libraries."
  :depends-on (#:magicl/core
               #:cffi
               #:cffi-libffi)
  :serial t
  :pathname "src/extensions/common"
  :components
  ((:file "package")
   (:file "library-tracking")
   (:file "with-array-pointers")
   (:file "cffi-types")
   (:file "ptr-ref")))

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
   (:module "bindings/allegro" :if-feature :allegro
    :components #1=((:file "blas-cffi")))
   (:module "bindings" :if-feature (:not :allegro)
    :components #1#)
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
   (:module "bindings/allegro" :if-feature :allegro
    :components #1=((:file "lapack00-cffi")
                    (:file "lapack01-cffi")
                    (:file "lapack02-cffi")
                    (:file "lapack03-cffi")
                    (:file "lapack04-cffi")
                    (:file "lapack05-cffi")
                    (:file "lapack06-cffi")
                    (:file "lapack07-cffi")))
   (:module "bindings" :if-feature (:not :allegro)
    :components #1#)
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

(defun dynamic-library-extension ()
  "Return the dynamic library extension on the current OS as a string."
  (cond
    ((uiop:os-windows-p) "dll")
    ((uiop:os-macosx-p)  "dylib")
    ((uiop:os-unix-p)    "so")
    (t                   (error "unsupported OS"))))

(defmethod output-files ((operation compile-op) (component f->so))
  (values (list (apply-output-translations
                 (make-pathname :name "libexpokit"
                                :type (dynamic-library-extension)
                                :defaults (component-pathname component))))
          t))

(defmethod perform ((operation load-op) (component f->so))
  t)

(defmethod perform ((operation compile-op) (component f->so))
  (flet ((nn (x) (uiop:native-namestring x)))
    (let* ((fortran-file (component-pathname component))
           (object-file (apply-output-translations
                         (make-pathname :type "o" :defaults fortran-file)))
           (shared-object (first (output-files operation component))))
      (ensure-directories-exist shared-object)
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
   (:file #+allegro "src/bindings/allegro/expokit-cffi"
          #-allegro "src/bindings/expokit-cffi")
   (:file "src/extensions/expokit/expm")))
