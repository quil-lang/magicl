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
               #:cffi                   ; for extension support
               #:policy-cond
               #:interface              ; for CALLING-FORM

               #:magicl/lisp-lapack     ; for Lisp fallbacks
               )
  :around-compile (lambda (compile)
                    (let (#+sbcl (sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :pathname "src/"
  :serial t
  :components
  ((:file "packages")
   (:file "backend-function")
   (:file "library-tracking")
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
                 (:file "lapack-templates")
                 (:file "csd")
                 (:file "expi")
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

;;; Generate Interface

(asdf:defsystem #:magicl/generate-interface
  :depends-on (#:magicl/ext #:cl-ppcre #:f2cl #:abstract-classes)
  :serial t
  :pathname "src/"
  :components ((:file "packages")
               (:file "library-tracking")
               (:file "generate-interface/generate-interface")))

;;; Lisp LAPACK

(asdf:defsystem #:magicl/lisp-lapack
  :description "Lisp LAPACK routines in MAGICL"
  :depends-on (#:f2cl)
  :serial t
  :pathname "lapack/"
  :components
  ((:file "package") (:file "fortran-intrinsics") (:file "lisp-src/scabs1")
   (:file "lisp-src/caxpy") (:file "lisp-src/ccopy") (:file "lisp-src/cdotc")
   (:file "lisp-src/cdotu") (:file "lisp-src/lsame") (:file "lisp-src/xerbla")
   (:file "lisp-src/cgbmv") (:file "lisp-src/cgemm") (:file "lisp-src/cgemv")
   (:file "lisp-src/cgerc") (:file "lisp-src/cgeru") (:file "lisp-src/chbmv")
   (:file "lisp-src/chemm") (:file "lisp-src/chemv") (:file "lisp-src/cher")
   (:file "lisp-src/cher2") (:file "lisp-src/cher2k") (:file "lisp-src/cherk")
   (:file "lisp-src/chpmv") (:file "lisp-src/chpr") (:file "lisp-src/chpr2")
   (:file "lisp-src/crotg") (:file "lisp-src/cscal") (:file "lisp-src/csrot")
   (:file "lisp-src/csscal") (:file "lisp-src/cswap") (:file "lisp-src/csymm")
   (:file "lisp-src/csyr2k") (:file "lisp-src/csyrk") (:file "lisp-src/ctbmv")
   (:file "lisp-src/ctbsv") (:file "lisp-src/ctpmv") (:file "lisp-src/ctpsv")
   (:file "lisp-src/ctrmm") (:file "lisp-src/ctrmv") (:file "lisp-src/ctrsm")
   (:file "lisp-src/ctrsv") (:file "lisp-src/dasum") (:file "lisp-src/daxpy")
   (:file "lisp-src/dcabs1") (:file "lisp-src/dcopy") (:file "lisp-src/ddot")
   (:file "lisp-src/dgbmv") (:file "lisp-src/dgemm") (:file "lisp-src/dgemv")
   (:file "lisp-src/dger") (:file "lisp-src/dnrm2") (:file "lisp-src/drot")
   (:file "lisp-src/drotg") (:file "lisp-src/drotm") (:file "lisp-src/drotmg")
   (:file "lisp-src/dsbmv") (:file "lisp-src/dscal") (:file "lisp-src/dsdot")
   (:file "lisp-src/dspmv") (:file "lisp-src/dspr") (:file "lisp-src/dspr2")
   (:file "lisp-src/dswap") (:file "lisp-src/dsymm") (:file "lisp-src/dsymv")
   (:file "lisp-src/dsyr") (:file "lisp-src/dsyr2") (:file "lisp-src/dsyr2k")
   (:file "lisp-src/dsyrk") (:file "lisp-src/dtbmv") (:file "lisp-src/dtbsv")
   (:file "lisp-src/dtpmv") (:file "lisp-src/dtpsv") (:file "lisp-src/dtrmm")
   (:file "lisp-src/dtrmv") (:file "lisp-src/dtrsm") (:file "lisp-src/dtrsv")
   (:file "lisp-src/dzasum") (:file "lisp-src/dznrm2")
   (:file "lisp-src/icamax") (:file "lisp-src/idamax")
   (:file "lisp-src/isamax") (:file "lisp-src/izamax") (:file "lisp-src/sasum")
   (:file "lisp-src/saxpy") (:file "lisp-src/scasum") (:file "lisp-src/scnrm2")
   (:file "lisp-src/scopy") (:file "lisp-src/sdot") (:file "lisp-src/sdsdot")
   (:file "lisp-src/sgbmv") (:file "lisp-src/sgemm") (:file "lisp-src/sgemv")
   (:file "lisp-src/sger") (:file "lisp-src/snrm2") (:file "lisp-src/srot")
   (:file "lisp-src/srotg") (:file "lisp-src/srotm") (:file "lisp-src/srotmg")
   (:file "lisp-src/ssbmv") (:file "lisp-src/sscal") (:file "lisp-src/sspmv")
   (:file "lisp-src/sspr") (:file "lisp-src/sspr2") (:file "lisp-src/sswap")
   (:file "lisp-src/ssymm") (:file "lisp-src/ssymv") (:file "lisp-src/ssyr")
   (:file "lisp-src/ssyr2") (:file "lisp-src/ssyr2k") (:file "lisp-src/ssyrk")
   (:file "lisp-src/stbmv") (:file "lisp-src/stbsv") (:file "lisp-src/stpmv")
   (:file "lisp-src/stpsv") (:file "lisp-src/strmm") (:file "lisp-src/strmv")
   (:file "lisp-src/strsm") (:file "lisp-src/strsv") (:file "lisp-src/zaxpy")
   (:file "lisp-src/zcopy") (:file "lisp-src/zdotc") (:file "lisp-src/zdotu")
   (:file "lisp-src/zdrot") (:file "lisp-src/zdscal") (:file "lisp-src/zgbmv")
   (:file "lisp-src/zgemm") (:file "lisp-src/zgemv") (:file "lisp-src/zgerc")
   (:file "lisp-src/zgeru") (:file "lisp-src/zhbmv") (:file "lisp-src/zhemm")
   (:file "lisp-src/zhemv") (:file "lisp-src/zher") (:file "lisp-src/zher2")
   (:file "lisp-src/zher2k") (:file "lisp-src/zherk") (:file "lisp-src/zhpmv")
   (:file "lisp-src/zhpr") (:file "lisp-src/zhpr2") (:file "lisp-src/zrotg")
   (:file "lisp-src/zscal") (:file "lisp-src/zswap") (:file "lisp-src/zsymm")
   (:file "lisp-src/zsyr2k") (:file "lisp-src/zsyrk") (:file "lisp-src/ztbmv")
   (:file "lisp-src/ztbsv") (:file "lisp-src/ztpmv") (:file "lisp-src/ztpsv")
   (:file "lisp-src/ztrmm") (:file "lisp-src/ztrmv") (:file "lisp-src/ztrsm")
   (:file "lisp-src/ztrsv") (:file "lisp-src/dlamch") (:file "lisp-src/dlabad")
   (:file "lisp-src/dgetf2") (:file "lisp-src/dlaswp")
   (:file "lisp-src/ieeeck") (:file "lisp-src/ilaenv")
   (:file "lisp-src/dgetrf") (:file "lisp-src/dgebak")
   (:file "lisp-src/dgebal") (:file "lisp-src/dlarf") (:file "lisp-src/dlapy2")
   (:file "lisp-src/dlarfg") (:file "lisp-src/dgehd2")
   (:file "lisp-src/dlahrd") (:file "lisp-src/dlarfb")
   (:file "lisp-src/dgehrd") (:file "lisp-src/dlassq")
   (:file "lisp-src/dlanhs") (:file "lisp-src/dlacpy")
   (:file "lisp-src/dlanv2") (:file "lisp-src/dlahqr")
   (:file "lisp-src/dlarfx") (:file "lisp-src/dlaset")
   (:file "lisp-src/dhseqr") (:file "lisp-src/dlartg")
   (:file "lisp-src/dlascl") (:file "lisp-src/dlarft")
   (:file "lisp-src/dorg2r") (:file "lisp-src/dorgqr")
   (:file "lisp-src/dorghr") (:file "lisp-src/dladiv")
   (:file "lisp-src/dlaln2") (:file "lisp-src/dtrevc")
   (:file "lisp-src/dlange") (:file "lisp-src/dgeev") (:file "lisp-src/dlas2")
   (:file "lisp-src/dlasq4") (:file "lisp-src/dlasq5")
   (:file "lisp-src/dlasq6") (:file "lisp-src/dlasq3")
   (:file "lisp-src/dlasrt") (:file "lisp-src/dlasq2")
   (:file "lisp-src/dlasq1") (:file "lisp-src/dlasr") (:file "lisp-src/dlasv2")
   (:file "lisp-src/dbdsqr") (:file "lisp-src/dgebd2")
   (:file "lisp-src/dlabrd") (:file "lisp-src/dgebrd")
   (:file "lisp-src/dgelq2") (:file "lisp-src/dgelqf")
   (:file "lisp-src/dgeqr2") (:file "lisp-src/dgeqrf")
   (:file "lisp-src/dorgl2") (:file "lisp-src/dorglq")
   (:file "lisp-src/dorgbr") (:file "lisp-src/dorml2")
   (:file "lisp-src/dormlq") (:file "lisp-src/dorm2r")
   (:file "lisp-src/dormqr") (:file "lisp-src/dormbr")
   (:file "lisp-src/dgesvd") (:file "lisp-src/zgetf2")
   (:file "lisp-src/zlaswp") (:file "lisp-src/zgetrf")
   (:file "lisp-src/zgebak") (:file "lisp-src/dlaisnan")
   (:file "lisp-src/disnan") (:file "lisp-src/zgebal")
   (:file "lisp-src/ilazlr") (:file "lisp-src/ilazlc") (:file "lisp-src/zlarf")
   (:file "lisp-src/dlapy3") (:file "lisp-src/zladiv")
   (:file "lisp-src/zlarfg") (:file "lisp-src/zgehd2")
   (:file "lisp-src/zlacpy") (:file "lisp-src/zlacgv")
   (:file "lisp-src/zlahr2") (:file "lisp-src/zlarfb")
   (:file "lisp-src/zgehrd") (:file "lisp-src/zlahqr")
   (:file "lisp-src/zlaset") (:file "lisp-src/zlartg") (:file "lisp-src/zrot")
   (:file "lisp-src/ztrexc") (:file "lisp-src/zlarft")
   (:file "lisp-src/zunm2r") (:file "lisp-src/zunmqr")
   (:file "lisp-src/zunmhr") (:file "lisp-src/zlaqr2")
   (:file "lisp-src/zlaqr1") (:file "lisp-src/zlaqr5")
   (:file "lisp-src/zlaqr4") (:file "lisp-src/zlaqr3")
   (:file "lisp-src/zlaqr0") (:file "lisp-src/zhseqr")
   (:file "lisp-src/zlascl") (:file "lisp-src/zlatrs")
   (:file "lisp-src/ztrevc") (:file "lisp-src/zung2r")
   (:file "lisp-src/zungqr") (:file "lisp-src/zunghr")
   (:file "lisp-src/zlassq") (:file "lisp-src/zlange") (:file "lisp-src/zgeev")
   (:file "lisp-src/zlasr") (:file "lisp-src/zbdsqr") (:file "lisp-src/zgebd2")
   (:file "lisp-src/zlabrd") (:file "lisp-src/zgebrd")
   (:file "lisp-src/zgelq2") (:file "lisp-src/zgelqf")
   (:file "lisp-src/zgeqr2") (:file "lisp-src/zgeqrf")
   (:file "lisp-src/zungl2") (:file "lisp-src/zunglq")
   (:file "lisp-src/zungbr") (:file "lisp-src/zunml2")
   (:file "lisp-src/zunmlq") (:file "lisp-src/zunmbr")
   (:file "lisp-src/zgesvd") (:file "lisp-src/zlanhe")
   (:file "lisp-src/dlanst") (:file "lisp-src/dlae2") (:file "lisp-src/dsterf")
   (:file "lisp-src/zhetd2") (:file "lisp-src/zlatrd")
   (:file "lisp-src/zhetrd") (:file "lisp-src/dlaev2")
   (:file "lisp-src/zsteqr") (:file "lisp-src/zung2l")
   (:file "lisp-src/zungql") (:file "lisp-src/zungtr")
   (:file "lisp-src/zheev")))
