(defpackage #:magicl.foreign-libraries
  (:use #:common-lisp)
  (:export #:libgfortran
           #:libblas
           #:liblapack))

(defpackage #:magicl.cffi-types
  (:nicknames #:cffi-types)
  (:use #:common-lisp
        #:cffi 
        #:fnv)

  (:export #:complex-single-float
           #:complex-double-float
           #:fortran-int
           #:fortran-float
           #:fortran-double
           #:fortran-complex-float
           #:fortran-complex-double
           #:fortran-logical))

(defpackage #:magicl.blas-cffi
  (:nicknames #:blas-cffi)
  (:use #:foreign-numeric-vector
        #:fnv-utils
        #:magicl.cffi-types))

(defpackage #:magicl.lapack-cffi
  (:nicknames #:lapack-cffi)
  (:use #:foreign-numeric-vector
        #:fnv-utils
        #:magicl.cffi-types))

(defpackage #:magicl
  (:use #:common-lisp
        #:cffi
        #:foreign-numeric-vector
        #:blas-cffi)
  (:export #:with-blapack
           #:make-complex-matrix
           #:print-matrix
           #:qr
           #:svd
           #:multiply-complex-matrices
           #:ref
           #:make-matrix))
