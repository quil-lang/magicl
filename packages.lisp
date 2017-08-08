(defpackage #:magicl.foreign-libraries
  (:use #:common-lisp)
  (:export #:libgfortran
           #:libblas
           #:liblapack
           #:foreign-symbol-available-p
           #:print-availability-report))

(defpackage #:magicl.cffi-types
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
  (:use #:foreign-numeric-vector
        #:fnv-utils
        #:magicl.cffi-types)
  #-package-local-nicknames
  (:nicknames #:blas))

(defpackage #:magicl.lapack-cffi
  (:use #:foreign-numeric-vector
        #:fnv-utils
        #:magicl.cffi-types)
  #-package-local-nicknames
  (:nicknames #:lapack))

(defpackage #:magicl
  (:use #:common-lisp
        #:cffi
        #:foreign-numeric-vector)
  #+package-local-nicknames
  (:local-nicknames (#:blas #:magicl.blas-cffi)
                    (#:lapack #:magicl.lapack-cffi))
  (:import-from #:magicl.foreign-libraries
                #:print-availability-report)
  (:export #:print-availability-report
           #:with-blapack
           #:make-complex-matrix
           #:print-matrix
           #:qr
           #:ql
           #:svd
           #:multiply-complex-matrices
           #:ref
           #:make-matrix
           #:matrix-rows
           #:matrix-cols
           #:matrix-data
           #:csd
           #:det
           #:inv))
