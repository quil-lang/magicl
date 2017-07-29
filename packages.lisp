(defpackage #:magicl.cffi-types
  (:nicknames #:cffi-types)
  (:use :common-lisp :cffi :fnv)

  (:export :fortran-int
           :fortran-float
           :fortran-double
           :fortran-complex-float
           :fortran-complex-double
           :fortran-logical
           :logical))

(defpackage #:magicl.blas-cffi
  (:nicknames #:blas-cffi)
  (:use :common-lisp :cffi :foreign-numeric-vector :fnv-utils
        :magicl.cffi-types))

(defpackage #:magicl.lapack-cffi
  (:nicknames #:lapack-cffi)
  (:use :common-lisp :cffi :foreign-numeric-vector :fnv-utils
        :magicl.cffi-types))

(defpackage #:magicl
  (:use :common-lisp :cffi :foreign-numeric-vector :blas-cffi)
  (:export :with-blapack))
