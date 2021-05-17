(defpackage #:magicl.cffi-types
  (:use #:common-lisp
        #:cffi)
  (:export #+sbcl #:array-pointer       ; FUNCTION
           #:with-array-pointers        ; MACRO
           #:complex-single-float
           #:complex-double-float
           #:fortran-int
           #:fortran-float
           #:fortran-double
           #:fortran-complex-float
           #:fortran-complex-double
           #:fortran-logical))
