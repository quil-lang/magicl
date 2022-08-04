(defpackage #:magicl.cffi-types
  (:use #:common-lisp
        #:cffi)
  (:export #+sbcl #:array-pointer       ; FUNCTION
           #:with-array-pointers        ; MACRO
           #:ptr-ref                    ; GENERIC, METHOD
           #:complex-single-float
           #:complex-double-float
           #:fortran-int
           #:fortran-float
           #:fortran-double
           #:fortran-complex-float
           #:fortran-complex-double
           #:fortran-logical))

(uiop:define-package #:magicl.foreign-libraries
  (:use #:common-lisp)
  (:export #:*foreign-libraries*
           #:track-symbols              ; used in MAGICL-GEN
           #:foreign-symbol-available-p
           #:print-availability-report))
