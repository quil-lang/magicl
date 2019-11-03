;;;; constants.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl-tests)

(defconstant +magicl-types+
  '(single-float
    double-float
    (complex single-float)
    (complex double-float)
    (signed-byte 32)))

(defconstant +magicl-float-types+
  '(single-float
    double-float
    (complex single-float)
    (complex double-float)))

(defconstant +magicl-matrix-classes+
  '(magicl::matrix/single-float
    magicl::matrix/double-float
    magicl::matrix/complex-single-float
    magicl::matrix/complex-double-float
    magicl::matrix/int32))
