;;;; constants.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl-tests)

(alexandria:define-constant +magicl-types+
  '(single-float
    double-float
    (complex single-float)
    (complex double-float)
    (signed-byte 32))
  :test #'equal)

(alexandria:define-constant +magicl-float-types+
  '(single-float
    double-float
    (complex single-float)
    (complex double-float))
  :test #'equal)

(alexandria:define-constant +magicl-matrix-classes+
  '(magicl::matrix/single-float
    magicl::matrix/double-float
    magicl::matrix/complex-single-float
    magicl::matrix/complex-double-float
    magicl::matrix/int32)
  :test #'equal)
