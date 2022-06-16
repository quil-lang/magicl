;;;; mult-methods.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:magicl)

;;; Matrix-Matrix
(register-matrix-matrix-multiply matrix/single-float single-float)
(register-matrix-matrix-multiply matrix/double-float double-float)
(register-matrix-matrix-multiply matrix/complex-single-float (complex single-float))
(register-matrix-matrix-multiply matrix/complex-double-float (complex double-float))
(register-matrix-matrix-multiply matrix/int32 (signed-byte 32))

;;; Matrix-Vector
(register-matrix-vector-multiply matrix/single-float vector/single-float single-float)
(register-matrix-vector-multiply matrix/double-float vector/double-float double-float)
(register-matrix-vector-multiply matrix/complex-single-float vector/complex-single-float (complex single-float))
(register-matrix-vector-multiply matrix/complex-double-float vector/complex-single-float (complex double-float))
(register-matrix-vector-multiply matrix/int32 vector/int32 (signed-byte 32))
