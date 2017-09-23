;;;; cffi-types.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:magicl.cffi-types)

;;; CFFI definitions to allow (COMPLEX SINGLE-FLOAT) and (COMPLEX
;;; DOUBLE-FLOAT) to be interpreted efficiently across the Lisp-C
;;; boundary.

(cffi:defcstruct (complex-single-float :class foreign-complex-single-float)
  (real :float)
  (imag :float))

(cffi:defctype complex-single-float (:struct complex-single-float))

(defmethod cffi:translate-from-foreign (ptr (type foreign-complex-single-float))
  (cffi:with-foreign-slots ((real imag) ptr (:struct complex-single-float))
    (complex real imag)))

(defmethod cffi:expand-from-foreign (ptr (type foreign-complex-single-float))
  `(cffi:with-foreign-slots ((real imag) ,ptr (:struct complex-single-float))
     (complex real imag)))

(defmethod cffi:translate-into-foreign-memory (value (type foreign-complex-single-float) ptr)
  (cffi:with-foreign-slots ((real imag) ptr (:struct complex-single-float))
    (setf real (realpart value)
          imag (imagpart value))))

(cffi:defcstruct (complex-double-float :class foreign-complex-double-float)
  (real :double)
  (imag :double))

(cffi:defctype complex-double-float (:struct complex-double-float))

(defmethod cffi:translate-from-foreign (ptr (type foreign-complex-double-float))
  (cffi:with-foreign-slots ((real imag) ptr (:struct complex-double-float))
    (complex real imag)))

(defmethod cffi:expand-from-foreign (ptr (type foreign-complex-double-float))
  `(cffi:with-foreign-slots ((real imag) ,ptr (:struct complex-double-float))
     (complex real imag)))

(defmethod cffi:translate-into-foreign-memory (value (type foreign-complex-double-float) ptr)
  (cffi:with-foreign-slots ((real imag) ptr (:struct complex-double-float))
    (setf real (realpart value)
          imag (imagpart value))))

;;; A test function to see if the above stuff works.

(cffi:defcfun ("conjf" %conjf) complex-single-float
  (z complex-single-float))

;;; Establish the value types used in Fortran.

(cffi:defctype fortran-int            :int32)
(cffi:defctype fortran-float          :float)
(cffi:defctype fortran-double         :double)
(cffi:defctype fortran-complex-float  complex-single-float)
(cffi:defctype fortran-complex-double complex-double-float)
(cffi:defctype fortran-logical        :int32)
