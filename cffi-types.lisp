(in-package #:magicl.cffi-types)

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

(cffi:defcfun ("conjf" %conjf) complex-single-float
  (z complex-single-float))

(defmacro define-fortran-type (name actual-type)
  "Define types for interfacing with fortran code.  Uses
  CFFI:DEFINE-FOREIGN-TYPE with special options to make sure that the
  values are correctly translated with TRANSLATE-TO-FOREIGN and
  TRANSLATE-FROM-FOREIGN."
  (let ((type-name (intern (concatenate 'string (symbol-name
                                                 name)
                                        "-TYPE"))))
    `(cffi:define-foreign-type ,type-name ()
       ()
       (:actual-type ,actual-type)
       (:simple-parser ,name))))

(cffi:defctype fortran-int            :int32)
(cffi:defctype fortran-float          :float)
(cffi:defctype fortran-double         :double)
(cffi:defctype fortran-complex-float  complex-single-float)
(cffi:defctype fortran-complex-double complex-double-float)
(cffi:defctype fortran-logical        :int32)


;; (define-fortran-type fortran-int :pointer)
;; (define-fortran-type fortran-float :pointer)
;; (define-fortran-type fortran-double :pointer)
;; (define-fortran-type fortran-complex-float :pointer)
;; (define-fortran-type fortran-complex-double :pointer)
;; (define-fortran-type fortran-logical :pointer)

;; (defmethod translate-to-foreign (val (name fortran-int-type))
;;   (if (typep val 'fnv-int32)
;;       (fnv-foreign-pointer val)
;;       (fnv-foreign-pointer (make-fnv-int32 1 :initial-value val))))

;; #+ignore
;; (defmethod translate-from-foreign (val (name fortran-int-type))
;;   ())

;; (defmethod translate-to-foreign (val (name fortran-float-type))
;;   (if (typep val 'fnv-float)
;;       (fnv-foreign-pointer val)
;;       (fnv-foreign-pointer (make-fnv-float 1 :initial-value val))))

;; (defmethod translate-to-foreign (val (name fortran-double-type))
;;   (if (typep val 'fnv-double)
;;       (fnv-foreign-pointer val)
;;       (fnv-foreign-pointer (make-fnv-double 1 :initial-value val))))

;; (defmethod translate-to-foreign (val (name fortran-complex-float-type))
;;   (if (typep val 'fnv-complex-float)
;;       (fnv-foreign-pointer val)
;;       (fnv-foreign-pointer (make-fnv-complex-float 1 :initial-value val))))

;; (defmethod translate-to-foreign (val (name fortran-complex-double-type))
;;   (if (typep val 'fnv-complex-double)
;;       (fnv-foreign-pointer val)
;;       (fnv-foreign-pointer (make-fnv-complex-double 1 :initial-value val))))

;; (defmethod translate-to-foreign (val (name fortran-logical-type))
;;   (if (typep val 'fnv-int32)
;;       (fnv-foreign-pointer val)
;;       (fnv-foreign-pointer (make-fnv-int32 1 :initial-value (if val 1 -1)))))

;; (setf (cffi-type-to-fnv-type 'logical) 'cffi-fnv-int32)

;; (CFFI:DEFCFUN ("cdotc_" %%CDOTC)
;;     COMPLEX-SINGLE-FLOAT
;;   (N :pointer)
;;   (CX :pointer)
;;   (INCX :pointer)
;;   (CY :pointer)
;;   (INCY :pointer))

;; (defun %cdotc (n cx incx cy incy)
;;   (declare (type fixnum n)
;;            (type fixnum incx)
;;            (type fixnum incy))
;;   (with-foreign-objects ((n-ref :int)
;;                          (incx-ref :int)
;;                          (incy-ref :int))
;;     (setf (mem-ref n-ref :int) n
;;           (mem-ref incx-ref :int) incx
;;           (mem-ref incy-ref :int) incy)

;;     (%%CDOTC n-ref
;;              (fnv:fnv-foreign-pointer cx)
;;              incx-ref
;;              (fnv:fnv-foreign-pointer cy)
;;              incy-ref)))
