;; Copyright rif 2006.
;; Modified BSD License (see LICENSE file in this directory).

(defpackage :org.middleangle.blapack-cffi-types
  (:nicknames :blapack-cffi-types)
  (:use :common-lisp :cffi :fnv)

  (:export :fortran-int
           :fortran-float
           :fortran-double
           :fortran-complex-float
           :fortran-complex-double
           :fortran-logical
           :logical))

(in-package :org.middleangle.blapack-cffi-types)

(defmacro define-fortran-type (name actual-type)
  "Define types for interfacing with fortran code.  Uses
  CFFI:DEFINE-FOREIGN-TYPE with special options to make sure that the
  values are correctly translated with TRANSLATE-TO-FOREIGN and
  TRANSLATE-FROM-FOREIGN."
  (let ((type-name(intern (concatenate 'string (symbol-name
                                                        name)
                                               "-TYPE"))))
   `(define-foreign-type ,type-name ()
      ()
      (:actual-type ,actual-type)
      (:simple-parser ,name))))

(define-fortran-type fortran-int :pointer)
(define-fortran-type fortran-float :pointer)
(define-fortran-type fortran-double :pointer)
(define-fortran-type fortran-complex-float :pointer)
(define-fortran-type fortran-complex-double :pointer)
(define-fortran-type fortran-logical :pointer)
(define-fortran-type logical :int)

(defmethod translate-to-foreign (val (name fortran-int-type))
  (if (typep val 'fnv-int32)
      (fnv-foreign-pointer val)
      (fnv-foreign-pointer (make-fnv-int32 1 :initial-value val))))

(defmethod translate-from-foreign (val (name fortran-int-type))
  ())

(defmethod translate-to-foreign (val (name fortran-float-type))
  (if (typep val 'fnv-float)
      (fnv-foreign-pointer val)
      (fnv-foreign-pointer (make-fnv-float 1 :initial-value val))))

(defmethod translate-to-foreign (val (name fortran-double-type))
  (if (typep val 'fnv-double)
      (fnv-foreign-pointer val)
      (fnv-foreign-pointer (make-fnv-double 1 :initial-value val))))

(defmethod translate-to-foreign (val (name fortran-complex-float-type))
  (if (typep val 'fnv-complex-float)
      (fnv-foreign-pointer val)
      (fnv-foreign-pointer (make-fnv-complex-float 1 :initial-value val))))

(defmethod translate-to-foreign (val (name fortran-complex-double-type))
  (if (typep val 'fnv-complex-double)
      (fnv-foreign-pointer val)
      (fnv-foreign-pointer (make-fnv-complex-double 1 :initial-value val))))

(defmethod translate-to-foreign (val (name fortran-logical-type))
  (if (typep val 'fnv-int32)
      (fnv-foreign-pointer val)
      (fnv-foreign-pointer (make-fnv-int32 1 :initial-value (if val 1 -1)))))

(setf (cffi-type-to-fnv-type 'logical) 'cffi-fnv-int32)
