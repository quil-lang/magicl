;;;; specialize-constructor.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(defgeneric compatible-tensor-constructors (type)
  (:documentation "Get compatible tensor class from element type")
  (:method (type)
    (cond
      ((subtypep type 'single-float) 'tensor/single-float)
      ((subtypep type 'double-float) 'tensor/double-float)
      ((subtypep type '(complex single-float)) 'tensor/complex-single-float)
      ((subtypep type '(complex double-float)) 'tensor/complex-double-float)
      ((subtypep type '(signed-byte 32)) 'tensor/int32)
      (t (error "No compatible tensor constructor for type ~a" type)))))

;; Maybe a bunch of :after to check nil -> do own check (like node does)
;;
;; This could possibly be changed to just return the type to use with COMPATIBLE-TENSOR-CONSTRUCTORS
(defgeneric compatible-tensor-constructors-from-value (value)
  (:documentation "Get compatible tensor class and type from element value")
  (:method (value)
    (etypecase value
      (single-float (values 'tensor/single-float 'single-float))
      (double-float (values 'tensor/double-float 'double-float))
      ((complex single-float) (values 'tensor/complex-single-float '(complex single-float)))
      ((complex double-float) (values 'tensor/complex-double-float '(complex double-float)))
      ((signed-byte 32) (values 'tensor/int32 '(signed-byte 32))))))
