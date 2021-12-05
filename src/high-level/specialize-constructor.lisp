;;;; specialize-constructor.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

;; NOTE: I hate this
(defun infer-tensor-type (type shape val)
  (declare (type list shape)
           (optimize (speed 3) (safety 0)))
  (if type
      (case (length shape)
        (1 (cond ((or (eq type 'single-float) (subtypep type 'single-float)) 'vector/single-float)
                 ((or (eq type 'double-float) (subtypep type 'double-float)) 'vector/double-float)
                 ((or (equal type '(complex single-float)) (subtypep type '(complex single-float))) 'vector/complex-single-float)
                 ((or (equal type '(complex double-float)) (subtypep type '(complex double-float))) 'vector/complex-double-float)
                 ((or (equal type '(signed-byte 32)) (subtypep type '(signed-byte 32))) 'vector/int32)
                 (t (error "no compatible tensor constructor for type ~a" type))))
        (2 (cond ((or (eq type 'single-float) (subtypep type 'single-float)) 'matrix/single-float)
                 ((or (eq type 'double-float) (subtypep type 'double-float)) 'matrix/double-float)
                 ((or (equal type '(complex single-float)) (subtypep type '(complex single-float))) 'matrix/complex-single-float)
                 ((or (equal type '(complex double-float)) (subtypep type '(complex double-float))) 'matrix/complex-double-float)
                 ((or (equal type '(signed-byte 32)) (subtypep type '(signed-byte 32))) 'matrix/int32)
                 (t (error "no compatible tensor constructor for type ~a" type))))
        (t (cond ((or (eq type 'single-float) (subtypep type 'single-float)) 'tensor/single-float)
                 ((or (eq type 'double-float) (subtypep type 'double-float)) 'tensor/double-float)
                 ((or (equal type '(complex single-float)) (subtypep type '(complex single-float))) 'tensor/complex-single-float)
                 ((or (equal type '(complex double-float)) (subtypep type '(complex double-float))) 'tensor/complex-double-float)
                 ((or (equal type '(signed-byte 32)) (subtypep type '(signed-byte 32))) 'tensor/int32)
                 (t (error "no compatible tensor constructor for type ~a" type)))))
      (case (length shape)
        (1 (etypecase val
             (single-float 'vector/single-float)
             (double-float 'vector/double-float)
             ((complex single-float) 'vector/complex-single-float)
             ((complex double-float) 'vector/complex-double-float)
             ((signed-byte 32) 'vector/int32)))
        (2 (etypecase val
             (single-float 'matrix/single-float)
             (double-float 'matrix/double-float)
             ((complex single-float) 'matrix/complex-single-float)
             ((complex double-float) 'matrix/complex-double-float)
             ((signed-byte 32) 'matrix/int32)))
        (t (etypecase val
             (single-float 'tensor/single-float)
             (double-float 'tensor/double-float)
             ((complex single-float) 'tensor/complex-single-float)
             ((complex double-float) 'tensor/complex-double-float)
             ((signed-byte 32) 'tensor/int32))))))
