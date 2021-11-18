;;;; specialize-constructor.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

;; NOTE: I hate this
(defun infer-tensor-type (type shape val)
  (declare (type list shape)
           (optimize (speed 3) (safety 0)))
  #+allegro
  (when type
    (case (length shape)
      (1 (cond ((eq type 'single-float) (return-from infer-tensor-type 'vector/single-float))
               ((eq type 'double-float) (return-from infer-tensor-type 'vector/double-float))
               ((equal type '(complex single-float)) (return-from infer-tensor-type 'vector/complex-single-float))
               ((equal type '(complex double-float)) (return-from infer-tensor-type 'vector/complex-double-float))))
      (2 (cond ((eq type 'single-float) (return-from infer-tensor-type 'matrix/single-float))
               ((eq type 'double-float) (return-from infer-tensor-type 'matrix/double-float))
               ((equal type '(complex single-float)) (return-from infer-tensor-type 'matrix/complex-single-float))
               ((equal type '(complex double-float)) (return-from infer-tensor-type 'matrix/complex-double-float))))
      (t (cond ((eq type 'single-float) (return-from infer-tensor-type 'tensor/single-float))
               ((eq type 'double-float) (return-from infer-tensor-type 'tensor/double-float))
               ((equal type '(complex single-float)) (return-from infer-tensor-type 'tensor/complex-single-float))
               ((equal type '(complex double-float)) (return-from infer-tensor-type 'tensor/complex-double-float))))))
  (if type
      (cond
        ((cl:= 1 (length shape))
         (cond
           ((subtypep type 'single-float) 'vector/single-float)
           ((subtypep type 'double-float) 'vector/double-float)
           ((subtypep type '(complex single-float)) 'vector/complex-single-float)
           ((subtypep type '(complex double-float)) 'vector/complex-double-float)
           ((subtypep type '(signed-byte 32)) 'vector/int32)
           (t (error "no compatible tensor constructor for type ~a" type))))
        ((cl:= 2 (length shape))
         (cond
           ((subtypep type 'single-float) 'matrix/single-float)
           ((subtypep type 'double-float) 'matrix/double-float)
           ((subtypep type '(complex single-float)) 'matrix/complex-single-float)
           ((subtypep type '(complex double-float)) 'matrix/complex-double-float)
           ((subtypep type '(signed-byte 32)) 'matrix/int32)
           (t (error "no compatible tensor constructor for type ~a" type))))
        (t
         (cond
           ((subtypep type 'single-float) 'tensor/single-float)
           ((subtypep type 'double-float) 'tensor/double-float)
           ((subtypep type '(complex single-float)) 'tensor/complex-single-float)
           ((subtypep type '(complex double-float)) 'tensor/complex-double-float)
           ((subtypep type '(signed-byte 32)) 'tensor/int32)
           (t (error "no compatible tensor constructor for type ~a" type)))))
      (cond
        ((cl:= 1 (length shape))
         (etypecase val
           (single-float 'vector/single-float)
           (double-float 'vector/double-float)
           ((complex single-float) 'vector/complex-single-float)
           ((complex double-float) 'vector/complex-double-float)
           ((signed-byte 32) 'vector/int32)))
        ((cl:= 2 (length shape))
         (etypecase val
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
