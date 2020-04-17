;;;; single-float.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(deftensor tensor/single-float single-float)

(defmatrix matrix/single-float single-float tensor/single-float)

(defvector vector/single-float single-float tensor/single-float)

(defcompatible
    (lambda (tensor)
      (case (order tensor)
        (1 '(column-vector/single-float
             row-vector/single-float
             tensor/single-float))
        (2 '(matrix/single-float
             tensor/single-float))
        (t '(tensor/single-float))))
  tensor/single-float
  matrix/single-float
  vector/single-float)

(defmethod = ((tensor1 tensor/single-float) (tensor2 tensor/single-float) &optional (epsilon *float-comparison-threshold*))
  (unless (equal (shape tensor1) (shape tensor2))
    (return-from = nil))
  (map-indexes
   (shape tensor1)
   (lambda (&rest pos)
     (unless (<= (abs (- (apply #'tref tensor1 pos)
                         (apply #'tref tensor2 pos)))
                 epsilon)
       (return-from = nil))))
  t)

(defmethod = ((tensor1 matrix/single-float) (tensor2 matrix/single-float) &optional (epsilon *float-comparison-threshold*))
  (unless (equal (shape tensor1) (shape tensor2))
    (return-from = nil))
  (map-indexes
   (shape tensor1)
   (lambda (&rest pos)
     (unless (<= (abs (- (apply #'tref tensor1 pos)
                         (apply #'tref tensor2 pos)))
                 epsilon)
       (return-from = nil))))
  t)

(declaim (inline single-float-vector=))
(defun single-float-vector= (tensor1 tensor2 epsilon)  
  (unless (equal (shape tensor1) (shape tensor2))
    (return-from single-float-vector= nil))
  (map-indexes
   (shape tensor1)
   (lambda (&rest pos)
     (unless (<= (abs (- (apply #'tref tensor1 pos)
                         (apply #'tref tensor2 pos)))
                 epsilon)
       (return-from single-float-vector= nil))))
  t)

(defmethod = ((tensor1 column-vector/single-float) (tensor2 column-vector/single-float) &optional (epsilon *float-comparison-threshold*))
  (single-float-vector= tensor1 tensor2 epsilon))
(defmethod = ((tensor1 row-vector/single-float) (tensor2 row-vector/single-float) &optional (epsilon *float-comparison-threshold*))
  (single-float-vector= tensor1 tensor2 epsilon))
