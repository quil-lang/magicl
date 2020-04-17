;;;; double-float.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(deftensor tensor/double-float double-float)

(defmatrix matrix/double-float double-float tensor/double-float)

(defvector vector/double-float double-float tensor/double-float)

(defcompatible
    (lambda (tensor)
      (case (order tensor)
        (1 '(column-vector/double-float
             row-vector/double-float
             tensor/double-float))
        (2 '(matrix/double-float
             tensor/double-float))
        (t '(tensor/double-float))))
  tensor/double-float
  matrix/double-float
  vector/double-float)

(defmethod = ((tensor1 tensor/double-float) (tensor2 tensor/double-float) &optional (epsilon *double-comparison-threshold*))
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

(defmethod = ((tensor1 matrix/double-float) (tensor2 matrix/double-float) &optional (epsilon *double-comparison-threshold*))
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

(declaim (inline double-float-vector=))
(defun double-float-vector= (tensor1 tensor2 epsilon)
  (unless (equal (shape tensor1) (shape tensor2))
    (return-from double-float-vector= nil))
  (map-indexes
   (shape tensor1)
   (lambda (&rest pos)
     (unless (<= (abs (- (apply #'tref tensor1 pos)
                         (apply #'tref tensor2 pos)))
                 epsilon)
       (return-from double-float-vector= nil))))
  t)

(defmethod = ((tensor1 column-vector/double-float) (tensor2 column-vector/double-float) &optional (epsilon *double-comparison-threshold*))
  (double-float-vector= tensor1 tensor2 epsilon))
(defmethod = ((tensor1 row-vector/double-float) (tensor2 row-vector/double-float) &optional (epsilon *double-comparison-threshold*))
  (double-float-vector= tensor1 tensor2 epsilon))

