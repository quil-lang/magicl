;;;; single-float.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(deftensor tensor/single-float single-float)

(defmatrix matrix/single-float single-float tensor/single-float)

(defvector vector/single-float single-float tensor/single-float)

(defcompatible
    (lambda (tensor)
      (case (rank tensor)
        (1 '(vector/single-float
             tensor/single-float))
        (2 '(matrix/single-float
             tensor/single-float))
        (t '(tensor/single-float))))
  tensor/single-float
  matrix/single-float
  vector/single-float)

(defmethod = ((tensor1 tensor/single-float) (tensor2 tensor/single-float) &optional (epsilon 0.0))
  (unless (equal (shape tensor1) (shape tensor2))
    (return-from = nil))
  (map-indexes
   (shape tensor1)
   (lambda (&rest pos)
     (unless (> epsilon
                (abs (cl:- (apply #'tref tensor1 pos)
                           (apply #'tref tensor2 pos))))
       (return-from = nil))))
  t)

(defmethod = ((tensor1 matrix/single-float) (tensor2 matrix/single-float) &optional (epsilon 0.0))
  (unless (equal (shape tensor1) (shape tensor2))
    (return-from = nil))
  (map-indexes
   (shape tensor1)
   (lambda (&rest pos)
     (unless (> epsilon
                (abs (cl:- (apply #'tref tensor1 pos)
                           (apply #'tref tensor2 pos))))
       (return-from = nil))))
  t)
