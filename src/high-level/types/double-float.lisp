;;;; double-float.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(deftensor tensor/double-float double-float)

(defmatrix matrix/double-float double-float tensor/double-float)

(defvector vector/double-float double-float tensor/double-float matrix/double-float)

(defcompatible
    (lambda (tensor)
      (case (rank tensor)
        (1 '(vector/double-float
             matrix/double-float
             tensor/double-float))
        (2 '(matrix/double-float
             tensor/double-float))
        (t '(tensor/double-float))))
  tensor/double-float
  matrix/double-float
  vector/double-float)

(defmethod = ((tensor1 tensor/double-float) (tensor2 tensor/double-float) &optional (epsilon 0d0))
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

(defmethod = ((tensor1 matrix/double-float) (tensor2 matrix/double-float) &optional (epsilon 0d0))
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
