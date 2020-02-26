;;;; complex-single-float.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(deftensor tensor/complex-single-float (complex single-float))

(defmatrix matrix/complex-single-float (complex single-float) tensor/complex-single-float)

(defvector vector/complex-single-float (complex single-float) tensor/complex-single-float)

(defcompatible
    (lambda (tensor)
      (case (order tensor)
        (1 '(vector/complex-single-float
             tensor/complex-single-float))
        (2 '(matrix/complex-single-float
             tensor/complex-single-float))
        (t '(tensor/complex-single-float))))
  tensor/complex-single-float
  matrix/complex-single-float
  vector/complex-single-float)

(defmethod dot ((vector1 vector/complex-single-float) (vector2 vector/complex-single-float))
  (assert (cl:= (size vector1) (size vector2))
          () "Vectors must have the same size. The first vector is size ~a and the second vector is size ~a."
          (size vector1) (size vector2))
  (loop :for i :below (size vector1)
        :sum (* (tref vector1 i) (conjugate (tref vector2 i)))))

(defmethod = ((tensor1 tensor/complex-single-float) (tensor2 tensor/complex-single-float) &optional (epsilon *float-comparison-threshold*))
  (unless (equal (shape tensor1) (shape tensor2))
    (return-from = nil))
  (map-indexes
   (shape tensor1)
   (lambda (&rest pos)
     (unless (and (<= (abs (- (realpart (apply #'tref tensor1 pos))
                              (realpart (apply #'tref tensor2 pos))))
                      epsilon)
                  (<= (abs (- (imagpart (apply #'tref tensor1 pos))
                              (imagpart (apply #'tref tensor2 pos))))
                      epsilon))
       (return-from = nil))))
  t)

(defmethod = ((tensor1 matrix/complex-single-float) (tensor2 matrix/complex-single-float) &optional (epsilon *float-comparison-threshold*))
  (unless (equal (shape tensor1) (shape tensor2))
    (return-from = nil))
  (map-indexes
   (shape tensor1)
   (lambda (&rest pos)
     (unless (and (<= (abs (- (realpart (apply #'tref tensor1 pos))
                              (realpart (apply #'tref tensor2 pos))))
                      epsilon)
                  (<= (abs (- (imagpart (apply #'tref tensor1 pos))
                              (imagpart (apply #'tref tensor2 pos))))
                      epsilon))
       (return-from = nil))))
  t)

(defmethod = ((tensor1 vector/complex-single-float) (tensor2 vector/complex-single-float) &optional (epsilon *float-comparison-threshold*))
  (unless (equal (shape tensor1) (shape tensor2))
    (return-from = nil))
  (map-indexes
   (shape tensor1)
   (lambda (&rest pos)
     (unless (and (<= (abs (- (realpart (apply #'tref tensor1 pos))
                              (realpart (apply #'tref tensor2 pos))))
                      epsilon)
                  (<= (abs (- (imagpart (apply #'tref tensor1 pos))
                              (imagpart (apply #'tref tensor2 pos))))
                      epsilon))
       (return-from = nil))))
  t)
