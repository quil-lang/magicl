;;;; complex-double-float.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(deftensor tensor/complex-double-float (complex double-float))

(defmatrix matrix/complex-double-float (complex double-float) tensor/complex-double-float)

(defvector vector/complex-double-float (complex double-float) tensor/complex-double-float)

(defcompatible
    (lambda (tensor)
      (case (order tensor)
        (1 '(vector/complex-double-float
             tensor/complex-double-float))
        (2 '(matrix/complex-double-float
             tensor/complex-double-float))
        (t '(tensor/complex-double-float))))
  tensor/complex-double-float
  matrix/complex-double-float
  vector/complex-double-float)

(defmethod dot ((vector1 vector/complex-double-float) (vector2 vector/complex-double-float))
  (assert (cl:= (size vector1) (size vector2))
          () "Vectors must have the same size. The first vector is size ~a and the second vector is size ~a."
          (size vector1) (size vector2))
  (loop :for i :below (size vector1)
        :sum (* (tref vector1 i) (conjugate (tref vector2 i)))))

(defmethod = ((tensor1 tensor/complex-double-float) (tensor2 tensor/complex-double-float) &optional (epsilon *double-comparison-threshold*))
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

(defmethod = ((tensor1 matrix/complex-double-float) (tensor2 matrix/complex-double-float) &optional (epsilon *double-comparison-threshold*))
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

(defmethod = ((tensor1 vector/complex-double-float) (tensor2 vector/complex-double-float) &optional (epsilon *double-comparison-threshold*))
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


