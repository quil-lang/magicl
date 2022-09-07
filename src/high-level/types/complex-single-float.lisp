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

(defmethod .realpart-lisp ((m matrix/complex-single-float))
  (let ((re-m (zeros (shape m) :type 'single-float)))
    (map-to #'realpart m re-m)
    re-m))

(defmethod .imagpart-lisp ((m matrix/complex-single-float))
  (let ((im-m (zeros (shape m) :type 'single-float)))
    (map-to #'imagpart m im-m)
    im-m))

(defmethod =-lisp ((tensor1 tensor/complex-single-float) (tensor2 tensor/complex-single-float) &optional (epsilon *float-comparison-threshold*))
  (unless (equal (shape tensor1) (shape tensor2))
    (return-from =-lisp nil))
  (map-indexes
   (shape tensor1)
   (lambda (&rest pos)
     (unless (and (<= (abs (- (realpart (apply #'tref tensor1 pos))
                              (realpart (apply #'tref tensor2 pos))))
                      epsilon)
                  (<= (abs (- (imagpart (apply #'tref tensor1 pos))
                              (imagpart (apply #'tref tensor2 pos))))
                      epsilon))
       (return-from =-lisp nil))))
  t)

(defmethod =-lisp ((tensor1 matrix/complex-single-float) (tensor2 matrix/complex-single-float) &optional (epsilon *float-comparison-threshold*))
  (unless (equal (shape tensor1) (shape tensor2))
    (return-from =-lisp nil))
  (map-indexes
   (shape tensor1)
   (lambda (&rest pos)
     (unless (and (<= (abs (- (realpart (apply #'tref tensor1 pos))
                              (realpart (apply #'tref tensor2 pos))))
                      epsilon)
                  (<= (abs (- (imagpart (apply #'tref tensor1 pos))
                              (imagpart (apply #'tref tensor2 pos))))
                      epsilon))
       (return-from =-lisp nil))))
  t)

(defmethod =-lisp ((tensor1 vector/complex-single-float) (tensor2 vector/complex-single-float) &optional (epsilon *float-comparison-threshold*))
  (unless (equal (shape tensor1) (shape tensor2))
    (return-from =-lisp nil))
  (map-indexes
   (shape tensor1)
   (lambda (&rest pos)
     (unless (and (<= (abs (- (realpart (apply #'tref tensor1 pos))
                              (realpart (apply #'tref tensor2 pos))))
                      epsilon)
                  (<= (abs (- (imagpart (apply #'tref tensor1 pos))
                              (imagpart (apply #'tref tensor2 pos))))
                      epsilon))
       (return-from =-lisp nil))))
  t)
