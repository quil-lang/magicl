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
        (1 '(vector/single-float
             tensor/single-float))
        (2 '(matrix/single-float
             tensor/single-float))
        (t '(tensor/single-float))))
  tensor/single-float
  matrix/single-float
  vector/single-float)

(defmethod .realpart-lisp ((m matrix/single-float))
  m)

(defmethod .imagpart-lisp ((m matrix/single-float))
  (zeros (shape m) :type 'single-float))

(defmethod .complex-lisp ((re matrix/single-float) (im matrix/single-float))
  (assert (equal (shape re) (shape im)))
  (let ((z (zeros (shape re)
                  :type '(complex single-float))))
    (into! (lambda (i j)
             (complex (tref re i j)
                      (tref im i j)))
           z)
    z))

(defmethod =-lisp ((tensor1 tensor/single-float) (tensor2 tensor/single-float) &optional (epsilon *float-comparison-threshold*))
  (unless (equal (shape tensor1) (shape tensor2))
    (return-from =-lisp nil))
  (map-indexes
   (shape tensor1)
   (lambda (&rest pos)
     (unless (<= (abs (- (apply #'tref tensor1 pos)
                         (apply #'tref tensor2 pos)))
                 epsilon)
       (return-from =-lisp nil))))
  t)

(defmethod =-lisp ((tensor1 matrix/single-float) (tensor2 matrix/single-float) &optional (epsilon *float-comparison-threshold*))
  (unless (equal (shape tensor1) (shape tensor2))
    (return-from =-lisp nil))
  (map-indexes
   (shape tensor1)
   (lambda (&rest pos)
     (unless (<= (abs (- (apply #'tref tensor1 pos)
                         (apply #'tref tensor2 pos)))
                 epsilon)
       (return-from =-lisp nil))))
  t)

(defmethod =-lisp ((tensor1 vector/single-float) (tensor2 vector/single-float) &optional (epsilon *float-comparison-threshold*))
  (unless (equal (shape tensor1) (shape tensor2))
    (return-from =-lisp nil))
  (map-indexes
   (shape tensor1)
   (lambda (&rest pos)
     (unless (<= (abs (- (apply #'tref tensor1 pos)
                         (apply #'tref tensor2 pos)))
                 epsilon)
       (return-from =-lisp nil))))
  t)

