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
        (1 '(vector/double-float
             tensor/double-float))
        (2 '(matrix/double-float
             tensor/double-float))
        (t '(tensor/double-float))))
  tensor/double-float
  matrix/double-float
  vector/double-float)

(defmethod =-lisp ((scalar1 double-float) (scalar2 double-float) &optional (epsilon *double-comparison-threshold*))
  (%scalar= scalar1 scalar2 epsilon))

(defmethod =-lisp ((scalar1 rational) (scalar2 double-float) &optional (epsilon *double-comparison-threshold*))
 (%scalar= scalar1 scalar2 epsilon))

(defmethod =-lisp ((scalar1 double-float) (scalar2 rational) &optional (epsilon *double-comparison-threshold*))
  (%scalar= scalar1 scalar2 epsilon))

(defmethod =-lisp ((tensor1 tensor/double-float) (tensor2 tensor/double-float) &optional (epsilon *double-comparison-threshold*))
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

(defmethod =-lisp ((tensor1 matrix/double-float) (tensor2 matrix/double-float) &optional (epsilon *double-comparison-threshold*))
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

(defmethod =-lisp ((tensor1 vector/double-float) (tensor2 vector/double-float) &optional (epsilon *double-comparison-threshold*))
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
