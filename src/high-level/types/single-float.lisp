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

;; Might want to inline this
(defun %scalar= (s1 s2 epsilon)
  "For equality checks of inexact scalars."
  (declare (type number s1 s2) ; you can use this on complex but it might be slower because of the sqrt and two multiplies. Maybe.
           (type real epsilon))
  (<= (abs (- s1
              s2))
      epsilon))

(defmethod =-lisp ((scalar1 rational) (scalar2 rational) &optional epsilon)
  (declare (ignore epsilon))
   "Rationals (integers and ratios) should be compared exactly."
  (common-lisp:= scalar1 scalar2))

(defmethod =-lisp ((scalar1 single-float) (scalar2 single-float) &optional (epsilon *float-comparison-threshold*))
  (%scalar= scalar1 scalar2 epsilon))

(defmethod =-lisp ((scalar1 rational) (scalar2 single-float) &optional (epsilon *float-comparison-threshold*))
  (%scalar= scalar1 scalar2 epsilon))

(defmethod =-lisp ((scalar1 single-float) (scalar2 rational) &optional (epsilon *float-comparison-threshold*))
  (%scalar= scalar1 scalar2 epsilon))

(defmethod =-lisp ((scalar1 float) (scalar2 single-float) &optional (epsilon *float-comparison-threshold*))
  "This covers comparing double-float to single-float. Use least precise epsilon."
  (%scalar= scalar1 scalar2 epsilon))

(defmethod =-lisp ((scalar1 single-float) (scalar2 float) &optional (epsilon *float-comparison-threshold*))
  "This covers comparing single-float to double-float. Use least precise epsilon."
  (%scalar= scalar1 scalar2 epsilon))

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
