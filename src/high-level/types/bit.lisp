;;;; bit.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:magicl)

(deftensor tensor/bit bit)

(defmatrix matrix/bit bit tensor/bit)

(defvector vector/bit bit tensor/bit)

(defcompatible
    (lambda (tensor)
      (case (order tensor)
        (1 '(vector/bit tensor/bit))
        (2 '(matrix/bit tensor/bit))
        (t '(tensor/bit))))
  tensor/bit
  matrix/bit
  vector/bit)

(defun mod2+ (source1 source2 &optional target)
  (binary-operator (lambda (x y) (mod (+ x y) 2)) source1 source2 target))

;;; TODO improve this.
(defmethod .+ ((source1 vector/bit) (source2 vector/bit) &optional target)
  (mod2+ source1 source2 target))

(defmethod .+ ((source1 matrix/bit) (source2 matrix/bit) &optional target)
  (mod2+ source1 source2 target))

(defmethod .+ ((source1 tensor/bit) (source2 tensor/bit) &optional target)
  (mod2+ source1 source2 target))
