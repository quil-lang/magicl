;;;; double-float.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(deftensor tensor/int32 (signed-byte 32))

(defmatrix matrix/int32 (signed-byte 32) tensor/int32)

(defvector vector/int32 (signed-byte 32) tensor/int32)


;; There is no Common Lisp class that corresponds exactly to (SIGNED-BYTE 32).
;; Even FIXNUM is too general, so we might as well just specialize on INTEGER.
(defmethod = ((val1 integer) (val2 integer) &optional (epsilon 0))
  (<= (abs (- val1 val2))
      epsilon))

(defcompatible
    (lambda (tensor)
      (case (order tensor)
        (1 '(column-vector/int32
             row-vector/int32
             tensor/int32))
        (2 '(matrix/int32
             tensor/int32))
        (t '(tensor/int32))))
  tensor/int32
  matrix/int32
  vector/int32)
