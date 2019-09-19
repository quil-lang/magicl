;;;; lapack-macros.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(defmacro def-lapack-mult (class type blas-function)
  `(defmethod mult ((a ,class) (b ,class) &key target (alpha ,(coerce 1 type)) (beta ,(coerce 0 type)) (transa :n) (transb :n))
     (check-type transa (member nil :n :t :c))
     (check-type transb (member nil :n :t :c))
     (let* ((m (if (eq :n transa) (nrows a) (ncols a)))
            (k (if (eq :n transa) (ncols a) (nrows a)))
            (n (if (eq :n transb) (ncols b) (nrows b)))
            (brows (if (eq :n transb) (nrows b) (ncols b))))
       (assert (cl:= k brows)
               () "Incompatible matrix sizes ~a and ~a." (list m k) (list brows n))
       (when target
         (assert (equal (shape target) (list m n))
                 () "Incompatible target shape. Target needs shape ~a but has shape ~a"
                 (shape target) (list m n)))
       (let ((ta
               (if (eql :row-major (order a))
                   (case transa
                     (:n :t)
                     (:t :n)
                     (:c (error "Specifying TRANSA to be :C is not allowed if A is ROW-MAJOR")))
                   transa))
             (tb (if (eql :row-major (order b))
                     (case transb
                       (:n :t)
                       (:t :n)
                       (:c (error "Specifying TRANSB to be :C is not allowed if B is ROW-MAJOR")))
                     transb))
             (target (or target
                         (empty
                          (list m n)
                          :type ',type))))
         (,blas-function
          (ecase ta
            (:t "T")
            (:c "C")
            (:n "N"))
          (ecase tb
            (:t "T")
            (:c "C")
            (:n "N"))
          m
          n
          k
          alpha
          (storage a)
          (if (eql :n ta) m k)
          (storage b)
          (if (eql :n tb) k n)
          beta
          (storage target)
          m)
         target)))
     )
