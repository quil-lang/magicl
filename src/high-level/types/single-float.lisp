;;;; single-float.lisp
;;;;
;;;; Author: Cole Scotts

(in-package #:magicl)

(deftensor tensor/single-float single-float)

(defmatrix matrix/single-float single-float tensor/single-float)

(defvector vector/single-float single-float tensor/single-float matrix/single-float)

(defcompatible
    (lambda (tensor)
      (case (rank tensor)
        (1 '(vector/single-float
             matrix/single-float
             tensor/single-float))
        (2 '(matrix/single-float
             tensor/single-float))
        (t '(tensor/single-float))))
  tensor/single-float
  matrix/single-float
  vector/single-float)

(defmethod @ ((a matrix/single-float) (b matrix/single-float) &key target (alpha 1.0) (beta 0.0) transa transb)
    
    ;; (check-type target (or matrix/single-float nil))
    (check-type transa (member nil :n :t :c))
  (check-type transb (member nil :n :t :c))
  ;; TODO: make sure we can trans mixed order matrices (hard)
  ;; Maybe just error with :row-major if :transX is :t or :c
  (let* ((transa (or transa
                     (if (eq :row-major (order a)) :t :n)))
         (transb (or transb
                     (if (eq :row-major (order b)) :t :n)))
         (m (if (eq :n transa) (nrows a) (ncols a)))
         (k (if (eq :n transa) (ncols a) (nrows a)))
         (n (if (eq :n transb) (ncols b) (nrows b)))
         (brows (if (eq :n transb) (nrows b) (ncols b)))
         (target (or target
                     (empty
                      (list m n)
                      :type 'single-float))))
    (assert (= k brows)
            () "Incompatible matrix sizes ~a and ~a." (list m k) (list brows n))
    (magicl.blas-cffi:%sgemm
     (string transa)
     (string transb)
     m
     n
     k
     alpha
     (storage a)
     (nrows a)
     (storage b)
     (nrows b)
     beta
     (storage target)
     m)
    target))

(defmethod lu ((matrix matrix/single-float))
  (let* ((a-tensor (deep-copy-tensor matrix))
         (a (storage a-tensor))
         (m (nrows a-tensor))
         (n (ncols a-tensor))
         (lda m)
         (ipiv-tensor (empty (list (max m n)) :type '(signed-byte 32)))
         (ipiv (storage ipiv-tensor))
         (info 0))
    (when (eql :row-major (order a-tensor)) (transpose! a-tensor))
    (magicl.lapack-cffi:%sgetrf
     m
     n
     a
     lda
     ipiv
     info)
    (values a-tensor ipiv-tensor)))
