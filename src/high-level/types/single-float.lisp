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

(def-lapack-mult matrix/single-float single-float magicl.blas-cffi:%sgemm)

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
