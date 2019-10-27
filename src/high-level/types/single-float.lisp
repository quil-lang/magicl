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

(defmethod = ((tensor1 tensor/single-float) (tensor2 tensor/single-float) &optional (epsilon +double-comparison-threshold-strict+))
  (unless (equal (shape tensor1) (shape tensor2))
    (return-from = nil))
  (map-indexes
   (shape tensor1)
   (lambda (&rest pos)
     (unless (> epsilon
                (abs (cl:- (apply #'tref tensor1 pos)
                           (apply #'tref tensor2 pos))))
       (return-from = nil))))
  t)

(defmethod = ((tensor1 matrix/single-float) (tensor2 matrix/single-float) &optional (epsilon +double-comparison-threshold-strict+))
  (unless (equal (shape tensor1) (shape tensor2))
    (return-from = nil))
  (map-indexes
   (shape tensor1)
   (lambda (&rest pos)
     (unless (> epsilon
                (abs (cl:- (apply #'tref tensor1 pos)
                           (apply #'tref tensor2 pos))))
       (return-from = nil))))
  t)

(def-lapack-mult matrix/single-float single-float magicl.blas-cffi:%sgemm)
(def-lapack-lu matrix/single-float single-float magicl.lapack-cffi:%sgetrf)
(def-lapack-inv matrix/single-float single-float magicl.lapack-cffi:%sgetrf magicl.lapack-cffi:%sgetri)
(def-lapack-svd matrix/single-float single-float magicl.lapack-cffi:%sgesvd)
(def-lapack-eig matrix/single-float single-float magicl.lapack-cffi:%sgeev)
(def-lapack-ql-qr-rq-lq matrix/single-float single-float
  magicl.lapack-cffi:%sgeqlf magicl.lapack-cffi:%sgeqrf magicl.lapack-cffi:%sgerqf magicl.lapack-cffi:%sgelqf
  magicl.lapack-cffi:%sorgql magicl.lapack-cffi:%sorgqr magicl.lapack-cffi:%sorgrq magicl.lapack-cffi:%sorglq)
