;;;; complex-single-float.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(deftensor tensor/complex-single-float (complex single-float))

(defmatrix matrix/complex-single-float (complex single-float) tensor/complex-single-float)

(defvector vector/complex-single-float (complex single-float) tensor/complex-single-float matrix/complex-single-float)

(defcompatible
    (lambda (tensor)
      (case (rank tensor)
        (1 '(vector/complex-single-float
             matrix/complex-single-float
             tensor/complex-single-float))
        (2 '(matrix/complex-single-float
             tensor/complex-single-float))
        (t '(tensor/complex-single-float))))
  tensor/complex-single-float
  matrix/complex-single-float
  vector/complex-single-float)

(defmethod = ((tensor1 tensor/complex-single-float) (tensor2 tensor/complex-single-float) &optional (epsilon 0))
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

(defmethod = ((tensor1 matrix/complex-single-float) (tensor2 matrix/complex-single-float) &optional (epsilon 0))
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

(def-lapack-mult matrix/complex-single-float (complex single-float) magicl.blas-cffi:%cgemm)
(def-lapack-lu matrix/complex-single-float (complex single-float) magicl.lapack-cffi:%cgetrf)
(def-lapack-inv matrix/complex-single-float (complex single-float) magicl.lapack-cffi:%cgetrf magicl.lapack-cffi:%cgetri)
(def-lapack-svd matrix/complex-single-float (complex single-float) magicl.lapack-cffi:%cgesvd single-float)
(def-lapack-eig matrix/complex-single-float (complex single-float) magicl.lapack-cffi:%cgeev single-float)
(def-lapack-hermitian-eig matrix/complex-single-float (complex single-float) magicl.lapack-cffi:%cheev single-float)
(def-lapack-ql-qr-rq-lq matrix/complex-single-float (complex single-float)
  magicl.lapack-cffi:%cgeqlf magicl.lapack-cffi:%cgeqrf magicl.lapack-cffi:%cgerqf magicl.lapack-cffi:%cgelqf
  magicl.lapack-cffi:%cungql magicl.lapack-cffi:%cungqr magicl.lapack-cffi:%cungrq magicl.lapack-cffi:%cunglq)

(defmethod dot ((vector1 vector/complex-single-float) (vector2 vector/complex-single-float))
  (assert (cl:= (size vector1) (size vector2))
          () "Vectors must have the same size. The first vector is size ~a and the second vector is size ~a."
          (size vector1) (size vector2))
  (loop :for i :below (size vector1)
        :sum (* (tref vector1 i) (conjugate (tref vector2 i)))))

(defmethod orthonormalize!((m matrix/complex-single-float))
  "Applies Gram-Schmidt to the columns of a full rank square matrix to produce a unitary matrix, replacing the elements"
  (assert-square-matrix m)
  ;; consider each column
  (dotimes (j (ncols m))
    ;; consider each preceding column, which together form an orthonormal set
    (dotimes (jp j)
      ;; compute the dot product of the columns...
      (let ((scalar
              (loop :for i :below (nrows m)
                    :sum (* (tref m i j)
                            (conjugate (tref m i jp))))))
        ;; ... and do the subtraction.
        (dotimes (i (nrows m))
          (setf (tref m i j)
                (cl:- (tref m i j)
                      (* scalar
                         (tref m i jp)))))))
    ;; now j is orthogonal to the things that came before it. normalize it.
    (let ((scalar
            (sqrt
             (loop :for i :below (nrows m)
                   :sum (* (abs (tref m i j))
                           (abs (tref m i j)))))))
      (dotimes (i (nrows m))
        (setf (tref m i j)
              (/ (tref m i j) scalar)))))
  m)
