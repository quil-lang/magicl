;;;; complex-double-float.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(deftensor tensor/complex-double-float (complex double-float))

(defmatrix matrix/complex-double-float (complex double-float) tensor/complex-double-float)

(defvector vector/complex-double-float (complex double-float) tensor/complex-double-float matrix/complex-double-float)

(defcompatible
    (lambda (tensor)
      (case (rank tensor)
        (1 '(vector/complex-double-float
             matrix/complex-double-float
             tensor/complex-double-float))
        (2 '(matrix/complex-double-float
             tensor/complex-double-float))
        (t '(tensor/complex-double-float))))
  tensor/complex-double-float
  matrix/complex-double-float
  vector/complex-double-float)

(defmethod dot ((vector1 vector/complex-double-float) (vector2 vector/complex-double-float))
  (assert (cl:= (size vector1) (size vector2))
          () "Vectors must have the same size. The first vector is size ~a and the second vector is size ~a."
          (size vector1) (size vector2))
  (loop :for i :below (size vector1)
        :sum (* (tref vector1 i) (conjugate (tref vector2 i)))))

(defmethod orthonormalize!((m matrix/complex-double-float))
    "Applies Gram-Schmidt to the columns of a full rank square matrix to produce a unitary matrix, replacing the elements"
    (assert (= (nrows m) (ncols m))
            () "Matrix must be square")
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

(defmethod @ ((a matrix/complex-double-float) (b matrix/complex-double-float) &key target (alpha #C(1d0 0d0)) (beta #C (0d0 0d0)) transa transb)
    
    ;; (check-type target (or matrix/complex-double-float nil))
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
                      :type '(complex double-float)))))
    (assert (= k brows)
            () "Incompatible matrix sizes ~a and ~a." (list m k) (list brows n))
    (magicl.blas-cffi:%zgemm
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
