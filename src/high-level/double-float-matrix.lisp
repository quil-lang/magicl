(in-package #:magicl)

(defmatrix matrix/double-float double-float 0.0)

(defmethod @ ((a matrix/double-float) (b matrix/double-float) &key target (alpha 1.0) (beta 0.0) transa transb)
    "Multiply matrix a by matrix b, storing in target or creating a new matrix if target is not specified.
Target cannot be the same as a or b."
  ;; (check-type target (or matrix/double-float nil))
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
                     (make-matrix/double-float
                      (list m n)
                      :storage (make-array
                                (list (* m n))
                                :element-type 'double-float)))))
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
