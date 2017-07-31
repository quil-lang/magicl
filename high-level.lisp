(in-package #:magicl)

(defstruct matrix
  rows
  cols
  data)

(defun make-complex-vector (&rest entries)
  "Makes a complex vector out entries, a list of complex numbers."
  (let* ((n (length entries))
         (v (fnv:make-fnv-complex-double n)))
    (dotimes (i n)
      (let ((e (nth i entries)))
        (assert (numberp e) () "~S is not a number" e)
        (setf (fnv:fnv-complex-double-ref v i) (complex (coerce e 'double-float)))))
    (values v)))

(defun make-complex-matrix (m n &rest entries)
  "Makes an m-by-n matrix assuming entries is a list of complex numbers in column major order."
  (let ((entries-size (length entries))
        (expected-size (* m n)))
    (assert (eq entries-size expected-size)
            ()
            "Length of entries is ~S, is not ~S*~S = ~S" 
            entries-size m n expected-size)
    (values (make-matrix :rows m :cols n :data (apply #'make-complex-vector entries)))))

(defun ref (m i j)
  "Accessor method for the element in the i-th row and j-th column of m, assuming zero indexing."
  (let ((rows (matrix-rows m))
        (cols (matrix-cols m))
        (data (matrix-data m)))
    (assert (integerp i) () "row index ~S is not an integer" i)
    (assert (integerp j) () "column index ~S is not an integer" j)
    (assert (< -1 i rows) () "row index ~S is out of range" i)
    (assert (< -1 j cols) () "col index ~S is out of range" j)
    (fnv:fnv-complex-double-ref data (+ (* rows j) i))))

(defun qr (m)
  "Finds the QR factorization of the matrix m."
  (let ((rows (matrix-rows m))
        (cols (matrix-cols m))
        (a (copy-fnv-complex-double (matrix-data m)))
        (lwork -1)
        (info 0))
    (let ((lda rows)
          (tau (fnv:make-fnv-complex-double (min rows cols)))
          (work (fnv:make-fnv-complex-double (max 1 lwork))))
      ; run it once as a workspace query
      (magicl.lapack-cffi::%zgeqrf rows cols a lda tau work lwork info)
      (setf lwork (truncate (realpart (fnv:fnv-complex-double-ref work 0))))
      (setf work (fnv:make-fnv-complex-double (max 1 lwork)))
      ; run it again with optimal workspace size
      (magicl.lapack-cffi::%zgeqrf rows cols a lda tau work lwork info)
      (values a tau))))

; (defun svd (m))

