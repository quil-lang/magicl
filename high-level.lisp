(in-package #:magicl)

(defstruct matrix
  rows
  cols
  data)

(defun make-complex-vector (&rest entries)
  "Makes a complex vector out entries, a list of complex numbers."
  (let ((v (fnv:make-fnv-complex-double (length entries)))
        (i 0))
    (over-fnv-complex-double (p) v
      (let ((e (nth i entries)))
        (assert (numberp e) () "~S is not a number" e)
        (setf p (coerce e (list 'complex 'double-float)))
        (incf i)))
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

(defun set-val (m i j val)
  (let ((rows (matrix-rows m))
        (cols (matrix-cols m))
        (data (matrix-data m)))
    (assert (integerp i) () "row index ~S is not an integer" i)
    (assert (integerp j) () "column index ~S is not an integer" j)
    (assert (< -1 i rows) () "row index ~S is out of range" i)
    (assert (< -1 j cols) () "col index ~S is out of range" j)
    (assert (numberp val) () "value ~S is not a number" val)
    (setf (fnv:fnv-complex-double-ref data (+ (* rows j) i)) 
          (coerce val (list 'complex 'double-float)))))

(defun print-matrix (m)
  "Print method for matrices."
  (dotimes (i (matrix-rows m))
    (dotimes (j (matrix-cols m))
      (princ (ref m i j))
      (princ #\Space))
    (princ #\Newline)))

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
      (let* ((r (qr-helper-get-r a cols))
             (q (qr-helper-get-q a tau cols)))
        ; change signs if diagonal elements of r are negative
        (dotimes (j cols)
          (let ((diag-elt (ref r j j)))
            (assert (= (imagpart diag-elt) 0) 
                    () "Diagonal element R_~S~S=~S is not real" j j diag-elt)
            (setf diag-elt (realpart diag-elt))
            (if (> 0 diag-elt)
                (dotimes (i rows)
                  (if (<= j i (1- cols))
                      (set-val r j i (- (ref r j i))))
                  (set-val q i j (- (ref q i j)))))))
        (values q r)))))

(defun qr-helper-get-r (a n)
  "Get the matrix R from the upper triangular portion of a, where n is the number of columns"
  (let ((m (/ (fnv:fnv-length a) n))
        (r (fnv:make-fnv-complex-double (* n n))))
    (dotimes (j n)
      (dotimes (i n)
        (let ((entry (coerce #C (0 0) (list 'complex 'double-float))))
          (if (>= j i)
              (setf entry (fnv:fnv-complex-double-ref a (+ (* m j) i))))
          (setf (fnv:fnv-complex-double-ref r (+ (* n j) i)) entry))))
    (values (make-matrix :rows n :cols n :data r))))

(defun qr-helper-get-q (a tau n)
  "Get the matrix Q as a product of reflectors, from results given by ZGEQRF."
  (let ((m (/ (fnv:fnv-length a) n))
        (k (fnv:fnv-length tau))
        (lwork -1)
        (info 0))
    (let ((lda m)
          (work (fnv:make-fnv-complex-double (max 1 lwork))))
      ; run it once as a workspace query
      (magicl.lapack-cffi::%zungqr m n k a lda tau work lwork info)
      (setf lwork (truncate (realpart (fnv:fnv-complex-double-ref work 0))))
      (setf work (fnv:make-fnv-complex-double (max 1 lwork)))
      ; run it again with optimal workspace size
      (magicl.lapack-cffi::%zungqr m n k a lda tau work lwork info)
      (values (make-matrix :rows m :cols n :data a)))))

; (defun svd (m))

