(in-package #:magicl)

(defstruct matrix
  rows
  cols
  data)

(defun make-complex-vector (&rest entries)
  "Makes a complex vector out ENTRIES, a list of complex numbers."
  (let* ((len (length entries))
         (v (fnv:make-fnv-complex-double len)))
    (loop :for i :below len
          :for e :in entries
          :do (progn
                (check-type e number)
                (setf (fnv:fnv-complex-double-ref v i) (coerce e '(complex double-float))))
          :finally (return v))))

(defun complex-vector-to-list (v)
  "Make a list from a complex double fnv."
  (loop :for i :below (fnv:fnv-length v)
        :collect (fnv:fnv-complex-double-ref v i)))

(defun make-complex-matrix (m n &rest entries)
  "Makes an m-by-n matrix assuming entries is a list of complex numbers in column major order."
  (let ((entries-size (length entries))
        (expected-size (* m n)))
    (assert (= entries-size expected-size)
            ()
            "Length of entries is ~D, is not ~D * ~D = ~D" 
            entries-size m n expected-size)
    (make-matrix :rows m
                 :cols n
                 :data (apply #'make-complex-vector entries))))

(defun diag (m n &rest entries)
  "Creates a matrix with entries along the diagonal"
  (let ((entries-size (length entries))
        (expected-size (min m n)))
    (assert (= entries-size expected-size) ()
            "Min dimension is ~S but number of entries is ~S" expected-size entries-size)
    (let ((mat (apply #'make-complex-matrix m n (make-list (* m n) :initial-element #C(0.0d0 0.0d0)))))
      (dotimes (i entries-size mat)
        (setf (ref mat i i) (nth i entries))))))

(defun ref (m i j)
  "Accessor method for the element in the i-th row and j-th column of m, assuming zero indexing."
  (let ((rows (matrix-rows m))
        (cols (matrix-cols m))
        (data (matrix-data m)))
    (check-type i integer)
    (check-type j integer)
    (assert (< -1 i rows) () "row index ~D is out of range" i)
    (assert (< -1 j cols) () "col index ~D is out of range" j)
    (let ((idx (+ (* rows j) i)))
      (etypecase data
        (fnv:fnv-float          (fnv:fnv-float-ref data idx))
        (fnv:fnv-double         (fnv:fnv-double-ref data idx))
        (fnv:fnv-complex-float  (fnv:fnv-complex-float-ref data idx))
        (fnv:fnv-complex-double (fnv:fnv-complex-double-ref data idx))))))

(defun (setf ref) (new-value m i j)
  "Set the value of m_ij to val."
  (let ((rows (matrix-rows m))
        (cols (matrix-cols m))
        (data (matrix-data m)))
    (check-type i integer)
    (check-type j integer)
    (check-type new-value number)
    (assert (< -1 i rows) () "row index ~S is out of range" i)
    (assert (< -1 j cols) () "col index ~S is out of range" j)
    (let ((idx (+ (* rows j) i)))
      (etypecase data
        (fnv:fnv-float          (setf (fnv:fnv-float-ref data idx) (coerce new-value 'single-float)))
        (fnv:fnv-double         (setf (fnv:fnv-double-ref data idx) (coerce new-value 'double-float)))
        (fnv:fnv-complex-float  (setf (fnv:fnv-complex-float-ref data idx) (coerce new-value '(complex single-float))))
        (fnv:fnv-complex-double (setf (fnv:fnv-complex-double-ref data idx) (coerce new-value '(complex double-float))))))))

(defun print-matrix (m)
  "Print method for matrices."
  (dotimes (i (matrix-rows m))
    (dotimes (j (matrix-cols m))
      (princ (ref m i j))
      (princ #\Space))
    (princ #\Newline)))

(defun multiply-complex-matrices (ma mb)
  (assert (= (matrix-cols ma) (matrix-rows mb)) ()
          "Matrix A has ~S columns while matrix B has ~S rows" (matrix-cols ma) (matrix-rows mb))
  (let ((transa "N")
        (transb "N")
        (m (matrix-rows ma))
        (n (matrix-cols mb))
        (k (matrix-cols ma))
        (alpha (coerce 1 '(complex double-float)))
        (a (fnv:copy-fnv-complex-double (matrix-data ma)))
        (b (fnv:copy-fnv-complex-double (matrix-data mb)))
        (beta (coerce 0 '(complex double-float))))
    (let ((lda m)
          (ldb k)
          (ldc m)
          (c (fnv:make-fnv-complex-double (* m n))))
      (magicl.blas-cffi::%zgemm transa transb m n k alpha a lda b ldb beta c ldc)
      (make-matrix :rows m :cols n :data c))))

(defun qr (m)
  "Finds the QR factorization of the matrix m."
  (let ((rows (matrix-rows m))
        (cols (matrix-cols m))
        (a (fnv:copy-fnv-complex-double (matrix-data m)))
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
            (if (minusp diag-elt)
                (dotimes (i rows)
                  (if (<= j i (1- cols))
                      (setf (ref r j i) (- (ref r j i))))
                  (setf (ref q i j) (- (ref q i j)))))))
        (values q r)))))

(defun qr-helper-get-r (a n)
  "Get the matrix R from the upper triangular portion of a, where n is the number of columns"
  (let ((m (/ (fnv:fnv-length a) n))
        (r (fnv:make-fnv-complex-double (* n n))))
    (check-type m integer)
    (dotimes (j n)
      (dotimes (i n)
        (let ((entry (coerce 0 '(complex double-float))))
          (if (>= j i)
              (setf entry (fnv:fnv-complex-double-ref a (+ (* m j) i))))
          (setf (fnv:fnv-complex-double-ref r (+ (* n j) i)) entry))))
    (make-matrix :rows n :cols n :data r)))

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
      (make-matrix :rows m :cols n :data a))))

(defun svd (m)
  "Find the SVD of a matrix M. Return (VALUES U SIGMA Vt) where M = U*SIGMA*Vt"
  (let ((jobu "A")
        (jobvt "A")
        (rows (matrix-rows m))
        (cols (matrix-cols m))
        (a (fnv:copy-fnv-complex-double (matrix-data m)))
        (lwork -1)
        (info 0))
    (let ((lda rows)
          (s (fnv:make-fnv-double (min rows cols)))
          (ldu rows)
          (ldvt cols)
          (work (fnv:make-fnv-complex-double (max 1 lwork)))
          (rwork (fnv:make-fnv-double (* 5 (min rows cols)))))
      (let ((u (fnv:make-fnv-complex-double (* ldu rows)))
            (vt (fnv:make-fnv-complex-double (* ldvt cols))))
        ; run it once as a workspace query
        (magicl.lapack-cffi::%zgesvd jobu jobvt rows cols a lda s u ldu vt ldvt 
                                     work lwork rwork info)
        (setf lwork (truncate (realpart (fnv:fnv-complex-double-ref work 0))))
        (setf work (fnv:make-fnv-complex-double (max 1 lwork)))
        ; run it again with optimal workspace size
        (magicl.lapack-cffi::%zgesvd jobu jobvt rows cols a lda s u ldu vt ldvt 
                                     work lwork rwork info)
        (let ((smat (fnv:make-fnv-double (* rows cols) :initial-value 0.0d0)))
          (dotimes (i (min rows cols))
            (setf (fnv:fnv-double-ref smat (+ (* rows i) i)) (fnv-double-ref s i)))          
          (values (make-matrix :rows rows :cols rows :data u)
                  (make-matrix :rows rows :cols cols :data smat)
                  (make-matrix :rows cols :cols cols :data vt)))))))
