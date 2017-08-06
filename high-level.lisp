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

(defun vector-to-list (v)
  "Make a list from a fnv."
  (loop :for i :below (fnv:fnv-length v)
        :collect 
        (etypecase v
          (fnv:fnv-float          (fnv:fnv-float-ref v i))
          (fnv:fnv-double         (fnv:fnv-double-ref v i))
          (fnv:fnv-complex-float  (fnv:fnv-complex-float-ref v i))
          (fnv:fnv-complex-double (fnv:fnv-complex-double-ref v i)))))

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
        (cols (matrix-cols m)))
    (multiple-value-bind (a tau) (lapack-qr m)
      ; serial let is important because qr-helper-get-q modifies a
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

(defun lapack-qr (m)
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
      (values a tau))))

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

(defun slice (m rmin rmax cmin cmax)
  "Get the subarray given by M(rmin:rmax, cmin:cmax)."
  (let ((rows (matrix-rows m))
        (cols (matrix-cols m)))
    (assert (<= 0 rmin rmax rows) () "Invalid row indices")
    (assert (<= 0 cmin cmax cols) () "Invalid column indices")
    (let* ((sliced-rows (- rmax rmin))
          (sliced-cols (- cmax cmin))
          (v (fnv:make-fnv-complex-double (* sliced-rows sliced-cols))))
      (dotimes (j sliced-cols)
        (dotimes (i sliced-rows)
          (setf (fnv:fnv-complex-double-ref 
                 v (+ (* j sliced-rows) i)) (ref m (+ rmin i) (+ cmin j)))))
      (values (make-matrix :rows sliced-rows :cols sliced-cols :data v)))))

(defun csd (x p q)
  "Find the Cosine-Sine Decomposition of a matrix X given that it is to be partitioned
with upper left block with dimension P-by-Q."
  (let ((m (matrix-rows x))
        (n (matrix-cols x)))
    (assert (= m n) () "X is not a square matrix")
    (check-type p integer)
    (check-type q integer)
    (assert (<= 1 p (1- m)) () "P = ~D is out of range" p)
    (assert (<= 1 q (1- m)) () "Q = ~D is out of range" q)
    (let ((jobu1 "Y")
          (jobu2 "Y")
          (jobv1t "Y")
          (jobv2t "Y")
          (trans "F")
          (signs "D")
          (ldx11 p)
          (ldx12 p)
          (ldx21 (- m p))
          (ldx22 (- m p))
          (r (min p (- m p) q (- m q)))
          (ldu1 p)
          (ldu2 (- m p))
          (ldv1t q)
          (ldv2t (- m q))
          (lwork -1)
          (work (fnv:make-fnv-complex-double 1))
          (lrwork -1)
          (rwork (fnv:make-fnv-double 1))
          (info 0))
      (let ((x11 (matrix-data (slice x 0 p 0 q)))
            (x12 (matrix-data (slice x 0 p q m)))
            (x21 (matrix-data (slice x p m 0 q)))
            (x22 (matrix-data (slice x p m q m)))
            (theta (fnv:make-fnv-double r))
            (u1 (fnv:make-fnv-complex-double (* ldu1 p)))
            (u2 (fnv:make-fnv-complex-double (* ldu2 (- m p))))
            (v1t (fnv:make-fnv-complex-double (* ldv1t q)))
            (v2t (fnv:make-fnv-complex-double (* ldv2t (- m q))))
            (iwork (fnv:make-fnv-int32 (- m r))))
        ; run it once as a workspace query
        (magicl.lapack-cffi::%zuncsd jobu1 jobu2 jobv1t jobv2t 
                                     trans signs m p q 
                                     x11 ldx11 x12 ldx12 x21 ldx21 x22 ldx22 
                                     theta u1 ldu1 u2 ldu2 v1t ldv1t v2t ldv2t 
                                     work lwork rwork lrwork iwork info)
        (setf lwork (truncate (realpart (fnv:fnv-complex-double-ref work 0))))
        (setf work (fnv:make-fnv-complex-double (max 1 lwork)))
        (setf lrwork (truncate (fnv:fnv-double-ref rwork 0)))
        (setf rwork (fnv:make-fnv-double (max 1 lrwork)))
        ; run it again with optimal workspace size
        (magicl.lapack-cffi::%zuncsd jobu1 jobu2 jobv1t jobv2t 
                                     trans signs m p q 
                                     x11 ldx11 x12 ldx12 x21 ldx21 x22 ldx22 
                                     theta u1 ldu1 u2 ldu2 v1t ldv1t v2t ldv2t 
                                     work lwork rwork lrwork iwork info)
        (values (make-matrix :rows p :cols p :data u1)
                (make-matrix :rows (- m p) :cols (- m p) :data u2)
                (make-matrix :rows q :cols q :data v1t)
                (make-matrix :rows (- m q) :cols (- m q) :data v2t)
                (vector-to-list theta))))))

(defun csd-from-blocks (u1 u2 v1t v2t theta)
  (let ((p (matrix-rows u1))
        (q (matrix-rows v1t))
        (m (+ (matrix-rows u1) (matrix-rows u2)))
        (r (length theta)))
    (let ((u (make-matrix :rows m :cols m 
                          :data (fnv:make-fnv-complex-double (* m m) 
                                                             :initial-value #C (0.0d0 0.0d0))))
          (sigma (make-matrix :rows m :cols m 
                          :data (fnv:make-fnv-complex-double (* m m) 
                                                             :initial-value #C (0.0d0 0.0d0))))
          (vt (make-matrix :rows m :cols m 
                          :data (fnv:make-fnv-complex-double (* m m) 
                                                             :initial-value #C (0.0d0 0.0d0)))))
      ; Create U block by block
      (loop for i from 0 to (1- p)
            do (loop for j from 0 to (1- p)
                     do (setf (ref u i j) (ref u1 i j))))
      (loop for i from 0 to (- m p 1)
            do (loop for j from 0 to (- m p 1)
                     do (setf (ref u (+ i p) (+ j p)) (ref u2 i j))))
      
      ; Create SIGMA block by block
      (let ((diag11 (min p q))
            (diag12 (min p (- m q)))
            (diag21 (min (- m p) q))
            (diag22 (min (- m p) (- m q))))
        (let ((iden11 (- diag11 r))
              (iden12 (- diag12 r))
              (iden21 (- diag21 r))
              (iden22 (- diag22 r)))
          (loop for i from 0 to (1- iden11)
                do (setf (ref sigma i i) #C (1.0d0 0.0d0)))
          (loop for i from iden11 to (1- diag11)
                do (setf (ref sigma i i) (cos (nth (- i iden11) theta))))
          (loop for i from 0 to (1- iden12)
                do (setf (ref sigma (- p 1 i) (- m 1 i)) #C (-1.0d0 0.0d0)))
          (loop for i from iden12 to (1- diag12)
                do (setf (ref sigma (- p 1 i) (- m 1 i))
                         (- (sin (nth (- r 1 (- i iden12)) theta)))))
          (loop for i from 0 to (1- iden21)
                do (setf (ref sigma (- m 1 i) (- q 1 i)) #C (1.0d0 0.0d0)))
          (loop for i from iden21 to (1- diag21)
                do (setf (ref sigma (- m 1 i) (- q 1 i))
                         (sin (nth (- r 1 (- i iden21)) theta))))
          (loop for i from 0 to (1- iden22)
                do (setf (ref sigma (+ p i) (+ q i)) #C (1.0d0 0.0d0)))
          (loop for i from iden22 to (1- diag22)
                do (setf (ref sigma (+ p i) (+ q i)) (cos (nth (- i iden22) theta))))))
      
      ; Ceate VT block by block
      (loop for i from 0 to (1- q)
            do (loop for j from 0 to (1- q)
                     do (setf (ref vt i j) (ref v1t i j))))
      (loop for i from 0 to (- m q 1)
            do (loop for j from 0 to (- m q 1)
                     do (setf (ref vt (+ i q) (+ j q)) (ref v2t i j))))
      
      (values u sigma vt))))

(defun lu-factorize (m)
  (let ((rows (matrix-rows m))
        (cols (matrix-cols m))
        (a (matrix-data m))
        (info 0))
    (let ((lda rows)
          (ipiv (fnv:make-fnv-int32 (min rows cols))))
      (magicl.lapack-cffi::%zgetrf rows cols a lda ipiv info)
      (values a ipiv))))


