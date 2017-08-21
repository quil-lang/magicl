;;;; high-level.lisp
;;;;
;;;; Author: Joseph Lin

(in-package #:magicl)

(defstruct matrix
  rows
  cols
  data)

(defun make-complex-foreign-vector (&rest entries)
  "Makes a complex double FNV out ENTRIES, a list of complex numbers."
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

(defun make-complex-vector (&rest entries)
  "Makes a complex column vector out of ENTRIES, a list of complex numbers."
  (apply #'make-complex-matrix (length entries) 1 entries))

(defun make-complex-matrix (m n &rest entries)
  "Makes an M-by-N matrix assuming ENTRIES is a list of complex numbers in column major order."
  (let ((entries-size (length entries))
        (expected-size (* m n)))
    (assert (= entries-size expected-size)
            ()
            "Length of entries is ~D, is not ~D * ~D = ~D" 
            entries-size m n expected-size)
    (make-matrix :rows m
                 :cols n
                 :data (apply #'make-complex-foreign-vector entries))))

(defun diag (m n &rest entries)
  "Creates a matrix with ENTRIES along the diagonal"
  (let ((entries-size (length entries))
        (expected-size (min m n)))
    (assert (= entries-size expected-size) ()
            "Min dimension is ~S but number of entries is ~S" expected-size entries-size)
    (let ((mat (apply #'make-complex-matrix 
                      m n (make-list (* m n) :initial-element #C(0.0d0 0.0d0)))))
      (dotimes (i entries-size mat)
        (setf (ref mat i i) (nth i entries))))))

(defun ref (m i j)
  "Accessor method for the element in the I-th row and J-th column of a matrix M, assuming zero indexing."
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

(defun ptr-ref (m i j)
  "Accessor method for the pointer to the element in the I-th row and J-th column of a matrix M, assuming zero indexing."
  (let ((rows (matrix-rows m))
        (cols (matrix-cols m))
        (data (matrix-data m)))
    (check-type i integer)
    (check-type j integer)
    (assert (< -1 i rows) () "row index ~D is out of range" i)
    (assert (< -1 j cols) () "col index ~D is out of range" j)
    (let ((head (fnv:fnv-foreign-pointer data))
          (idx (+ (* rows j) i)))
      (etypecase data
        (fnv:fnv-float          (cffi:mem-aptr head :float idx))
        (fnv:fnv-double         (cffi:mem-aptr head :double idx))
        (fnv:fnv-complex-float  (cffi:mem-aptr head :float (* 2 idx)))
        (fnv:fnv-complex-double (cffi:mem-aptr head :double (* 2 idx)))))))

(defun (setf ref) (new-value m i j)
  "Set the value of M_IJ to NEW-VALUE."
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
      (format t "~D " (ref m i j)))
    (format t "~%"))
  (format t "~%"))

(defun multiply-complex-matrices (ma mb)
  "Multiplies two complex marices MA and MB, returning MA*MB. If MA is M x KA and MB is KB x N,
it must be that KA = KB, and the resulting matrix is M x N."
  (let ((ka (matrix-cols ma))
        (kb (matrix-rows mb)))
    (assert (= ka kb) ()
            "Matrix A has ~S columns while matrix B has ~S rows" ka kb)
    (let ((n (matrix-cols mb))
          (m (matrix-rows ma))
          (a (fnv:copy-fnv-complex-double (matrix-data ma)))
          (b (fnv:copy-fnv-complex-double (matrix-data mb))))
      (if (= n 1)
          ; mb is a column vector
          (if (= m 1)
              ; ma is a row vector
              ; use dot product
              (magicl.blas-cffi::%zdotu ka a 1 b 1)
              ; use matrix-vector multiplication
              (let ((trans "N")
                    (alpha (coerce 1 '(complex double-float)))
                    (beta (coerce 0 '(complex double-float)))
                    (y (fnv:make-fnv-complex-double kb)))
                (magicl.blas-cffi::%zgemv trans m ka alpha a m b 1 beta y 1)
                (make-matrix :rows m :cols n :data y)))
          ; use matrix-matrix multiplication
          (let ((transa "N")
                (transb "N")
                (alpha (coerce 1 '(complex double-float)))
                (beta (coerce 0 '(complex double-float)))
                (c (fnv:make-fnv-complex-double (* m n))))
            (magicl.blas-cffi::%zgemm transa transb m n ka alpha a m b kb beta c m)
            (make-matrix :rows m :cols n :data c))))))

(defun scale (alpha x)
  "Scale a complex double matrix X by a complex double ALPHA, i.e. return ALPHA*X."
  (let* ((zx (fnv:copy-fnv-complex-double (matrix-data x)))
         (za (coerce alpha '(complex double-float)))
         (n (fnv:fnv-length zx)))
    (magicl.blas-cffi::%zscal n za zx 1)
    (values (make-matrix :rows (matrix-rows x) :cols (matrix-cols x) :data zx))))

(defun qr (m)
  "Finds the QR factorization of the matrix M."
  (let ((rows (matrix-rows m))
        (cols (matrix-cols m)))
    (multiple-value-bind (a tau) (lapack-unitary-triangular-decomposition m "QR")
      (let* ((amat (make-matrix :rows rows :cols cols :data a))
             (r (get-square-triangular amat T cols))
             (q (unitary-triangular-helper-get-q amat tau "QR")))
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

(defun ql (m)
  "Finds the QL factorization of the matrix M."
  (let ((rows (matrix-rows m))
        (cols (matrix-cols m)))
    (multiple-value-bind (a tau) (lapack-unitary-triangular-decomposition m "QL")
      (let* ((amat (make-matrix :rows rows :cols cols :data a))
             (l (get-square-triangular amat nil cols))
             (q (unitary-triangular-helper-get-q amat tau "QL")))
          ; change signs if diagonal elements of L are negative
          (dotimes (j cols)
            (let ((diag-elt (ref l j j)))
              (assert (= (imagpart diag-elt) 0) 
                      () "Diagonal element L_~S~S=~S is not real" j j diag-elt)
              (setf diag-elt (realpart diag-elt))
              (if (minusp diag-elt)
                  (dotimes (i rows)
                    (if (<= i j)
                        (setf (ref l j i) (- (ref l j i))))
                    (setf (ref q i j) (- (ref q i j)))))))
        (values q l)))))

(defun rq (m)
  "Finds the RQ factorization of the matrix M."
  (let ((rows (matrix-rows m))
        (cols (matrix-cols m)))
    (multiple-value-bind (a tau) (lapack-unitary-triangular-decomposition m "RQ")
      (let* ((amat (make-matrix :rows rows :cols cols :data a))
             (r (get-square-triangular amat T rows))
             (q (unitary-triangular-helper-get-q amat tau "RQ")))
          ; change signs if diagonal elements of r are negative
          (dotimes (i rows)
            (let ((diag-elt (ref r i i)))
              (assert (= (imagpart diag-elt) 0) 
                      () "Diagonal element R_~S~S=~S is not real" i i diag-elt)
              (setf diag-elt (realpart diag-elt))
              (if (minusp diag-elt)
                  (dotimes (j cols)
                    (if (<= j i)
                        (setf (ref r j i) (- (ref r j i))))
                    (setf (ref q i j) (- (ref q i j)))))))
        (values q r)))))

(defun lq (m)
  "Finds the LQ factorization of the matrix M."
  (let ((rows (matrix-rows m))
        (cols (matrix-cols m)))
    (multiple-value-bind (a tau) (lapack-unitary-triangular-decomposition m "LQ")
      (let* ((amat (make-matrix :rows rows :cols cols :data a))
             (r (get-square-triangular amat nil rows))
             (q (unitary-triangular-helper-get-q amat tau "LQ")))
          ; change signs if diagonal elements of l are negative
          (dotimes (i rows)
            (let ((diag-elt (ref r i i)))
              (assert (= (imagpart diag-elt) 0) 
                      () "Diagonal element R_~S~S=~S is not real" i i diag-elt)
              (setf diag-elt (realpart diag-elt))
              (if (minusp diag-elt)
                  (dotimes (j cols)
                    (if (<= i j (1- rows))
                        (setf (ref r j i) (- (ref r j i))))
                    (setf (ref q i j) (- (ref q i j)))))))
        (values q r)))))

(defun lapack-unitary-triangular-decomposition (m option)
  "Finds the QR/QL/RQ/LQ factorization of the matrix M to the intermediate representation, as given by the LAPACK ZGE(QR/QL/RQ/LQ)F subroutine, depending on the string option OPTION."
  (let ((lapack-func))
    (cond ((string-equal option "QR")
           (setq lapack-func #'magicl.lapack-cffi::%zgeqrf))
          ((string-equal option "QL")
           (setq lapack-func #'magicl.lapack-cffi::%zgeqlf))
          ((string-equal option "RQ")
           (setq lapack-func #'magicl.lapack-cffi::%zgerqf))
          ((string-equal option "LQ")
           (setq lapack-func #'magicl.lapack-cffi::%zgelqf))
          (t (error "Option ~S is not one of: QR, QL, RQ, LQ" option)))
    (let ((rows (matrix-rows m))
          (cols (matrix-cols m))
          (a (fnv:copy-fnv-complex-double (matrix-data m)))
          (lwork -1)
          (info 0))
      (let ((lda rows)
            (tau (fnv:make-fnv-complex-double (min rows cols)))
            (work (fnv:make-fnv-complex-double (max 1 lwork))))
        ; run it once as a workspace query
        (apply lapack-func (list rows cols a lda tau work lwork info))
        (setf lwork (truncate (realpart (fnv:fnv-complex-double-ref work 0))))
        (setf work (fnv:make-fnv-complex-double (max 1 lwork)))
        ; run it again with optimal workspace size
        (apply lapack-func (list rows cols a lda tau work lwork info))
        (values a tau)))))

(defun get-square-triangular (a upper ord)
  "Creates a square matrix from a triangular or trapezoidal portion of A. The square matrix is upper triangular and taken from the upper portion of A if and only if UPPER is T. The order of the square matrix is given by ORD."
  (check-type upper boolean)
  (check-type ord (integer 1 *) "a positive integer")
  (let ((m (matrix-rows a))
        (n (matrix-cols a)))
    (assert (<= ord (max m n)) () "ORD, given as ~D, is greater than the maximum dimension of A, ~D." ord (max m n))

    (let ((tri (apply #'make-complex-matrix ord ord 
                                    (make-list (* ord ord) :initial-element #C(0.0d0 0.0d0)))))
      (if (> m n)
          (if upper
              (loop for i from 0 to (1- ord)
                    do (loop for j from (max 0 (+ (- n ord) i)) to (1- n)
                             do (setf (ref tri i (+ j (- ord n))) (ref a i j))))
              (loop for i from (- m ord) to (1- m)
                    do (loop for j from 0 to (min (+ (- ord m) i) (1- n))
                             do (setf (ref tri (- i (- m ord)) j) (ref a i j)))))
          (if upper
              (loop for j from (- n ord) to (1- n)
                    do (loop for i from 0 to (min (+ (- ord n) j) (1- m))
                             do (setf (ref tri i (- j (- n ord))) (ref a i j))))
              (loop for j from 0 to (1- ord)
                    do (loop for i from (max 0 (+ (- m ord) j)) to (1- m)
                             do (setf (ref tri (+ i (- ord m)) j) (ref a i j))))))
      (values tri))))

(defun unitary-triangular-helper-get-q (amat tau option)
  "Finds the unitary matrix Q from QR/QL/RQ/LQ factorization of the matrix M, given the reflectors and intermediate representation provided by the LAPACK ZGE(QR/QL/RQ/LQ)F subroutine. Whether the factorization is QR, QL, RQ or LQ is given by string option OPTION."
  (let ((lapack-func))
    (cond ((string-equal option "QR")
           (setq lapack-func #'magicl.lapack-cffi::%zungqr))
          ((string-equal option "QL")
           (setq lapack-func #'magicl.lapack-cffi::%zungql))
          ((string-equal option "RQ")
           (setq lapack-func #'magicl.lapack-cffi::%zungrq))
          ((string-equal option "LQ")
           (setq lapack-func #'magicl.lapack-cffi::%zunglq))
          (t (error "Option ~S is not one of: QR, QL, RQ, LQ" option)))
    (let ((m (matrix-rows amat))
          (n (matrix-cols amat))
          (a (matrix-data amat))
          (k (fnv:fnv-length tau))
          (lwork -1)
          (info 0))
      (let ((lda m)
            (work (fnv:make-fnv-complex-double (max 1 lwork))))
        ; run it once as a workspace query
        (apply lapack-func (list m n k a lda tau work lwork info))
        (setf lwork (truncate (realpart (fnv:fnv-complex-double-ref work 0))))
        (setf work (fnv:make-fnv-complex-double (max 1 lwork)))
        ; run it again with optimal workspace size
        (apply lapack-func (list m n k a lda tau work lwork info))
        (make-matrix :rows m :cols n :data a)))))

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
  "Get the subarray of M containing all elements M_IJ, where RMIN<=I<RMAX and CMIN<=J<CMAX."
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
  "Find the Cosine-Sine Decomposition of a matrix X given that it is to be partitioned with upper left block of dimension P-by-Q. Returns the CSD elements (VALUES U SIGMA VT) such that X=U*SIGMA*VT."
  (multiple-value-bind (u1 u2 v1t v2t theta) (lapack-csd x p q)
    (csd-from-blocks u1 u2 v1t v2t theta)))

(defun lapack-csd (x p q)
  "Find the Cosine-Sine Decomposition of a matrix X given that it is to be partitioned
with upper left block with dimension P-by-Q. Returns the intermediate representation given by the ZUNCSD LAPACK subroutine."
  (let* ((m (matrix-rows x))
         (n (matrix-cols x))
         (xcopy (make-matrix :rows m :cols n :data (fnv:copy-fnv-complex-double (matrix-data x)))))
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
          ; leading dimension is M because full X array will be used
          (ldx11 m)
          (ldx12 m)
          (ldx21 m)
          (ldx22 m)
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
      ; rather than slice up matrix, use full array with pointers to head of blocks
      (let ((x11 (matrix-data xcopy))
            (x12 (fnv:make-fnv-complex-double 
                  (* (- m q) m) 
                  :foreign-ptr (ptr-ref xcopy 0 q)))
            (x21 (fnv:make-fnv-complex-double 
                  (- (* m m) p) 
                  :foreign-ptr (ptr-ref xcopy p 0)))
            (x22 (fnv:make-fnv-complex-double 
                  (- (* (- m q) m) p) 
                  :foreign-ptr (ptr-ref xcopy p q)))
            (theta (fnv:make-fnv-double r))
            (u1 (fnv:make-fnv-complex-double (* ldu1 p)))
            (u2 (fnv:make-fnv-complex-double (* ldu2 (- m p))))
            (v1t (fnv:make-fnv-complex-double (* ldv1t q)))
            (v2t (fnv:make-fnv-complex-double (* ldv2t (- m q))))
            (iwork (fnv:make-fnv-int32 (- m r))))
        (sb-ext:cancel-finalization x12)
        (sb-ext:cancel-finalization x21)
        (sb-ext:cancel-finalization x22)
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
  "Calculates the matrices U, SIGMA, and VT of the CSD of a matrix from its intermediate representation, as calculated from ZUNCSD."
  (let ((p (matrix-rows u1))
        (q (matrix-rows v1t))
        (m (+ (matrix-rows u1) (matrix-rows u2)))
        (r (length theta)))
    (let ((u (apply #'make-complex-matrix m m 
                    (make-list (* m m) :initial-element #C(0.0d0 0.0d0))))
          (sigma (apply #'make-complex-matrix m m 
                        (make-list (* m m) :initial-element #C(0.0d0 0.0d0))))
          (vt (apply #'make-complex-matrix m m 
                     (make-list (* m m) :initial-element #C(0.0d0 0.0d0)))))
      ; Create U block by block
      (magicl.lapack-cffi::%zlacpy "A" p p (matrix-data u1) p (matrix-data u) m)
      (let ((u2ptr (fnv:make-fnv-complex-double (- (* (- m p) m) p) :foreign-ptr (ptr-ref u p p))))
        (sb-ext:cancel-finalization u2ptr)
        (magicl.lapack-cffi::%zlacpy "A" (- m p) (- m p) (matrix-data u2) (- m p) 
                                     u2ptr m))
      
      ; Create VT block by block
      (magicl.lapack-cffi::%zlacpy "A" q q (matrix-data v1t) q (matrix-data vt) m)
      (let ((v2ptr (fnv:make-fnv-complex-double (- (* (- m q) m) q) :foreign-ptr (ptr-ref vt q q))))
        (sb-ext:cancel-finalization v2ptr)
        (magicl.lapack-cffi::%zlacpy "A" (- m q) (- m q) (matrix-data v2t) (- m q) 
                                     v2ptr m))
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
      (values u sigma vt))))

(defun lapack-lu (m)
  "Finds the LU decomposition of a square matrix M, in terms of the intermediate representation given by the ZGETRF LAPACK subroutine."
  (let ((rows (matrix-rows m))
        (cols (matrix-cols m))
        (a (fnv:copy-fnv-complex-double (matrix-data m)))
        (info 0))
    (let ((lda rows)
          (ipiv (fnv:make-fnv-int32 (min rows cols))))
      (magicl.lapack-cffi::%zgetrf rows cols a lda ipiv info)
      (values a ipiv))))

(defun det (m)
  "Finds the determinant of a square matrix M."
  (let ((rows (matrix-rows m))
        (cols (matrix-cols m))
        (d 1))
    (assert (= rows cols) () "M is not a square matrix.")
    (multiple-value-bind (a ipiv) (lapack-lu m)
      (dotimes (i rows)
        (setq d (* d (fnv:fnv-complex-double-ref a (+ (* i rows) i)))))
      (dotimes (i (fnv:fnv-length ipiv))
        (if (not (= (1+ i) (fnv:fnv-int32-ref ipiv i)))
            (setq d (- d))))
      (values d))))

(defun inv (m)
  "Finds the inverse of a square matrix M."
  (let ((rows (matrix-rows m))
        (cols (matrix-cols m)))
    (assert (= rows cols) () "M is not a square matrix.")
    (multiple-value-bind (a ipiv) (lapack-lu m)
      (let ((lda rows)
            (lwork -1)
            (work (fnv:make-fnv-complex-double 1))
            (info 0))
        ; run it once as a workspace query
        (magicl.lapack-cffi::%zgetri rows a lda ipiv work lwork info)
        (setf lwork (truncate (realpart (fnv:fnv-complex-double-ref work 0))))
        (setf work (fnv:make-fnv-complex-double (max 1 lwork)))
        ; run it again with optimal workspace size
        (magicl.lapack-cffi::%zgetri rows a lda ipiv work lwork info)
        (values (make-matrix :rows rows :cols cols :data a))))))

(defun expm (m)
  "Finds the exponential of a square matrix M."
  (let ((ideg 6)
        (rows (matrix-rows m))
        (tcoef (coerce 1.0 'double-float))
        (h (fnv:copy-fnv-complex-double (matrix-data m)))
        (iexph 0)
        (ns 0)
        (iflag 0))
    (let ((lwsp (+ (* 4 rows rows) ideg 1))
          (ipiv (fnv:make-fnv-int32 rows)))
      (let ((wsp (fnv:make-fnv-complex-double lwsp)))
        ; Requires direct foreign function call due to need to access a pointer 
        ; to an integer (IEXPH).
        (CFFI:WITH-FOREIGN-OBJECTS ((IDEG-REF103 ':INT32) (M-REF104 ':INT32)
                                    (T-REF105 ':DOUBLE) (LDH-REF107 ':INT32)
                                    (LWSP-REF109 ':INT32) (IEXPH-REF111 ':INT32)
                                    (NS-REF112 ':INT32) (IFLAG-REF113 ':INT32))
          (COMMON-LISP:SETF (CFFI:MEM-REF IDEG-REF103 :INT32) IDEG)
          (COMMON-LISP:SETF (CFFI:MEM-REF M-REF104 :INT32) ROWS)
          (COMMON-LISP:SETF (CFFI:MEM-REF T-REF105 :DOUBLE) TCOEF)
          (COMMON-LISP:SETF (CFFI:MEM-REF LDH-REF107 :INT32) ROWS)
          (COMMON-LISP:SETF (CFFI:MEM-REF LWSP-REF109 :INT32) LWSP)
          (COMMON-LISP:SETF (CFFI:MEM-REF IEXPH-REF111 :INT32) IEXPH)
          (COMMON-LISP:SETF (CFFI:MEM-REF NS-REF112 :INT32) NS)
          (COMMON-LISP:SETF (CFFI:MEM-REF IFLAG-REF113 :INT32) IFLAG)
          (magicl.expokit-cffi::%%zgpadm IDEG-REF103 M-REF104 T-REF105 
                                         (FNV-FOREIGN-POINTER H) LDH-REF107
                                         (FNV-FOREIGN-POINTER WSP) LWSP-REF109 
                                         (FNV-FOREIGN-POINTER IPIV)
                                         IEXPH-REF111 NS-REF112 IFLAG-REF113)
          (setf iexph (CFFI:MEM-REF IEXPH-REF111 :INT32)))
        (print-matrix (make-matrix :rows 1 :cols lwsp :data wsp))
        (let ((exph (fnv:make-fnv-complex-double (* rows rows))))
          (dotimes (i (* rows rows))
            (setf (fnv:fnv-complex-double-ref exph i) 
                  (fnv:fnv-complex-double-ref wsp (+ i (1- iexph)))))
          (values (make-matrix :rows rows :cols rows :data exph)))))))
