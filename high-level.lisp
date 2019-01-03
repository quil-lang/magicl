;;;; high-level.lisp
;;;;
;;;; Author: Joseph Lin
;;;;         Robert Smith

(in-package #:magicl)

(deftype matrix-storage ()
  "Representation of the smallest supertype of matrix storage."
  `(simple-array * (*)))

(deftype matrix-dimension ()
  "Representation of a valid dimension of a matrix."
  `(integer 1 ,array-total-size-limit))

(deftype matrix-index ()
  "Representation of a valid matrix index."
  `(integer 0 (,array-total-size-limit)))

(defun make-int32-storage (n)
  (make-array n :element-type '(signed-byte 32) :initial-element 0))

(defun make-S-storage (n)
  (make-array n :element-type 'single-float :initial-element 0.0f0))

(defun make-D-storage (n)
  (make-array n :element-type 'double-float :initial-element 0.0d0))

(defun make-C-storage (n)
  (make-array n :element-type '(complex single-float) :initial-element #C(0.0f0 0.0f0)))

(defun make-Z-storage (n)
  (make-array n :element-type '(complex double-float) :initial-element #C(0.0d0 0.0d0)))

(deftype lapack-data-type ()
  "The data type symbols used in LAPACK."
  `(member S D C Z))

(deftype lapack-storage-type ()
  "The storage type of the data for LAPACK."
  `(member
    ;; general
    ge gb gt gg
    ;; symmetric
    sy sb sp st
    ;; Hermitian
    he hb hp
    ;; SPD / HPD
    po pb pp pt
    ;; triangular
    tr tb tp tg
    ;; upper Hessenberg
    hs hg
    ;; trapezoidal
    tz
    ;; orthogonal
    or op
    ;; unitary
    un up
    ;; diagonal
    di
    ;; bidiagonal
    bd))

(defstruct (matrix (:constructor %make-matrix (rows cols data-type data)))
  "Representation of a dense matrix."
  (rows (error "Required argument")
   :type matrix-dimension
   :read-only t)
  (cols (error "Required argument")
   :type matrix-dimension
   :read-only t)
  (data-type (error "Required argument")
   :type lapack-data-type
   :read-only t)
  (storage-type 'ge
   :type lapack-storage-type
   :read-only t)
  (data (error "Required argument")
   :type matrix-storage
   :read-only t))

;; Allow MATRIX objects to be dumped.
(defmethod make-load-form ((object matrix) &optional environment)
  (make-load-form-saving-slots object :slot-names '(rows cols data-type storage-type data)
                                      :environment environment))

(defun matrix-storage-size (v)
  "Compute the size (i.e., number of elements) of the matrix storage vector V."
  (declare (type matrix-storage v))
  (length v))

(defun make-matrix (&key rows cols data)
  (declare (type matrix-dimension rows cols)
           (type matrix-storage data))
  (assert (= (matrix-storage-size data)
             (* rows cols))
          (rows cols)
          "The number of rows=~D and cols=~D does not match the storage size=~D"
          rows
          cols
          (matrix-storage-size data))
  (let ((data-type (alexandria:eswitch ((array-element-type data) :test 'equal)
                     ('single-float           'S)
                     ('double-float           'D)
                     ('(complex single-float) 'C)
                     ('(complex double-float) 'Z))))
    (%make-matrix rows cols data-type data)))

(defun copy-matrix-storage (v)
  "Copy a matrix storage vector V suitable for the DATA slot on a MATRIX structure."
  (declare (type matrix-storage v))
  (copy-seq v))

(defun matrix-element-type (m)
  "Return the element type of the matrix M."
  (array-element-type (matrix-data m)))

(defun pprint-matrix (stream matrix)
  "Pretty-print a matrix MATRIX to the stream STREAM."
  (flet ((print-real (x)
           (format stream "~6,3f" x))
         (print-complex (z)
           (format stream "~6,3f ~:[+~;-~]~6,3fj"
                   (realpart z)
                   (minusp (imagpart z))
                   (abs (imagpart z)))))
    (let* ((rows (matrix-rows matrix))
           (cols (matrix-cols matrix))
           (type (matrix-data-type matrix))
           (print-entry (ecase type
                          ((S D) #'print-real)
                          ((C Z) #'print-complex))))
      (pprint-logical-block (stream nil)
        (print-unreadable-object (matrix stream :type t)
          (format stream "[~A] ~Dx~D:" (symbol-name type) rows cols)
          (dotimes (r rows)
            (pprint-newline :mandatory stream)
            (dotimes (c cols)
              (funcall print-entry (ref matrix r c))
              (unless (= c (1- cols))
                (write-string "    " stream)))))))))

(set-pprint-dispatch 'matrix 'pprint-matrix)

(defun make-complex-foreign-vector (entries)
  "Makes a complex double FNV out ENTRIES, a list of complex numbers."
  (let* ((len (length entries))
         (v (make-Z-storage len)))
    (loop :for i :below len
          :for e :in entries
          :do (setf (aref v i) (coerce e '(complex double-float)))
          :finally (return v))))

(defun vector-to-list (v)
  "Make a list from a fnv."
  (coerce v 'list))

(defun make-complex-vector (entries)
  "Makes a complex column vector out of ENTRIES, a list of complex numbers."
  (funcall #'make-complex-matrix (length entries) 1 entries))

(defun make-complex-matrix (m n entries)
  "Makes an M-by-N matrix assuming ENTRIES is a list of complex numbers in column major order."
  (check-type m matrix-dimension)
  (check-type n matrix-dimension)
  (let ((entries-size (length entries))
        (expected-size (* m n)))
    (assert (= entries-size expected-size)
            ()
            "Length of entries is ~D, is not ~D * ~D = ~D"
            entries-size m n expected-size)
    (make-matrix :rows m
                 :cols n
                 :data (funcall #'make-complex-foreign-vector entries))))

(defun make-zero-matrix (rows cols)
  (make-matrix
   :rows rows
   :cols cols
   :data (make-Z-storage (* rows cols))))

(defun square-matrix-p (m)
  "Is M a square matrix?"
  (= (matrix-rows m)
     (matrix-cols m)))

(defparameter *default-zero-comparison-epsilon* 1d-10
  "The default absolute radius about zero considered to still be zero.")

(defun identityp (m)
  "Is the matrix M an identity matrix up to the epsilon *DEFAULT-ZERO-COMPARISON-EPSILON*?"
  ;; Sorry, this function is a little bit convoluted.
  (flet ((within-epsilon (z)
           (<= (abs z) *default-zero-comparison-epsilon*)))
    (and (square-matrix-p m)
         (progn
           (map-indexes (matrix-rows m)
                        (matrix-cols m)
                        (lambda (r c)
                          (cond
                            ;; Diagonal is 1.
                            ((= r c)
                             (when (not (within-epsilon (- 1.0d0 (ref m r c))))
                               (return-from identityp nil)))
                            ;; Everything else is 0.
                            ((not (within-epsilon (ref m r c)))
                             (return-from identityp nil)))))
           t))))

(defun unitaryp (m)
  "Is the matrix M is unitary up to the epsilon *DEFAULT-ZERO-COMPARISON-EPSILON*."
  (if (numberp m)
      (<= (abs (- 1.0 (abs m))) *default-zero-comparison-epsilon*)
      (identityp (multiply-complex-matrices m (conjugate-transpose m)))))

(defun map-indexes (rows cols f)
  "Map the binary function F(i, j) across all matrix indexes 0 <= i < ROWS and 0 <= j < COLS. Return NIL."
  (dotimes (c cols)
    (dotimes (r rows)
      (funcall f r c))))

(defun tabulate (rows cols f)
  "Generate a matrix of ROWS x COLS by calling the function F(i, j) for 0 <= i < ROWS and 0 <= j < COLS."
  (let ((m (make-zero-matrix rows cols)))
    (map-indexes rows cols (lambda (r c) (setf (ref m r c) (funcall f r c))))
    m))

(defun lift-unary-function (function)
  "Produces a unitary function that takes a matrix and returns a
matrix of the same dimension where each element of the output matrix
is the result of applying the unitary FUNCTION to the corresponding
element in the input matrix."
  (check-type function function)
  (lambda (matrix)
    (check-type matrix matrix)
    (tabulate (matrix-rows matrix) (matrix-cols matrix)
              (lambda (i j) (funcall function (ref matrix i j))))))

(setf (symbol-function 'inc-matrix) (lift-unary-function #'1+))
(setf (documentation #'inc-matrix 'function)
      "Returns matrix with each element + 1.")
(setf (symbol-function 'dec-matrix) (lift-unary-function #'1-))
(setf (documentation #'dec-matrix 'function)
      "Returns matrix with each element - 1.")

(defun lift-binary-function (function)
  "Produces a binary function that takes a matrix and returns a
matrix of the same dimension where each element of the output matrix
is the result of applying the binary FUNCTION to the corresponding
elements in the input matrices."
  (check-type function function)
  (lambda (a b)
    (check-type a matrix)
    (check-type b matrix)
    (tabulate (matrix-rows a) (matrix-cols a)
              (lambda (i j) (funcall function
                                (ref a i j)
                                (ref b i j))))))

(defgeneric add-matrix-generic (a b))
(defgeneric sub-matrix-generic (a b))

(let ((lifted-+ (lift-binary-function #'+))
      (lifted-- (lift-binary-function #'-)))
  (defmethod add-matrix-generic ((a matrix) (b matrix))
    (funcall lifted-+ a b))

  (defmethod add-matrix-generic ((a number) (b matrix))
    (let ((mat-a (tabulate (matrix-rows b) (matrix-cols b)
                           (lambda (i j) (declare (ignore i j)) a))))
      (add-matrix-generic mat-a b)))

  (defmethod add-matrix-generic ((b matrix) (a number))
    (add-matrix-generic a b))

  (defmethod sub-matrix-generic ((a matrix) (b matrix))
    (funcall lifted-- a b))

  (defmethod sub-matrix-generic ((a number) (b matrix))
    (let ((mat-a (tabulate (matrix-rows b) (matrix-cols b)
                           (lambda (i j) (declare (ignore i j)) a))))
      (sub-matrix-generic mat-a b)))

  (defmethod sub-matrix-generic ((b matrix) (a number))
    (let ((mat-a (tabulate (matrix-rows b) (matrix-cols b)
                           (lambda (i j) (declare (ignore i j)) a))))
      (sub-matrix-generic b mat-a))))

(defun add-matrix (matrix &rest more-matrices)
  "Element-wise addition of input matrices."
  (reduce #'add-matrix-generic more-matrices :initial-value matrix))

(defun sub-matrix (matrix &rest more-matrices)
  "Element-wise subtraction of input matrices."
  (reduce #'sub-matrix-generic more-matrices :initial-value matrix))

(defun make-identity-matrix (dimension)
  "Make an identity matrix of dimension DIMENSION."
  (tabulate dimension dimension (lambda (i j) (if (= i j) 1 0))))

(defun diag (m n entries)
  "Creates a matrix with ENTRIES along the diagonal"
  (let ((entries-size (length entries))
        (expected-size (min m n)))
    (assert (= entries-size expected-size) ()
            "Min dimension is ~S but number of entries is ~S" expected-size entries-size)
    (let ((mat (make-zero-matrix m n)))
      (dotimes (i entries-size mat)
        (setf (ref mat i i) (nth i entries))))))

(defun matrix-diagonal (m)
  "Get the diagonal elements of the matrix M as a list."
  (loop :for i :below (min (matrix-rows m) (matrix-cols m))
        :collect (ref m i i)))

(declaim (inline column-major-index)
         (ftype (function (matrix-dimension matrix-index matrix-index) matrix-index)
                column-major-index))
(defun column-major-index (rows i j)
  "Give the linear index of a matrix element addressed by the I'th row and J'th column, for a matrix with ROWS rows."
  (+ i (* j rows)))

(defun ref (m i j)
  "Accessor method for the element in the I-th row and J-th column of a matrix M, assuming zero indexing."
  (let ((rows (matrix-rows m))
        (cols (matrix-cols m))
        (data (matrix-data m)))
    (check-type i matrix-index)
    (check-type j matrix-index)
    (assert (< -1 i rows) () "row index ~D is out of range" i)
    (assert (< -1 j cols) () "col index ~D is out of range" j)
    (aref data (column-major-index rows i j))))

(defun ptr-ref (m base i j)
  "Accessor method for the pointer to the element in the I-th row and J-th column of a matrix M, assuming zero indexing."
  (let ((rows (matrix-rows m))
        (cols (matrix-cols m))
        (data (matrix-data m)))
    (check-type i matrix-index)
    (check-type j matrix-index)
    (assert (< -1 i rows) () "row index ~D is out of range" i)
    (assert (< -1 j cols) () "col index ~D is out of range" j)
    (let ((idx (column-major-index rows i j)))
      (etypecase (row-major-aref data 0)
        (single-float           (cffi:mem-aptr base :float idx))
        (double-float           (cffi:mem-aptr base :double idx))
        ((complex single-float) (cffi:mem-aptr base :float (* 2 idx)))
        ((complex double-float) (cffi:mem-aptr base :double (* 2 idx)))))))

(defparameter *type-strictness* nil
  "Be strict about types when setting values in a matrix.")

(defun (setf ref) (new-value m i j)
  "Set the value of M_IJ to NEW-VALUE."
  (let* ((rows (matrix-rows m))
         (cols (matrix-cols m))
         (data (matrix-data m))
         (type (array-element-type data)))
    (check-type i integer)
    (check-type j integer)
    (assert (< -1 i rows) () "row index ~S is out of range" i)
    (assert (< -1 j cols) () "col index ~S is out of range" j)
    (setf (aref data (column-major-index rows i j))
          (if *type-strictness*
              new-value
              (coerce new-value type)))))

(defun multiply-complex-matrices (ma mb)
  "Multiplies two complex marices MA and MB, returning MA*MB. If MA is M x KA and MB is KB x N,
it must be that KA = KB, and the resulting matrix is M x N."
  (assert (equal (matrix-element-type ma) (matrix-element-type mb)))
  (assert (equal '(complex double-float) (matrix-element-type ma)))
  (let ((m  (matrix-rows ma))
        (ka (matrix-cols ma))
        (kb (matrix-rows mb))
        (n  (matrix-cols mb)))
    (assert (= ka kb) ()
            "Matrix A has ~D columns while matrix B has ~D rows" ka kb)
    (let ((a (copy-matrix-storage (matrix-data ma)))
          (b (copy-matrix-storage (matrix-data mb))))
      (if (= n 1)
          ;; mb is a column vector
          (if (= m 1)
              ;; ma is a row vector
              ;; use dot product
              (make-complex-matrix 1 1 (list (magicl.blas-cffi::%zdotu ka a 1 b 1)))
              ;; use matrix-vector multiplication
              (let ((trans "N")
                    (alpha #C(1.0d0 0.0d0))
                    (beta #C(0.0d0 0.0d0))
                    (y (make-Z-storage (* m n))))
                (magicl.blas-cffi::%zgemv trans m ka alpha a m b 1 beta y 1)
                (make-matrix :rows m :cols n :data y)))
          ;; use matrix-matrix multiplication
          (let ((transa "N")
                (transb "N")
                (alpha #C(1.0d0 0.0d0))
                (beta #C(0.0d0 0.0d0))
                (c (make-Z-storage (* m n))))
            (magicl.blas-cffi::%zgemm transa transb m n ka alpha a m b kb beta c m)
            (make-matrix :rows m :cols n :data c))))))

(defun conjugate-entrywise (m)
  "Computes the conjugate of each entry of matrix M."
  (check-type m matrix)
  (let ((m* (make-zero-matrix (matrix-cols m)
                              (matrix-rows m))))
    (dotimes (i (matrix-rows m) m*)
      (dotimes (j (matrix-cols m))
        (setf (ref m* i j) (conjugate (ref m i j)))))))

(defun transpose (m)
  "Computes the transpose of the matrix M."
  (check-type m matrix)
  (let ((mT (make-zero-matrix (matrix-cols m)
                              (matrix-rows m))))
    (dotimes (i (matrix-rows m) mT)
      (dotimes (j (matrix-cols m))
        (setf (ref mT j i) (ref m i j))))))

(defun conjugate-transpose (m)
  "Computes the conjugate transpose of the matrix M."
  (check-type m matrix)
  (let ((mT (make-zero-matrix (matrix-cols m)
                              (matrix-rows m))))
    (dotimes (i (matrix-rows m) mT)
      (dotimes (j (matrix-cols m))
        (setf (ref mT j i) (conjugate (ref m i j)))))))


(defun scale (alpha x)
  "Scale a complex double matrix X by a complex double ALPHA, i.e. return ALPHA*X."
  (let* ((zx (copy-matrix-storage (matrix-data x)))
         (za (coerce alpha '(complex double-float)))
         (n (matrix-storage-size zx)))
    (magicl.blas-cffi::%zscal n za zx 1)
    (make-matrix :rows (matrix-rows x) :cols (matrix-cols x) :data zx)))

(defun qr (m)
  "Finds the QR factorization of the matrix M."
  (let ((rows (matrix-rows m))
        (cols (matrix-cols m)))
    (multiple-value-bind (a tau) (lapack-unitary-triangular-decomposition m "QR")
      (let* ((amat (make-matrix :rows rows :cols cols :data a))
             (r (get-square-triangular amat T cols))
             (q (unitary-triangular-helper-get-q amat tau "QR")))
        ;; change signs if diagonal elements of r are negative
        (dotimes (j cols)
          (let ((diag-elt (ref r j j)))
            (assert (zerop (imagpart diag-elt))
                    () "Diagonal element R_~D~D=~A is not real" j j diag-elt)
            (setf diag-elt (realpart diag-elt))
            (when (minusp diag-elt)
              (dotimes (i rows)
                (when (<= j i (1- cols))
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
        ;; change signs if diagonal elements of L are negative
        (dotimes (j cols)
          (let ((diag-elt (ref l j j)))
            (assert (zerop (imagpart diag-elt))
                    () "Diagonal element L_~D~D=~A is not real" j j diag-elt)
            (setf diag-elt (realpart diag-elt))
            (when (minusp diag-elt)
              (dotimes (i rows)
                (when (<= i j)
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
          ;; change signs if diagonal elements of r are negative
        (dotimes (i rows)
          (let ((diag-elt (ref r i i)))
            (assert (zerop (imagpart diag-elt))
                    () "Diagonal element R_~D~D=~A is not real" i i diag-elt)
            (setf diag-elt (realpart diag-elt))
            (when (minusp diag-elt)
              (dotimes (j cols)
                (when (<= j i)
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
        ;; change signs if diagonal elements of l are negative
        (dotimes (i rows)
          (let ((diag-elt (ref r i i)))
            (assert (zerop (imagpart diag-elt))
                    () "Diagonal element R_~D~D=~A is not real" i i diag-elt)
            (setf diag-elt (realpart diag-elt))
            (when (minusp diag-elt)
              (dotimes (j cols)
                (when (<= i j (1- rows))
                  (setf (ref r j i) (- (ref r j i))))
                (setf (ref q i j) (- (ref q i j)))))))
        (values q r)))))

(defun lapack-unitary-triangular-decomposition (m option)
  "Finds the QR/QL/RQ/LQ factorization of the matrix M to the intermediate representation, as given by the LAPACK ZGE(QR/QL/RQ/LQ)F subroutine, depending on the string option OPTION."
  (let ((lapack-func (alexandria:eswitch (option :test #'string=)
                       ("QR" #'magicl.lapack-cffi::%zgeqrf)
                       ("QL" #'magicl.lapack-cffi::%zgeqlf)
                       ("RQ" #'magicl.lapack-cffi::%zgerqf)
                       ("LQ" #'magicl.lapack-cffi::%zgelqf)))
        (rows (matrix-rows m))
        (cols (matrix-cols m))
        (a (copy-matrix-storage (matrix-data m)))
        (lwork -1)
        (info 0))
    (let ((lda rows)
          (tau (make-Z-storage (min rows cols)))
          (work (make-Z-storage (max 1 lwork))))
      ;; run it once as a workspace query
      (funcall lapack-func rows cols a lda tau work lwork info)
      (setf lwork (round (realpart (aref work 0))))
      (setf work (make-Z-storage (max 1 lwork)))
      ;; run it again with optimal workspace size
      (funcall lapack-func rows cols a lda tau work lwork info)
      (values a tau))))

(defun get-square-triangular (a upper ord)
  "Creates a square matrix from a triangular or trapezoidal portion of A. The square matrix is upper triangular and taken from the upper portion of A if and only if UPPER is T. The order of the square matrix is given by ORD."
  (check-type upper boolean)
  (check-type ord (integer 1 *) "a positive integer")
  (let ((m (matrix-rows a))
        (n (matrix-cols a)))
    (assert (<= ord (max m n)) () "ORD, given as ~D, is greater than the maximum dimension of A, ~D." ord (max m n))

    (let ((tri (make-zero-matrix ord ord)))
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
  (let ((lapack-func (alexandria:eswitch (option :test #'string=)
                       ("QR" #'magicl.lapack-cffi::%zungqr)
                       ("QL" #'magicl.lapack-cffi::%zungql)
                       ("RQ" #'magicl.lapack-cffi::%zungrq)
                       ("LQ" #'magicl.lapack-cffi::%zunglq)))
        (m (matrix-rows amat))
        (n (matrix-cols amat))
        (a (matrix-data amat))
        (k (matrix-storage-size tau))
        (lwork -1)
        (info 0))
    (let ((lda m)
          (work (make-Z-storage (max 1 lwork))))
      ;; run it once as a workspace query
      (funcall lapack-func m n k a lda tau work lwork info)
      (setf lwork (round (realpart (aref work 0))))
      (setf work (make-Z-storage (max 1 lwork)))
      ;; run it again with optimal workspace size
      (funcall lapack-func m n k a lda tau work lwork info)
      (make-matrix :rows m :cols n :data a))))

(defun qr-helper-get-q (a tau n)
  "Get the matrix Q as a product of reflectors, from results given by ZGEQRF."
  (let ((m (/ (matrix-storage-size a) n))
        (k (matrix-storage-size tau))
        (lwork -1)
        (info 0))
    (let ((lda m)
          (work (make-Z-storage (max 1 lwork))))
      ;; run it once as a workspace query
      (magicl.lapack-cffi::%zungqr m n k a lda tau work lwork info)
      (setf lwork (truncate (realpart (aref work 0))))
      (setf work (make-Z-storage (max 1 lwork)))
      ;; run it again with optimal workspace size
      (magicl.lapack-cffi::%zungqr m n k a lda tau work lwork info)
      (make-matrix :rows m :cols n :data a))))

(defun svd (m)
  "Find the SVD of a matrix M. Return (VALUES U SIGMA Vt) where M = U*SIGMA*Vt"
  (let ((jobu "A")
        (jobvt "A")
        (rows (matrix-rows m))
        (cols (matrix-cols m))
        (a (copy-matrix-storage (matrix-data m)))
        (lwork -1)
        (info 0))
    (let ((lda rows)
          (s (make-D-storage (min rows cols)))
          (ldu rows)
          (ldvt cols)
          (work1 (make-Z-storage (max 1 lwork)))
          (work nil)
          (rwork (make-D-storage (* 5 (min rows cols)))))
      (let ((u (make-Z-storage (* ldu rows)))
            (vt (make-Z-storage (* ldvt cols))))
        ;; run it once as a workspace query
        (magicl.lapack-cffi::%zgesvd jobu jobvt rows cols a lda s u ldu vt ldvt
                                     work1 lwork rwork info)
        (setf lwork (round (realpart (aref work1 0))))
        (setf work (make-Z-storage (max 1 lwork)))
        ;; run it again with optimal workspace size
        (magicl.lapack-cffi::%zgesvd jobu jobvt rows cols a lda s u ldu vt ldvt
                                     work lwork rwork info)
        (let ((smat (make-D-storage (* rows cols))))
          (dotimes (i (min rows cols))
            (setf (aref smat (column-major-index rows i i))
                  (aref s i)))
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
           (v (make-Z-storage (* sliced-rows sliced-cols))))
      (dotimes (j sliced-cols)
        (dotimes (i sliced-rows)
          (setf (aref v (+ (* j sliced-rows) i)) ; TODO: use COLUMN-MAJOR-INDEX
                (ref m (+ rmin i) (+ cmin j)))))
      (make-matrix :rows sliced-rows :cols sliced-cols :data v))))


(defun csd (x p q)
  "Find the Cosine-Sine Decomposition of a matrix X given that it is to be partitioned with upper left block of dimension P-by-Q. Returns the CSD elements (VALUES U SIGMA VT) such that X=U*SIGMA*VT."
  (multiple-value-bind (u1 u2 v1t v2t theta) (lapack-csd x p q)
    (csd-from-blocks u1 u2 v1t v2t theta)))

(COMMON-LISP:DEFUN %ZUNCSD-XPOINTERS
                   (JOBU1 JOBU2 JOBV1T JOBV2T TRANS SIGNS M P Q X11 LDX11 X12
                    LDX12 X21 LDX21 X22 LDX22 THETA U1 LDU1 U2 LDU2 V1T LDV1T
                    V2T LDV2T WORK LWORK RWORK LRWORK IWORK INFO)
  (CFFI:WITH-FOREIGN-OBJECTS ((M-REF71665 ':INT32) (P-REF71666 ':INT32)
                              (Q-REF71667 ':INT32) (LDX11-REF71669 ':INT32)
                              (LDX12-REF71671 ':INT32) (LDX21-REF71673 ':INT32)
                              (LDX22-REF71675 ':INT32) (LDU1-REF71678 ':INT32)
                              (LDU2-REF71680 ':INT32) (LDV1T-REF71682 ':INT32)
                              (LDV2T-REF71684 ':INT32) (LWORK-REF71686 ':INT32)
                              (LRWORK-REF71688 ':INT32) (INFO-REF71690 ':INT32))
    (COMMON-LISP:SETF (CFFI:MEM-REF M-REF71665 :INT32) M)
    (COMMON-LISP:SETF (CFFI:MEM-REF P-REF71666 :INT32) P)
    (COMMON-LISP:SETF (CFFI:MEM-REF Q-REF71667 :INT32) Q)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDX11-REF71669 :INT32) LDX11)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDX12-REF71671 :INT32) LDX12)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDX21-REF71673 :INT32) LDX21)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDX22-REF71675 :INT32) LDX22)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDU1-REF71678 :INT32) LDU1)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDU2-REF71680 :INT32) LDU2)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDV1T-REF71682 :INT32) LDV1T)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDV2T-REF71684 :INT32) LDV2T)
    (COMMON-LISP:SETF (CFFI:MEM-REF LWORK-REF71686 :INT32) LWORK)
    (COMMON-LISP:SETF (CFFI:MEM-REF LRWORK-REF71688 :INT32) LRWORK)
    (COMMON-LISP:SETF (CFFI:MEM-REF INFO-REF71690 :INT32) INFO)
    (MAGICL.CFFI-TYPES:WITH-ARRAY-POINTERS ((THETA-REF71676 THETA)
                                            (U1-REF71677 U1) (U2-REF71679 U2)
                                            (V1T-REF71681 V1T)
                                            (V2T-REF71683 V2T)
                                            (WORK-REF71685 WORK)
                                            (RWORK-REF71687 RWORK)
                                            (IWORK-REF71689 IWORK))
      (MAGICL.LAPACK-CFFI::%%ZUNCSD
       JOBU1 JOBU2 JOBV1T JOBV2T TRANS SIGNS M-REF71665 P-REF71666
       Q-REF71667 X11 LDX11-REF71669 X12 LDX12-REF71671
       X21 LDX21-REF71673 X22 LDX22-REF71675 THETA-REF71676
       U1-REF71677 LDU1-REF71678 U2-REF71679 LDU2-REF71680 V1T-REF71681
       LDV1T-REF71682 V2T-REF71683 LDV2T-REF71684 WORK-REF71685 LWORK-REF71686
       RWORK-REF71687 LRWORK-REF71688 IWORK-REF71689 INFO-REF71690))))


(defun lapack-csd (x p q)
  "Find the Cosine-Sine Decomposition of a matrix X given that it is to be partitioned
with upper left block with dimension P-by-Q. Returns the intermediate representation given by the ZUNCSD LAPACK subroutine."
  (let* ((m (matrix-rows x))
         (n (matrix-cols x))
         (xcopy (make-matrix :rows m :cols n :data (copy-matrix-storage (matrix-data x)))))
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
          ;; leading dimension is M because full X array will be used
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
          (work (make-Z-storage 1))
          (lrwork -1)
          (rwork (make-D-storage 1))
          (info 0))
      ;; rather than slice up matrix, use full array with pointers to head of blocks
      ;;
      ;; WARNING: THIS ABYSS IS WHERE DRAGONS LIVE. HERE THE GARBAGE
      ;; COLLECTOR MUST BE TAMED SO WE DON'T SCREW UP THE POINTERS
      ;; INTO THE LISP HEAP.
      (magicl.cffi-types:with-array-pointers ((xcopy-ptr (matrix-data xcopy)))
        (let ((x11 xcopy-ptr)
              (x12 (ptr-ref xcopy xcopy-ptr 0 q))
              (x21 (ptr-ref xcopy xcopy-ptr p 0))
              (x22 (ptr-ref xcopy xcopy-ptr p q))
              (theta (make-D-storage r))
              (u1 (make-Z-storage (* ldu1 p)))
              (u2 (make-Z-storage (* ldu2 (- m p))))
              (v1t (make-Z-storage (* ldv1t q)))
              (v2t (make-Z-storage (* ldv2t (- m q))))
              (iwork (make-int32-storage (- m r))))
          ;; run it once as a workspace query

          (%ZUNCSD-XPOINTERS jobu1 jobu2 jobv1t jobv2t
                             trans signs m p q
                             x11 ldx11 x12 ldx12 x21 ldx21 x22 ldx22
                             theta u1 ldu1 u2 ldu2 v1t ldv1t v2t ldv2t
                             work lwork rwork lrwork iwork info)
          (setf lwork (truncate (realpart (row-major-aref work 0))))
          (setf work (make-Z-storage (max 1 lwork)))
          (setf lrwork (truncate (row-major-aref rwork 0)))
          (setf rwork (make-D-storage (max 1 lrwork)))
          ;; run it again with optimal workspace size
          (%ZUNCSD-XPOINTERS jobu1 jobu2 jobv1t jobv2t
                             trans signs m p q
                             x11 ldx11 x12 ldx12 x21 ldx21 x22 ldx22
                             theta u1 ldu1 u2 ldu2 v1t ldv1t v2t ldv2t
                             work lwork rwork lrwork iwork info)
          (values (make-matrix :rows p :cols p :data u1)
                  (make-matrix :rows (- m p) :cols (- m p) :data u2)
                  (make-matrix :rows q :cols q :data v1t)
                  (make-matrix :rows (- m q) :cols (- m q) :data v2t)
                  (vector-to-list theta)))))))


;;; TODO FIXME
(defun lisp-zlacpy (UPLO M N A rows-a B rows-b &optional (offx 0) (offy 0))
  (alexandria:switch (uplo :test #'string=)
    ("U"
     (loop :for j :below n :do
       (loop :for i :below (min j m) :do
         (setf (aref b (column-major-index rows-b (+ offy i) (+ offx j)))
               (aref a (column-major-index rows-a i j))))))

    ("L"
     (loop :for j :below n :do
       (loop :for i :from j :below m :do
         (setf (aref b (column-major-index rows-b (+ offy i) (+ offx j)))
               (aref a (column-major-index rows-a i j))))))

    (otherwise
     (loop :for j :below n :do
       (loop :for i :below m :do
         (setf (aref b (column-major-index rows-b (+ offy i)  (+ offx j)))
               (aref a (column-major-index rows-a i j))))))))

(defun csd-from-blocks (u1 u2 v1t v2t theta)
  "Calculates the matrices U, SIGMA, and VT of the CSD of a matrix from its intermediate representation, as calculated from ZUNCSD."
  (let ((p (matrix-rows u1))
        (q (matrix-rows v1t))
        (m (+ (matrix-rows u1) (matrix-rows u2)))
        (r (length theta)))
    (let ((u (make-zero-matrix m m))
          (sigma (make-zero-matrix m m))
          (vt (make-zero-matrix m m)))
      ;; Create U block by block
      (lisp-zlacpy "A" p p (matrix-data u1) p (matrix-data u) m)
      (lisp-zlacpy "A" (- m p) (- m p) (matrix-data u2) (- m p) (matrix-data u) m p p)

      ;; Create VT block by block
      (lisp-zlacpy "A" q q (matrix-data v1t) q (matrix-data vt) m)
      (lisp-zlacpy "A" (- m q) (- m q) (matrix-data v2t) (- m q) (matrix-data vt) m q q)

      (let ((diag11 (min p q))
            (diag12 (min p (- m q)))
            (diag21 (min (- m p) q))
            (diag22 (min (- m p) (- m q))))
        (let ((iden11 (- diag11 r))
              (iden12 (- diag12 r))
              (iden21 (- diag21 r))
              (iden22 (- diag22 r)))
          (loop for i from 0 to (1- iden11)
                do (setf (ref sigma i i) #C(1.0d0 0.0d0)))
          (loop for i from iden11 to (1- diag11)
                do (setf (ref sigma i i) (cos (nth (- i iden11) theta))))
          (loop for i from 0 to (1- iden12)
                do (setf (ref sigma (- p 1 i) (- m 1 i)) #C(-1.0d0 0.0d0)))
          (loop for i from iden12 to (1- diag12)
                do (setf (ref sigma (- p 1 i) (- m 1 i))
                         (- (sin (nth (- r 1 (- i iden12)) theta)))))
          (loop for i from 0 to (1- iden21)
                do (setf (ref sigma (- m 1 i) (- q 1 i)) #C(1.0d0 0.0d0)))
          (loop for i from iden21 to (1- diag21)
                do (setf (ref sigma (- m 1 i) (- q 1 i))
                         (sin (nth (- r 1 (- i iden21)) theta))))
          (loop for i from 0 to (1- iden22)
                do (setf (ref sigma (+ p i) (+ q i)) #C(1.0d0 0.0d0)))
          (loop for i from iden22 to (1- diag22)
                do (setf (ref sigma (+ p i) (+ q i)) (cos (nth (- i iden22) theta))))))
      (values u sigma vt))))

(defun lapack-lu (m)
  "Finds the LU decomposition of a square matrix M, in terms of the intermediate representation given by the ZGETRF LAPACK subroutine."
  (let ((rows (matrix-rows m))
        (cols (matrix-cols m))
        (a (copy-matrix-storage (matrix-data m)))
        (info 0))
    (let ((lda rows)
          (ipiv (make-int32-storage (min rows cols))))
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
        (setq d (* d (aref a (+ (* i rows) i)))))
      (dotimes (i (matrix-storage-size ipiv))
        (unless (= (1+ i) (aref ipiv i))
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
            (work (make-Z-storage 1))
            (info 0))
        ;; run it once as a workspace query
        (magicl.lapack-cffi::%zgetri rows a lda ipiv work lwork info)
        (setf lwork (truncate (realpart (row-major-aref work 0))))
        (setf work (make-Z-storage (max 1 lwork)))
        ;; run it again with optimal workspace size
        (magicl.lapack-cffi::%zgetri rows a lda ipiv work lwork info)
        (values (make-matrix :rows rows :cols cols :data a))))))

(declaim (inline dagger))
(defun dagger (m)
  "Synonym for CONJUGATE-TRANSPOSE."
  (conjugate-transpose m))

(defun direct-sum (a b)
  "Compute the direct sum of A and B."
  (let* ((arows (matrix-rows a))
         (acols (matrix-cols a))
         (brows (matrix-rows b))
         (bcols (matrix-cols b))
         (rrows (+ arows brows))
         (rcols (+ acols bcols))
         (result (make-zero-matrix rrows rcols)))
    (loop :for r :below arows :do
      (loop :for c :below acols :do
        (setf (ref result r c) (ref a r c))))
    (loop :for r :from arows :below rrows :do
      (loop :for c :from acols :below rcols :do
        (setf (ref result r c) (ref b (- r arows) (- c acols)))))
    result))


(defun eig (m)
  "Finds the (right) eigenvectors and corresponding eigenvalues of a square matrix M. Returns as two lists (EIGENVALUES, EIGENVECTORS) where the eigenvalues and eigenvectors are in corresponding order."
  (let ((rows (matrix-rows m))
        (cols (matrix-cols m)))
    (assert (= rows cols) () "M is not a square matrix")
    (let ((jobvl "N")
          (jobvr "V")
          (a (copy-matrix-storage (matrix-data m)))
          (w (make-Z-storage rows))
          (vl (make-Z-storage rows))
          (vr (make-Z-storage (* rows rows)))
          (lwork -1)
          (rwork (make-D-storage (* 2 rows)))
          (info 0))
      (let ((work (make-Z-storage (max 1 lwork))))
        ;; run it once as a workspace query
        (magicl.lapack-cffi::%zgeev jobvl jobvr rows a rows w vl 1 vr rows work lwork rwork info)
        (setf lwork (truncate (realpart (row-major-aref work 0))))
        (setf work (make-Z-storage (max 1 lwork)))
        ;; run it again with optimal workspace size
        (magicl.lapack-cffi::%zgeev jobvl jobvr rows a rows w vl 1 vr rows work lwork rwork info)
        (values (vector-to-list w) (make-matrix :rows rows :cols cols :data vr))))))

(defun kron (a b &rest rest)
  "Compute the kronecker product of matrices A and B."
  ;; This can be sped up by explicitly looping
  (let ((ma (matrix-rows a))
        (mb (matrix-rows b))
        (na (matrix-cols a))
        (nb (matrix-cols b)))
    (flet ((calc-i-j (i j) (* (ref a (floor i mb) (floor j nb))
                              (ref b (mod i mb) (mod j nb)))))
      (reduce #'kron rest :initial-value (tabulate (* ma mb) (* na nb) #'calc-i-j)))))

(defun exptm (m power)
  (check-type power fixnum)
  (check-type m matrix)
  (labels ((recurse (y m power)
             (cond
               ;; We are done recursing. Return answer.
               ((zerop power) y)
               ;; power is even, so square.
               ((evenp power) (recurse y (multiply-complex-matrices m m) (ash power -1)))
               ;; n is odd, so multiply out and make it even.
               (t (recurse (multiply-complex-matrices m y) m (1- power))))))
    (if (minusp power)
        (inv (recurse (make-identity-matrix (matrix-rows m)) m (- power)))
        (recurse (make-identity-matrix (matrix-rows m)) m power))))

(define-condition singular-matrix-error (error)
  ((matrix :initarg :matrix :reader singular-matrix-error-matrix))
  (:documentation "A singular matrix is used where non-singular expected.")
  (:report (lambda (condition stream)
             (format stream "Singular matrix used where non-singular expected~%~A"
                     (singular-matrix-error-matrix condition)))))

(defun solve (a b)
  "Computes the solution column vector X to the system of linear
equations A * X = B, where A is a square matrix and B is an NxM
matrix, the columns of which are different right-hand sides to the
above equation. 

The three return values are
  1. The solution X to each of the RHS B; 
  2. The L and U factors from the A = P * L * U factorization; and
  3. The pivot indices."
  (check-type a matrix)
  (check-type b matrix)
  (assert (square-matrix-p a))
  (with-foreign-objects ((n ':int32) (nrhs ':int32)
                         (lda ':int32) (ldb ':int32)
                         (info ':int32))
    (setf (cffi:mem-ref n :int32) (matrix-rows a))
    (setf (cffi:mem-ref nrhs :int32) (matrix-rows b))
    (setf (cffi:mem-ref lda :int32) (mem-ref n :int32))
    (setf (cffi:mem-ref ldb :int32) (mem-ref nrhs :int32))
    (setf (cffi:mem-ref info :int32) 0)
    (let ((a* (copy-matrix-storage (matrix-data a)))
          (ipiv (make-int32-storage (cffi:mem-ref n :int32)))
          (b* (copy-matrix-storage (matrix-data b))))
      (magicl.cffi-types:with-array-pointers
          ((a*-ref a*)
           (ipiv-ref ipiv)
           (b*-ref b*))
        (lapack::%%zgesv n nrhs a*-ref lda ipiv-ref b*-ref ldb info)
        (when (> (cffi:mem-ref info :int32) 0)
          (error 'singular-matrix-error :matrix a))
        (values (make-matrix :rows (matrix-rows b) :cols (matrix-cols b) :data b*)
                (make-matrix :rows (cffi:mem-ref n :int32) :cols (cffi:mem-ref n :int32) :data a*)
                ipiv)))))
