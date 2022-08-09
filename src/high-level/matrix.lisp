;;;; matrix.lisp
;;;;
;;;; Author: Cole Scott
;;;;         Robert Smith

(in-package #:magicl)

(deftype matrix-storage (&optional type)
  `(simple-array ,type (*)))

(defstruct (matrix (:include abstract-tensor)
                   (:constructor nil)
                   (:copier nil))
  (nrows 0 :type alexandria:non-negative-fixnum)
  (ncols 0 :type alexandria:non-negative-fixnum)
  (size 0 :type alexandria:non-negative-fixnum :read-only t)
  (layout :column-major :type (member :row-major :column-major)))

(defmethod nrows ((m matrix))
  (matrix-nrows m))

(defmethod ncols ((m matrix))
  (matrix-ncols m))

(defmethod size ((m matrix))
  (matrix-size m))

(defmethod layout ((m matrix))
  (matrix-layout m))

;;; Specfic matrix classes
(defmacro defmatrix (name type tensor-class)
  "Define a new matrix subclass with the specified NAME and element TYPE,
compatible with TENSOR-CLASS, as well as the abstract-tensor methods
required not specified by the generic MATRIX class (MAKE-TENSOR,
ELEMENT-TYPE, CAST, COPY-TENSOR, DEEP-COPY-TENSOR, TREF, SETF TREF)"
  (let ((constructor-sym (intern (format nil "MAKE-~:@(~A~)" name)))
        (copy-sym (intern (format nil "COPY-~:@(~A~)" name)))
        (storage-sym (intern (format nil "~:@(~A~)-STORAGE" name))))
    `(progn
       (defstruct (,name (:include matrix)
                         (:constructor ,constructor-sym
                             (nrows ncols size layout storage))
                         (:copier ,copy-sym))
         (storage nil :type (matrix-storage ,type)))
       #+sbcl (declaim (sb-ext:freeze-type ,name))
       #+allegro (set-pprint-dispatch ',name 'pprint-matrix)

       (defmethod storage ((m ,name))
         (,storage-sym m))

       (defmethod (setf storage) (new-value (m ,name))
         (setf (,storage-sym m) new-value))

       (defmethod element-type ((m ,name))
         (declare (ignore m))
         ',type)

       (defmethod make-tensor ((class (eql ',name)) shape &key initial-element layout storage)
         (declare (type list shape)
                  (optimize (speed 3) (safety 0))) ;; This is probably not what you want...
         (policy-cond:with-expectations (> speed safety)
             ((type shape shape)
              (assertion (cl:= 2 (length shape))))
           (let ((rows (first shape))
                 (cols (second shape)))
             (declare (type fixnum rows cols))
             (let ((size (the fixnum (* rows cols))))
               (multiple-value-bind (actual-storage finalizer)
                   (or storage
                       (allocate size
                                 :element-type ',type
                                 :initial-element initial-element))
                 (let ((matrix
                         (funcall #',constructor-sym
                                  rows
                                  cols
                                  size
                                  (or layout :column-major)
                                  actual-storage)))
                   (finalize matrix finalizer)
                   matrix))))))

       (defmethod cast ((tensor ,name) (class (eql ',name)))
         (declare (ignore class))
         tensor)
       (defmethod cast :before ((tensor ,tensor-class) (class (eql ',name)))
         (declare (ignore class))
         (assert (cl:= 2 (order tensor))
                 ()
                 "Cannot change non-2 dimensional tensor to matrix."))
       (defmethod cast ((tensor ,tensor-class) (class (eql ',name)))
         (declare (ignore class))
         (make-tensor ',name (shape tensor)
                      :storage (storage tensor)
                      :layout (layout tensor)))
       (defmethod cast ((tensor ,name) (class (eql ',tensor-class)))
         (declare (ignore class))
         (make-tensor ',tensor-class (shape tensor)
                      :storage (storage tensor)
                      :layout (layout tensor)))

       ;; TODO: This does not allow for args. Make this allow for args.
       (defmethod copy-tensor ((m ,name) &rest args)
         (declare (ignore args))
         (let ((new-m (,copy-sym m)))
           (multiple-value-bind (storage finalizer)
               (allocate (matrix-size m)
                         :element-type (element-type m))
             (setf (,storage-sym new-m) storage)
             (finalize new-m finalizer))
           new-m))

       (defmethod deep-copy-tensor ((m ,name) &rest args)
         (declare (ignore args))
         (let ((new-m (copy-tensor m)))
           (dotimes (i (matrix-size m))
             (setf (aref (,storage-sym new-m) i)
                   (aref (,storage-sym m) i)))
           new-m))
       
       (defmethod tref ((matrix ,name) &rest pos)
         (declare (dynamic-extent pos)
                  (optimize (speed 3) (safety 1)))
         (let ((numrows (matrix-nrows matrix))
               (numcols (matrix-ncols matrix)))
           (declare (type fixnum numrows numcols))
           (policy-cond:with-expectations (> speed safety)
               ((assertion (valid-matrix-index-p pos numrows numcols)))
             (let ((row (first pos))
                   (col (second pos)))
               (declare (type fixnum row col))
               (let ((index (ecase (matrix-layout matrix)
                              (:row-major (+ col (the fixnum (* row numcols))))
                              (:column-major (+ row (the fixnum (* col numrows)))))))
                 (declare (type alexandria:array-index index))
                 (aref (,storage-sym matrix) index))))))
       
       (defmethod (setf tref) (new-value (matrix ,name) &rest pos)
         (declare (dynamic-extent pos)
                  (optimize (speed 3) (safety 1)))
         (let ((numrows (matrix-nrows matrix))
               (numcols (matrix-ncols matrix)))
           (declare (type alexandria:non-negative-fixnum numrows numcols))
           (policy-cond:with-expectations (> speed safety)
               ((assertion (valid-matrix-index-p pos numrows numcols)))
             (let ((row (first pos))
                   (col (second pos)))
               (declare (type alexandria:non-negative-fixnum row col))
               (let ((index (ecase (matrix-layout matrix)
                              (:row-major (+ col (the fixnum (* row numcols))))
                              (:column-major (+ row (the fixnum (* col numrows)))))))
                 (declare (type alexandria:array-index index))
                 (setf (aref (,storage-sym matrix) index)
                       (coerce new-value ',type)))))))
       
       (defmethod into! ((function function) (matrix ,name))
         (let ((index 0) ;; TODO: make if less iffy
               (storage (storage matrix)))
           (if (eql :row-major (layout matrix))
               (loop :for j :below (nrows matrix) :do
                 (loop :for i :below (ncols matrix) :do
                   (setf (aref storage index)
                         (coerce (funcall function j i) ',type))
                   (incf index)))
               (loop :for j :below (ncols matrix) :do
                 (loop :for i :below (nrows matrix) :do
                   (setf (aref storage index)
                         (coerce (funcall function i j) ',type))
                   (incf index))))
           matrix)))))

(defun pprint-matrix (stream matrix &optional colon-p at-sign-p)
  "Pretty-print a matrix MATRIX to the stream STREAM."
  (declare (ignore colon-p)
           (ignore at-sign-p))
  (flet ((print-real (x)
           (format stream "~6,3f" x))
         (print-complex (z)
           (format stream "~6,3f ~:[+~;-~]~6,3fj"
                   (realpart z)
                   (minusp (imagpart z))
                   (abs (imagpart z))))
         (print-int (x)
           (format stream "~3d" x)))
    (let* ((rows (nrows matrix))
           (cols (ncols matrix))
           (type (element-type matrix))
           (print-entry
             (cond
               ((subtypep type 'complex) #'print-complex)
               ((subtypep type 'integer) #'print-int)
               (t #'print-real))))
      (pprint-logical-block (stream nil)
        (print-unreadable-object (matrix stream :type t)
          (format stream "(~Dx~D):" rows cols)
          (dotimes (r rows)
            (pprint-newline :mandatory stream)
            (dotimes (c cols)
              (funcall print-entry (tref matrix r c))
              (unless (cl:= c (1- cols))
                (write-string "    " stream)))))))))

(set-pprint-dispatch 'matrix 'pprint-matrix)

(defun square-matrix-p (matrix)
  "Is MATRIX square?"
  (cl:= (nrows matrix) (ncols matrix)))

(defun identity-matrix-p (matrix &optional (epsilon *double-comparison-threshold*))
  "Is the MATRIX an identity matrix"
  (unless (square-matrix-p matrix)
    (return-from identity-matrix-p nil))
  (map-indexes (shape matrix)
               (lambda (r c)
                 (unless (<= (abs (- (tref matrix r c)
                                     (if (cl:= r c)
                                         1
                                         0)))
                             epsilon)
                   (return-from identity-matrix-p nil))))
  t)

(defun unitary-matrix-p (matrix &optional (epsilon *double-comparison-threshold*))
  "Is MATRIX a unitary matrix?"
  (identity-matrix-p (@ matrix (conjugate-transpose matrix)) epsilon))

(defun hermitian-matrix-p (matrix &optional (epsilon *double-comparison-threshold*))
  "Is MATRIX a hermitian matrix?"
  (= matrix (conjugate-transpose matrix) epsilon))

(defmacro assert-square-matrix (&rest matrices)
  `(progn
     ,@(loop :for matrix in matrices
             :collect `(assert (square-matrix-p ,matrix)
                               ()
                               "The shape of ~a is ~a, which is not a square"
                               ,(symbol-name matrix)
                               (shape ,matrix)))))

;;; Required abstract-tensor methods

(defmethod order ((m matrix))
  (declare (ignore m))
  2)

(defmethod shape ((m matrix))
  (list (matrix-nrows m) (matrix-ncols m)))

(defmethod (setf shape) (new-value (m matrix))
  (policy-cond:with-expectations (> speed safety)
      ((type shape new-value)
       (assertion (cl:= 2 (length new-value))))
    (setf (matrix-nrows m) (first new-value)
          (matrix-ncols m) (second new-value))))

;; Specific constructors

(defgeneric row (matrix index)
  (:documentation "Get row vector from a matrix")
  (:method ((m matrix) index)
    (check-type index alexandria:non-negative-fixnum)
    (slice m
           (list index 0)
           (list (1+ index) (ncols m)))))

(defgeneric column (matrix index)
  (:documentation "Get column vector from a matrix")
  (:method ((m matrix) index)
    (slice m
           (list 0 index)
           (list (nrows m) (1+ index)))))

(define-extensible-function (mult mult-lisp) (a b &key target alpha beta transa transb)
  (:documentation
   "Library users: consider using MAGICL:@ instead.

Multiply A by B, storing in target or creating a new tensor if target is not specified. More specifically, calculate

    TARGET := ALPHA*opA(A)*opB(B) + BETA*TARGET

where
             { X    if TRANSX = NIL or TRANSX = :N
    opX(X) = { X^T  if TRANSX = :T
             { X^H  if TRANSX = :C

TARGET cannot be the same as A or B.

In the world of BLAS/LAPACK, this is known as GEMM.
")
  ;; Methods are defined in matrix-functions/mult.lisp
  )

(defgeneric @ (matrix &rest matrices)
  (:documentation "Multiplication of matrices")
  (:method (matrix &rest matrices)
    (reduce #'mult matrices
            :initial-value matrix)))

;;; Generic matrix methods

(define-extensible-function (block-diag block-diag-lisp) (blocks)
  (:documentation "Construct a matrix from its diagonal blocks.")
  (:method (blocks)
    (when (null blocks)
      (error "Unable to construct block diagonal matrix from empty list."))
    (let((nrows 0)
         (ncols 0)
         (type (element-type (first blocks))))
      (dolist (mat blocks)
        (incf nrows (nrows mat))
        (incf ncols (ncols mat))
        ;; TODO: type coercion? for now we can be strict
        (unless (equalp type (element-type mat))
          (error "Unable to construct block matrix from blocks with disagreeing types.")))
      (let ((result (zeros (list nrows ncols) :type type)))
        (loop :with i := 0
              :with j := 0
              :for mat :in blocks
              :do (slice-to mat (list 0 0) (shape mat) result (list i j))
                  (incf i (nrows mat))
                  (incf j (ncols mat)))
        result))))

(define-extensible-function (block-matrix block-matrix-lisp) (blocks shape)
  (:documentation "Construct a matrix a list of blocks. Here SHAPE denotes the number of blocks in each row and column.")
  (:method (blocks shape)
    (let ((len (length blocks)))
      (policy-cond:with-expectations (> speed safety)
          ((type shape shape)
           (assertion (cl:= 2 (length shape)))
           (assertion (cl:= len (reduce #'* shape))))
        (let ((block-rows
                (loop :with ncols := (second shape)
                      :and tail := blocks
                      :while tail
                      :collect (hstack
                                (loop :for elts :on tail
                                      :for i :below ncols
                                      :collect (first elts)
                                      :finally (setf tail elts))))))
          (vstack block-rows))))))

(define-extensible-function (hstack hstack-lisp) (matrices)
  (:documentation "Concatenate matrices 'horizontally' (column-wise).")
  (:method (matrices)
    (when (null matrices)
      (error "Unable to concatenate empty matrix list."))
    (let* ((mat (first matrices))
           (rows (nrows mat))
           (type (element-type mat))
           (cols
             (loop :for mat :in matrices
                   :unless (cl:= rows (nrows mat))
                     :do (error "Matrices have conflicting number of rows: ~D vs ~D." rows (nrows mat))
                   :unless (equalp type (element-type mat))
                     :do (error "Matrices have conflicting types: ~A vs ~A." type (element-type mat))
                   :summing (ncols mat))))
      (loop :with result := (zeros (list rows cols) :type type)
            :with j := 0
            :for mat :in matrices
            :do (slice-to mat (list 0 0) (shape mat) result (list 0 j))
            :do (incf j (ncols mat))
            :finally (return result)))))

(define-extensible-function (vstack vstack-lisp) (matrices)
  (:documentation "Concatenate matrices 'vertically' (row wise).")
  (:method (matrices)
    (when (null matrices)
      (error "Unable to concatenate empty matrix list."))
    (let* ((mat (first matrices))
           (cols (ncols mat))
           (type (element-type mat))
           (rows
             (loop :for mat :in matrices
                   :unless (cl:= cols (ncols mat))
                     :do (error "Matrices have conflicting number of columns: ~D vs ~D." cols (ncols mat))
                   :unless (eq type (element-type mat))
                     :do (error "Matrices have conflicting types: ~A vs ~A." type (element-type mat))
                   :summing (nrows mat))))
      (loop :with result := (zeros (list rows cols) :type type)
            :with i := 0
            :for mat :in matrices
            :do (slice-to mat (list 0 0) (shape mat) result (list i 0))
            :do (incf i (nrows mat))
            :finally (return result)))))

(define-extensible-function (direct-sum direct-sum-lisp) (a b)
  (:documentation "Compute the direct sum of A and B")
  (:method ((a matrix) (b matrix))
    (block-diag (list a b))))

(define-extensible-function (kron kron-lisp) (a b &rest rest)
  (:documentation "Compute the Kronecker product of A and B")
  (:method (a b &rest rest)
    (let ((ma (nrows a))
          (mb (nrows b))
          (na (ncols a))
          (nb (ncols b)))
      (flet ((calc-i-j (i j)
               (* (tref a (floor i mb) (floor j nb))
                  (tref b (mod i mb) (mod j nb)))))
        (reduce #'kron rest :initial-value (into! #'calc-i-j
                                                  (empty (list (* ma mb) (* na nb))
                                                         :type '(complex double-float))))))))

(define-extensible-function (transpose! transpose!-lisp) (matrix &key fast)
  (:documentation "Transpose MATRIX, replacing the elements of MATRIX, optionally performing a faster change of layout if FAST is specified")
  (:method ((matrix matrix) &key fast)
    "Transpose a matrix by copying values.
If FAST is t then just change layout. Fast can cause problems when you want to multiply specifying transpose."
    (if fast
        (progn (rotatef (matrix-ncols matrix) (matrix-nrows matrix))
               (setf (matrix-layout matrix) (ecase (matrix-layout matrix)
                                              (:row-major :column-major)
                                              (:column-major :row-major))))
        (let ((index-function
                (ecase (matrix-layout matrix)
                  (:row-major #'matrix-row-major-index)
                  (:column-major #'matrix-column-major-index)))
              (shape (shape matrix)))
          (loop :for row :below (matrix-nrows matrix)
                :do (loop :for col :from row :below (matrix-ncols matrix)
                          :do (rotatef
                               (aref (storage matrix) (apply index-function row col shape))
                               (aref (storage matrix) (apply index-function col row shape)))))
          (rotatef (matrix-ncols matrix) (matrix-nrows matrix))))
    matrix))

(define-extensible-function (transpose transpose-lisp) (matrix)
  (:documentation "Create a new matrix containing the transpose of MATRIX")
  (:method ((matrix matrix))
    "Transpose a matrix by copying values.
If fast is t then just change layout. Fast can cause problems when you want to multiply specifying transpose."
    (let ((new-matrix (copy-tensor matrix)))
      (setf (matrix-ncols new-matrix) (matrix-nrows matrix)
            (matrix-nrows new-matrix) (matrix-ncols matrix))
      (let ((index-function
              (ecase (layout matrix)
                (:row-major #'matrix-row-major-index)
                (:column-major #'matrix-column-major-index)))
            (shape (shape matrix)))
        (loop :for row :below (matrix-nrows matrix)
              :do (loop :for col :below (matrix-ncols matrix)
                        :do (let ((index1 (apply index-function row col shape))
                                  (index2 (apply index-function col row (shape new-matrix))))
                              (setf (aref (storage new-matrix) index2) (aref (storage matrix) index1))))))
      new-matrix)))

;; TODO: allow setf on matrix diag
(define-extensible-function (diag diag-lisp) (matrix)
  (:documentation "Get a list of the diagonal elements of MATRIX")
  (:method ((matrix matrix))
    (policy-cond:with-expectations (> speed safety)
        ((assertion (square-matrix-p matrix)))
      (let ((rows (nrows matrix)))
        (loop :for i :below rows
              :collect (tref matrix i i))))))

(define-extensible-function (trace trace-lisp) (matrix)
  (:documentation "Get the trace of MATRIX (sum of diagonals)")
  (:method ((matrix matrix))
    (policy-cond:with-expectations (> speed safety)
        ((assertion (square-matrix-p matrix)))
      (loop :for i :below (nrows matrix)
            :sum (tref matrix i i)))))

(define-extensible-function (det det-lisp) (matrix)
  (:documentation "Compute the determinant of a square matrix MATRIX")
  (:method ((matrix matrix))
    (policy-cond:with-expectations (> speed safety)
        ((assertion (square-matrix-p matrix)))
      (let ((d 1))
        (multiple-value-bind (a ipiv) (lu matrix)
          (dotimes (i (nrows matrix))
            (setq d (* d (tref a i i))))
          (dotimes (i (size ipiv))
            (unless (cl:= (1+ i) (tref ipiv i))
              (setq d (- d))))
          d)))))

(define-extensible-function (upper-triangular upper-triangular-lisp) (matrix &key square)
  (:documentation "Get the upper triangular portion of the matrix.

If :SQUARE is T, then the result will be restricted to the upper rightmost square submatrix of the input.")
  (:method ((matrix matrix) &key square)
    (let ((m (nrows matrix))
          (n (ncols matrix)))
      (let* ((end-i (if square (min m n) m))
             (start-j (if square (- n end-i) 0))
             (target (zeros (list end-i (- n start-j))
                            :layout (layout matrix) :type (element-type matrix))))
        (loop :for i :below end-i
              :do (loop :for j :from (+ start-j i) :below n
                        :for j0 :from i
                        :do (setf (tref target i j0)
                                  (tref matrix i j))))
        target))))
;;; Synonym for upper-triangular
(setf (fdefinition 'triu) #'upper-triangular)

(define-extensible-function (lower-triangular lower-triangular-lisp) (matrix &key square)
  (:documentation "Get the lower triangular portion of the matrix.

If :SQUARE is T, then the result will be restricted to the lower leftmost square submatrix of the input.")
  (:method ((matrix matrix) &key square)
    (let ((m (nrows matrix))
          (n (ncols matrix)))
      (let* ((end-j (if square (min m n) n))
             (start-i (if square (- m end-j) 0))
             (target (zeros (list (- m start-i) end-j)
                            :layout (layout matrix) :type (element-type matrix))))
        (loop :for j :below end-j
              :do (loop :for i :from (+ start-i j) :below m
                        :for i0 :from j
                        :do (setf (tref target i0 j)
                                  (tref matrix i j))))
        target))))
;;; Synonym for lower-triangular
(setf (fdefinition 'tril) #'lower-triangular)

(define-extensible-function (conjugate-transpose conjugate-transpose-lisp) (matrix)
  (:documentation "Compute the conjugate transpose of a matrix")
  (:method ((matrix matrix))
    (map #'conjugate (transpose matrix))))
(setf (fdefinition 'dagger) #'conjugate-transpose)

(define-extensible-function (conjugate-transpose! conjugate-transpose!-lisp) (matrix)
  (:documentation "Compute the conjugate transpose of a matrix, replacing the elements")
  (:method ((matrix matrix))
    (map! #'conjugate (transpose! matrix))))
(setf (fdefinition 'dagger!) #'conjugate-transpose!)

(define-backend-function eig (matrix)
  "Find the (right) eigenvectors and corresponding eigenvalues of a square matrix M. Returns a list and a tensor (EIGENVALUES, EIGENVECTORS)")

(define-extensible-function (hermitian-eig hermitian-eig-lisp) (matrix)
  (:documentation "Like EIG, but specialized for Hermitian matrices."))

(define-extensible-function (lu lu-lisp) (matrix)
  (:documentation
   "Get the LU decomposition of MATRIX. Results in two tensors LU and IPIV:

    - LU is a matrix whose upper triangle is U and lower triangle is L. It is assumed that L has a diagonal of 1 values, and so it is not stored.

    - IPIV is an integer vector stating that row I was interchanged with IPIV(I)-1. (The one-based indexing is due to LAPACK convention.)

So assuming P is a permutation matrix representing IPIV, we have

    MATRIX == (@ P L U)
"))

(define-backend-function lu-solve (lu ipiv b)
  "Solve the system AX=B, where A is a square matrix, B is a compatibly shaped matrix, and A has PLU factorization indicated by the permutation vector IPIV and lower & upper triangular portions of the argument LU.")

(define-extensible-function (csd-blocks csd-blocks-lisp) (matrix p q)
  (:documentation "Compute the cosine-sine decomposition of the matrix MATRIX and return the result as blocks. See LISP-CSD-BLOCKS for mathematical details.

See also: MAGICL:CSD"))

;;; CSD is also defined (in terms of CSD-BLOCKS) in a different file..

(define-backend-function inv (matrix)
  "Get the inverse of the matrix")

(define-extensible-function (svd svd-lisp) (matrix &key reduced)
  (:documentation "Find the SVD of a matrix M. Return (VALUES U SIGMA Vt) where M = U @ SIGMA @ Vt"))

(define-backend-function qz (matrix1 matrix2)
  "Compute the QZ decomposition (aka the generalized Schur decomposition) on the pair of square matrices MATRIX1 and MATRIX2. Return (VALUES AA BB Q Z) such that

    (MATRIX1, MATRIX2) = (Q @ AA @ Z*, Q @ BB @ Z*).")

(define-extensible-function (qr qr-lisp) (matrix)
  (:documentation "Finds the QR factorization of MATRIX. Returns two matrices (Q, R).

NOTE: If MATRIX is not square, this will compute the reduced QR factorization."))

(define-backend-function ql (matrix)
  "Finds the QL factorization of MATRIX. Returns two matrices (Q, L).

NOTE: If MATRIX is not square, this will compute the reduced QL factorization.")

(define-backend-function rq (matrix)
  "Finds the RQ factorization of MATRIX. Returns two matrices (R, Q).

NOTE: If MATRIX is not square, this will compute the reduced RQ factorization.")

(define-backend-function lq (matrix)
  "Finds the LQ factorization of MATRIX. Returns two matrices (L, Q).

NOTE: If MATRIX is not square, this will compute the reduced LQ factorization.")

(define-backend-function expm (matrix)
  "Computes the exponential of a square matrix M.")

(define-backend-function expih (H)
  "Compute the exponential exp(iH) of a hermitian matrix H.

NOTE: If H is not Hermitian, the behavior is undefined.")

(define-backend-implementation expih :lisp
  (lambda (matrix)
    ;; NOTE: doing this for non-normal matrices is a bad idea, cf. the
    ;; discussion in "Nineteen Dubious Ways to Compute the Exponential
    ;; of a Matrix" by Moler & van Loan. but here we are fine.
    (multiple-value-bind (lambdas V) (magicl:hermitian-eig matrix)
      (let ((D (magicl:from-diag
                (mapcar #'cis lambdas)
                :type (element-type matrix))))
        (magicl:@ V D (magicl:conjugate-transpose V))))))

(define-backend-function logm (matrix)
  "Finds the matrix logarithm of a given square matrix M assumed to be diagonalizable, with nonzero eigenvalues.")

(define-backend-implementation logm :lisp
  (lambda (matrix)
    (multiple-value-bind (vals vects) (magicl:eig matrix)
      (let ((new-log-diag
              (let ((log-vals (mapcar #'log vals)))
                (magicl:from-diag log-vals))))
        (magicl:@ vects
                  new-log-diag
                  (magicl:inv vects))))))

(defmethod map!-lisp (function (tensor matrix))
  ;; XXX: If we ever have a "stride" or the like, this could be
  ;; dangerous.
  (map-into (storage tensor) function (storage tensor))
  tensor)

(define-condition rank-deficiency-error (error)
  ((message :initarg :message
            :initform "Unable to handle rank-deficient matrix."
            :reader rank-deficiency-error-message)
   (matrix :initarg :matrix :reader rank-deficiency-error-matrix))
  (:report (lambda (condition stream)
             (format stream "~A~&" (rank-deficiency-error-message condition)))))

(defun linear-solve (a b)
  "Attempt to solve the linear system Ax=b, where A is an invertible matrix and b is a vector."
  (unless (eq (element-type a) (element-type b))
    (error "Type mismatch: A has element type ~A, but b has ~A"
           (element-type a)
           (element-type b)))
  ;; Note that using the LU factorizaton is preferable to computing
  ;; A^-1 * b, particularly in the case where A is poorly conditioned.
  ;; Eventually, we could do sophisticated stuff here, e.g. all sorts
  ;; of O(n^2) checks on the structure of A (banded, triangular,
  ;; symmetric) and dispatch to specific solve routines.
  (let ((bmat (from-storage (storage b) (list (size b) 1))))
    (multiple-value-bind (lu ipiv)
        (lu a)
      (dotimes (i (nrows lu))
        (when (zerop (tref lu i i))
          (error 'rank-deficiency-error
                 :message "Left-hand side is rank deficient; giving up."
                 :matrix A)))
      (let ((rmat (lu-solve lu ipiv bmat)))
        (from-storage (storage rmat) (shape b))))))
