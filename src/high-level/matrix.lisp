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

       (defmethod storage ((m ,name))
         (,storage-sym m))

       (defmethod element-type ((m ,name))
         (declare (ignore m))
         ',type)

       (defmethod make-storage ((class (eql ',name)) size initial-element)
         (apply #'make-array
                size
                :element-type ',type
                (if initial-element
                    (list :initial-element (coerce initial-element ',type))
                    nil)))

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
               (funcall #',constructor-sym
                        rows
                        cols
                        size
                        (or layout :column-major)
                        (or
                         storage
                         (make-storage ',name size initial-element)))))))

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
           (setf (,storage-sym new-m)
                 (make-array (matrix-size m) :element-type (element-type m)))
           new-m))

       (defmethod deep-copy-tensor ((m ,name) &rest args)
         (declare (ignore args))
         (let ((new-m (,copy-sym m)))
           (setf (,storage-sym new-m)
                 (copy-seq (,storage-sym m)))
           new-m))
       
       (defmethod tref ((matrix ,name) &rest pos)
         (declare (dynamic-extent pos))
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
         (declare (dynamic-extent pos))
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

(defgeneric random-unitary (shape &key type)
  (:documentation "Generate a uniformly random element of U(n).")
  (:method (shape &key (type 'double-float))
    (policy-cond:with-expectations (> speed safety)
        ((assertion (square-shape-p shape)))
      (multiple-value-bind (q r) (qr (rand shape :type type :distribution #'alexandria:gaussian-random))
        (let ((d (diag r)))
          (setf d (cl:map 'list (lambda (di) (/ di (sqrt (* di (conjugate di))))) d))
          (@ q (funcall #'from-diag d)))))))

(defun hilbert-matrix (n &key type)
  "Generate the N by N Hilbert matrix."
  (let ((H (empty (list n n) :type type)))
    (into! (lambda (i j) (/ (+ i j 1)))
           H)
    H))

(defun random-hermitian (n)
  "Generate a random N by N complex Hermitian matrix."
  (let ((a (rand (list n n) :type '(complex double-float)))
        (b (rand (list n n) :type '(complex double-float))))
    (scale! b #C(0d0 1d0))
    (let ((c (.+ a b)))
      (.+ c (conjugate-transpose c)))))

;; TODO: This should be generic to abstract-tensor
(defgeneric ptr-ref (m base i j)
  (:documentation
   "Accessor method for the pointer to the element in the I-th row and J-th column of a matrix M, assuming zero indexing.")
  (:method ((m matrix) base i j)
    (policy-cond:with-expectations (> speed safety)
        ((assertion (valid-index-p (list i j) (shape m))))
      (let ((type (element-type m)))
        (let ((idx (apply (ecase (layout m)
                            (:column-major #'matrix-column-major-index)
                            (:row-major #'matrix-row-major-index))
                          i j (shape m))))
          (cond
            ((subtypep type 'single-float) (cffi:mem-aptr base :float idx))
            ((subtypep type 'double-float) (cffi:mem-aptr base :double idx))
            ((subtypep type '(complex single-float)) (cffi:mem-aptr base :float (* 2 idx)))
            ((subtypep type '(complex double-float)) (cffi:mem-aptr base :double (* 2 idx)))
            (t (error "Incompatible element type ~a." type))))))))

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

Multiply a by b, storing in target or creating a new tensor if target is not specified.

Target cannot be the same as a or b.")
  ;; TODO: write a lisp impl of this
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
             (target (empty (list end-i (- n start-j))
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
             (target (empty (list (- m start-i) end-j)
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

(define-extensible-function (eig eig-lisp) (matrix)
  (:documentation "Find the (right) eigenvectors and corresponding eigenvalues of a square matrix M. Returns a list and a tensor (EIGENVALUES, EIGENVECTORS)"))

(define-extensible-function (hermitian-eig hermitian-eig-lisp) (matrix)
  (:documentation "Find the (right) eigenvectors and corresponding eigenvalues of a (complex) hermitian matrix M. Returns a list and a tensor (EIGENVALUES, EIGENVECTORS)"))

(define-extensible-function (lu lu-lisp) (matrix)
  (:documentation "Get the LU decomposition of MATRIX. Returns two tensors (LU, IPIV)"))

;; TODO: Make this one generic and move to lapack-macros
;;       This is being blocked by the ZUNCSD shenanigans
(define-extensible-function (csd csd-lisp) (matrix p q)
  (:documentation "Find the Cosine-Sine Decomposition of a matrix X given that it is to be partitioned with upper left block of dimension P-by-Q. Returns the CSD elements (VALUES U SIGMA VT) such that X=U*SIGMA*VT.")
  (:method ((matrix matrix) p q)
    (multiple-value-bind (u1 u2 v1h v2h angles) (csd-blocks matrix p q)
      (values (block-diag (list u1 u2))
              (from-diag angles :type (element-type matrix))
              (block-diag (list v1h v2h))))))

(define-extensible-function (csd-blocks csd-blocks-lisp) (matrix p q)
  (:documentation "Compute the 2x2 Cosine-Sine decomposition of MATRIX (assumed to be unitary and 2n×2n) partitioned into n×n blocks A1 A2 A3 A4 as shown below:
       ⎡ A1  A3 ⎤        ⎡ A1  A3 ⎤   ⎡ U1     ⎤ ⎡ C  -S ⎤ ⎡ V1     ⎤H
If A = ⎢        ⎥, then  ⎢        ⎥ = ⎢        ⎥ ⎢       ⎥ ⎢        ⎥,
       ⎣ A2  A4 ⎦        ⎣ A2  A4 ⎦   ⎣     U2 ⎦ ⎣ S   C ⎦ ⎣     V2 ⎦
where U1, U2, V1, and V2 are unitary and C^2 + S^2 = I. The values of P and Q determine the size of the partition of A or, in other words, the dimensions of the blocks A1, A2, A3, and A4.
When the partition is P = Q = 1, we have
⎡ a1  A3 ⎤   ⎡ u1     ⎤ ⎡ c  -Sᵀ ⎤ ⎡ v1     ⎤H
⎢        ⎥ = ⎢        ⎥ ⎢        ⎥ ⎢        ⎥,
⎣ A2  A4 ⎦   ⎣     U2 ⎦ ⎣ S   C  ⎦ ⎣     V2 ⎦
where a1, u1, and, v1 are complex numbers, c = cos θ, s = sin θ, Sᵀ = [ 0ᵀ s ], and
    ⎡ I   0 ⎤
C = ⎢       ⎥.
    ⎣ 0ᵀ  c ⎦
The function returns the matrices U1, U2, V1H, V2H, and the list of principal angles.
See also http://www.netlib.org/lapack/explore-html/de/d0d/zuncsd_8f.html."))

(define-backend-function inv (matrix)
  "Get the inverse of the matrix")

(define-extensible-function (svd svd-lisp) (matrix &key reduced)
  (:documentation "Find the SVD of a matrix M. Return (VALUES U SIGMA Vt) where M = U*SIGMA*Vt"))

(define-extensible-function (qr qr-extension-lisp) (matrix)
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

(define-backend-function logm (matrix)
  "Finds the matrix logarithm of a given square matrix M assumed to be diagonalizable, with nonzero eigenvalues.")

;;; TODO: do something better here
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

