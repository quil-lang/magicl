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

(defgeneric square-matrix-p (matrix)
  (:documentation "Whether the MATRIX is square")
  (:method ((matrix matrix))
    (cl:= (nrows matrix) (ncols matrix))))

(defgeneric identity-matrix-p (matrix &optional epsilon)
  (:documentation "Whether MATRIX is an idenity matrix")
  (:method ((matrix matrix) &optional (epsilon +double-comparison-threshold-strict+))
    (unless (square-matrix-p matrix) (return-from identity-matrix-p nil))
    (map-indexes (shape matrix)
                 (lambda (r c)
                   (unless (>= epsilon
                               (abs
                                (- (tref matrix r c)
                                   (if (cl:= r c)
                                       1 0))))
                     (return-from identity-matrix-p nil))))
    t))

(defgeneric unitary-matrix-p (matrix &optional epsilon)
  (:documentation "Whether MATRIX is a unitary matrix")
  (:method ((matrix matrix) &optional (epsilon +double-comparison-threshold-strict+))
    (identity-matrix-p (@ matrix (conjugate-transpose matrix)) epsilon)))

(defgeneric hermitian-matrix-p (matrix &optional epsilon)
  (:documentation "Whether MATRIX is a unitary matrix")
  (:method ((matrix matrix) &optional (epsilon +double-comparison-threshold-strict+))
    (= matrix (conjugate-transpose matrix) epsilon)))

(defmacro assert-square-matrix (&rest matrices)
  `(progn
     ,@(loop :for matrix in matrices
             :collect `(assert (square-matrix-p ,matrix)
                               ()
                               ,"The shape of ~a is ~a, which is not a square" ,(symbol-name matrix) (shape ,matrix)))))

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

;; TODO: This should be generic to abstract-tensor
(defgeneric ptr-ref (m base i j)
  (:documentation
   "Accessor method for the pointer to the element in the I-th row and J-th column of a matrix M, assuming zero indexing.")
  (:method ((m matrix) base i j)
    (policy-cond:with-expectations (> speed safety)
        ((assertion (valid-index-p (list i j) (shape m))))
      (let ((type (element-type m)))
        ;; TODO: compensate for layout
        (let ((idx (apply #'matrix-column-major-index i j (shape m))))
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

(defgeneric mult (a b &key target alpha beta transa transb)
  (:documentation "Multiply matrix a by matrix b, storing in target or creating a new matrix if target is not specified.
Target cannot be the same as a or b."))

(defgeneric @ (matrix &rest matrices)
    (:documentation "Multiplication of matrices")
  (:method (matrix &rest matrices)
    (reduce #'mult matrices
            :initial-value matrix)))

;;; Generic matrix methods

(defgeneric direct-sum (a b)
  (:method ((a matrix) (b matrix))
    "Compute the direct sum of A and B"
    (let* ((arows (nrows a))
           (acols (ncols a))
           (brows (nrows b))
           (bcols (ncols b))
           (rrows (+ arows brows))
           (rcols (+ acols bcols))
           (result (magicl:empty (list rrows rcols)
                                 :type '(complex double-float))))
      (loop :for r :below arows :do
        (loop :for c :below acols :do
          (setf (tref result r c) (tref a r c))))
      (loop :for r :from arows :below rrows :do
        (loop :for c :from acols :below rcols :do
          (setf (tref result r c) (tref b (- r arows) (- c acols)))))
      result)))

(defgeneric kron (a b &rest rest)
  (:documentation "Compute the kronecker product of A and B")
  (:method (a b &rest rest)
    (let ((ma (nrows a))
          (mb (nrows b))
          (na (ncols a))
          (nb (ncols b)))
      (flet ((calc-i-j (i j) (* (tref a (floor i mb) (floor j nb))
                                (tref b (mod i mb) (mod j nb)))))
        (reduce #'kron rest :initial-value (into! #'calc-i-j
                                                  (empty (list (* ma mb) (* na nb))
                                                         :type '(complex double-float))))))))

(defgeneric transpose! (matrix &key fast)
  (:documentation "Transpose MATRIX, replacing the elements of MATRIX, optionally performing a faster change of layout if FAST is specified")
  (:method ((matrix matrix) &key fast)
    "Transpose a matrix by copying values.
If fast is t then just change layout. Fast can cause problems when you want to multiply specifying transpose."
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

(defgeneric transpose (matrix)
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
(defgeneric diag (matrix)
  (:documentation "Get a list of the diagonal elements of MATRIX")
  (:method ((matrix matrix))
    (policy-cond:with-expectations (> speed safety)
        ((assertion (square-matrix-p matrix)))
      (let ((rows (nrows matrix)))
        (loop :for i :below rows
              :collect (tref matrix i i))))))

(defgeneric trace (matrix)
  (:documentation "Get the trace of MATRIX (sum of diagonals)")
  (:method ((matrix matrix))
    (policy-cond:with-expectations (> speed safety)
        ((assertion (square-matrix-p matrix)))
      (loop :for i :below (nrows matrix)
            :sum (tref matrix i i)))))

(defgeneric det (matrix)
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

(defgeneric upper-triangular (matrix &optional order)
  (:documentation "Get the upper triangular portion of the matrix")
  (:method ((matrix matrix) &optional (order (ncols matrix)))
    (assert-square-matrix matrix)
    (let ((m (nrows matrix))
          (n (ncols matrix)))
      (policy-cond:with-expectations (> speed safety)
          ((assertion (<= order (max (nrows matrix) (ncols matrix)))))
        (let ((target (empty (list order order) :layout (layout matrix) :type (element-type matrix))))
          (if (> m n)
              (loop for i from 0 to (1- order)
                    do (loop for j from (max 0 (+ (- n order) i)) to (1- n)
                             do (setf (tref target i (+ j (- order n))) (tref matrix i j))))
              (loop for j from (- n order) to (1- n)
                    do (loop for i from 0 to (min (+ (- order n) j) (1- m))
                             do (setf (tref target i (- j (- n order))) (tref matrix i j)))))
          target)))))
;;; Synonym for upper-triangular
(setf (fdefinition 'triu) #'upper-triangular)

(defgeneric lower-triangular (matrix &optional order)
  (:documentation "Get the lower triangular portion of the matrix")
  (:method ((matrix matrix) &optional (order (ncols matrix)))
    (assert-square-matrix matrix)
    (let ((m (nrows matrix))
          (n (ncols matrix)))
      (policy-cond:with-expectations (> speed safety)
          ((assertion (<= order (max (nrows matrix) (ncols matrix)))))
        (let ((target (empty (list order order) :layout (layout matrix) :type (element-type matrix))))
          (if (> m n)
              (loop for i from (- m order) to (1- m)
                    do (loop for j from 0 to (min (+ (- order m) i) (1- n))
                             do (setf (tref target (- i (- m order)) j) (tref matrix i j))))
              (loop for j from 0 to (1- order)
                    do (loop for i from (max 0 (+ (- m order) j)) to (1- m)
                             do (setf (tref target (+ i (- order m)) j) (tref matrix i j)))))
          target)))))
;;; Synonym for lower-triangular
(setf (fdefinition 'tril) #'lower-triangular)

(defgeneric conjugate-transpose (matrix)
  (:documentation "Compute the conjugate transpose of a matrix")
  (:method ((matrix matrix))
    (map #'conjugate (transpose matrix))))
(setf (fdefinition 'dagger) #'conjugate-transpose)

(defgeneric conjugate-transpose! (matrix)
  (:documentation "Compute the conjugate transpose of a matrix, replacing the elements")
  (:method ((matrix matrix))
    (map! #'conjugate (transpose! matrix))))
(setf (fdefinition 'dagger!) #'conjugate-transpose!)

(defgeneric eig (matrix)
  (:documentation "Find the (right) eigenvectors and corresponding eigenvalues of a square matrix M. Returns a list and a tensor (EIGENVALUES, EIGENVECTORS)")
  (:method ((matrix matrix))
    (declare (ignore matrix))
    (error "EIG is not defined for the generic matrix type.")))

(defgeneric lu (matrix)
  (:documentation "Get the LU decomposition of MATRIX. Returns two tensors (LU, IPIV)")
  (:method ((matrix matrix))
    (declare (ignore matrix))
    (error "LU is not defined for the generic matrix type.")))

;; TODO: Make this one generic and move to lapack-macros
;;       This is being blocked by the ZUNCSD shenanigans
(defgeneric csd (matrix p q)
  (:documentation "Find the Cosine-Sine Decomposition of a matrix X given that it is to be partitioned with upper left block of dimension P-by-Q. Returns the CSD elements (VALUES U SIGMA VT) such that X=U*SIGMA*VT.")
  (:method ((matrix matrix) p q)
    (labels ((csd-from-blocks (u1 u2 v1t v2t theta)
               "Calculates the matrices U, SIGMA, and VT of the CSD of a matrix from its intermediate representation, as calculated from LAPACK-CSD."
               (let ((p (nrows u1))
                     (q (nrows v1t))
                     (m (+ (nrows u1) (nrows u2)))
                     (r (length theta)))
                 (let ((u (direct-sum u1 u2))
                       (sigma (const 0 (list m m) :type (element-type matrix)))
                       (vt (direct-sum v1t v2t)))
                   (let ((diag11 (min p q))
                         (diag12 (min p (- m q)))
                         (diag21 (min (- m p) q))
                         (diag22 (min (- m p) (- m q))))
                     (let ((iden11 (- diag11 r))
                           (iden12 (- diag12 r))
                           (iden21 (- diag21 r))
                           (iden22 (- diag22 r)))
                       ;; Construct sigma from theta
                       (loop :for i :from 0 :to (1- iden11)
                             do (setf (tref sigma i i) 1))
                       (loop :for i :from iden11 :to (1- diag11)
                             do (setf (tref sigma i i) (cos (nth (- i iden11) theta))))
                       (loop :for i :from 0 :to (1- iden12)
                             do (setf (tref sigma (- p 1 i) (- m 1 i)) -1))
                       (loop :for i :from iden12 :to (1- diag12)
                             do (setf (tref sigma (- p 1 i) (- m 1 i))
                                      (- (sin (nth (- r 1 (- i iden12)) theta)))))
                       (loop :for i :from 0 :to (1- iden21)
                             do (setf (tref sigma (- m 1 i) (- q 1 i)) 1))
                       (loop :for i :from iden21 :to (1- diag21)
                             do (setf (tref sigma (- m 1 i) (- q 1 i))
                                      (sin (nth (- r 1 (- i iden21)) theta))))
                       (loop :for i :from 0 :to (1- iden22)
                             do (setf (tref sigma (+ p i) (+ q i)) 1))
                       (loop :for i :from iden22 :to (1- diag22)
                             do (setf (tref sigma (+ p i) (+ q i)) (cos (nth (- i iden22) theta))))))
                   (values u sigma vt)))))
      (multiple-value-bind (u1 u2 v1t v2t theta) (lapack-csd matrix p q)
        (csd-from-blocks u1 u2 v1t v2t theta)))))

(defgeneric inv (matrix)
  (:documentation "Get the inv of the matrix")
  (:method ((matrix matrix))
    (declare (ignore matrix))
    (error "INV is not defined for the generic matrix type.")))

(defgeneric svd (matrix &key reduced)
  (:documentation "Find the SVD of a matrix M. Return (VALUES U SIGMA Vt) where M = U*SIGMA*Vt")
  (:method ((matrix matrix) &key reduced)
    (declare (ignore matrix reduced))
    (error "SVD is not defined for the generic matrix type.")))

(defgeneric qr (matrix)
  (:documentation "Finds the QL factorization of the matrix M. Returns two tensors (Q, R)
NOTE: Only square matrices supported")
  (:method ((matrix matrix))
    (declare (ignore matrix))
    (error "QR is not defined for the generic matrix type.")))

(defgeneric ql (matrix)
  (:documentation "Finds the QL factorization of the matrix M. Returns two tensors (Q, L)
NOTE: Only square matrices supported")
  (:method ((matrix matrix))
    (declare (ignore matrix))
    (error "QL is not defined for the generic matrix type.")))

(defgeneric rq (matrix)
  (:documentation "Finds the RQ factorization of the matrix M. Returns two tensors (R, Q)
NOTE: Only square matrices supported")
  (:method ((matrix matrix))
    (declare (ignore matrix))
    (error "RQ is not defined for the generic matrix type.")))

(defgeneric lq (matrix)
  (:documentation "Finds the LQ factorization of the matrix M. Returns two tensors (L, Q)
NOTE: Only square matrices supported")
  (:method ((matrix matrix))
    (declare (ignore matrix))
    (error "LQ is not defined for the generic matrix type.")))

