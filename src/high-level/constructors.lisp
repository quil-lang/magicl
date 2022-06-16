;;;; constructors.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(defvar *default-tensor-type* 'double-float
  "The default element type to be used for constructing tensors when TYPE is not specified.")

(defun empty (shape &key (type *default-tensor-type*) layout)
  "Create a tensor without intializing the contents of the storage

If TYPE is not specified then *DEFAULT-TENSOR-TYPE* is used.
LAYOUT specifies the internal storage representation ordering of the returned tensor.
The tensor specialized on the specified SHAPE and TYPE."
  (policy-cond:with-expectations (> speed safety)
      ((type shape shape))
    (let ((tensor-type (infer-tensor-type type shape nil)))
      (make-tensor tensor-type shape :layout layout))))

(defun const (const shape &key type layout)
  "Create a tensor with the specified SHAPE with each element being set to CONST

If TYPE is not specified then it is inferred from the type of CONST.
LAYOUT specifies the internal storage representation ordering of the returned tensor.
The tensor is specialized on SHAPE and TYPE."
  (policy-cond:with-expectations (> speed safety)
      ((type shape shape))
    (let ((tensor-class (infer-tensor-type type shape const)))
      (make-tensor tensor-class shape :layout layout :initial-element const))))

(defun rand (shape &key (type *default-tensor-type*) layout distribution)
  "Create tensor with random elements from DISTRIBUTION

DISTRIBUTION is a function with no arguments which returns a value for the element.
If DISTRIBUTION is not specified then a uniform distribution on [0,1] (or [0,1] + [0,1]i for complex types) is used.
If TYPE is not specified then *DEFAULT-TENSOR-TYPE* is used.
LAYOUT specifies the internal storage representation ordering of the returned tensor.
The tensor is specialized on SHAPE and TYPE."
  (policy-cond:with-expectations (> speed safety)
      ((type shape shape))
    (let* ((tensor-class (infer-tensor-type type shape nil))
           (rand-function
             (or distribution
                 (cond
                   ((subtypep type 'complex)
                    (lambda ()
                      (complex
                       (random 1d0)
                       (random 1d0))))
                   (t
                    (lambda ()
                      (random 1d0))))))
           (f (lambda (&rest rest)
                (declare (ignore rest))
                (coerce  (funcall rand-function) type))))
      (into! f (make-tensor tensor-class shape :layout layout)))))

(defun eye (shape &key value (offset 0) (type *default-tensor-type*) layout)
  "Create an identity tensor

SHAPE can either be a list of dimensions or a fixnum defining the length of the side a square matrix.

If VALUE is not specified then 1 is used.
If TYPE is not specified then it is inferred from the type of VALUE, defaulting to *DEFAULT-TENSOR-TYPE*.
LAYOUT specifies the internal storage representation ordering of the returned tensor.
The tensor is specialized on SHAPE and TYPE.
If OFFSET is specified then the diagonal band will be offset by that much, positive shifting up and negative shifting down."
  (declare (type (or shape fixnum) shape)
           (type fixnum offset))
  (let ((shape (if (integerp shape)
                   (fixnum-to-shape shape)
                   shape)))
    (cond
      ((/= offset 0)
       (assert (cl:= (length shape) 2)
               (shape)
               "The length of SHAPE must be 2 when OFFSET is specified. Shape has length ~A." (length shape))
       ;; Now we know we are dealing with a matrix and can use offsets
       ;;
       ;; NOTE: We infer the tensor type this late to allow for
       ;; reassignment of SHAPE in the assertion.
       (let* ((tensor-class (infer-tensor-type type shape value))
              (tensor (make-tensor tensor-class shape :layout layout :initial-element 0))
              (fill-value (coerce (or value 1) (element-type tensor)))) ;; TODO: use registry)
         (loop :for i :from (max 0 (- offset)) :below (first shape)
               :for j :from (max 0 offset) :below (second shape) :do
                 (setf (tref tensor i j) fill-value))
         tensor))
      (t
       (let* ((tensor-class (infer-tensor-type type shape value))
              (tensor (make-tensor tensor-class shape :layout layout :initial-element 0))
              (fill-value (coerce (or value 1) (element-type tensor))) ;; TODO: use registry
              (shape-length (length shape)))
         (dotimes (i (reduce #'min shape) tensor)
           (setf (apply #'tref tensor (make-list shape-length :initial-element i)) fill-value)))))))

(defun arange (range &key (type *default-tensor-type*))
  "Create a 1-dimensional tensor of elements from 0 up to but not including the RANGE

If TYPE is not specified then it is inferred from the type of RANGE.
LAYOUT specifies the internal storage representation ordering of the returned tensor.
The tensor is specialized on TYPE with shape (floor(RANGE))."
  (let ((tensor-class (infer-tensor-type type (list (floor range)) range)))
    (let ((tensor (make-tensor tensor-class (list (floor range))))
          (f (lambda (index)
               (coerce index type))))
      (into! f tensor))))

;; NOTE: This does not accout for changing array layout. If the input
;;       is in row-major but the constructor specifies column-major
;;       then the tensor will not return the correct values. Not a
;;       huge deal but something that be improved (similarly to how
;;       FROM-LIST does it.)
;; XXX: This gets the type of an array from the array, not
;;       by inspecting the elements. This can cause issues when
;;       the type of the array is T
(defun from-array (array shape &key (type (array-element-type array)) (layout :row-major) (input-layout :row-major))
  "Create a tensor from ARRAY, calling ADJUST-ARRAY on ARRAY to flatten to a 1-dimensional array of length equal to the product of the elements in SHAPE

If TYPE is not specified then it is inferred from the element type of ARRAY.
LAYOUT specifies the internal storage representation ordering of the returned tensor.
INPUT-LAYOUT specifies the layout of ARRAY.
The tensor is specialized on SHAPE and TYPE."
  (policy-cond:with-expectations (> speed safety)
      ((type shape shape))
    (let* ((tensor-class (infer-tensor-type type shape nil))
           (storage-size (reduce #'* shape))
           (array-dims (array-dimensions array)))
      (multiple-value-bind (storage finalizer)
          (allocate storage-size
                    :element-type type)
        (let ((index-function
                (if (eq layout ':row-major)
                    #'row-major-index
                    #'column-major-index))
              (input-index-function
                (if (eq input-layout ':row-major)
                    #'row-major-index
                    #'column-major-index)))
          (cond
            ((not (cdr array-dims))
             (map-indexes
              shape
              (lambda (&rest pos)
                (setf (aref storage (funcall index-function pos shape))
                      (aref array (funcall input-index-function pos shape))))))
            (t (map-indexes
                shape
                (lambda (&rest pos)
                  (setf (aref storage (funcall index-function pos shape))
                        (apply #'aref array pos)))))))
        (let ((tensor
                (make-tensor tensor-class shape
                             :storage storage
                             :layout layout)))
          (finalize tensor finalizer)
          tensor)))))

(defun from-storage (storage shape &key layout)
  "Create a tensor with the specified STORAGE and SHAPE.

This shares STORAGE; mutate at your own peril!

LAYOUT specifies the internal storage representation ordering of the returned tensor."
  (policy-cond:with-expectations (> speed safety)
      ((type shape shape)
       (assertion (cl:= 1 (array-rank storage)))
       (assertion (cl:= (reduce #'* shape)
                        (array-total-size storage))))
    (let* ((tensor-type (infer-tensor-type (array-element-type storage) shape nil))
           (tensor (make-tensor tensor-type shape :layout layout)))
      (setf (storage tensor) storage)
      tensor)))

(defun from-list (list shape &key type layout (input-layout :row-major))
  "Create a tensor with the elements of LIST, placing in layout INPUT-LAYOUT

If INPUT-LAYOUT is not specified then row-major is assumed.

If TYPE is not specified then it is inferred from the type of the first element of LIST.
LAYOUT specifies the internal storage representation ordering of the returned tensor.
The tensor is specialized on SHAPE and TYPE."
  (let ((len (length list)))
    (policy-cond:with-expectations (> speed safety)
        ((type shape shape)
         (assertion (cl:= len (reduce #'* shape))))
      (let* ((tensor-class (infer-tensor-type type shape (first list)))
             (tensor (make-tensor tensor-class shape :layout layout))
             (index-function (if (eq input-layout ':row-major)
                                 #'from-row-major-index
                                 #'from-column-major-index)))
        ;; XXX: Optimizations for rank 1 and 2 tensors should be added
        (dotimes (i len tensor)
          (setf (apply #'tref tensor (funcall index-function i shape))
                (pop list)))))))

(defun from-diag (list &key (order 2) (offset 0) type layout)
  "Create a tensor from a list, placing along the diagonal

If ORDER is specified then the tensor will be of that order, otherwise 2 is assumed.
If TYPE is not specified then it is inferred from the type of the first element of LIST.
LAYOUT specifies the internal storage representation ordering of the returned tensor.
The tensor is specialized on SHAPE and TYPE.
If OFFSET is specified then the diagonal band will be offset by that much, positive shifting up and negative shifting down."
  (cond
    ((/= offset 0)
     (assert (cl:= order 2)
             (order)
             "ORDER must be 2 when OFFSET is specified. ORDER is ~A." order)
     ;; Now we know we are dealing with a matrix.
     (let* ((length (+ (length list) (abs offset)))
            (shape (fixnum-to-shape length order))
            (tensor-class (infer-tensor-type type shape (first list)))
            (tensor (make-tensor tensor-class shape :layout layout :initial-element 0)))
       (loop :for i :from (max 0 (- offset)) :below (first shape)
             :for j :from (max 0 offset) :below (second shape) :do
               (setf (tref tensor i j) (pop list)))
       tensor))
    (t
     (let* ((length (+ (length list) (abs offset)))
            (shape (fixnum-to-shape length order))
            (tensor-class (infer-tensor-type type shape (first list)))
            (tensor (make-tensor tensor-class shape :layout layout :initial-element 0)))
       (dotimes (i (reduce #'min shape) tensor)
         (setf (apply #'tref tensor (make-list order :initial-element i))
               (pop list)))))))

;;; Constructors for convenience

(defun zeros (shape &key (type *default-tensor-type*) layout)
  "Create a tensor with the specified SHAPE of zeros

If TYPE is not specified then *DEFAULT-TENSOR-TYPE* is used.
LAYOUT specifies the internal storage representation ordering of the returned tensor.
The tensor specialized on the specified SHAPE and TYPE."
  (const 0 shape :type type :layout layout))

(defun ones (shape &key (type *default-tensor-type*) layout)
  "Create a tensor with the specified SHAPE of ones

If TYPE is not specified then *DEFAULT-TENSOR-TYPE* is used.
LAYOUT specifies the internal storage representation ordering of the returned tensor.
The tensor specialized on the specified SHAPE and TYPE."
  (const 1 shape :type type :layout layout))

;;; Random Matrices
;;;
;;; See also RAND.

(defgeneric fill-with-random-normal! (matrix)
  (:method ((vector vector/single-float))
    (into! (lambda (i)
             (declare (ignore i))
             (coerce (alexandria:gaussian-random) 'single-float))
           vector))
  (:method ((vector vector/double-float))
    (into! (lambda (i)
             (declare (ignore i))
             (alexandria:gaussian-random))
           vector))
  (:method ((matrix vector/complex-single-float))
    (into! (lambda (i)
             (declare (ignore i))
             (multiple-value-bind (re im) (alexandria:gaussian-random)
               (complex (coerce re 'single-float)
                        (coerce im 'single-float))))
           matrix))
  (:method ((matrix vector/complex-double-float))
    (into! (lambda (i)
             (declare (ignore i))
             (multiple-value-call #'complex (alexandria:gaussian-random)))
           matrix))
  (:method ((matrix matrix/single-float))
    (into! (lambda (i j)
             (declare (ignore i j))
             (coerce (alexandria:gaussian-random) 'single-float))
           matrix))
  (:method ((matrix matrix/double-float))
    (into! (lambda (i j)
             (declare (ignore i j))
             (values (alexandria:gaussian-random)))
           matrix))
  (:method ((matrix matrix/complex-single-float))
    (into! (lambda (i j)
             (declare (ignore i j))
             (multiple-value-bind (re im) (alexandria:gaussian-random)
               (complex (coerce re 'single-float)
                        (coerce im 'single-float))))
           matrix))
  (:method ((matrix matrix/complex-double-float))
    (into! (lambda (i j)
             (declare (ignore i j))
             (multiple-value-call #'complex (alexandria:gaussian-random)))
           matrix)))

(defun random-normal (shape &key (type *default-tensor-type*))
  "Produce a matrix of entries which are normally distributed (mean 0, variance 1). Complex types will have their real and imaginary parts normally distributed."
  (fill-with-random-normal! (zeros shape :type type)))

(defun random-unitary (n &key (type `(complex ,*default-tensor-type*)))
  "Generate a uniformly random element of U(n)."
  ;; See "How to generate random matrices from the classical compact
  ;; groups" for details [https://arxiv.org/abs/math-ph/0609050], like
  ;; why there is scaling.
  (multiple-value-bind (q r) (qr (random-normal (list n n) :type type))
    (let ((d (diag r)))
      (setf d (cl:map 'list (lambda (di) (/ di (sqrt (* di (conjugate di))))) d))
      (@ q (funcall #'from-diag d)))))

(defun random-special-unitary (n &key (type `(complex ,*default-tensor-type*)))
  "Generate a uniformly random element of SU(n)."
  (let ((m (random-unitary n :type type)))
    (magicl:scale! m (expt (magicl:det m) (/ (- n))))))
