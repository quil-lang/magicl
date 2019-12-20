;;;; abstract-tensor.lisp
;;;;
;;;; Author: Cole Scott
;;;;
;;;; Collaborators: Robert Smith

(in-package #:magicl)

(defstruct (abstract-tensor))

;;; abstract-tensor protocol
;;; Must be implemented by subclasses

(defgeneric shape (tensor)
  (:documentation "The shape (dimensions) of the tensor. eg. '(2 3) for a 2x3 tensor"))

(defgeneric tref (tensor &rest pos)
  (:documentation "Get a reference to element at position"))

(defgeneric (setf tref) (new-value tensor &rest pos)
  (:documentation "Set the value of element at position"))

(defgeneric copy-tensor (tensor &rest args)
  (:documentation "Create a new tensor with the same properties as the given tensor, not initializing the storage"))

(defgeneric deep-copy-tensor (tensor &rest args)
  (:documentation "Create a new tensor with the same properties as the given tensor, copying storage"))

;;; abstract-tensor generic methods
;;; Can be optimized by subclasses

(defgeneric rank (tensor)
  (:documentation "Rank (number of dimensions) of the tensor")
  (:method ((tensor abstract-tensor))
    (length (shape tensor))))

(defgeneric size (tensor)
  (:documentation "Total number of elements in the tensor")
  (:method ((tensor abstract-tensor))
    (reduce #'* (shape tensor))))

(defgeneric element-type (tensor)
  (:documentation "The type of the elements in the tensor")
  (:method ((tensor abstract-tensor))
    (declare (ignore tensor))
    t))

(defgeneric lisp-array (tensor &optional target)
  (:documentation "Return a lisp array containing the data from the tensor

If TARGET is specified then the contents of the tensor are copied into the array.
In the event TARGET is not specified, the result may return an array sharing memory with the input tensor.")
  (:method :before (tensor &optional target)
    (unless (null target)
      (policy-cond:policy-if
       (< speed safety)
       (assert (and (= (rank tensor) (array-rank target))
                    (equal (shape tensor) (array-dimensions target))))
       nil)))
  (:method ((tensor abstract-tensor) &optional target)
    (let  ((arr (or target (make-array (shape tensor) :element-type (element-type tensor)))))
      (map-indexes
       (shape tensor)
       (lambda (&rest pos)
         (let ((val (apply #'tref tensor pos)))
           (apply #'(setf aref) val arr pos))))
      arr)))

(defgeneric map! (function tensor)
  (:documentation "Map elements of TENSOR by replacing the value with the output of FUNCTION on the element")
  (:method ((function function) (tensor abstract-tensor))
    (map-indexes
     (shape tensor)
     (lambda (&rest dims)
       (apply #'(setf tref) (funcall function (apply #'tref tensor dims)) tensor dims)))
    tensor))

(defgeneric into! (function tensor &key order)
  (:documentation "Map indices of TENSOR by replacing the value with the output of FUNCTION on the index at the element.

If ORDER is specified then traverse TENSOR in the specified order (column major or row major).")
  (:method ((function function) (tensor abstract-tensor) &key (order :row-major))
    (let ((map-func (if (eql order :column-major)
                        #'map-column-indexes
                        #'map-indexes)))
      (funcall map-func
               (shape tensor)
               (lambda (&rest dims)
                 (apply #'(setf tref) (apply function dims) tensor dims))))
    tensor))

(defgeneric foreach (function tensor &key order)
  (:documentation "Call FUNCTION with each element of TENSOR

If ORDER is specified then traverse TENSOR in the specified order (column major or row major).")
  (:method ((function function) (tensor abstract-tensor) &key (order :row-major))
    (let ((map-func (if (eql order :column-major)
                        #'map-column-indexes
                        #'map-indexes)))
      (funcall map-func
               (shape tensor)
               (lambda (&rest dims)
                 (funcall function (apply #'tref tensor dims)))))
    tensor))

(defgeneric map-to (function source target)
  (:documentation "Map elements of SOURCE by replacing the corresponding element of TARGET the output of FUNCTION on the source element")
  (:method ((function function) (source abstract-tensor) (target abstract-tensor))
    (policy-cond:policy-if
     (< speed safety)
     (assert (equalp (shape source) (shape target))
             () "Incompatible shapes. Cannot map tensor of shape ~a to tensor of shape ~a."
             (shape source) (shape target))
     nil)
    (map-indexes
     (shape source)
     (lambda (&rest dims)
       (apply #'(setf tref) (funcall function (apply #'tref source dims)) target dims)))))

(defgeneric map (function tensor)
  (:documentation "Map elements of TENSOR, storing the output of FUNCTION on the element into the corresponding element of a new tensor")
  (:method ((function function) (tensor abstract-tensor))
    (map! function (deep-copy-tensor tensor))))

(defgeneric into (function tensor)
  (:documentation "Map indices of TENSOR, storing the output of FUNCTION on the index into the corresponding element of a new tensor

If ORDER is specified then traverse TENSOR in the specified order (column major or row major).")
  (:method ((function function) (tensor abstract-tensor))
    (into! function (deep-copy-tensor tensor))))

(defgeneric sum (tensor)
  (:documentation "Get the sum of the elements of TENSOR")
  (:method ((tensor abstract-tensor))
    (let ((sum 0))
      (foreach (lambda (x) (incf sum x)) tensor)
      sum)))

(defgeneric scale (tensor factor)
  (:documentation "Scale TENSOR by FACTOR, returning a new tensor of the same type as TENSOR")
  (:method ((tensor abstract-tensor) (factor number))
    (map! (lambda (x) (* x factor)) (deep-copy-tensor tensor))))

(defgeneric scale! (tensor factor)
  (:documentation "Scale TENSOR by FACTOR, storing back into the tensor")
  (:method ((tensor abstract-tensor) (factor number))
    (map! (lambda (x) (* x factor)) tensor)))

(defgeneric slice (tensor from to)
  (:documentation "Slice a tensor from FROM to TO, returning a new tensor with the contained elements")
  (:method ((tensor abstract-tensor) from to)
    (declare (type sequence from to))
    (policy-cond:policy-if
     (< speed safety)
     (progn
       (assert (and (valid-index-p from (shape tensor))
                    (cl:every #'< from (shape tensor)))
               () "Incompatible FROM position for TENSOR. Position ~a is not within tensor shape ~a"
               from (shape tensor))
       (assert (and (cl:= (rank tensor) (length to))
                    (valid-shape-p to)
                    (cl:every #'<= to (shape tensor)))
               () "Incompatible TO position for TENSOR. Position ~a is not within tensor shape ~a"
               to (shape tensor))
       (assert (cl:every #'<= from to)
               () "Incomaptible TO and FROM positions. ~a is not less than or equal to ~a"
               from to))
     nil)
    (let* ((dims (mapcar #'cl:- to from))
           (target (empty dims
                    :order (order tensor)
                    :type (element-type tensor))))
      (map-indexes dims
                   (lambda (&rest dims)
                     (setf (apply #'tref target dims)
                           (apply #'tref tensor (mapcar #'cl:+ dims from)))))
      target)))

(defgeneric + (source1 source2 &optional target)
  (:documentation "Add tensors elementwise, optionally storing the result in TARGET.
If TARGET is not specified then a new tensor is created with the same element type as the first source tensor")
  (:method ((source1 abstract-tensor) (source2 abstract-tensor) &optional target)
    (policy-cond:policy-if
     (< speed safety)
     (assert (equalp (shape source1) (shape source2))
             () "Incompatible shapes. Cannot add tensor of shape ~a to tensor of shape ~a."
             (shape source1) (shape source2))
     nil)
    (let ((target (or target (copy-tensor source1))))
      (map-indexes
       (shape source1)
       (lambda (&rest dims)
         (apply #'(setf tref)
                (cl:+ (apply #'tref source1 dims)
                      (apply #'tref source2 dims))
                target dims)))
      target)))

(defgeneric - (source1 source2 &optional target)
  (:documentation "Subtract tensors elementwise, optionally storing the result in TARGET.
If TARGET is not specified then a new tensor is created with the same element type as the first source tensor")
  (:method ((source1 abstract-tensor) (source2 abstract-tensor) &optional target)
    (policy-cond:policy-if
     (< speed safety)
     (assert (equalp (shape source1) (shape source2))
             () "Incompatible shapes. Cannot add tensor of shape ~a to tensor of shape ~a."
             (shape source1) (shape source2))
     nil)
    (let ((target (or target (copy-tensor source1))))
      (map-indexes
       (shape source1)
       (lambda (&rest dims)
         (apply #'(setf tref)
                (cl:- (apply #'tref source1 dims)
                      (apply #'tref source2 dims))
                target dims)))
      target)))

(defgeneric = (source1 source2 &optional epsilon)
  (:documentation "Check the equality of tensors with an optional EPSILON")
  (:method ((source1 abstract-tensor) (source2 abstract-tensor) &optional epsilon)
    (declare (ignore epsilon))
    (unless (equal (shape source1) (shape source2))
      (return-from = nil))
    (map-indexes
     (shape source1)
     (lambda (&rest pos)
       (unless (equal
                (apply #'tref source1 pos)
                (apply #'tref source2 pos))
         (return-from = nil))))
    t))

(defgeneric every (predicate tensor)
  (:documentation "Check that every element in TENSOR meets a given condition")
  (:method ((predicate function) (tensor abstract-tensor))
      (foreach (lambda (x)
                 (unless (funcall predicate x)
                   (return-from every nil)))
               tensor)
    t))

(defgeneric some (predicate tensor)
  (:documentation "Check that some elements in TENSOR meet a given condition")
  (:method ((predicate function) (tensor abstract-tensor))
      (foreach (lambda (x)
                 (when (funcall predicate x)
                   (return-from some t)))
               tensor)
    nil))

(defgeneric notevery (predicate tensor)
  (:documentation "Check that not every element in TENSOR meets a given condition")
  (:method ((predicate function) (tensor abstract-tensor))
    (not (every predicate tensor))))

(defgeneric notany (predicate tensor)
  (:documentation "Check that not any element in TENSOR meets a given condition")
  (:method ((predicate function) (tensor abstract-tensor))
    (not (some predicate tensor))))

(defgeneric coerce-type (tensor type)
  (:documentation "Coerce element type of TENSOR to TYPE by creating a new tensor")
  (:method ((tensor abstract-tensor) type)
    (let ((target (empty (shape tensor) :order (order tensor) :type type)))
      (map-to (lambda (x) (coerce x type))
              tensor
              target)
      target)))
