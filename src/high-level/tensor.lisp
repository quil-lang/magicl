;;;; tensor.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(deftype tensor-storage (&optional type)
  `(simple-array ,type (*)))

(defclass tensor (abstract-tensor)
  (;; abstract-tensor slots
   (rank
    :initarg :rank
    :initform 1
    :reader rank
    :type alexandria:non-negative-fixnum
    :documentation "The rank (number of dimensions) of the tensor")
   (shape
    :initarg :shape
    :initform '(0)
    :reader shape
    :type list
    :documentation "The shape of the tensor. eg. '(2 3) for a 2x3 tensor")
   (size
    :initarg :size
    :initform 0
    :reader size
    :type (alexandria:non-negative-fixnum)
    :documentation "Total number of elements in the tensor")
   (element-type
    :initarg :element-type
    :initform (error "Missing element-type") ; TODO: much better error messages
    :reader element-type
    :type type
    :documentation "The type of the elements in the tensor")
   ;; tensor-specific slots
   (storage
    :initarg :storage
    :initform (error "Missing storage")
    :reader storage
    :documentation "Storage of the tensor, typically in a vector in column major order")
   (order
    :initarg :order
    :initform :column-major
    :reader order
    :type (member :row-major :column-major)
    :documentation "Indexing order of storage (:column-major or :row-major)."))
  (:metaclass abstract-class:abstract-class))

;;; Required abstract-tensor methods

(defmethod tref ((tensor tensor) &rest pos)
  ;; TODO: Check pos type
  (assert (cl:= (rank tensor) (list-length pos))
          () "Invalid index ~a. Must be rank ~a" pos (rank tensor))
  (assert (cl:every #'< pos (shape tensor))
          () "Index ~a out of range" pos)
  (let ((index (case (order tensor)
                 (:row-major (row-major-index pos (shape tensor)))
                 (:column-major (column-major-index pos (shape tensor))))))
    (aref (storage tensor) index)))

(defmethod (setf tref) (new-value (tensor tensor) &rest pos)
       (assert (cl:= (rank tensor) (list-length pos))
               () "Invalid index ~a. Must be rank ~a" pos (rank tensor))
       (assert (cl:every #'< pos (shape tensor))
               () "Index ~a out of range" pos)
       (let ((index (case (order tensor)
                      (:row-major (row-major-index pos (shape tensor)))
                      (:column-major (column-major-index pos (shape tensor))))))
         (setf (aref (storage tensor) index) new-value)))

(defmethod copy-tensor ((tensor tensor) &rest args)
  (apply #'make-instance (class-of tensor)
         :rank (rank tensor)
         :shape (shape tensor)
         :size (size tensor)
         :element-type (element-type tensor)
         :storage (make-array (size tensor)
                              :element-type (element-type tensor))
         :order (order tensor)
         args))

(defmethod deep-copy-tensor ((tensor tensor) &rest args)
  (apply #'make-instance (class-of tensor)
         :rank (rank tensor)
         :shape (shape tensor)
         :size (size tensor)
         :element-type (element-type tensor)
         :storage (alexandria:copy-array (storage tensor))
         :order (order tensor)
         args))

;;; Optimized abstract-tensor methods

;; TODO: This does not work in column-major order
#+ignore
(defmethod map! ((f function) (tensor tensor))
  (setf (slot-value tensor 'storage) (cl:map 'vector f (storage tensor)))
  tensor)

;; TODO: this might be putting them in wrong. investigate
;;     NOTE: Might need map-column-indexes
#+ignore
(defmethod into! ((f function) (tensor tensor))
  (let ((i 0))
    (map-indexes
     (shape tensor)
     (lambda (&rest dims)
       (setf (aref (storage tensor) i) (apply f dims))
       (incf i)))
    tensor))

;;; Specfic tensor classes

(defgeneric make-tensor (shape class element-type &key initial-element order storage)
  (:documentation "Make a tensor with elements of the specified type")
  (:method (shape class element-type &key initial-element order storage)
    (check-type shape shape)
    (let ((size (reduce #'* shape)))
      (make-instance
       class
       :rank (length shape)
       :shape shape
       :size size
       :element-type element-type
       :storage (or
                 storage
                 (apply #'make-array
                        size
                        :element-type element-type
                        (if initial-element
                            (list  :initial-element initial-element)
                            nil)))
       :order (or order :column-major)))))

(defmacro deftensor (name type)
  `(progn
     (defclass ,name (tensor)
       ((storage :type (tensor-storage ,type)))
       (:documentation ,(format nil "Tensor with element type of ~a" type)))))

;; Methods to be specified by the specific tensor classes (maybe)



;;; Generic tensor methods

;; SHOULD BE BASED ON ORDER (MAYBE)
(defgeneric reshape (tensor shape)
  (:documentation "Change the shape of the tensor.
WARNING: This method acts differently depending on the order of the tensor. Do not expect row-major to act the same as column-major.")
  (:method ((tensor tensor) shape)
    ;; TODO: check type
    (let ((shape-size (reduce #'* shape)))
      (assert (cl:= (size tensor) shape-size)
              () "Incompatible shape. Must have the same total number of elements. The tensor has ~a elements and the new shape has ~a elements" (size tensor) shape-size))
  (setf (slot-value tensor 'shape) shape)
  (setf (slot-value tensor 'rank) (length shape))
  tensor))

(defgeneric stack (tensorA tensorB dim) ;; Also have specifics for 2d
  (:documentation "Create a new tensor from stacking the tensors in the specififed dimension"))
