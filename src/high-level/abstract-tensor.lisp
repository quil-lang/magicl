;;;; abstract-tensor.lisp
;;;;
;;;; Author: Cole Scott
;;;;
;;;; Collaborators: Robert Smith

(in-package #:magicl)

(defclass abstract-tensor ()
  ()
  (:documentation "Abstract tensor class as super class of tensor classes")
  (:metaclass abstract-class:abstract-class))

(defgeneric specialize-tensor (abstract-tensor)
  (:documentation "Specialize a tensor to the most specfic applicable tensor class"))

(defgeneric generalize-tensor (abstract-tensor)
  (:documentation "Generalize a tensor to the least specific applicable tensor class"))

;;; abstract-tensor protocol

;; Must be implemented by subclasses

(defgeneric shape (abstract-tensor)
  (:documentation "The shape (dimensions) of the tensor. eg. '(2 3) for a 2x3 tensor"))

(defgeneric tref (abstract-tensor &rest pos)
  (:documentation "Get a reference to element at position"))

(defgeneric (setf tref) (new-value abstract-tensor &rest pos)
  (:documentation "Set the value of element at position"))

(defgeneric copy-tensor (abstract-tensor &rest args)
  (:documentation "Create a new tensor with the same properties as the given tensor, not initializing the storage"))

(defgeneric deep-copy-tensor (abstract-tensor &rest args)
  (:documentation "Create a new tensor with the same properties as the given tensor, copying storage"))

;; Stuff with somewhat sensible defaults

(defgeneric rank (abstract-tensor)
  (:documentation "Rank (number of dimensions) of the tensor")
  (:method ((tensor abstract-tensor))
    (length (shape tensor))))

(defgeneric size (abstract-tensor)
  (:documentation "Total number of elements in the tensor")
  (:method ((tensor abstract-tensor))
    (reduce #'* (shape tensor))))

(defgeneric element-type (abstract-tensor)
  (:documentation "The type of the elements in the tensor")
  (:method ((tensor abstract-tensor))
    (declare (ignore tensor))
    t))

(defgeneric lisp-array (abstract-tensor &optional target)
  (:documentation "Return a lisp array containing the data from the tensor

If the target is specified then the contents of the tensor are copied into the array.
In the event target is not specified, the result may return an array sharing memory with the input tensor.")
  (:method :before (tensor &optional target)
    (unless (null target)
      (assert (and (= (rank tensor) (array-rank target))
                   (equal (shape tensor) (array-dimensions target))))))
  (:method ((tensor abstract-tensor) &optional target)
    (let  ((arr (or target (make-array (shape tensor) :element-type (element-type tensor)))))
      (map-indexes
       (shape tensor)
       (lambda (&rest pos)
         (let ((val (apply #'tref tensor pos)))
           (apply #'(setf aref) val arr pos))))
      arr)))

(defgeneric map! (function abstract-tensor)
  (:documentation "Map elements of a tensor
NOTE: mutates tensor!")
  (:method ((function function) (tensor abstract-tensor))
    (map-indexes
     (shape tensor)
     (lambda (&rest dims)
       (apply #'(setf tref) (funcall function (apply #'tref tensor dims)) tensor dims)))
    tensor))

(defgeneric into! (function abstract-tensor)
  (:documentation "Map indices to elements
NOTE: mutates tensor!")
  (:method ((function function) (tensor abstract-tensor))
    (map-indexes
     (shape tensor)
     (lambda (&rest dims)
       (apply #'(setf tref) (apply function dims) tensor dims)))
    tensor))

(defgeneric foreach (function abstract-tensor)
  (:documentation "Call a function with each element of a tensor")
  (:method ((function function) (tensor abstract-tensor))
    (map-indexes
     (shape tensor)
     (lambda (&rest dims)
       (funcall function (apply #'tref tensor dims))))
    tensor))

(defgeneric map-to (function source target)
  (:documentation "Map from one tensor to another using a given function")
  (:method ((function function) (source abstract-tensor) (target abstract-tensor))
    (assert (equalp (shape source) (shape target))
            () "Incompatible shapes. Cannot map tensor of shape ~a to tensor of shape ~a."
            (shape source) (shape target))
    (map-indexes
     (shape source)
     (lambda (&rest dims)
       (apply #'(setf tref) (funcall function (apply #'tref source dims)) target dims)))))

(defgeneric map (function abstract-tensor)
  (:documentation "Map elements of a tensor")
  (:method ((function function) (tensor abstract-tensor))
    (let ((target (copy-tensor tensor)))
      (map-to function tensor target)
      target)))

;; TODO: Determine if this makes sense
(defgeneric into (function abstract-tensor)
  (:documentation "Map indices to elements")
  (:method ((function function) (tensor abstract-tensor))
    (let ((target (copy-tensor tensor)))
      (into! function target)
      target)))

(defgeneric sum (abstract-tensor)
  (:documentation "Get the sum of the elements of a tensor")
  (:method ((tensor abstract-tensor))
    (let ((sum 0)) ;; TODO: Make sure this works for all types
      (foreach (lambda (x) (incf sum x)) tensor)
      sum)))

(defgeneric scale (abstract-tensor factor)
  (:documentation "Scale the tensor by a given factor, returning a new tensor")
  (:method ((tensor abstract-tensor) (factor number))
    (let ((target (copy-tensor tensor)))
      (map-to (lambda (x) (* x factor)) tensor target)
      target)))

(defgeneric scale! (abstract-tensor factor)
  (:documentation "Scale the tensor by the given factor, storing back into the tensor")
  (:method ((tensor abstract-tensor) (factor number))
    (map! (lambda (x) (* x factor)) tensor)))

;; TODO: make sure this doesn't destroy the vector type
;; TODO: Allow for any number of tensors. Might need to write something to remove keywords from rest
(defgeneric + (source1 source2 &key target)
  (:documentation "Add two tensors elementwise, storing results in target or creating a new tensor")
  (:method ((tensor1 abstract-tensor) (tensor2 abstract-tensor) &key target)
    (assert (equalp (shape tensor1) (shape tensor2))
            () "Incompatible shapes. Cannot add tensor of shape ~a to tensor of shape ~a."
            (shape tensor1) (shape tensor2))
    (let ((target (or target (copy-tensor tensor1))))
      (map-indexes
       (shape tensor1)
       (lambda (&rest dims)
         (apply #'(setf tref)
                (cl:+ (apply #'tref tensor1 dims)
                      (apply #'tref tensor2 dims))
                target dims)))
      target)))

(defgeneric - (source1 source2 &key target)
  (:documentation "Subtract two tensors elementwise, storing results in target or creating a new tensor")
  (:method ((tensor1 abstract-tensor) (tensor2 abstract-tensor) &key target)
    (assert (equalp (shape tensor1) (shape tensor2))
            () "Incompatible shapes. Cannot add tensor of shape ~a to tensor of shape ~a."
            (shape tensor1) (shape tensor2))
    (let ((target (or target (copy-tensor tensor1))))
      (map-indexes
       (shape tensor1)
       (lambda (&rest dims)
         (apply #'(setf tref)
                (cl:- (apply #'tref tensor1 dims)
                      (apply #'tref tensor2 dims))
                target dims)))
      target)))

;; TODO: Make this work for more than 2 elements (maybe)
(defgeneric = (tensor1 tensor2 &optional epsilon)
  (:documentation "Equality check for tensors")
  (:method ((tensor1 abstract-tensor) (tensor2 abstract-tensor) &optional epsilon)
    (declare (ignore epsilon))
    (unless (equal (shape tensor1) (shape tensor2))
      (return-from = nil))
    (map-indexes
     (shape tensor1)
     (lambda (&rest pos)
       (unless (equal
                (apply #'tref tensor1 pos)
                (apply #'tref tensor2 pos))
         (return-from = nil))))
    t))


(defgeneric every (predicate tensor)
  (:documentation "Check that every element in a tensor meets a given condition")
  (:method ((predicate function) (tensor abstract-tensor))
      (foreach (lambda (x)
                 (unless (funcall predicate x)
                   (return-from every nil)))
               tensor)
    t))

(defgeneric some (predicate tensor)
  (:documentation "Check that some elements in a tensor meet a given condition")
  (:method ((predicate function) (tensor abstract-tensor))
      (foreach (lambda (x)
                 (when (funcall predicate x)
                   (return-from some t)))
               tensor)
    nil))

(defgeneric notevery (predicate tensor)
  (:documentation "Check that not every element in a tensor meets a given condition")
  (:method ((predicate function) (tensor abstract-tensor))
    (not (every predicate tensor))))

(defgeneric notany (predicate tensor)
  (:documentation "Check that not any element in a tensor meets a given condition")
  (:method ((predicate function) (tensor abstract-tensor))
    (not (some predicate tensor))))

