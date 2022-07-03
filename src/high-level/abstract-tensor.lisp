;;;; abstract-tensor.lisp
;;;;
;;;; Author: Cole Scott
;;;;
;;;; Collaborators: Robert Smith

(in-package #:magicl)

(defstruct (abstract-tensor
            (:constructor nil)
            (:copier nil))
  "Abstract tensor class. Superclass for implementing the abstract-tensor protocol")

;;; abstract-tensor protocol
;;; These methods do not have generic definitions and must be implemented by subclasses

(defgeneric shape (tensor)
  (:documentation "The shape (dimensions) of the tensor. eg. '(2 3) for a 2x3 tensor"))

(defgeneric tref (tensor &rest pos)
  (:documentation "Get a reference to element at position"))

(defgeneric (setf tref) (new-value tensor &rest pos)
  (:documentation "Set the value of element at position"))

(defgeneric make-tensor (class shape &key initial-element layout storage)
  (:documentation "Make a dense tensor with elements of the specified type"))

(defgeneric copy-tensor (tensor &rest args)
  (:documentation "Create a new tensor with the same properties as the given tensor, creating new storage without initializing contents"))

(defgeneric deep-copy-tensor (tensor &rest args)
  (:documentation "Create a new tensor with the same properties as the given tensor, copying the contents of the storage"))

;;; abstract-tensor generic methods
;;; These methods have default generic implementations but can be optimized by subclasses

(defgeneric order (tensor)
  (:documentation "Order (number of dimensions) of the tensor")
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
  (:method ((tensor abstract-tensor) &optional target)
    (policy-cond:with-expectations (> speed safety)
        ((assertion (or (null target)
                        (and (= (order tensor) (array-rank target))
                             (equal (shape tensor) (array-dimensions target))))))
      (let  ((arr (or target (make-array (shape tensor) :element-type (element-type tensor)))))
        (map-indexes
         (shape tensor)
         (lambda (&rest pos)
           (let ((val (apply #'tref tensor pos)))
             (apply #'(setf aref) val arr pos))))
        arr))))

(define-extensible-function (map! map!-lisp) (function tensor)
  (:documentation "Map elements of TENSOR by replacing the value with the output of FUNCTION on the element")
  (:method ((function function) (tensor abstract-tensor))
    (map-indexes
     (shape tensor)
     (lambda (&rest dims)
       (apply #'(setf tref) (funcall function (apply #'tref tensor dims)) tensor dims)))
    tensor))

(defgeneric into! (function tensor)
  (:documentation "Map indices of TENSOR by replacing the value with the output of FUNCTION on the index at the element.")
  (:method ((function function) (tensor abstract-tensor))
    (map-indexes
     (shape tensor)
     (lambda (&rest dims)
       (apply #'(setf tref) (apply function dims) tensor dims)))
    tensor))

(defgeneric foreach (function tensor)
  (:documentation "Call FUNCTION with each element of TENSOR")
  (:method ((function function) (tensor abstract-tensor))
    (map-indexes
     (shape tensor)
     (lambda (&rest dims)
       (funcall function (apply #'tref tensor dims))))
    tensor))

(defgeneric map-to (function source target)
  (:documentation "Map elements of SOURCE by replacing the corresponding element of TARGET the output of FUNCTION on the source element")
  (:method ((function function) (source abstract-tensor) (target abstract-tensor))
    (policy-cond:with-expectations (> speed safety)
        ((assertion (equalp (shape source) (shape target))))
      (map-indexes
       (shape source)
       (lambda (&rest dims)
         (apply #'(setf tref) (funcall function (apply #'tref source dims)) target dims))))))

(defgeneric map (function tensor)
  (:documentation "Map elements of TENSOR, storing the output of FUNCTION on the element into the corresponding element of a new tensor")
  (:method ((function function) (tensor abstract-tensor))
    (map! function (deep-copy-tensor tensor))))

(defgeneric into (function tensor)
  (:documentation "Map indices of TENSOR, storing the output of FUNCTION on the index into the corresponding element of a new tensor

If LAYOUT is specified then traverse TENSOR in the specified order (column major or row major).")
  (:method ((function function) (tensor abstract-tensor))
    (into! function (deep-copy-tensor tensor))))

(defgeneric sum (tensor)
  (:documentation "Get the sum of the elements of TENSOR")
  (:method ((tensor abstract-tensor))
    (let ((sum 0))
      (foreach (lambda (x) (incf sum x)) tensor)
      sum)))

(define-backend-function scale! (tensor factor)
  "Scale TENSOR by FACTOR, storing back into the tensor")

(define-backend-implementation scale! :lisp
  (lambda (tensor factor)
    (map! (lambda (x) (* x factor)) tensor)))

(define-backend-function scale (tensor factor)
  "Scale TENSOR by FACTOR, returning a new tensor of the same type as TENSOR")

(define-backend-implementation scale :lisp
  (lambda (tensor factor)
    (scale! (deep-copy-tensor tensor) factor)))

(defgeneric slice (tensor from to)
  (:documentation "Slice a tensor from FROM to TO, returning a new tensor with the contained elements")
  (:method ((tensor abstract-tensor) from to)
    (declare (type sequence from to))
    (policy-cond:with-expectations (> speed safety)
        ((assertion (and (valid-index-p from (shape tensor))
                         (cl:every #'< from (shape tensor))))
         (assertion (and (cl:= (order tensor) (length to))
                         (valid-shape-p to)
                         (cl:every #'<= to (shape tensor))))
         (assertion (cl:every #'<= from to)))
      (let* ((dims (mapcar #'- to from))
             (target (empty dims
                            :layout (layout tensor)
                            :type (element-type tensor))))
        (map-indexes dims
                     (lambda (&rest dims)
                       (setf (apply #'tref target dims)
                             (apply #'tref tensor (mapcar #'+ dims from)))))
        target))))

(defgeneric slice-to (source from to target offset)
  (:documentation "Slice the tensor SOURCE from FROM to TO, storing results in TARGET with the prescribed OFFSET.")
  (:method ((source abstract-tensor) from to (target abstract-tensor) offset)
    (declare (type sequence from to))
    (policy-cond:with-expectations (> speed safety)
        ((assertion (and (valid-index-p from (shape source))
                         (cl:every #'< from (shape source))))
         (assertion (and (cl:= (order source) (length to))
                         (valid-shape-p to)
                         (cl:every #'<= to (shape source))))
         (assertion (cl:every #'<= from to))
         (assertion (equalp (element-type source)
                            (element-type target)))
         (assertion (and (cl:every #'<=
                                   (mapcar (lambda (off to from) (+ off (- to from)))
                                           offset to from)
                                   (shape target)))))
      (let ((dims (mapcar #'- to from)))
        (map-indexes dims
                     (lambda (&rest dims)
                       (setf (apply #'tref target (mapcar #'+ dims offset))
                             (apply #'tref source (mapcar #'+ dims from)))))
        target))))

(defgeneric binary-operator (function source1 source2 &optional target)
  (:documentation "Perform a binary operator on tensors elementwise, optionally storing the result in TARGET.
If TARGET is not specified then a new tensor is created with the same element type as the first source tensor")
  (:method ((function function) (source1 abstract-tensor) (source2 abstract-tensor) &optional target)
    (policy-cond:with-expectations (> speed safety)
        ((assertion (equalp (shape source1) (shape source2))))
      (let ((target (or target (copy-tensor source1))))
        (map-indexes
         (shape source1)
         (lambda (&rest dims)
           (apply #'(setf tref)
                  (funcall function
                           (apply #'tref source1 dims)
                           (apply #'tref source2 dims))
                  target dims)))
        target))))

;;; Extend bianary-operator to handle (TENSOR, NUMBER) and (NUMBER, TENSOR)
;;; arguments Recall that, e.g., TENSOR + NUMBER is commutative, but
;;; TENSOR - NUMBER isn't, so need two DEFMETHODs here
;;;
;;; N.B. This implementation still suffers from the existing issue that if the
;;; type of the arguments differ then we can get a TYPE-ERROR from the SETF
;;; Try adding a MATRIX/INT32 to a MATRIX/SINGLE-FLOAT

;;  (TENSOR, NUMBER)
(defmethod binary-operator ((function function) (source1 abstract-tensor) (source2 number) &optional target)
  (let ((target (or target (copy-tensor source1))))
    (map-indexes
     (shape source1)
     (lambda (&rest dims)
       (apply #'(setf tref)
              (funcall function
                       (apply #'tref source1 dims)
                       source2)
              target dims)))
    target))

;; (NUMBER, TENSOR)
(defmethod binary-operator ((function function) (source1 number) (source2 abstract-tensor) &optional target)
  (let ((target (or target (copy-tensor source2))))
    (map-indexes
     (shape source2)
     (lambda (&rest dims)
       (apply #'(setf tref)
              (funcall function
                       source1
                       (apply #'tref source2 dims))
              target dims)))
    target))

(define-backend-function .+ (source1 source2 &optional target)
  "Add tensors elementwise, optionally storing the result in TARGET.
If TARGET is not specified then a new tensor is created with the same element type as the first source tensor")

(define-backend-implementation .+ :lisp
  (lambda (source1 source2 &optional target)
    (binary-operator #'+ source1 source2 target)))

(define-backend-function .- (source1 source2 &optional target)
  "Subtract tensors elementwise, optionally storing the result in TARGET.
If TARGET is not specified then a new tensor is created with the same element type as the first source tensor")

(define-backend-implementation .- :lisp
  (lambda (source1 source2 &optional target)
    (binary-operator #'- source1 source2 target)))

(define-backend-function .* (source1 source2 &optional target)
  "Multiply tensors elementwise, optionally storing the result in TARGET.
If TARGET is not specified then a new tensor is created with the same element type as the first source tensor")

(define-backend-implementation .* :lisp
  (lambda (source1 source2 &optional target)
    (binary-operator #'* source1 source2 target)))

(define-backend-function ./ (source1 source2 &optional target)
  "Add tensors elementwise, optionally storing the result in TARGET.
If TARGET is not specified then a new tensor is created with the same element type as the first source tensor")

(define-backend-implementation ./ :lisp
  (lambda (source1 source2 &optional target)
    ;; This won't do the right thing if / is not closed in the set of
    ;; choice, like integers.
    (binary-operator #'/ source1 source2 target)))

(define-backend-function .^ (source1 source2 &optional target)
  "Exponentiate SOURCE1 by SOURCE2 elementwise, optionally storing the result in TARGET.
If TARGET is not specified then a new tensor is created with the same element type as the first source tensor")

(define-backend-implementation .^ :lisp
  (lambda (source1 source2 &optional target)
    ;; This won't do the right thing if / is not closed in the set of
    ;; choice, like integers.
    (binary-operator #'expt source1 source2 target)))



(defgeneric unary-operator (function source &optional target)
  (:documentation "Perform a unary operator on tensor elementwise, optionally storing the result in TARGET.
If TARGET is not specified then a new tensor is created with the same element type as the source tensor")
  (:method ((function function) (source abstract-tensor) &optional target)
    (let ((target (or target (copy-tensor source))))
      (map-indexes
       (shape source)
       (lambda (&rest dims)
         (apply #'(setf tref)
                (funcall function
                         (apply #'tref source dims))
                target dims)))
      target)))

(define-extensible-function (.exp exp-lisp) (source &optional target)
  (:documentation "Applies exponential function to tensor elementwise, optionally storing the restult in TARGET.
If TARGET is not specified then a new tensor is created with the same element type as the source tensor")
  (:method ((source abstract-tensor) &optional target)
    (unary-operator #'exp source target)))

(define-extensible-function (.log log-lisp) (source &optional target)
  (:documentation "Applies natural logarithm to tensor elementwise, optionally storing the restult in TARGET.
If TARGET is not specified then a new tensor is created with the same element type as the source tensor")
  (:method ((source abstract-tensor) &optional target)
    (unary-operator #'log source target)))

(define-extensible-function (= =-lisp) (source1 source2 &optional epsilon)
  (:documentation "Check the equality of tensors with an optional EPSILON")
  (:method ((source1 abstract-tensor) (source2 abstract-tensor) &optional epsilon)
    (unless (equal (shape source1) (shape source2))
      (return-from =-lisp nil))
    (map-indexes
     (shape source1)
     (if (null epsilon)
         (lambda (&rest pos)
           (unless (equal (apply #'tref source1 pos)
                          (apply #'tref source2 pos))
             (return-from =-lisp nil)))
         (lambda (&rest pos)
           (unless (<= (abs (- (apply #'tref source1 pos)
                               (apply #'tref source2 pos)))
                       epsilon)
             (return-from =-lisp nil)))))
    t))

(defgeneric every (predicate tensor &rest more-tensors)
  (:documentation "Check that every element in TENSOR meets a given condition")
  (:method ((predicate function) (tensor abstract-tensor) &rest more-tensors)
    (map-indexes
     (shape tensor)
     (lambda (&rest dims)
       (unless (apply predicate
                      (apply #'tref tensor dims)
                      (mapcar (apply #'alexandria:rcurry #'magicl:tref dims)
                              more-tensors))
         (return-from every nil))))
    t))

(defgeneric some (predicate tensor &rest more-tensors)
  (:documentation "Check that some elements in TENSOR meet a given condition")
  (:method ((predicate function) (tensor abstract-tensor) &rest more-tensors)
    (map-indexes
     (shape tensor)
     (lambda (&rest dims)
       (when (apply predicate
                    (apply #'tref tensor dims)
                    (mapcar (apply #'alexandria:rcurry #'magicl:tref dims)
                            more-tensors))
         (return-from some t))))
    nil))

(defgeneric notevery (predicate tensor &rest more-tensors)
  (:documentation "Check that not every element in TENSOR meets a given condition")
  (:method ((predicate function) (tensor abstract-tensor) &rest more-tensors)
    (not (apply #'every predicate tensor more-tensors))))

(defgeneric notany (predicate tensor &rest more-tensors)
  (:documentation "Check that not any element in TENSOR meets a given condition")
  (:method ((predicate function) (tensor abstract-tensor) &rest more-tensors)
    (not (apply #'some predicate tensor more-tensors))))

(defgeneric coerce-type (tensor type)
  (:documentation "Coerce element type of TENSOR to TYPE by creating a new tensor")
  (:method ((tensor abstract-tensor) type)
    (let ((target (empty (shape tensor) :layout (layout tensor) :type type)))
      (map-to (lambda (x) (coerce x type))
              tensor
              target)
      target)))
