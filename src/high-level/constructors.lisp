;;;; constructors.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(defconstant +default-tensor-type+ 'double-float)

(defgeneric make-tensor (class shape &key initial-element order storage)
  (:documentation "Make a dense tensor with elements of the specified type"))

(defun empty (shape &key (type +default-tensor-type+) order)
  "Create an empty tensor

If TYPE is not specified then +DEFAULT-TENSOR-TYPE+ is used.
ORDER specifies the internal storage represenation ordering of the returned tensor.
The tensor specialized on the specified SHAPE and TYPE."
  (policy-cond:with-expectations (> speed safety)
      ((type shape shape))
    (let ((tensor-type (infer-tensor-type type shape nil)))
      (make-tensor tensor-type shape :order order))))

(defun const (const shape &key type order)
  "Create a tensor with the specified SHAPE with each element being set to CONST

If TYPE is not specified then it is inferred from the type of CONST.
ORDER specifies the internal storage represenation ordering of the returned tensor.
The tensor is specialized on SHAPE and TYPE."
  (policy-cond:with-expectations (> speed safety)
      ((type shape shape))
    (let ((tensor-class (infer-tensor-type type shape const)))
      (make-tensor tensor-class shape :order order :initial-element const))))

(defun rand (shape &key (type +default-tensor-type+) order distribution)
  "Create tensor with random elements from DISTRIBUTION

DISTRIBUTION is a function with no arguments which returns a value for the element.
If DISTRIBUTION is not specified then CL:RANDOM is used. In the case that TYPE is complex, CL:RANDOM is called for each component.
If TYPE is not specified then +DEFAULT-TENSOR-TYPE+ is used.
ORDER specifies the internal storage represenation ordering of the returned tensor.
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
      (into! f (make-tensor tensor-class shape :order order)))))

(defun deye (d shape &key type order)
  "Create a 2-dimensional square tensor with D along the diagonal

SHAPE must have length 2 and be square.

If TYPE is not specified then it is inferred from the type of D.
ORDER specifies the internal storage represenation ordering of the returned tensor.
The tensor is specialized on SHAPE and TYPE."
  (policy-cond:with-expectations (> speed safety)
      ((type shape shape)
       (assertion (square-shape-p shape)))
    (let* ((tensor-class (infer-tensor-type type shape d))
           (tensor (make-tensor tensor-class shape :order order)))
      (loop :for i :below (first shape)
            :do (setf (tref tensor i i) d))
      tensor)))

(defun arange (range &key (type +default-tensor-type+) order)
  "Create a 1-dimensional tensor of elements from 0 up to but not including the RANGE

If TYPE is not specified then it is inferred from the type of RANGE.
ORDER specifies the internal storage represenation ordering of the returned tensor.
The tensor is specialized on TYPE with shape (floor(RANGE))."
  (let ((tensor-class (infer-tensor-type type (list (floor range)) range)))
    (let ((tensor (make-tensor tensor-class (list (floor range)) :order order))
          (f (lambda (index)
               (coerce index type))))
      (into! f tensor))))

;; NOTE: This does not accout for changing array order. If the input
;;       is in row-major but the constructor specifies column-major
;;       then the tensor will not return the correct values. Not a
;;       huge deal but something that be improved (similarly to how
;;       FROM-LIST does it.)
(defun from-array (array shape &key type (order :row-major))
  "Create a tensor from ARRAY, calling ADJUST-ARRAY on ARRAY to flatten to a 1-dimensional array of length equal to the product of the elements in SHAPE

If TYPE is not specified then it is inferred from the element type of ARRAY.
ORDER specifies the internal storage represenation ordering of the returned tensor.
The tensor is specialized on SHAPE and TYPE."
  (policy-cond:with-expectations (> speed safety)
      ((type shape shape))
    (let* (;; TODO: This gets the type of an array from the array, not
           ;; by inspecting the elements. This can cause issues when
           ;; the element-type of the array is T
           (element-type
             (if (null type)
                 (array-element-type array)
                 type))
           (tensor-class (infer-tensor-type element-type shape nil)))
      (adjust-array array (list (reduce #'* shape)) :element-type element-type)
      (make-tensor tensor-class shape
                   :storage array
                   :order order))))

(defun from-list (list shape &key type order (input-order :row-major))
  "Create a tensor with the elements of LIST, placing in order INPUT-ORDER

If INPUT-ORDER is not specified then row-major is assumed.

If TYPE is not specified then it is inferred from the type of the first element of LIST.
ORDER specifies the internal storage represenation ordering of the returned tensor.
The tensor is specialized on SHAPE and TYPE."
  (policy-cond:with-expectations (> speed safety)
      ((type shape shape)
       (assertion (cl:= (length list) (reduce #'* shape))))
    (let ((tensor-class (infer-tensor-type type shape (first list))))
      (let ((tensor (make-tensor tensor-class shape :order order)))
        (into!
         (lambda (&rest pos)
           (nth
            (if (eql input-order :row-major)
                (row-major-index pos shape)
                (column-major-index pos shape))
            list))
         tensor)))))

(defun from-diag (list shape &key type order)
  "Create a tensor of the specified shape from a list, placing along the diagonal

If TYPE is not specified then it is inferred from the type of the first element of LIST.
ORDER specifies the internal storage represenation ordering of the returned tensor.
The tensor is specialized on SHAPE and TYPE."
  (policy-cond:with-expectations (> speed safety)
      ((type shape shape)
       (assertion (cl:= 2 (length shape)))
       (assertion (square-shape-p shape))
       (assertion (cl:= (length list) (first shape))))
    (let ((tensor-class (infer-tensor-type type shape (first list))))
      (let ((tensor (make-tensor tensor-class shape :order order)))
        (loop :for i :below (first shape)
              :do (setf (tref tensor i i) (pop list)))
        tensor))))
