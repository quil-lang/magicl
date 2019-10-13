;;;; constructors.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(defconstant +default-tensor-type+ 'double-float)

(defgeneric empty (shape &key type order)
  (:documentation "Create an empty tensor of specified shape")
  (:method (shape &key (type +default-tensor-type+) order)
    (check-type shape shape)
    (let ((tensor-type (compatible-tensor-constructors type)))
      (specialize-tensor (make-tensor shape tensor-type type :order order)))))

(defgeneric const (const shape &key type order)
  (:documentation "Create tensor with all elements equal to a constant value")
  (:method (const shape &key type order)
    (check-type shape shape)
    (let ((tensor-class
            (if (null type)
                (compatible-tensor-constructors-from-value const)
                (compatible-tensor-constructors type)))
          (element-type
            (if (null type)
                (upgraded-array-element-type (type-of const))
                type)))
      (specialize-tensor (make-tensor shape tensor-class element-type :order order :initial-element (coerce const element-type))))))

(defgeneric rand (shape &key type distribution)
  (:documentation "Create tensor with elements random in the range [0,limit]")
  (:method (shape &key (type +default-tensor-type+) distribution)
    (progn
      (check-type shape shape)
      (let* ((tensor-class (compatible-tensor-constructors type))
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
        (specialize-tensor (into! f (make-tensor shape tensor-class type)))))))

(defgeneric deye (d shape &key type order)
  (:documentation "Create identity matrix scaled by factor D")
  (:method (d shape &key type order)
    (check-type shape shape)
    (assert-square-shape shape)
    (let* ((tensor-class
             (if (null type)
                 (compatible-tensor-constructors-from-value d)
                 (compatible-tensor-constructors type)))
           (element-type
             (if (null type)
                 (upgraded-array-element-type (type-of d))
                 type))
           (tensor (make-tensor shape tensor-class element-type :order order)))
      (loop :for i :below (first shape)
            :do (setf (tref tensor i i) (coerce d element-type)))
      (specialize-tensor tensor))))

(defgeneric arange (range &key type order)
  (:documentation "Create a 1d tensor of integers (not the type) from 0 up to but not including the specified range")
  (:method (range &key type order)
    (let* ((tensor-class
             (if (null type)
                 (compatible-tensor-constructors-from-value range)
                 (compatible-tensor-constructors type)))
           (element-type
             (if (null type)
                 (upgraded-array-element-type (type-of range))
                 type))
           (tensor (make-tensor (list (floor range)) tensor-class element-type :order order))
           (f (lambda (index)
                (coerce index element-type))))
      (specialize-tensor (into! f tensor)))))

(defgeneric from-array (array shape &key type order)
  (:documentation "Create a tensor from an array")
  (:method (array shape &key type (order :row-major))
    (let* ((element-type
             (if (null type)
                 (array-element-type array)
                 type))
           (tensor-class
             (if (null type)
                 (compatible-tensor-constructors element-type)
                 (compatible-tensor-constructors type))))
      (adjust-array array (list (reduce #'* shape)) :element-type element-type)
      (specialize-tensor
       (make-tensor shape tensor-class element-type
                    :storage array
                    :order order)))))

(defgeneric from-list (list shape &key type order input-order)
  (:documentation "Create a tensor of the specified shape from a list, putting elements in row-major order.
NOTE: When type is not specified, the type is inferred from the first element of the list")
  (:method (list shape &key type (order :column-major) (input-order :row-major))
    (check-type shape shape)
    (let ((shape-size (reduce #'* shape))
          (list-size (length list)))
      (assert (cl:= list-size shape-size)
              () "Incompatible shape. Must have the same total number of elements. The list has ~a elements and the new shape has ~a elements" list-size shape-size))
    (let* ((tensor-class
             (if (null type)
                 (compatible-tensor-constructors-from-value (first list))
                 (compatible-tensor-constructors type)))
           (element-type
             (if (null type)
                 (upgraded-array-element-type (type-of (first list)))
                 type))
           (tensor (make-tensor shape tensor-class element-type :order order)))
      (specialize-tensor
       (into!
        (lambda (&rest pos)
          (coerce (nth
                   (if (eql input-order :row-major)
                       (row-major-index pos shape)
                       (column-major-index pos shape))
                   list)
                  element-type))
        tensor)))))

(defgeneric from-diag (list shape &key type order)
  (:documentation "Create a tensor of the specified shape from a list, placing along the diagonal
NOTE: When type is not specified, the type is inferred from the first element of the list")
  (:method (list shape &key type (order :column-major))
    (check-type shape shape)
    (assert (cl:= 2 (length shape))
            () "Shape must be of rank 2.")
    (assert-square-shape shape)
    (let ((shape-size (reduce #'* shape))
          (list-size (length list)))
      (assert (cl:= (* list-size list-size) shape-size)
              () "Incompatible shape. Must have the same total number of elements. The list has ~a diagonal elements and the new shape has ~a elements" list-size shape-size))
    (let* ((tensor-class
             (if (null type)
                 (compatible-tensor-constructors-from-value (first list))
                 (compatible-tensor-constructors type)))
           (element-type
             (if (null type)
                 (upgraded-array-element-type (type-of (first list)))
                 type))
           (tensor (make-tensor shape tensor-class element-type :order order)))
      (loop :for i :below (first shape)
            :do (setf (tref tensor i i) (coerce (pop list) element-type)))
      (specialize-tensor tensor))))
