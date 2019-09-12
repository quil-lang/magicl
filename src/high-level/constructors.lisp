;;;; constructors.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(defconstant +default-tensor-type+ 'double-float)

(defgeneric empty (shape &key type order)
  (:documentation "Create an empty tensor")
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

(defgeneric rand (min max shape &key type distribution)
  (:documentation "Create tensor with elements random in the range [0,limit]")
  (:method (min max shape &key type (distribution :uniform))
    (error "Rand not implemented yet.")
    #+ignore
    (progn
      (check-type shape shape)
      (check-type distribution (member :uniform :normal))
      (let ((rand-function
              (ecase distribution
                (:uniform #'random)))))
      (specialize-tensor (make-tensor shape type)))))

(defgeneric deye (d shape &key type order)
  (:documentation "Create idenetity matrix scaled by factor d.
dim is the side length of the square matrix")
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
           (tensor (make-tensor shape tensor-class element-type :order order))
           (f (lambda (&rest pos)
                (if (cl:every #'= pos (rest pos))
                    (coerce d element-type)
                    (coerce 0 type)))))
      (specialize-tensor (into! f tensor))))) ;; TODO: WHOAH THERE BUCKAROO THAT THERE IS O(N^2) Oh(you messed up)

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


;; TODO: are these args backwards or are the others?
(defgeneric from-array (shape array)
  (:documentation "Create a tensor from an array"))

(defgeneric from-list (shape list &key type order)
  (:documentation "Create a tensor of the specified shape from a list, putting elements in row-major order.
NOTE: When type is not specified, the type is inferred from the first element of the list")
  (:method (shape list &key type (order :column-major))
    (check-type shape shape)
    (let ((shape-size (reduce #'* shape))
          (list-size (length list)))
      (assert (= list-size shape-size)
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
          (coerce (nth (row-major-index pos shape) list) element-type))
        tensor)))))

