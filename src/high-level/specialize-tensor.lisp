;;;; specialize-tensor.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

;; XXX: This is a little overkill for the small amount of compatible
;;      tensors we are defining. Possible to change this to just
;;      defining SPECIALIZE-TENSOR and GENERALIZE-TENSOR for a class
(defmacro defcompatible (function &rest tensors)
  "Generate COMPATIBLE-TENSORS methods specifying compatible abstract-tensor classes for a given tensor.

FUNCTION takes the tensor as an argument and returns a list of
compatible classes.
TENSORS specifies all the classes this function will be defined for"
  `(progn
     ,@(loop :for tensor :in tensors
             :collect `(defmethod compatible-tensors ((tensor ,tensor)) (funcall ,function tensor)))))

(defgeneric compatible-tensors (tensor)
  (:documentation "Get a list of all compatible classes for a tensor with most-specific first"))

(defgeneric cast (tensor class)
  (:documentation "Cast a tensor to CLASS"))

(defmethod specialize-tensor ((tensor abstract-tensor))
  (cast tensor (first (compatible-tensors tensor))))

(defmethod generalize-tensor ((tensor abstract-tensor))
  (cast tensor (car (last (compatible-tensors tensor)))))

