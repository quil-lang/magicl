;;;; specialize-tensor.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(defmacro defcompatible (function &rest tensors)
  `(progn
     ,@(loop :for tensor :in tensors
             :collect `(defmethod compatible-tensors ((tensor ,tensor)) (funcall ,function tensor)))))

(defgeneric compatible-tensors (tensor)
  (:documentation "Get a list of all compatible classes for a tensor with most-specific first"))

(defmethod specialize-tensor ((tensor abstract-tensor))
  (identity tensor)
  #+ignore
  (change-class tensor (first (compatible-tensors tensor)))
  )

(defmethod generalize-tensor ((tensor abstract-tensor))
  (change-class tensor (car (last (compatible-tensors tensor)))))
