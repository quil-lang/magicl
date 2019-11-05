;;;; shapes.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

;;; Shapes

;; Predicates

(defun valid-shape-p (shape)
  (and (typep shape 'list)
       (plusp (length shape))
       (cl:every (lambda (x) (typep x 'alexandria:positive-fixnum)) shape)))

(defun square-shape-p (shape)
  (and (valid-shape-p shape)
       (apply #'cl:= shape)))

(defun valid-index-p (index shape)
  (and (valid-shape-p shape)
       (valid-shape-p index)
       (cl:= (length index) (length shape))
       (cl:every #'< index shape)))

;; Types
(deftype shape (&optional rank)
  `(satisfies valid-shape-p))

(deftype index ()
  `(satisfies valid-index-p))

;; Assertions
(defmacro assert-square-shape (&rest shapes)
  `(progn
     ,@(loop :for shape in shapes
             :collect `(assert (square-shape-p ,shape)
                               ()
                               ,"The value of ~a is ~a, which is not a square SHAPE" ,(symbol-name shape) ,shape))))
