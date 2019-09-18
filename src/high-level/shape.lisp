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
       (cl:every #'cl:= shape (rest shape))))

;; Types
(deftype shape (&optional rank) ;; TODO: rank, maybe
  `(satisfies valid-shape-p))

;; Assertions
(defmacro assert-square-shape (&rest shapes)
  `(progn
     ,@(loop :for shape in shapes
             :collect `(assert (square-shape-p ,shape)
                               ()
                               ,"The value of ~a is ~a, which is not a square SHAPE" ,(symbol-name shape) ,shape))))
