;;;; shapes.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

;;; Shapes

;; Predicates

(declaim (inline valid-shape-p))
(defun valid-shape-p (shape)
  (and (typep shape 'list)
       (plusp (length shape))
       (cl:every (lambda (x) (typep x 'alexandria:positive-fixnum)) shape)))

(declaim (inline square-shape-p))
(defun square-shape-p (shape)
  (and (valid-shape-p shape)
       (apply #'cl:= shape)))

(declaim (inline valid-index-p))
(defun valid-index-p (index &optional shape)
  (declare (notinline valid-index-p))
  (if (null shape)
      (and (typep index 'list)
           (plusp (length index))
           (cl:every (lambda (x) (typep x 'alexandria:non-negative-fixnum)) index))
      (and (valid-index-p index)
           (cl:= (length index) (length shape))
           (cl:every #'< index shape))))

(declaim (inline valid-matrix-index-p))
(defun valid-matrix-index-p (index &optional nrows ncols)
  (if (or (null nrows) (null ncols))
      (and (typep index 'list)
           (cl:= 2 (length index))
           (cl:every (lambda (x) (typep x 'alexandria:non-negative-fixnum)) index))
      (and (typep index 'list)
           (cl:= 2 (length index))
           (cl:every (lambda (x) (typep x 'alexandria:non-negative-fixnum)) index)
           (< (first index) nrows)
           (< (second index) ncols))))

;; Types
(deftype shape (&optional order)
  `(satisfies valid-shape-p))

(deftype index ()
  `(satisfies valid-index-p))

;; Assertions
(defmacro assert-square-shape (&rest shapes)
  `(progn
     ,@(loop :for shape in shapes
             :collect `(assert (square-shape-p ,shape)
                               ()
                               "The value of ~a is ~a, which is not a square SHAPE" ,(symbol-name shape) ,shape))))
