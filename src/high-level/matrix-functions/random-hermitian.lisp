;;;; random-hermitian.lisp
;;;;
;;;; Author: Erik Davis

(in-package #:magicl)

(defun random-hermitian (n &key (type *default-tensor-type*))
  (cond ((member type '(single-float double-float))
         (let ((a (rand (list n n) :type type)))
           (.+ a (transpose a))))
        ((member type '((complex single-float) (complex double-float)) :test #'equal)
         (let ((a (rand (list n n) :type type))
               (b (rand (list n n) :type type)))
           (scale! b #C(0d0 1d0))
           (let ((c (.+ a b)))
             (.+ c (conjugate-transpose c)))))
        (t (error "Unexpected tensor type."))))
