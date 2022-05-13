;;;; specialized-vector.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:magicl)

;;;; Here we have some functions that specialize on many types

(defmacro define-common-real-vector-methods (vector-class elt-type)
  `(progn
     (defmethod dot-lisp ((vec1 ,vector-class) (vec2 ,vector-class))
       (let ((size1 (size vec1))
             (size2 (size vec2)))
         (assert (cl:= size1 size2))
         (let ((s1 (storage vec1))
               (s2 (storage vec2)))
           (declare (optimize speed)
                    (type (simple-array ,elt-type (*)) s1 s2))
           (loop :with s :of-type ,elt-type := ,(coerce 0 elt-type)
                 :for i :of-type alexandria:array-index :below size1
                 :do (incf s (* (aref s1 i) (aref s2 i)))
                 :finally (return s)))))))

(defmacro define-common-complex-vector-methods (vector-class elt-type)
  `(progn
     (defmethod dot-lisp ((vec1 ,vector-class) (vec2 ,vector-class))
       (let ((size1 (size vec1))
             (size2 (size vec2)))
         (assert (cl:= size1 size2))
         (let ((s1 (storage vec1))
               (s2 (storage vec2)))
           (declare (optimize speed)
                    (type (simple-array ,elt-type (*)) s1 s2))
           (loop :with s :of-type ,elt-type := ,(coerce 0 elt-type)
                 :for i :of-type alexandria:array-index :below size1
                 :do (incf s (* (aref s1 i) (conjugate (aref s2 i))))
                 :finally (return s)))))))

(define-common-real-vector-methods vector/single-float single-float)
(define-common-real-vector-methods vector/double-float double-float)
(define-common-complex-vector-methods vector/complex-single-float (complex single-float))
(define-common-complex-vector-methods vector/complex-double-float (complex double-float))
