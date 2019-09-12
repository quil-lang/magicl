;;;; util.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(defun row-major-index (pos dims)
  ;; TODO: check types
  (loop :for i in pos
        :for d in dims
        :for acc = i :then (cl:+ i (* d acc))
        :finally (return acc)))

(defun column-major-index (pos dims)
  ;; TODO: check types
  (loop :for i in (reverse pos)
        :for d in (reverse dims)
        :for acc = i :then (cl:+ i (* d acc))
        :finally (return acc)))

(defun from-row-major-index (index dims)
  ;; TODO: types
  (reverse (loop :for d in dims
                 :for acc = index :then (floor acc d)
                 :collect (rem acc d))))

(defun from-column-major-index (index dims)
  ;; TODO: types
  (loop :for d in dims
        :for acc = index :then (floor acc d)
        :collect (rem acc d)))

(defun map-indexes (dims f)
  "Call a function `f` for all indexes in shape `dims`"
  (declare (type function f))
  (let ((ihead (make-list (list-length dims))))
    (declare (dynamic-extent ihead))
    (labels ((rec (dims itail)
               (declare (type list dims itail))
               (cond
                 ((endp dims) (apply f ihead))
                 (t (dotimes (d (the alexandria:non-negative-fixnum (car dims)))
                      (rplaca itail d)
                      (rec (cdr dims) (cdr itail)))))))
      (rec dims ihead))))

;; TODO: write this properly
;; TODO: implement inside of function using
(defun map-column-indexes (dims f)
  (warn "This function is not implemented correctly. Use at your own risk and tell Cole to implement it correctly")
  (map-indexes
   dims
   (lambda (index)
     (funcall f (reverse index)))))

(defun normalize-type (type)
  (upgraded-array-element-type type))
