;;;; util.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(defun row-major-index (pos dims)
  (check-type pos index)
  (check-type dims shape)
  (loop :for i in pos
        :for d in dims
        :for acc = i :then (cl:+ i (* d acc))
        :finally (return acc)))

(defun column-major-index (pos dims)
  (check-type pos index)
  (check-type dims shape)
  (loop :for i in (reverse pos)
        :for d in (reverse dims)
        :for acc = i :then (cl:+ i (* d acc))
        :finally (return acc)))

(defun from-row-major-index (index dims)
  (check-type index fixnum)
  (check-type dims shape)
  (reverse
   (loop :for d :in (reverse dims)
         :with acc = index
         :collect
         (multiple-value-bind (a r)
             (floor acc d)
           (setf acc a)
           r))))

(defun from-column-major-index (index dims)
  (check-type index fixnum)
  (check-type dims shape)
  (loop :for d :in dims
        :with acc = index
        :collect
        (multiple-value-bind (a r)
            (floor acc d)
          (setf acc a)
          r)))

(defun map-indexes (dims f)
  "Call a function `f` for all indexes in shape `dims`, going in row-major order"
  (check-type f function)
  (check-type dims shape)
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

(defun map-column-indexes (dims f)
  (check-type f function)
  (check-type dims shape)
  (map-indexes
   dims
   (lambda (index)
     (funcall f (reverse index)))))

(defun normalize-type (type)
  (upgraded-array-element-type type))
