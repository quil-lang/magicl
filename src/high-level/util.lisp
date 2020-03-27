;;;; util.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(defvar *float-comparison-threshold* single-float-epsilon)
(defvar *double-comparison-threshold* double-float-epsilon)

(declaim (inline matrix-row-major-index))
(defun matrix-row-major-index (row col numrows numcols)
  (declare (optimize (speed 3) (safety 0))
           (ignore numrows)
           (type fixnum row col numcols)
           (values fixnum))
  (+ col (the fixnum (* row numcols))))

(declaim (inline matrix-column-major-index))
(defun matrix-column-major-index (row col numrows numcols)
  (declare (optimize (speed 3) (safety 0))
           (ignore numcols)
           (type fixnum row col numrows)
           (values fixnum))
  (+ row (the fixnum (* col numrows))))

(defun row-major-index (pos dims)
  (declare (type index pos)
           (type shape dims)
           (optimize (speed 3) (safety 0)))
  (if (and (cdr dims) (not (cddr dims))) ;; Quick test for length 2
      (matrix-row-major-index (first pos) (second pos) (first dims) (second dims))
      (loop :for i :of-type fixnum in pos
            :for d :of-type fixnum in dims
            :for acc :of-type fixnum := i
              :then (+ i (the fixnum (* d acc)))
            :finally (return acc))))

(defun column-major-index (pos dims)
  (declare (type index pos)
           (type shape dims))
  (if (and (cdr dims) (not (cddr dims))) ;; Quick test for length 2
      (matrix-column-major-index (first pos) (second pos) (first dims) (second dims))
      (loop :for i :of-type fixnum in (reverse pos)
            :for d :of-type fixnum in (reverse dims)
            :for acc :of-type fixnum := i
              :then (+ i (the fixnum (* d acc)))
            :finally (return acc))))

(defun from-row-major-index (index dims)
  (check-type index fixnum)
  (check-type dims shape)
  (reverse
   (loop :for d :in (reverse dims)
         :with acc := index
         :collect
         (multiple-value-bind (a r)
             (floor acc d)
           (setf acc a)
           r))))

(defun from-column-major-index (index dims)
  (check-type index fixnum)
  (check-type dims shape)
  (loop :for d :in dims
        :with acc := index
        :collect
        (multiple-value-bind (a r)
            (floor acc d)
          (setf acc a)
          r)))

(defun map-indexes (dims f)
  "Call a function F for all indexes in shape DIMS, going in row-major order"
  (declare (type function f)
           (type shape dims))
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
  "Call a function F for all indexes in shape DIMS, going in column-major order"
  (check-type f function)
  (check-type dims shape)
  (map-indexes
   dims
   (lambda (index)
     (funcall f (reverse index)))))

(defun normalize-type (type)
  (upgraded-array-element-type type))
