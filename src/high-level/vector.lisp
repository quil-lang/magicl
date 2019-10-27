;;;; vector.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(deftype vector-storage (&optional type)
  `(simple-array ,type (*)))

(defclass vector (abstract-tensor)
  (;; abstract-tensor slots
   (size
    :initarg :size
    :initform 0
    :reader size
    :type (alexandria:positive-fixnum)
    :documentation "Total number of elements in the vector")
   (element-type
    :initarg :element-type
    :initform (error "element-type must be specified when creating a vector instance") ; TODO: much better error messages
    :reader element-type
    :type type
    :documentation "The type of the elements in the vector")
   ;; vector-specific slots
   (storage
    :initarg :storage
    :initform (error "storage must be specified when creating a vector instance")
    :reader storage
    :documentation "Storage of the vector, typically in a vector in column major order"))
  (:metaclass abstract-class:abstract-class))

(defun pprint-vector (stream vector)
  "Pretty-print a vector VECTOR to the stream STREAM."
  (flet ((print-real (x)
           (format stream "~6,3f" x))
         (print-complex (z)
           (format stream "~6,3f ~:[+~;-~]~6,3fj"
                   (realpart z)
                   (minusp (imagpart z))
                   (abs (imagpart z)))))
    (let* ((size (size vector))
           (type (element-type vector))
           (print-entry #'print-real)) ;; TODO: Check for complex type
      (pprint-logical-block (stream nil)
        (print-unreadable-object (vector stream :type t)
          (format stream "~D:" size)
          (dotimes (e size)
            (pprint-newline :mandatory stream)
            (funcall print-entry (tref vector e))))))))

(set-pprint-dispatch 'vector 'pprint-vector)

;;; Required abstract-tensor methods

(defmethod rank ((vector vector))
  (declare (ignore vector))
  1)

(defmethod shape ((vector vector))
  (list (size vector)))

(defmethod tref ((vector vector) &rest pos)
  (assert (cl:= (rank vector) (list-length pos))
          () "Invalid index ~a. Must be rank ~a" pos (rank vector))
  (assert (cl:every #'< pos (shape vector))
          () "Index ~a out of range" pos)
  (aref (storage vector) (first pos)))

(defmethod (setf tref) (new-value (vector vector) &rest pos)
  (assert (cl:= (rank vector) (list-length pos))
          () "Invalid index ~a. Must be rank ~a" pos (rank vector))
  (assert (cl:every #'< pos (shape vector))
          () "Index ~a out of range" pos)
  (setf (aref (storage vector) (first pos)) new-value))

(defmacro defvector (name type tensor-name matrix-name)
  `(progn
     (defclass ,name (vector)
       ((storage :type (vector-storage ,type)))
       (:documentation ,(format nil "Vector with element type of ~a" type)))
     (defmethod update-instance-for-different-class :before
         ((old ,tensor-name)
          (new ,name)
          &key)
       (assert (cl:= 1 (rank old))))
     (defmethod update-instance-for-different-class :before
         ((old ,matrix-name)
          (new ,name)
          &key)
       (assert (cl:= 1 (rank old))))
     (defmethod update-instance-for-different-class :before
         ((old ,name)
          (new ,tensor-name)
          &key)
       (with-slots (shape rank) new
         (setf shape (shape old)
               rank 1)))
     (defmethod update-instance-for-different-class :before
         ((old ,name)
          (new ,matrix-name)
          &key)
       (with-slots (shape rank) new
         (setf shape (shape old)
               rank 1)))))

(defgeneric dot (vector1 vector2)
  (:documentation "Compute the dot product of two vectors")
  (:method ((vector1 vector) (vector2 vector))
    (assert (cl:= (size vector1) (size vector2))
            () "Vectors must have the same size. The first vector is size ~a and the second vector is size ~a."
            (size vector1) (size vector2))
    (loop :for i :below (size vector1)
          :sum (* (tref vector1 i) (tref vector2 i)))))

(defgeneric norm (vector &optional p)
  (:documentation "Compute the norm of a vector")
  (:method ((vector vector) &optional (p 2))
    (expt (reduce #'cl:+
                  (cl:map (list 'cl:vector (element-type vector))
                          (lambda (x) (expt x p))
                          (storage vector)))
          (/ 1 p))))
