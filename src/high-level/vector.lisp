;;;; vector.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(deftype vector-storage (&optional type)
  `(simple-array ,type (*)))

(defstruct (vector (:include abstract-tensor)
                   (:constructor nil)
                   (:copier nil))
  (size 0 :type alexandria:positive-fixnum :read-only t))

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
           (print-entry
             (cond
               ((subtypep type 'complex) #'print-complex)
               (t #'print-real))))
      (pprint-logical-block (stream nil)
        (print-unreadable-object (vector stream :type t)
          (format stream "(~D):" size)
          (dotimes (e size)
            (pprint-newline :mandatory stream)
            (funcall print-entry (tref vector e))))))))

(set-pprint-dispatch 'vector 'pprint-vector)

;;; Required abstract-tensor methods

(defmethod rank ((vector vector))
  (declare (ignore vector))
  1)

(defmethod shape ((vector vector))
  (list (vector-size vector)))

(defmethod tref ((vector vector) &rest pos)
  (policy-cond:with-expectations
      (> speed safety)
      ((assertion (valid-index-p pos (shape vector))))
    (aref (storage vector) (first pos))))

(defmethod (setf tref) (new-value (vector vector) &rest pos)
  (policy-cond:with-expectations
      (> speed safety)
      ((assertion (valid-index-p pos (shape vector))))    
    (setf (aref (storage vector) (first pos)) new-value)))

(defmacro defvector (name type &rest compat-classes)
  `(progn
     (defstruct (,name (:include vector)
                       (:constructor ,(intern (format nil "MAKE-~a" name))
                           (size storage)))
       (storage nil :type (vector-storage ,type)))
     #+sbcl (declaim (sb-ext:freeze-type ,name))

     (defmethod storage ((v ,name))
       (,(intern (format nil "~a-STORAGE" name)) v))

     (defmethod element-type ((v ,name))
       (declare (ignore v))
       ',type)

     (defmethod make-tensor ((class (eql ',name)) shape &key initial-element order storage)
       (declare (ignore order))
       (policy-cond:policy-if
        (< speed safety)
        (progn
          (check-type shape shape)
          (assert (cl:= 1 (length shape))
                  () "Vector shape must be of length 2"))
        nil)
       (let ((size (reduce #'* shape)))
         (funcall #',(intern (format nil "MAKE-~a" name))
                  size
                  (or
                   storage
                   (apply #'make-array
                          size
                          :element-type ',type
                          (if initial-element
                              (list :initial-element (coerce initial-element ',type))
                              nil))))))))

(defgeneric dot (vector1 vector2)
  (:documentation "Compute the dot product of two vectors")
  (:method ((vector1 vector) (vector2 vector))
    (policy-cond:policy-if
     (< speed safety)
     (assert (cl:= (size vector1) (size vector2))
             () "Vectors must have the same size. The first vector is size ~a and the second vector is size ~a."
             (size vector1) (size vector2))
     nil)
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
