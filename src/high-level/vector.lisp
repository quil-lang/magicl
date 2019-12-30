;;;; vector.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(deftype vector-storage (&optional type)
  `(simple-array ,type (*)))

(defstruct (vector (:include abstract-tensor)
                   (:constructor nil)
                   (:copier nil))
  (size 0 :type alexandria:positive-fixnum))

(defmacro defvector (name type &rest compat-classes)
  (let ((constructor-sym (intern (format nil "MAKE-~a" name)))
        (copy-sym (intern (format nil "COPY-~a" name)))
        (storage-sym (intern (format nil "~a-STORAGE" name))))
    `(progn
       (defstruct (,name (:include vector)
                         (:constructor ,constructor-sym
                             (size storage))
                         (:copier ,copy-sym))
         (storage nil :type (vector-storage ,type)))
       #+sbcl (declaim (sb-ext:freeze-type ,name))

       (defmethod storage ((v ,name))
         (,storage-sym v))

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
                    () "Vector shape must be of length 1"))
          nil)
         (let ((size (reduce #'* shape)))
           (funcall #',constructor-sym
                    size
                    (or
                     storage
                     (apply #'make-array
                            size
                            :element-type ',type
                            (if initial-element
                                (list :initial-element (coerce initial-element ',type))
                                nil))))))

       (defmethod tref ((vector ,name) &rest pos)
         (declare (ignore args))
         (aref (,storage-sym vector) (first pos)))

       (defmethod (setf tref) (new-value (vector ,name) &rest pos)
         (declare (ignore args))
         (setf (aref (,storage-sym vector) (first pos)) new-value)))))


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

(defmethod (setf shape) (new-value (vector vector))
  (policy-cond:policy-if
   (< speed safety)
   (progn
     (check-type new-value shape)
     (assert (cl:= 1 (length new-value))
             () "Vector shape must be of length 1"))
   nil)
  (setf (vector-size vector) (first new-value)))

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
