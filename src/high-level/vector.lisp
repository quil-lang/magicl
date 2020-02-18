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

(defmacro defvector (name type tensor-class)
  "Define a new vector subclass with the specified NAME and element TYPE,
compatible with TENSOR-CLASS, as well as the abstract-tensor methods
required not specified by the generic VECTOR class (MAKE-TENSOR,
ELEMENT-TYPE, CAST, COPY-TENSOR, DEEP-COPY-TENSOR, TREF, SETF TREF)"
  (let ((constructor-sym (intern (format nil "MAKE-~:@(~A~)" name)))
        (copy-sym (intern (format nil "COPY-~:@(~A~)" name)))
        (storage-sym (intern (format nil "~:@(~A~)-STORAGE" name))))
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

       (defmethod make-tensor ((class (eql ',name)) shape &key initial-element layout storage)
         (declare (ignore layout))
         (policy-cond:with-expectations (> speed safety)
             ((type shape shape)
              (assertion (cl:= 1 (length shape))))
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
                                  nil)))))))

       (defmethod cast ((tensor ,name) (class (eql ',name)))
         (declare (ignore class))
         tensor)
       (defmethod cast :before ((tensor ,tensor-class) (class (eql ',name)))
         (declare (ignore class))
         (assert (cl:= 1 (order tensor))
                 ()
                 "Cannot change non-1 dimensional tensor to vector."))
       (defmethod cast ((tensor ,tensor-class) (class (eql ',name)))
         (declare (ignore class))
         (make-tensor ',name (shape tensor)
                      :storage (storage tensor)))
       (defmethod cast ((tensor ,name) (class (eql ',tensor-class)))
         (declare (ignore class))
         (make-tensor ',tensor-class (shape tensor)
                      :storage (storage tensor)))

       ;; TODO: This does not allow for args. Make this allow for args.
       (defmethod copy-tensor ((v ,name) &rest args)
         (declare (ignore args))
         (let ((new-v (,copy-sym v)))
           (setf (,storage-sym new-v)
                 (make-array (vector-size v) :element-type (element-type v)))
           new-v))

       (defmethod deep-copy-tensor ((v ,name) &rest args)
         (declare (ignore args))
         (let ((new-v (,copy-sym v)))
           (setf (,storage-sym new-v)
                 (copy-seq (,storage-sym v)))
           new-v))

       (defmethod tref ((vector ,name) &rest pos)
         (policy-cond:with-expectations (> speed safety)
             ((assertion (valid-index-p pos (list (vector-size vector)))))
           (aref (,storage-sym vector) (first pos))))

       (defmethod (setf tref) (new-value (vector ,name) &rest pos)
         (policy-cond:with-expectations (> speed safety)
             ((assertion (valid-index-p pos (list (vector-size vector)))))
           (setf (aref (,storage-sym vector) (first pos)) new-value))))))


(defun pprint-vector (stream vector)
  "Pretty-print a vector VECTOR to the stream STREAM."
  (flet ((print-real (x)
           (format stream "~6,3f" x))
         (print-complex (z)
           (format stream "~6,3f ~:[+~;-~]~6,3fj"
                   (realpart z)
                   (minusp (imagpart z))
                   (abs (imagpart z))))
         (print-int (x)
           (format stream "~3d" x)))
    (let* ((size (size vector))
           (type (element-type vector))
           (print-entry
             (cond
               ((subtypep type 'complex) #'print-complex)
               ((subtypep type 'integer) #'print-int)
               (t #'print-real))))
      (pprint-logical-block (stream nil)
        (print-unreadable-object (vector stream :type t)
          (format stream "(~D):" size)
          (dotimes (e size)
            (pprint-newline :mandatory stream)
            (funcall print-entry (tref vector e))))))))

(set-pprint-dispatch 'vector 'pprint-vector)

;;; Required abstract-tensor methods

(defmethod order ((vector vector))
  (declare (ignore vector))
  1)

(defmethod shape ((vector vector))
  (list (vector-size vector)))

(defmethod (setf shape) (new-value (vector vector))
  (policy-cond:with-expectations (> speed safety)
      ((type shape new-value)
       (assertion (cl:= 1 (length new-value))))
    (setf (vector-size vector) (first new-value))))

(defgeneric dot (vector1 vector2)
  (:documentation "Compute the dot product of two vectors")
  (:method ((vector1 vector) (vector2 vector))
    (policy-cond:with-expectations (> speed safety)
        ((assertion (cl:= (size vector1) (size vector2))))
      (loop :for i :below (size vector1)
            :sum (* (tref vector1 i) (tref vector2 i))))))

(defgeneric norm (vector &optional p)
  (:documentation "Compute the p-norm of a vector.

If P is not specified then the Euclidean norm is computed.
Special values of P are: :INFINITY :INF :NEGATIVE-INFINITY :NEG-INF")
  (:method ((vector vector) &optional (p 2))
    (cond
      ((or (eql p :infinity)
           (eql p :inf))
       (reduce (lambda (x y) (max (abs x) (abs y))) (storage vector)))
      ((or (eql p :negative-infinity)
           (eql p :neg-inf))
       (reduce (lambda (x y) (min (abs x) (abs y))) (storage vector)))
      (t
       (assert (< 0 p)
               () "P must be a positive number.")
       (expt (reduce (lambda (x y) (+ x (abs (expt y p))))
                     (storage vector)
                     :initial-value 0)
             (/ p))))))
