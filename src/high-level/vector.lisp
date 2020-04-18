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

(defmethod size ((m vector))
  (vector-size m))

(defmethod layout ((v vector))
  :row-major)

(defun defvectorsubtype (parent-name name type tensor-class)
  (let ((constructor-sym (intern (format nil "MAKE-~:@(~A~)" name)))
        (copy-sym (intern (format nil "COPY-~:@(~A~)" name)))
        (storage-sym (intern (format nil "~:@(~A~)-STORAGE" name))))
    `((defstruct (,name (:include ,parent-name)
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

(defmacro defvector (name type tensor-class)
  "Define a new vector subclass with the specified NAME and element TYPE,
compatible with TENSOR-CLASS, as well as the abstract-tensor methods
required not specified by the generic VECTOR class (MAKE-TENSOR,
ELEMENT-TYPE, CAST, COPY-TENSOR, DEEP-COPY-TENSOR, TREF, SETF TREF)"
  (let* ((row-name (intern (format nil "ROW-~:@(~A~)" name)))
         (row-constructor (intern (format nil "MAKE-~:@(~A~)" row-name)))
         (conj-row-name (intern (format nil "CONJUGATE-TRANSPOSE-ROW-~:@(~A~)" name)))
         (conj-row-constructor (intern (format nil "MAKE-~:@(~A~)" conj-row-name)))
         (conj-row-storage-sym (intern (format nil "~:@(~A~)-STORAGE" conj-row-name)))
         (col-name (intern (format nil "COLUMN-~:@(~A~)" name)))
         (col-constructor (intern (format nil "MAKE-~:@(~A~)" col-name))))
  `(progn
       (defstruct (,name (:include vector)
                                (:constructor nil)
                                (:copier nil)))
            
     ,@(defvectorsubtype name row-name type tensor-class)
     ,@(defvectorsubtype name col-name type tensor-class)

     ;; Fast transpose and/or conjugate-transpose (with lazy accessors for the sake of correctness)
     ;;
     ;; NOTE: These are fast because they don't copy storage, but for the same reason they are
     ;; potentially dangerous.
     ,@(if (subtypep type 'complex)
        `((defstruct (,conj-row-name (:include ,name)
                                     (:constructor ,conj-row-constructor
                                                   (size storage)))
            (storage nil :type (vector-storage ,type)))
          (defmethod conjugate-transpose ((matrix ,col-name))
            (,conj-row-constructor (size matrix) (storage matrix)))
          (defmethod conjugate-transpose ((matrix ,conj-row-name))
            (,col-constructor (size matrix) (storage matrix)))
          
          (defmethod tref ((vector ,conj-row-name) &rest pos)
            (policy-cond:with-expectations (> speed safety)
                ((assertion (valid-index-p pos (list (vector-size vector)))))
              (conjugate (aref (,conj-row-storage-sym vector) (first pos)))))

          (defmethod (setf tref) (new-value (vector ,conj-row-name) &rest pos)
            (policy-cond:with-expectations (> speed safety)
                ((assertion (valid-index-p pos (list (vector-size vector)))))
              (setf (aref (,conj-row-storage-sym vector) (first pos))
                    (conjugate new-value)))))
        
         `((defmethod conjugate-transpose ((matrix ,col-name))
             (transpose matrix))
           (defmethod conjugate-transpose ((matrix ,row-name))
             (transpose matrix))
           (defmethod transpose ((matrix ,row-name))
             (,col-constructor (size matrix) (storage matrix)))
           (defmethod transpose ((matrix ,col-name))
             (,row-constructor (size matrix) (storage matrix)))))

     #+sbcl (declaim (sb-ext:freeze-type ,name)))))

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

(defgeneric outer (vector1 vector2)
  (:documentation "Compute the outer product of two vectors")
  (:method ((vector1 vector) (vector2 vector))
    (loop :with ret := (empty (list (size vector1) (size vector2))
                              :type (element-type vector1))
          :for i :below (size vector1)
          :do (loop :for j :below (size vector2)
                    :do (setf (tref ret i j)
                              (* (tref vector1 i) (tref vector2 j))))
          :finally (return ret))))

(deftype p-norm-type ()
  `(or (member :inf :infinity :positive-infinity)
       (integer 1)))

(defun norm (vector &optional (p 2))
  "Compute the p-norm of a vector.

If P is not specified then the Euclidean norm is computed.
Special values of P are: :INFINITY :INF :POSITIVE-INFINITY"
  (declare (type vector vector)
           (type p-norm-type p))
  (case p
    (1
     (reduce #'+ (storage vector) :key #'abs))
    ((:infinity :inf :positive-infinity)
     (abs (reduce (lambda (x y) (max (abs x) (abs y))) (storage vector))))
    (t
     (expt (reduce (lambda (x y) (+ x (abs (expt y p))))
                   (storage vector)
                   :initial-value 0)
           (/ p)))))
