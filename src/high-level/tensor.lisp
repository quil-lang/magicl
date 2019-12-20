;;;; tensor.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(deftype tensor-storage (&optional type)
  `(simple-array ,type (*)))

(defstruct (tensor (:include abstract-tensor)
                   (:constructor nil)
                   (:copier nil))
  (rank 0 :type alexandria:non-negative-fixnum :read-only t)
  (shape '(0) :type list :read-only t)
  (size 0 :type alexandria:positive-fixnum :read-only t)
  (order :column-major :type (member :row-major :column-major)))

(defmethod rank ((a tensor))
  (tensor-rank a))

(defmethod shape ((a tensor))
  (tensor-shape a))

;;; Specfic tensor classes
(defmacro deftensor (name type)
  `(progn
     (defstruct (,name (:include tensor)
                       (:constructor ,(intern (format nil "MAKE-~a" name))
                           (rank shape size order storage)))
       (storage nil :type (tensor-storage ,type)))
     #+sbcl (declaim (sb-ext:freeze-type ,name))
     
     (defmethod storage ((m ,name))
       (,(intern (format nil "~a-STORAGE" name)) m))

     (defmethod element-type ((m ,name))
       (declare (ignore m))
       ',type)
     
     (defmethod make-tensor ((class (eql ',name)) shape &key initial-element order storage)
       (policy-cond:policy-if
        (< speed safety)
        (check-type shape shape)
        nil)
       (let ((size (reduce #'* shape)))
         (funcall #',(intern (format nil "MAKE-~a" name))
                  (length shape)
                  shape
                  size
                  (or order :column-major)
                  (or
                   storage
                   (apply #'make-array
                          size
                          :element-type ',type
                          (if initial-element
                              (list :initial-element (coerce initial-element ',type))
                              nil))))))

     ;; TODO: This does not allow for args. Make this allow for args.
     (defmethod copy-tensor ((m ,name) &rest args)
       (,(intern (format nil "COPY-~a" name)) m))

     (defmethod deep-copy-tensor ((m ,name) &rest args)
       (let ((new-m (,(intern (format nil "COPY-~a" name)) m)))
         (setf (,(intern (format nil "~a-STORAGE" name)) new-m)
               (alexandria:copy-array (,(intern (format nil "~a-STORAGE" name)) m)))
         new-m))

     (defmethod tref ((tensor ,name) &rest pos)
       (policy-cond:with-expectations
           (> speed safety)
           ((assertion (valid-index-p pos (shape tensor))))
         (let ((index (case (tensor-order tensor)
                        (:row-major (row-major-index pos (tensor-shape tensor)))
                        (:column-major (column-major-index pos (tensor-shape tensor))))))
           (aref (,(intern (format nil "~a-STORAGE" name)) tensor) index))))

     (defmethod (setf tref) (new-value (tensor ,name) &rest pos)
       (policy-cond:with-expectations
           (> speed safety)
           ((assertion (valid-index-p pos (shape tensor))))
         (let ((index (case (tensor-order tensor)
                        (:row-major (row-major-index pos (tensor-shape tensor)))
                        (:column-major (column-major-index pos (tensor-shape tensor))))))
           (setf (aref (,(intern (format nil "~a-STORAGE" name)) tensor) index)
                 new-value))))))

(defun pprint-tensor (stream tensor &optional colon-p at-sign-p)
  "Pretty-print a matrix MATRIX to the stream STREAM."
  (declare (ignore colon-p)
           (ignore at-sign-p))
  (pprint-logical-block (stream nil)
    (print-unreadable-object (tensor stream :type t)
      (format stream "(~{~D~^x~})" (shape tensor)))))

(set-pprint-dispatch 'tensor 'pprint-tensor)

;;; Generic tensor methods

(defgeneric reshape (tensor shape)
  (:documentation "Change the shape of the tensor.
WARNING: This method acts differently depending on the order of the tensor. Do not expect row-major to act the same as column-major.")
  (:method ((tensor tensor) shape)
    (policy-cond:policy-if
     (< speed safety)
     (let ((shape-size (reduce #'* shape)))
       (assert (cl:= (size tensor) shape-size)
               () "Incompatible shape. Must have the same total number of elements. The tensor has ~a elements and the new shape has ~a elements" (size tensor) shape-size))
     nil)
    (setf (slot-value tensor 'shape) shape)
    (setf (slot-value tensor 'rank) (length shape))
    tensor))

(defgeneric stack (tensorA tensorB dim) ;; Also have specifics for 2d
  (:documentation "Create a new tensor from stacking the tensors in the specififed dimension"))
