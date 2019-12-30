;;;; tensor.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(deftype tensor-storage (&optional type)
  `(simple-array ,type (*)))

(defstruct (tensor (:include abstract-tensor)
                   (:constructor nil)
                   (:copier nil))
  (rank 0 :type alexandria:non-negative-fixnum)
  (shape '(0) :type list)
  (size 0 :type alexandria:positive-fixnum :read-only t)
  (order :column-major :type (member :row-major :column-major)))

(defmethod rank ((a tensor))
  (tensor-rank a))

(defmethod shape ((a tensor))
  (tensor-shape a))

(defmethod (setf shape) (new-value (a tensor))
  (reshape a new-value))

;;; Specfic tensor classes
(defmacro deftensor (name type)
  (let ((constructor-sym (intern (format nil "MAKE-~a-STRUCT" name)))
        (copy-sym (intern (format nil "COPY-~a" name)))
        (storage-sym (intern (format nil "~a-STORAGE" name))))
    `(progn
       (defstruct (,name (:include tensor)
                         (:constructor ,constructor-sym
                             (rank shape size order storage))
                         (:copier ,copy-sym))
         (storage nil :type (tensor-storage ,type)))
       #+sbcl (declaim (sb-ext:freeze-type ,name))
       
       (defmethod storage ((m ,name))
         (,storage-sym m))

       (defmethod element-type ((m ,name))
         (declare (ignore m))
         ',type)
       
       (defmethod make-tensor ((class (eql ',name)) shape &key initial-element order storage)
         (policy-cond:policy-if
          (< speed safety)
          (check-type shape shape)
          nil)
         (let ((size (reduce #'* shape)))
           (funcall #',constructor-sym
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
         (declare (ignore args))
         (let ((new-m (,copy-sym m)))
           (setf (,storage-sym new-m)
                 (make-array (tensor-size m) :element-type (element-type m)))
           new-m))

       (defmethod deep-copy-tensor ((m ,name) &rest args)
         (declare (ignore args))
         (let ((new-m (,copy-sym m)))
           (setf (,storage-sym new-m)
                 (alexandria:copy-array (,storage-sym m)))
           new-m))

       (defmethod tref ((tensor ,name) &rest pos)
         (let ((index (case (tensor-order tensor)
                        (:row-major (row-major-index pos (tensor-shape tensor)))
                        (:column-major (column-major-index pos (tensor-shape tensor))))))
           (aref (,storage-sym tensor) index)))

       (defmethod (setf tref) (new-value (tensor ,name) &rest pos)
         (let ((index (case (tensor-order tensor)
                        (:row-major (row-major-index pos (tensor-shape tensor)))
                        (:column-major (column-major-index pos (tensor-shape tensor))))))
           (setf (aref (,storage-sym tensor) index)
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
       (assert (cl:= (tensor-size tensor) shape-size)
               () "Incompatible shape. Must have the same total number of elements. The tensor has ~a elements and the new shape has ~a elements" (tensor-size tensor) shape-size))
     nil)
    (setf (tensor-shape tensor) shape)
    (setf (tensor-rank tensor) (length shape))
    tensor))
