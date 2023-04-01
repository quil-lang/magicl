;;;; tensor.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(deftype tensor-storage (&optional type)
  `(simple-array ,type (*)))

(defstruct (tensor (:include abstract-tensor)
                   (:constructor nil)
                   (:copier nil))
  (order 0 :type alexandria:non-negative-fixnum)
  (shape '(0) :type list)
  (size 0 :type alexandria:positive-fixnum :read-only t)
  (layout :column-major :type (member :row-major :column-major)))

(defmethod size ((a tensor))
  (tensor-size a))

(defmethod layout ((a tensor))
  (tensor-layout a))

(defmethod order ((a tensor))
  (tensor-order a))

(defmethod shape ((a tensor))
  (tensor-shape a))

(defmethod (setf shape) (new-value (a tensor))
  (reshape a new-value))

;;; Specfic tensor classes
(defmacro deftensor (name type)
  "Define a new tensor subclass with the specified NAME and element
TYPE as well as the abstract-tensor methods required not specified by
the generic TENSOR class (MAKE-TENSOR, ELEMENT-TYPE, CAST,
COPY-TENSOR, DEEP-COPY-TENSOR, TREF, SETF TREF)"
  (let ((constructor-sym (intern (format nil "MAKE-~:@(~A~)-STRUCT" name)))
        (copy-sym (intern (format nil "COPY-~:@(~A~)" name)))
        (storage-sym (intern (format nil "~:@(~A~)-STORAGE" name))))
    `(progn
       (defstruct (,name (:include tensor)
                         (:constructor ,constructor-sym
                             (order shape size layout storage))
                         (:copier ,copy-sym))
         (storage nil :type (tensor-storage ,type)))
       #+sbcl (declaim (sb-ext:freeze-type ,name))
       #+allegro (set-pprint-dispatch ',name 'pprint-tensor)
       
       (defmethod storage ((m ,name))
         (,storage-sym m))

       (defmethod (setf storage) (new-value (m ,name))
         (setf (,storage-sym m) new-value))

       (defmethod element-type ((m ,name))
         (declare (ignore m))
         ',type)
       
       (defmethod make-tensor ((class (eql ',name)) shape &key initial-element layout storage)
         (policy-cond:with-expectations (> speed safety)
             ((type shape shape))
           (let ((size (reduce #'* shape)))
             (multiple-value-bind (actual-storage finalizer)
                 (or storage
                     (allocate size
                               :element-type ',type
                               :initial-element initial-element))
               (let ((tensor
                       (funcall #',constructor-sym
                                (length shape)
                                shape
                                size
                                (or layout :column-major)
                                actual-storage)))
                 (finalize tensor finalizer)
                 tensor)))))
       (defmethod cast ((tensor ,name) (class (eql ',name)))
         (declare (ignore class))
         tensor)

       ;; TODO: This does not allow for args. Make this allow for args.
       (defmethod copy-tensor ((m ,name) &rest args)
         (declare (ignore args))
         (let ((new-m (,copy-sym m)))
           (multiple-value-bind (storage finalizer)
               (allocate (tensor-size m)
                         :element-type (element-type m))             
             (setf (,storage-sym new-m) storage)
             (finalize new-m finalizer))
           new-m))

       (defmethod deep-copy-tensor ((m ,name) &rest args)
         (declare (ignore args))
         (let ((new-m (copy-tensor m)))
           (dotimes (i (tensor-size m))
             (setf (aref (,storage-sym new-m) i)
                   (aref (,storage-sym m) i)))
           new-m))

       (defmethod tref ((tensor ,name) &rest pos)
         (policy-cond:with-expectations (> speed safety)
             ((assertion (valid-index-p pos (tensor-shape tensor))))
           (let ((index (case (tensor-layout tensor)
                          (:row-major (row-major-index pos (tensor-shape tensor)))
                          (:column-major (column-major-index pos (tensor-shape tensor))))))
             (aref (,storage-sym tensor) index))))

       (defmethod (setf tref) (new-value (tensor ,name) &rest pos)
         (policy-cond:with-expectations (> speed safety)
             ((assertion (valid-index-p pos (tensor-shape tensor))))
           (let ((index (case (tensor-layout tensor)
                          (:row-major (row-major-index pos (tensor-shape tensor)))
                          (:column-major (column-major-index pos (tensor-shape tensor))))))
             (setf (aref (,storage-sym tensor) index)
                   (coerce new-value ',type))))))))

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
WARNING: This method acts differently depending on the layout of the tensor. Do not expect row-major to act the same as column-major.")
  (:method ((tensor tensor) shape)
    (policy-cond:with-expectations (> speed safety)
        ((assertion (cl:= (tensor-size tensor) (reduce #'* shape))))
      (setf (tensor-shape tensor) shape)
      (setf (tensor-order tensor) (length shape))
      (specialize-tensor tensor)))
  (:method ((tensor abstract-tensor) shape)
    (reshape (generalize-tensor tensor) shape)))
