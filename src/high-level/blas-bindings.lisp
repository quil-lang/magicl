;;;; blas-bindings.lisp
;;;;
;;;; Author: Vasily Postnicov

(in-package #:magicl)

;; Map can be done on underlying array
;; FIXME: beware of side effects!
;; FIXME: Can we just specialize on abstact-tensor?
(macrolet ((def-map! (type)
             `(defmethod map! ((function function)
                               (tensor ,type))
                (map-into
                 (storage tensor)
                 function
                 (storage tensor))
                tensor)))
  (def-map! matrix)
  (def-map! vector))

;; Scalar - matrix multiplication
(macrolet ((def-scale (matrix-type scalar-type function)
             `(defmethod scale! ((tensor ,matrix-type)
                                 (factor ,scalar-type))
                (,function
                 (size tensor)
                 factor
                 (storage tensor)
                 1)
                tensor)))
  (def-scale matrix/single-float single-float magicl.blas-cffi:%sscal)
  (def-scale matrix/double-float double-float magicl.blas-cffi:%dscal)
  (def-scale vector/single-float single-float magicl.blas-cffi:%sscal)
  (def-scale vector/double-float double-float magicl.blas-cffi:%dscal))

(declaim (inline compatible-matrices-p
                 compatible-vectors-p))
(defun compatible-matrices-p (m1 m2)
  (and
   (eq (matrix-layout m1)
       (matrix-layout m2))
   (eq (type-of m1)
       (type-of m2))))

(defun compatible-vectors-p (v1 v2)
  (eq (type-of v1)
      (type-of v2)))

(defmacro with-compatibility-test ((tensor-type) &body body)
  `(cond
     (,(if (subtypep tensor-type 'matrix)
           `(and (compatible-matrices-p source1 source2)
                 (or (not target) (compatible-matrices-p source1 target)))
           `(or (not target) (compatible-vectors-p source1 target)))
      ,@body)
     (t (call-next-method))))

(defgeneric copy-to/w-same-layout (source target)
  (:documentation "Fast copy from SOURCE to TARGET with the same
layout. If TARGET is NIL, this function works just like
DEEP-COPY-TENSOR."))

(defmethod copy-to/w-same-layout ((source abstract-tensor)
                                  (target null))
  (deep-copy-tensor source))

(macrolet ((def-copier (tensor-type scalar-type)
             `(defmethod copy-to/w-same-layout ((source ,tensor-type)
                                                (target ,tensor-type))
                (declare (optimize (speed 3)))
                (when (not (eq source target))
                  (let ((storage-t (storage target))
                        (storage-s (storage source)))
                    (declare (type (simple-array ,scalar-type)
                                   storage-s storage-t))
                    (map-into storage-t #'identity storage-s)))
                target)))
  (def-copier matrix/single-float single-float)
  (def-copier matrix/double-float double-float)
  (def-copier vector/single-float single-float)
  (def-copier vector/double-float double-float))

;; Fast matrix/vector addition
(macrolet ((def-+ (tensor-type scalar-type function)
             `(defmethod .+ ((source1 ,tensor-type)
                             (source2 ,tensor-type)
                             &optional target)
                (with-compatibility-test (,tensor-type)
                  (policy-cond:with-expectations (> speed safety)
                      ((assertion (equalp (shape source1)
                                          (shape source2)))))
                  (let ((source1 (if (eq target source1)
                                     (deep-copy-tensor source1)
                                     source1))
                        (source2 (copy-to/w-same-layout source2 target)))
                    (,function
                     (size source2)
                     (coerce 1 ',scalar-type)
                     (storage source1) 1
                     (storage source2) 1)
                    source2)))))
  (def-+ matrix/single-float single-float magicl.blas-cffi:%saxpy)
  (def-+ matrix/double-float double-float magicl.blas-cffi:%daxpy)
  (def-+ vector/single-float single-float magicl.blas-cffi:%saxpy)
  (def-+ vector/double-float double-float magicl.blas-cffi:%daxpy))

;; Fast matrix/vector subtraction
(macrolet ((def-- (tensor-type scalar-type function)
             `(defmethod .- ((source1 ,tensor-type)
                             (source2 ,tensor-type)
                             &optional target)
                (with-compatibility-test (,tensor-type)
                  (policy-cond:with-expectations (> speed safety)
                      ((assertion (equalp (shape source1)
                                          (shape source2)))))
                  (let ((source2 (if (eq target source2)
                                     (deep-copy-tensor source2)
                                     source2))
                        (source1 (copy-to/w-same-layout source1 target)))
                    (,function
                     (size source2)
                     (coerce -1 ',scalar-type)
                     (storage source2) 1
                     (storage source1) 1)
                    source1)))))
  (def-- matrix/single-float single-float magicl.blas-cffi:%saxpy)
  (def-- matrix/double-float double-float magicl.blas-cffi:%daxpy)
  (def-- vector/single-float single-float magicl.blas-cffi:%saxpy)
  (def-- vector/double-float double-float magicl.blas-cffi:%daxpy))

;; Fast matrix/vector element-wise multiplication/division

(macrolet ((def-op (name tensor-type scalar-type function)
             `(defmethod ,name ((source1 ,tensor-type)
                                (source2 ,tensor-type)
                                &optional target)
                (declare (optimize (speed 3)))
                (with-compatibility-test (,tensor-type)
                  (policy-cond:with-expectations (> speed safety)
                      ((assertion (equalp (shape source1)
                                          (shape source2)))))
                  (let ((target
                          (or target
                              (empty (shape source1) :type ',scalar-type))))
                    (let ((target-st  (storage target))
                          (source1-st (storage source1))
                          (source2-st (storage source2)))
                      (declare (type (simple-array ,scalar-type)
                                     target-st source1-st source2-st))
                      (map-into target-st #',function source1-st source2-st))
                    target)))))
  (def-op .* matrix/single-float single-float *)
  (def-op .* matrix/double-float double-float *)
  (def-op ./ matrix/single-float single-float /)
  (def-op ./ matrix/double-float double-float /)
  (def-op .* vector/single-float single-float *)
  (def-op .* vector/double-float double-float *)
  (def-op ./ vector/single-float single-float /)
  (def-op ./ vector/double-float double-float /))
