;;;; lapack-schur.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:magicl-lapack)

(defmethod schur-extension ((a magicl:matrix/double-float))
  (assert (magicl:square-matrix-p a))
  ;; TODO: This probably doesn't properly take into account the tensor
  ;; layout, etc.
  (let* ((aa     (magicl:deep-copy-tensor a))
         (n      (magicl:nrows a))
         (ttr    (make-array n :element-type 'double-float :initial-element 0.0d0))
         (tti    (make-array n :element-type 'double-float :initial-element 0.0d0))
         (tt     (magicl:zeros (list n n) :type '(complex double-float)))
         (zz     (magicl:zeros (magicl:shape a) :type 'double-float))
         (lwork  (* 3 n))
         (info   0))
    (flet ((arr (i  &optional (ty 'double-float))
             (make-array i :element-type ty)))
      (declare (inline arr))
      (magicl.lapack-cffi:%dgees
       "V"
       "N"
       0
       n
       (magicl::storage aa)
       n
       0
       ttr
       tti
       (magicl::storage zz)
       n
       (arr lwork)
       lwork
       (arr n '(signed-byte 32))        ; not referenced
       info)                      ; INFO
      ;; TODO: we need to check info
      (dotimes (i n)
        (setf (magicl:tref tt i i) (complex (aref ttr i) (aref tti i))))
      (values zz tt))))

(defmethod schur-extension ((a magicl:matrix/complex-double-float))
  (assert (magicl:square-matrix-p a))
  ;; TODO: This probably doesn't properly take into account the tensor
  ;; layout, etc.
  (let* ((aa      (magicl:deep-copy-tensor a))
         (n       (magicl:nrows a))
         (tt-diag (make-array n :element-type '(complex double-float)
                                :initial-element #C(0.0d0 0.0d0)))
         (tt      (magicl:zeros (list n n) :type '(complex double-float)))
         (zz      (magicl:zeros (magicl:shape a) :type '(complex double-float)))
         (lwork   (* 2 n))
         (info    0))
    (flet ((arr (i  &optional (ty '(complex double-float)))
             (make-array i :element-type ty)))
      (declare (inline arr))
      (magicl.lapack-cffi:%zgees
       "V"
       "N"
       0
       n
       (magicl::storage aa)
       n
       0
       tt-diag
       (magicl::storage zz)
       n
       (arr lwork)
       lwork
       (arr n 'double-float)
       (arr n '(signed-byte 32))  ; not referenced
       info)                      ; INFO
      ;; TODO: we need to check info
      (dotimes (i n)
        (setf (magicl:tref tt i i) (aref tt-diag i)))
      (values zz tt))))

