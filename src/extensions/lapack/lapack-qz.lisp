;;;; lapack-qz.lisp
;;;;
;;;; Author: Robert Smith
;;;;         Graham Enos

(in-package #:magicl-lapack)


(defmethod qz-extension ((a magicl:matrix/double-float) (b magicl:matrix/double-float))
  (assert (magicl:square-matrix-p a))
  (assert (magicl:square-matrix-p b))
  ;; TODO: This probably doesn't properly take into account the tensor
  ;; layout, etc.
  (let* ((aa     (magicl:deep-copy-tensor a))
         (bb     (magicl:deep-copy-tensor b))
         (n      (magicl:nrows a))
         (nn     (magicl:size a))
         (q      (magicl:zeros (magicl:shape a) :type 'double-float))
         (z      (magicl:zeros (magicl:shape a) :type 'double-float))
         (lwork  (* 8 (+ n 2)))
         (info   0))
    (flet ((arr (i  &optional (ty 'double-float))
             (make-array i :element-type ty)))
      (declare (inline arr))
      (magicl.lapack-cffi:%dgges
       "V"                        ; JOBSVL
       "V"                        ; JOBSVR
       "N"                        ; SORT
       0                          ; SELCTG [not referenced if SORT = "N"]
       n                          ; N
       (magicl::storage aa)       ; A
       n                          ; LDA
       (magicl::storage bb)       ; B
       n                          ; LDB
       0                          ; SDIM [0 if SORT = "N"]
       (arr nn)                   ; ALPHAR
       (arr nn)                   ; ALPHAI
       (arr nn)                   ; BETA
       (magicl::storage q)        ; VSL
       n                          ; LDVSL
       (magicl::storage z)        ; VSR
       n                          ; LDVSR
       (arr lwork)                ; WORK
       lwork                      ; LWORK
       (arr nn '(signed-byte 32)) ; BWORK [not referenced if SORT = "N"]
       info)                      ; INFO
      (values aa bb q z))))


;;; example from https://docs.scipy.org/doc/scipy/reference/generated/scipy.linalg.qz.html
(defun %qz-example ()
  (let ((a (magicl:from-list (alexandria:iota 9 :start 1.0d0) '(3 3)))
        (b (magicl:from-list
            (loop :repeat 9 :collect (alexandria:gaussian-random))
            '(3 3))))
    (multiple-value-bind (aa bb q z) (magicl:qz a b)
      (format t "A: ~A~%B: ~A~%AA: ~A~%BB: ~A~%Q: ~A~%Z: ~A~%" a b aa bb q z)
      (format t "Q @ AA @ Z†∶ ~A~%" (magicl:@ q aa (magicl:dagger z)))
      (format t "Q @ BB @ Z†∶ ~A~%" (magicl:@ q bb (magicl:dagger z)))
      (format t "Q @ AA @ Z† = A? ~A~%" (magicl:= a (magicl:@ q aa (magicl:dagger z))))
      (format t "Q @ BB @ Z† = B? ~A~%" (magicl:= b (magicl:@ q bb (magicl:dagger z)))))))
