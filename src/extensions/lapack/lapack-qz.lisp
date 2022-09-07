;;;; lapack-qz.lisp
;;;;
;;;; Author: Robert Smith
;;;;         Graham Enos

(in-package #:magicl-lapack)

(defmethod qz-extension ((a magicl:matrix/double-float)
                         (b magicl:matrix/double-float))
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
      ;; TODO: we need to check info
      (values aa bb q z))))

(defmethod qz-extension ((a magicl:matrix/complex-double-float)
                         (b magicl:matrix/complex-double-float))
  (assert (magicl:square-matrix-p a))
  (assert (magicl:square-matrix-p b))
  ;; TODO: This probably doesn't properly take into account the tensor
  ;; layout, etc.
  (let* ((aa     (magicl:deep-copy-tensor a))
         (bb     (magicl:deep-copy-tensor b))
         (n      (magicl:nrows a))
         (nn     (magicl:size a))
         (q      (magicl:zeros (magicl:shape a) :type '(complex double-float)))
         (z      (magicl:zeros (magicl:shape a) :type '(complex double-float)))
         (lwork  (* 8 n))               ; larger is better, min(1, 2n)
         (info   0))
    (flet ((arr (i  &optional (ty '(complex double-float)))
             (make-array i :element-type ty)))
      (declare (inline arr))
      (magicl.lapack-cffi:%zgges
       "V"                         ; JOBSVL
       "V"                         ; JOBSVR
       "N"                         ; SORT
       0                           ; SELCTG [not referenced if SORT = "N"]
       n                           ; N
       (magicl::storage aa)        ; A
       n                           ; LDA
       (magicl::storage bb)        ; B
       n                           ; LDB
       0                           ; SDIM [0 if SORT = "N"]
       (arr nn)                    ; ALPHA
       (arr nn)                    ; BETA
       (magicl::storage q)         ; VSL
       n                           ; LDVSL
       (magicl::storage z)         ; VSR
       n                           ; LDVSR
       (arr lwork)                 ; WORK
       lwork                       ; LWORK
       (arr (* 8 n) 'double-float) ; RWORK
       (arr nn '(signed-byte 32))  ; BWORK [not referenced if SORT = "N"]
       info)                       ; INFO
      ;; TODO: we need to check info
      (values aa bb q z))))
