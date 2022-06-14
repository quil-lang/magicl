(in-package #:magicl-lapack)

;; ZUNCSD is broken in magicl lapack bindings (Issue #72)
(COMMON-LISP:DEFUN %ZUNCSD-XPOINTERS
    (JOBU1 JOBU2 JOBV1T JOBV2T TRANS SIGNS M P Q X11 LDX11 X12
     LDX12 X21 LDX21 X22 LDX22 THETA U1 LDU1 U2 LDU2 V1T LDV1T
     V2T LDV2T WORK LWORK RWORK LRWORK IWORK INFO)
  (CFFI:WITH-FOREIGN-OBJECTS ((M-REF71665 ':INT32) (P-REF71666 ':INT32)
                              (Q-REF71667 ':INT32) (LDX11-REF71669 ':INT32)
                              (LDX12-REF71671 ':INT32) (LDX21-REF71673 ':INT32)
                              (LDX22-REF71675 ':INT32) (LDU1-REF71678 ':INT32)
                              (LDU2-REF71680 ':INT32) (LDV1T-REF71682 ':INT32)
                              (LDV2T-REF71684 ':INT32) (LWORK-REF71686 ':INT32)
                              (LRWORK-REF71688 ':INT32) (INFO-REF71690 ':INT32))
    (COMMON-LISP:SETF (CFFI:MEM-REF M-REF71665 :INT32) M)
    (COMMON-LISP:SETF (CFFI:MEM-REF P-REF71666 :INT32) P)
    (COMMON-LISP:SETF (CFFI:MEM-REF Q-REF71667 :INT32) Q)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDX11-REF71669 :INT32) LDX11)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDX12-REF71671 :INT32) LDX12)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDX21-REF71673 :INT32) LDX21)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDX22-REF71675 :INT32) LDX22)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDU1-REF71678 :INT32) LDU1)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDU2-REF71680 :INT32) LDU2)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDV1T-REF71682 :INT32) LDV1T)
    (COMMON-LISP:SETF (CFFI:MEM-REF LDV2T-REF71684 :INT32) LDV2T)
    (COMMON-LISP:SETF (CFFI:MEM-REF LWORK-REF71686 :INT32) LWORK)
    (COMMON-LISP:SETF (CFFI:MEM-REF LRWORK-REF71688 :INT32) LRWORK)
    (COMMON-LISP:SETF (CFFI:MEM-REF INFO-REF71690 :INT32) INFO)
    (MAGICL.CFFI-TYPES:WITH-ARRAY-POINTERS ((THETA-REF71676 THETA)
                                            (U1-REF71677 U1) (U2-REF71679 U2)
                                            (V1T-REF71681 V1T)
                                            (V2T-REF71683 V2T)
                                            (WORK-REF71685 WORK)
                                            (RWORK-REF71687 RWORK)
                                            (IWORK-REF71689 IWORK))
      (MAGICL.LAPACK-CFFI::%%ZUNCSD
       JOBU1 JOBU2 JOBV1T JOBV2T TRANS SIGNS M-REF71665 P-REF71666
       Q-REF71667 X11 LDX11-REF71669 X12 LDX12-REF71671
       X21 LDX21-REF71673 X22 LDX22-REF71675 THETA-REF71676
       U1-REF71677 LDU1-REF71678 U2-REF71679 LDU2-REF71680 V1T-REF71681
       LDV1T-REF71682 V2T-REF71683 LDV2T-REF71684 WORK-REF71685 LWORK-REF71686
       RWORK-REF71687 LRWORK-REF71688 IWORK-REF71689 INFO-REF71690))))

(defmethod csd-2x2-basic ((unitary-matrix-2x2 matrix/complex-double-float) p q)
  "Returns the Cosine-Sine decomposition of an equipartitioned UNITARY-MATRIX-2x2. The values of P and Q are assumed to be equal to one and ignored. See the documentation of LAPACK-CSD for details about the returned values."
  ;; This function is meant to be used within LAPACK-CSD in the base case
  ;; where the unitary matrix is 2x2 (i.e., it is equivalent to a ZYZ
  ;; decomposition). It computes the CS decomposition in Lisp faster (more
  ;; than three times) and with less memory overhead (conses less than half
  ;; as much) than the corresponding LAPACK wrapper.
  (declare (values matrix/complex-double-float
                   matrix/complex-double-float
                   matrix/complex-double-float
                   matrix/complex-double-float
                   list)
           (ignorable p q)
           (optimize (speed 3) (safety 0) (debug 0) (space 0)))

  (let* ((data (magicl::storage unitary-matrix-2x2))
         (a1 (aref data 0))
         (a2 (aref data 1))
         (a3 (aref data 2))
         (a4 (aref data 3))

         (c (abs a1))
         (u1 (cis (phase a1)))
         (s (abs a2))
         (u2 (cis (phase a2))))

    (declare (type (simple-array (complex double-float) (4)) data)
             (type (double-float -1.0d0 1.0d0) c s)
             (type (complex double-float) u1 u2)
             (dynamic-extent c s u1 u2))

    (let ((v2h (conjugate (/ 1.0d0 (- (* c (conjugate u2) a4)
                                      (* s (conjugate u1) a3)))))
          (mu1 (empty '(1 1) :type '(complex double-float)))
          (mu2 (empty '(1 1) :type '(complex double-float)))
          (mv1h (empty '(1 1) :type '(complex double-float)))
          (mv2h (empty '(1 1) :type '(complex double-float))))

      (macrolet ((matrix-1x1-data (matrix)
                   `(the (simple-array (complex double-float) (1)) (magicl::storage ,matrix))))

        (setf (aref (matrix-1x1-data mu1) 0) u1
              (aref (matrix-1x1-data mu2) 0) u2
              (aref (matrix-1x1-data mv1h) 0) #C(1.0d0 0.0d0)
              (aref (matrix-1x1-data mv2h) 0) v2h))

      (values mu1 mu2 mv1h mv2h (list (atan s c))))))

(defmethod csd-blocks-extension ((x matrix/complex-double-float) p q)
  (let* ((m (nrows x))
         (layout (layout x))
         (xcopy (deep-copy-tensor x)))
    (magicl::assert-square-matrix x)
    (check-type p integer)
    (check-type q integer)
    (assert (<= 1 p (1- m)) () "P = ~D is out of range" p)
    (assert (<= 1 q (1- m)) () "Q = ~D is out of range" q)
    (let ((jobu1 "Y")
          (jobu2 "Y")
          (jobv1t "Y")
          (jobv2t "Y")
          (trans
            (if (eq :row-major layout)
                "T"
                "F"))
          (signs "D")
          ;; leading dimension is M because full MATRIX array will be used
          (ldx11 m)
          (ldx12 m)
          (ldx21 m)
          (ldx22 m)
          (r (min p (- m p) q (- m q)))
          (ldu1 p)
          (ldu2 (- m p))
          (ldv1t q)
          (ldv2t (- m q))
          (lwork -1)
          (work (make-array 1 :element-type '(complex double-float)))
          (lrwork -1)
          (rwork (make-array 1 :element-type 'double-float))
          (info 0))
      ;; rather than slice up matrix, use full array with pointers to head of blocks
      ;;
      ;; WARNING: THIS ABYSS IS WHERE DRAGONS LIVE. HERE THE GARBAGE
      ;; COLLECTOR MUST BE TAMED SO WE DON'T SCREW UP THE POINTERS
      ;; INTO THE LISP HEAP.
      ;;
      ;; HOURS WASTED HERE: 10
      (magicl.cffi-types:with-array-pointers ((xcopy-ptr (magicl::storage xcopy)))
        ;; The next line is necessary for Allegro, because we are passing array objects directly in foreign calls.
        ;; But in this function, `magicl.cffi-types:ptr-ref` needs to calculate pointer addresses by using `cffi:mem-aptr`, so it certainly expects an integer.
        ;; If in the future more "weird MAGICL bindings" ("weird" in the sense that it needs to calculate more integer addresses from the given one) are required, --
        ;; then it does need to be taken care of for Allegro.
        #+allegro (setq xcopy-ptr (ff:fslot-address-typed :unsigned-char :lisp (magicl::storage xcopy)))
        (let ((x11 xcopy-ptr)
              (x12 (magicl.cffi-types:ptr-ref xcopy xcopy-ptr 0 q))
              (x21 (magicl.cffi-types:ptr-ref xcopy xcopy-ptr p 0))
              (x22 (magicl.cffi-types:ptr-ref xcopy xcopy-ptr p q))
              (theta (make-array r
                                 :element-type 'double-float))
              (u1 (make-array (* ldu1 p)
                              :element-type '(complex double-float)))
              (u2 (make-array (* ldu2 (- m p))
                              :element-type '(complex double-float)))
              (v1t (make-array (* ldv1t q)
                               :element-type '(complex double-float)))
              (v2t (make-array (* ldv2t (- m q))
                               :element-type '(complex double-float)))
              (iwork (make-array (- m r)
                                 :element-type '(signed-byte 32))))
          ;; run it once as a workspace query
          (%ZUNCSD-XPOINTERS jobu1 jobu2 jobv1t jobv2t
                             trans signs m p q
                             x11 ldx11 x12 ldx12 x21 ldx21 x22 ldx22
                             theta u1 ldu1 u2 ldu2 v1t ldv1t v2t ldv2t
                             work lwork rwork lrwork iwork info)
          (setf lwork (truncate (realpart (row-major-aref work 0))))
          (setf work (make-array (max 1 lwork) :element-type '(complex double-float)))
          (setf lrwork (truncate (row-major-aref rwork 0)))
          (setf rwork (make-array (max 1 lrwork) :element-type 'double-float))
          ;; run it again with optimal workspace size
          (%ZUNCSD-XPOINTERS jobu1 jobu2 jobv1t jobv2t
                             trans signs m p q
                             x11 ldx11 x12 ldx12 x21 ldx21 x22 ldx22
                             theta u1 ldu1 u2 ldu2 v1t ldv1t v2t ldv2t
                             work lwork rwork lrwork iwork info)
          (values (from-array u1 (list p p) :input-layout layout)
                  (from-array u2 (list (- m p) (- m p)) :input-layout layout)
                  (from-array v1t (list q q) :input-layout layout)
                  (from-array v2t (list (- m q) (- m q)) :input-layout layout)
                  (coerce theta 'list)))))))

