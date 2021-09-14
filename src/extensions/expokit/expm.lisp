;;;; expm.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:magicl-expokit)

(declaim (inline call-expm))
(defun call-expm (cffi-fun type m)
  (let ((ideg 6)
        (rows (magicl:nrows m))
        (tcoef (coerce 1.0 'double-float))
        (h (copy-seq (magicl::storage m)))
        (iexph 0)
        (ns 0)
        (iflag 0))
    (let ((lwsp (+ (* 4 rows rows) ideg 1))
          (ipiv (magicl::make-array (list rows) :element-type '(signed-byte 32))))
      (let ((wsp (magicl::make-array (list lwsp) :element-type type)))
        ;; Requires direct foreign function call due to need to
        ;; access a pointer to an integer (IEXPH).
        (CFFI:WITH-FOREIGN-OBJECTS ((IDEG-REF103 ':INT32) (M-REF104 ':INT32)
                                    (T-REF105 ':DOUBLE) (LDH-REF107 ':INT32)
                                    (LWSP-REF109 ':INT32) (IEXPH-REF111 ':INT32)
                                    (NS-REF112 ':INT32) (IFLAG-REF113 ':INT32))
          (COMMON-LISP:SETF (CFFI:MEM-REF IDEG-REF103 :INT32) IDEG)
          (COMMON-LISP:SETF (CFFI:MEM-REF M-REF104 :INT32) ROWS)
          (COMMON-LISP:SETF (CFFI:MEM-REF T-REF105 :DOUBLE) TCOEF)
          (COMMON-LISP:SETF (CFFI:MEM-REF LDH-REF107 :INT32) ROWS)
          (COMMON-LISP:SETF (CFFI:MEM-REF LWSP-REF109 :INT32) LWSP)
          (COMMON-LISP:SETF (CFFI:MEM-REF IEXPH-REF111 :INT32) IEXPH)
          (COMMON-LISP:SETF (CFFI:MEM-REF NS-REF112 :INT32) NS)
          (COMMON-LISP:SETF (CFFI:MEM-REF IFLAG-REF113 :INT32) IFLAG)
          (magicl.cffi-types:with-array-pointers ((H-ptr H)
                                                  (WSP-ptr WSP)
                                                  (IPIV-ptr IPIV))
            (funcall cffi-fun IDEG-REF103 M-REF104 T-REF105
                     H-ptr LDH-REF107
                     WSP-ptr LWSP-REF109
                     IPIV-ptr
                     IEXPH-REF111 NS-REF112 IFLAG-REF113))
          (setf iexph (CFFI:MEM-REF IEXPH-REF111 :INT32)))
        (let ((exph (magicl::make-array (list (* rows rows)) :element-type type)))
          (dotimes (i (* rows rows))
            (setf (row-major-aref exph i)
                  (row-major-aref wsp (+ i (1- iexph)))))
          (values (magicl:from-array exph (list rows rows) :input-layout :column-major)))))))

(magicl:define-extensible-function (magicl:expm expm-expokit :expokit) (matrix)
  (:method ((m magicl:matrix/double-float))
    (call-expm #'magicl.expokit-cffi::%%dgpadm 'double-float m))
  (:method ((m magicl:matrix/complex-double-float))
    (call-expm #'magicl.expokit-cffi::%%zgpadm '(complex double-float) m)))


