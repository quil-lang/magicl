;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl2.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 1409c1352feb 2013/03/24 20:44:50 toy $")

;;; Using Lisp SBCL 2.0.9
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package :magicl.lisp-lapack)


(let* ((czero (f2cl-lib:cmplx 0.0d0 0.0d0))
       (cone (f2cl-lib:cmplx 1.0d0 0.0d0))
       (zero 0.0d0)
       (one 1.0d0))
  (declare (type (f2cl-lib:complex16) czero)
           (type (f2cl-lib:complex16) cone)
           (type (double-float 0.0d0 0.0d0) zero)
           (type (double-float 1.0d0 1.0d0) one)
           (ignorable czero cone zero one))
  (defun zgesvd (jobu jobvt m n a lda s u ldu vt ldvt work lwork rwork info)
    (declare (type (array double-float (*)) rwork s)
             (type (array f2cl-lib:complex16 (*)) work vt u a)
             (type (f2cl-lib:integer4) info lwork ldvt ldu lda n m)
             (type (string 1) jobvt jobu))
    (f2cl-lib:with-multi-array-data
        ((jobu character jobu-%data% jobu-%offset%)
         (jobvt character jobvt-%data% jobvt-%offset%)
         (a f2cl-lib:complex16 a-%data% a-%offset%)
         (u f2cl-lib:complex16 u-%data% u-%offset%)
         (vt f2cl-lib:complex16 vt-%data% vt-%offset%)
         (work f2cl-lib:complex16 work-%data% work-%offset%)
         (s double-float s-%data% s-%offset%)
         (rwork double-float rwork-%data% rwork-%offset%))
      (prog ((cdum (make-array 1 :element-type 'f2cl-lib:complex16))
             (dum (make-array 1 :element-type 'double-float)) (anrm 0.0d0)
             (bignum 0.0d0) (eps 0.0d0) (smlnum 0.0d0) (lwork_zgeqrf 0)
             (lwork_zungqr_n 0) (lwork_zungqr_m 0) (lwork_zgebrd 0)
             (lwork_zungbr_p 0) (lwork_zungbr_q 0) (lwork_zgelqf 0)
             (lwork_zunglq_n 0) (lwork_zunglq_m 0) (blk 0) (chunk 0) (i 0)
             (ie 0) (ierr 0) (ir 0) (irwork 0) (iscl 0) (itau 0) (itaup 0)
             (itauq 0) (iu 0) (iwork 0) (ldwrkr 0) (ldwrku 0) (maxwrk 0)
             (minmn 0) (minwrk 0) (mnthr 0) (ncu 0) (ncvt 0) (nru 0) (nrvt 0)
             (wrkbl 0) (lquery nil) (wntua nil) (wntuas nil) (wntun nil)
             (wntuo nil) (wntus nil) (wntva nil) (wntvas nil) (wntvn nil)
             (wntvo nil) (wntvs nil))
        (declare (type (array f2cl-lib:complex16 (1)) cdum)
                 (type (array double-float (1)) dum)
                 (type (double-float) anrm bignum eps smlnum)
                 (type (f2cl-lib:integer4) lwork_zgeqrf lwork_zungqr_n
                  lwork_zungqr_m lwork_zgebrd lwork_zungbr_p lwork_zungbr_q
                  lwork_zgelqf lwork_zunglq_n lwork_zunglq_m blk chunk i ie
                  ierr ir irwork iscl itau itaup itauq iu iwork ldwrkr ldwrku
                  maxwrk minmn minwrk mnthr ncu ncvt nru nrvt wrkbl)
                 (type f2cl-lib:logical lquery wntua wntuas wntun wntuo wntus
                  wntva wntvas wntvn wntvo wntvs))
        (setf info 0)
        (setf minmn (min (the f2cl-lib:integer4 m) (the f2cl-lib:integer4 n)))
        (setf wntua (lsame jobu "A"))
        (setf wntus (lsame jobu "S"))
        (setf wntuas (or wntua wntus))
        (setf wntuo (lsame jobu "O"))
        (setf wntun (lsame jobu "N"))
        (setf wntva (lsame jobvt "A"))
        (setf wntvs (lsame jobvt "S"))
        (setf wntvas (or wntva wntvs))
        (setf wntvo (lsame jobvt "O"))
        (setf wntvn (lsame jobvt "N"))
        (setf lquery (coerce (= lwork -1) 'f2cl-lib:logical))
        (cond ((not (or wntua wntus wntuo wntun)) (setf info -1))
              ((or (not (or wntva wntvs wntvo wntvn)) (and wntvo wntuo))
               (setf info -2))
              ((< m 0) (setf info -3)) ((< n 0) (setf info -4))
              ((< lda
                  (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)))
               (setf info -6))
              ((or (< ldu 1) (and wntuas (< ldu m))) (setf info -9))
              ((or (< ldvt 1) (and wntva (< ldvt n))
                   (and wntvs (< ldvt minmn)))
               (setf info -11)))
        (cond
         ((= info 0) (setf minwrk 1) (setf maxwrk 1)
          (cond
           ((and (>= m n) (> minmn 0))
            (setf mnthr
                    (ilaenv 6 "ZGESVD" (f2cl-lib:f2cl-// jobu jobvt) m n 0 0))
            (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                (zgeqrf m n a lda
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1))) -1
                 ierr)
              (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
              (setf ierr var-7))
            (setf lwork_zgeqrf (f2cl-lib:int (f2cl-lib:fref cdum (1) ((1 1)))))
            (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                (zungqr m n n a lda
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1))) -1
                 ierr)
              (declare
               (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
              (setf ierr var-8))
            (setf lwork_zungqr_n
                    (f2cl-lib:int (f2cl-lib:fref cdum (1) ((1 1)))))
            (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                (zungqr m m n a lda
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1))) -1
                 ierr)
              (declare
               (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
              (setf ierr var-8))
            (setf lwork_zungqr_m
                    (f2cl-lib:int (f2cl-lib:fref cdum (1) ((1 1)))))
            (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10)
                (zgebrd n n a lda s
                 (f2cl-lib:array-slice dum double-float (1) ((1 1)))
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1))) -1
                 ierr)
              (declare
               (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                var-9))
              (setf ierr var-10))
            (setf lwork_zgebrd (f2cl-lib:int (f2cl-lib:fref cdum (1) ((1 1)))))
            (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
                (zungbr "P" n n n a lda
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1))) -1
                 ierr)
              (declare
               (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8))
              (setf ierr var-9))
            (setf lwork_zungbr_p
                    (f2cl-lib:int (f2cl-lib:fref cdum (1) ((1 1)))))
            (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
                (zungbr "Q" n n n a lda
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1))) -1
                 ierr)
              (declare
               (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8))
              (setf ierr var-9))
            (setf lwork_zungbr_q
                    (f2cl-lib:int (f2cl-lib:fref cdum (1) ((1 1)))))
            (cond
             ((>= m mnthr)
              (cond
               (wntun (setf maxwrk (f2cl-lib:int-add n lwork_zgeqrf))
                (setf maxwrk
                        (max (the f2cl-lib:integer4 maxwrk)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zgebrd))))
                (if (or wntvo wntvas)
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                        lwork_zungbr_p)))))
                (setf minwrk (f2cl-lib:int-mul 3 n)))
               ((and wntuo wntvn)
                (setf wrkbl (f2cl-lib:int-add n lwork_zgeqrf))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add n lwork_zungqr_n))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zgebrd))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zungbr_q))))
                (setf maxwrk
                        (max
                         (the f2cl-lib:integer4
                              (f2cl-lib:int-add (f2cl-lib:int-mul n n) wrkbl))
                         (the f2cl-lib:integer4
                              (f2cl-lib:int-add (f2cl-lib:int-mul n n)
                                                (f2cl-lib:int-mul m n)))))
                (setf minwrk (f2cl-lib:int-add (f2cl-lib:int-mul 2 n) m)))
               ((and wntuo wntvas)
                (setf wrkbl (f2cl-lib:int-add n lwork_zgeqrf))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add n lwork_zungqr_n))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zgebrd))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zungbr_q))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zungbr_p))))
                (setf maxwrk
                        (max
                         (the f2cl-lib:integer4
                              (f2cl-lib:int-add (f2cl-lib:int-mul n n) wrkbl))
                         (the f2cl-lib:integer4
                              (f2cl-lib:int-add (f2cl-lib:int-mul n n)
                                                (f2cl-lib:int-mul m n)))))
                (setf minwrk (f2cl-lib:int-add (f2cl-lib:int-mul 2 n) m)))
               ((and wntus wntvn)
                (setf wrkbl (f2cl-lib:int-add n lwork_zgeqrf))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add n lwork_zungqr_n))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zgebrd))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zungbr_q))))
                (setf maxwrk (f2cl-lib:int-add (f2cl-lib:int-mul n n) wrkbl))
                (setf minwrk (f2cl-lib:int-add (f2cl-lib:int-mul 2 n) m)))
               ((and wntus wntvo)
                (setf wrkbl (f2cl-lib:int-add n lwork_zgeqrf))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add n lwork_zungqr_n))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zgebrd))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zungbr_q))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zungbr_p))))
                (setf maxwrk (f2cl-lib:int-add (f2cl-lib:int-mul 2 n n) wrkbl))
                (setf minwrk (f2cl-lib:int-add (f2cl-lib:int-mul 2 n) m)))
               ((and wntus wntvas)
                (setf wrkbl (f2cl-lib:int-add n lwork_zgeqrf))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add n lwork_zungqr_n))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zgebrd))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zungbr_q))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zungbr_p))))
                (setf maxwrk (f2cl-lib:int-add (f2cl-lib:int-mul n n) wrkbl))
                (setf minwrk (f2cl-lib:int-add (f2cl-lib:int-mul 2 n) m)))
               ((and wntua wntvn)
                (setf wrkbl (f2cl-lib:int-add n lwork_zgeqrf))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add n lwork_zungqr_m))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zgebrd))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zungbr_q))))
                (setf maxwrk (f2cl-lib:int-add (f2cl-lib:int-mul n n) wrkbl))
                (setf minwrk (f2cl-lib:int-add (f2cl-lib:int-mul 2 n) m)))
               ((and wntua wntvo)
                (setf wrkbl (f2cl-lib:int-add n lwork_zgeqrf))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add n lwork_zungqr_m))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zgebrd))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zungbr_q))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zungbr_p))))
                (setf maxwrk (f2cl-lib:int-add (f2cl-lib:int-mul 2 n n) wrkbl))
                (setf minwrk (f2cl-lib:int-add (f2cl-lib:int-mul 2 n) m)))
               ((and wntua wntvas)
                (setf wrkbl (f2cl-lib:int-add n lwork_zgeqrf))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add n lwork_zungqr_m))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zgebrd))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zungbr_q))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zungbr_p))))
                (setf maxwrk (f2cl-lib:int-add (f2cl-lib:int-mul n n) wrkbl))
                (setf minwrk (f2cl-lib:int-add (f2cl-lib:int-mul 2 n) m)))))
             (t
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                   var-10)
                  (zgebrd m n a lda s
                   (f2cl-lib:array-slice dum double-float (1) ((1 1)))
                   (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                   (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                   (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                   -1 ierr)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                  var-9))
                (setf ierr var-10))
              (setf lwork_zgebrd
                      (f2cl-lib:int (f2cl-lib:fref cdum (1) ((1 1)))))
              (setf maxwrk
                      (f2cl-lib:int-add (f2cl-lib:int-mul 2 n) lwork_zgebrd))
              (cond
               ((or wntus wntuo)
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9)
                    (zungbr "Q" m n n a lda
                     (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                     (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                     -1 ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8))
                  (setf ierr var-9))
                (setf lwork_zungbr_q
                        (f2cl-lib:int (f2cl-lib:fref cdum (1) ((1 1)))))
                (setf maxwrk
                        (max (the f2cl-lib:integer4 maxwrk)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zungbr_q))))))
              (cond
               (wntua
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9)
                    (zungbr "Q" m m n a lda
                     (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                     (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                     -1 ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8))
                  (setf ierr var-9))
                (setf lwork_zungbr_q
                        (f2cl-lib:int (f2cl-lib:fref cdum (1) ((1 1)))))
                (setf maxwrk
                        (max (the f2cl-lib:integer4 maxwrk)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zungbr_q))))))
              (cond
               ((not wntvn)
                (setf maxwrk
                        (max (the f2cl-lib:integer4 maxwrk)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                    lwork_zungbr_p))))
                (setf minwrk (f2cl-lib:int-add (f2cl-lib:int-mul 2 n) m)))))))
           ((> minmn 0)
            (setf mnthr
                    (ilaenv 6 "ZGESVD" (f2cl-lib:f2cl-// jobu jobvt) m n 0 0))
            (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                (zgelqf m n a lda
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1))) -1
                 ierr)
              (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
              (setf ierr var-7))
            (setf lwork_zgelqf (f2cl-lib:int (f2cl-lib:fref cdum (1) ((1 1)))))
            (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                (zunglq n n m
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1))) n
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1))) -1
                 ierr)
              (declare
               (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
              (setf ierr var-8))
            (setf lwork_zunglq_n
                    (f2cl-lib:int (f2cl-lib:fref cdum (1) ((1 1)))))
            (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                (zunglq m n m a lda
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1))) -1
                 ierr)
              (declare
               (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
              (setf ierr var-8))
            (setf lwork_zunglq_m
                    (f2cl-lib:int (f2cl-lib:fref cdum (1) ((1 1)))))
            (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10)
                (zgebrd m m a lda s
                 (f2cl-lib:array-slice dum double-float (1) ((1 1)))
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1))) -1
                 ierr)
              (declare
               (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                var-9))
              (setf ierr var-10))
            (setf lwork_zgebrd (f2cl-lib:int (f2cl-lib:fref cdum (1) ((1 1)))))
            (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
                (zungbr "P" m m m a n
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1))) -1
                 ierr)
              (declare
               (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8))
              (setf ierr var-9))
            (setf lwork_zungbr_p
                    (f2cl-lib:int (f2cl-lib:fref cdum (1) ((1 1)))))
            (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
                (zungbr "Q" m m m a n
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                 (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1))) -1
                 ierr)
              (declare
               (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8))
              (setf ierr var-9))
            (setf lwork_zungbr_q
                    (f2cl-lib:int (f2cl-lib:fref cdum (1) ((1 1)))))
            (cond
             ((>= n mnthr)
              (cond
               (wntvn (setf maxwrk (f2cl-lib:int-add m lwork_zgelqf))
                (setf maxwrk
                        (max (the f2cl-lib:integer4 maxwrk)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zgebrd))))
                (if (or wntuo wntuas)
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                        lwork_zungbr_q)))))
                (setf minwrk (f2cl-lib:int-mul 3 m)))
               ((and wntvo wntun)
                (setf wrkbl (f2cl-lib:int-add m lwork_zgelqf))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add m lwork_zunglq_m))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zgebrd))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zungbr_p))))
                (setf maxwrk
                        (max
                         (the f2cl-lib:integer4
                              (f2cl-lib:int-add (f2cl-lib:int-mul m m) wrkbl))
                         (the f2cl-lib:integer4
                              (f2cl-lib:int-add (f2cl-lib:int-mul m m)
                                                (f2cl-lib:int-mul m n)))))
                (setf minwrk (f2cl-lib:int-add (f2cl-lib:int-mul 2 m) n)))
               ((and wntvo wntuas)
                (setf wrkbl (f2cl-lib:int-add m lwork_zgelqf))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add m lwork_zunglq_m))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zgebrd))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zungbr_p))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zungbr_q))))
                (setf maxwrk
                        (max
                         (the f2cl-lib:integer4
                              (f2cl-lib:int-add (f2cl-lib:int-mul m m) wrkbl))
                         (the f2cl-lib:integer4
                              (f2cl-lib:int-add (f2cl-lib:int-mul m m)
                                                (f2cl-lib:int-mul m n)))))
                (setf minwrk (f2cl-lib:int-add (f2cl-lib:int-mul 2 m) n)))
               ((and wntvs wntun)
                (setf wrkbl (f2cl-lib:int-add m lwork_zgelqf))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add m lwork_zunglq_m))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zgebrd))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zungbr_p))))
                (setf maxwrk (f2cl-lib:int-add (f2cl-lib:int-mul m m) wrkbl))
                (setf minwrk (f2cl-lib:int-add (f2cl-lib:int-mul 2 m) n)))
               ((and wntvs wntuo)
                (setf wrkbl (f2cl-lib:int-add m lwork_zgelqf))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add m lwork_zunglq_m))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zgebrd))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zungbr_p))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zungbr_q))))
                (setf maxwrk (f2cl-lib:int-add (f2cl-lib:int-mul 2 m m) wrkbl))
                (setf minwrk (f2cl-lib:int-add (f2cl-lib:int-mul 2 m) n)))
               ((and wntvs wntuas)
                (setf wrkbl (f2cl-lib:int-add m lwork_zgelqf))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add m lwork_zunglq_m))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zgebrd))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zungbr_p))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zungbr_q))))
                (setf maxwrk (f2cl-lib:int-add (f2cl-lib:int-mul m m) wrkbl))
                (setf minwrk (f2cl-lib:int-add (f2cl-lib:int-mul 2 m) n)))
               ((and wntva wntun)
                (setf wrkbl (f2cl-lib:int-add m lwork_zgelqf))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add m lwork_zunglq_n))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zgebrd))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zungbr_p))))
                (setf maxwrk (f2cl-lib:int-add (f2cl-lib:int-mul m m) wrkbl))
                (setf minwrk (f2cl-lib:int-add (f2cl-lib:int-mul 2 m) n)))
               ((and wntva wntuo)
                (setf wrkbl (f2cl-lib:int-add m lwork_zgelqf))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add m lwork_zunglq_n))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zgebrd))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zungbr_p))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zungbr_q))))
                (setf maxwrk (f2cl-lib:int-add (f2cl-lib:int-mul 2 m m) wrkbl))
                (setf minwrk (f2cl-lib:int-add (f2cl-lib:int-mul 2 m) n)))
               ((and wntva wntuas)
                (setf wrkbl (f2cl-lib:int-add m lwork_zgelqf))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add m lwork_zunglq_n))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zgebrd))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zungbr_p))))
                (setf wrkbl
                        (max (the f2cl-lib:integer4 wrkbl)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zungbr_q))))
                (setf maxwrk (f2cl-lib:int-add (f2cl-lib:int-mul m m) wrkbl))
                (setf minwrk (f2cl-lib:int-add (f2cl-lib:int-mul 2 m) n)))))
             (t
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                   var-10)
                  (zgebrd m n a lda s
                   (f2cl-lib:array-slice dum double-float (1) ((1 1)))
                   (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                   (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                   (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                   -1 ierr)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                  var-9))
                (setf ierr var-10))
              (setf lwork_zgebrd
                      (f2cl-lib:int (f2cl-lib:fref cdum (1) ((1 1)))))
              (setf maxwrk
                      (f2cl-lib:int-add (f2cl-lib:int-mul 2 m) lwork_zgebrd))
              (cond
               ((or wntvs wntvo)
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9)
                    (zungbr "P" m n m a n
                     (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                     (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                     -1 ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8))
                  (setf ierr var-9))
                (setf lwork_zungbr_p
                        (f2cl-lib:int (f2cl-lib:fref cdum (1) ((1 1)))))
                (setf maxwrk
                        (max (the f2cl-lib:integer4 maxwrk)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zungbr_p))))))
              (cond
               (wntva
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9)
                    (zungbr "P" n n m a n
                     (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                     (f2cl-lib:array-slice cdum f2cl-lib:complex16 (1) ((1 1)))
                     -1 ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8))
                  (setf ierr var-9))
                (setf lwork_zungbr_p
                        (f2cl-lib:int (f2cl-lib:fref cdum (1) ((1 1)))))
                (setf maxwrk
                        (max (the f2cl-lib:integer4 maxwrk)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zungbr_p))))))
              (cond
               ((not wntun)
                (setf maxwrk
                        (max (the f2cl-lib:integer4 maxwrk)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 2 m)
                                                    lwork_zungbr_q))))
                (setf minwrk (f2cl-lib:int-add (f2cl-lib:int-mul 2 m) n))))))))
          (setf maxwrk
                  (max (the f2cl-lib:integer4 maxwrk)
                       (the f2cl-lib:integer4 minwrk)))
          (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                  (coerce maxwrk 'f2cl-lib:complex16))
          (cond ((and (< lwork minwrk) (not lquery)) (setf info -13)))))
        (cond
         ((/= info 0) (xerbla "ZGESVD" (f2cl-lib:int-sub info)) (go end_label))
         (lquery (go end_label)))
        (cond ((or (= m 0) (= n 0)) (go end_label)))
        (setf eps (dlamch "P"))
        (setf smlnum (/ (f2cl-lib:fsqrt (dlamch "S")) eps))
        (setf bignum (/ one smlnum))
        (setf anrm (zlange "M" m n a lda dum))
        (setf iscl 0)
        (cond
         ((and (> anrm zero) (< anrm smlnum)) (setf iscl 1)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
              (zlascl "G" 0 0 anrm smlnum m n a lda ierr)
            (declare
             (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8))
            (setf ierr var-9)))
         ((> anrm bignum) (setf iscl 1)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
              (zlascl "G" 0 0 anrm bignum m n a lda ierr)
            (declare
             (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8))
            (setf ierr var-9))))
        (cond
         ((>= m n)
          (cond
           ((>= m mnthr)
            (cond
             (wntun (setf itau 1) (setf iwork (f2cl-lib:int-add itau n))
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                  (zgeqrf m n a lda
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (itau)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iwork)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                (setf ierr var-7))
              (zlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1) czero
               czero
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (2 1)
                                     ((1 lda) (1 *)) a-%offset%)
               lda)
              (setf ie 1) (setf itauq 1)
              (setf itaup (f2cl-lib:int-add itauq n))
              (setf iwork (f2cl-lib:int-add itaup n))
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                   var-10)
                  (zgebrd n n a lda s
                   (f2cl-lib:array-slice rwork-%data% double-float (ie) ((1 *))
                                         rwork-%offset%)
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (itauq)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (itaup)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iwork)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                  var-9))
                (setf ierr var-10))
              (setf ncvt 0)
              (cond
               ((or wntvo wntvas)
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9)
                    (zungbr "P" n n n a lda
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itaup) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8))
                  (setf ierr var-9))
                (setf ncvt n)))
              (setf irwork (f2cl-lib:int-add ie n))
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                   var-10 var-11 var-12 var-13 var-14)
                  (zbdsqr "U" n ncvt 0 0 s
                   (f2cl-lib:array-slice rwork-%data% double-float (ie) ((1 *))
                                         rwork-%offset%)
                   a lda cdum 1 cdum 1
                   (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                         ((1 *)) rwork-%offset%)
                   info)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                  var-9 var-10 var-11 var-12 var-13))
                (setf info var-14))
              (if wntvas
                  (zlacpy "F" n n a lda vt ldvt)))
             ((and wntuo wntvn)
              (cond
               ((>= lwork
                    (f2cl-lib:int-add (f2cl-lib:int-mul n n)
                                      (f2cl-lib:int-mul 3 n)))
                (setf ir 1)
                (cond
                 ((>= lwork
                      (f2cl-lib:int-add
                       (max (the f2cl-lib:integer4 wrkbl)
                            (the f2cl-lib:integer4 (f2cl-lib:int-mul lda n)))
                       (f2cl-lib:int-mul lda n)))
                  (setf ldwrku lda) (setf ldwrkr lda))
                 ((>= lwork
                      (f2cl-lib:int-add
                       (max (the f2cl-lib:integer4 wrkbl)
                            (the f2cl-lib:integer4 (f2cl-lib:int-mul lda n)))
                       (f2cl-lib:int-mul n n)))
                  (setf ldwrku lda) (setf ldwrkr n))
                 (t
                  (setf ldwrku
                          (the f2cl-lib:integer4
                               (truncate (- lwork (* n n)) n)))
                  (setf ldwrkr n)))
                (setf itau (f2cl-lib:int-add ir (f2cl-lib:int-mul ldwrkr n)))
                (setf iwork (f2cl-lib:int-add itau n))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                    (zgeqrf m n a lda
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itau) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                  (setf ierr var-7))
                (zlacpy "U" n n a lda
                 (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                       ((1 *)) work-%offset%)
                 ldwrkr)
                (zlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1) czero
                 czero
                 (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                       ((+ ir 1)) ((1 *)) work-%offset%)
                 ldwrkr)
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                    (zungqr m n n a lda
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itau) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                  (setf ierr var-8))
                (setf ie 1) (setf itauq itau)
                (setf itaup (f2cl-lib:int-add itauq n))
                (setf iwork (f2cl-lib:int-add itaup n))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10)
                    (zgebrd n n
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                           ((1 *)) work-%offset%)
                     ldwrkr s
                     (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                           ((1 *)) rwork-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itauq) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itaup) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8 var-9))
                  (setf ierr var-10))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9)
                    (zungbr "Q" n n n
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                           ((1 *)) work-%offset%)
                     ldwrkr
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itauq) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8))
                  (setf ierr var-9))
                (setf irwork (f2cl-lib:int-add ie n))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14)
                    (zbdsqr "U" n 0 n 0 s
                     (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                           ((1 *)) rwork-%offset%)
                     cdum 1
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                           ((1 *)) work-%offset%)
                     ldwrkr cdum 1
                     (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                           ((1 *)) rwork-%offset%)
                     info)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8 var-9 var-10 var-11 var-12 var-13))
                  (setf info var-14))
                (setf iu itauq)
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i ldwrku))
                              ((> i m) nil)
                  (tagbody
                    (setf chunk
                            (min
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-sub m i) 1))
                             (the f2cl-lib:integer4 ldwrku)))
                    (zgemm "N" "N" chunk n n cone
                     (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i 1)
                                           ((1 lda) (1 *)) a-%offset%)
                     lda
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                           ((1 *)) work-%offset%)
                     ldwrkr czero
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                           ((1 *)) work-%offset%)
                     ldwrku)
                    (zlacpy "F" chunk n
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                           ((1 *)) work-%offset%)
                     ldwrku
                     (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i 1)
                                           ((1 lda) (1 *)) a-%offset%)
                     lda)
                   label10)))
               (t (setf ie 1) (setf itauq 1)
                (setf itaup (f2cl-lib:int-add itauq n))
                (setf iwork (f2cl-lib:int-add itaup n))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10)
                    (zgebrd m n a lda s
                     (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                           ((1 *)) rwork-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itauq) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itaup) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8 var-9))
                  (setf ierr var-10))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9)
                    (zungbr "Q" m n n a lda
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itauq) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8))
                  (setf ierr var-9))
                (setf irwork (f2cl-lib:int-add ie n))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14)
                    (zbdsqr "U" n 0 m 0 s
                     (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                           ((1 *)) rwork-%offset%)
                     cdum 1 a lda cdum 1
                     (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                           ((1 *)) rwork-%offset%)
                     info)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8 var-9 var-10 var-11 var-12 var-13))
                  (setf info var-14)))))
             ((and wntuo wntvas)
              (cond
               ((>= lwork
                    (f2cl-lib:int-add (f2cl-lib:int-mul n n)
                                      (f2cl-lib:int-mul 3 n)))
                (setf ir 1)
                (cond
                 ((>= lwork
                      (f2cl-lib:int-add
                       (max (the f2cl-lib:integer4 wrkbl)
                            (the f2cl-lib:integer4 (f2cl-lib:int-mul lda n)))
                       (f2cl-lib:int-mul lda n)))
                  (setf ldwrku lda) (setf ldwrkr lda))
                 ((>= lwork
                      (f2cl-lib:int-add
                       (max (the f2cl-lib:integer4 wrkbl)
                            (the f2cl-lib:integer4 (f2cl-lib:int-mul lda n)))
                       (f2cl-lib:int-mul n n)))
                  (setf ldwrku lda) (setf ldwrkr n))
                 (t
                  (setf ldwrku
                          (the f2cl-lib:integer4
                               (truncate (- lwork (* n n)) n)))
                  (setf ldwrkr n)))
                (setf itau (f2cl-lib:int-add ir (f2cl-lib:int-mul ldwrkr n)))
                (setf iwork (f2cl-lib:int-add itau n))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                    (zgeqrf m n a lda
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itau) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                  (setf ierr var-7))
                (zlacpy "U" n n a lda vt ldvt)
                (if (> n 1)
                    (zlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1)
                     czero czero
                     (f2cl-lib:array-slice vt-%data% f2cl-lib:complex16 (2 1)
                                           ((1 ldvt) (1 *)) vt-%offset%)
                     ldvt))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                    (zungqr m n n a lda
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itau) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                  (setf ierr var-8))
                (setf ie 1) (setf itauq itau)
                (setf itaup (f2cl-lib:int-add itauq n))
                (setf iwork (f2cl-lib:int-add itaup n))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10)
                    (zgebrd n n vt ldvt s
                     (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                           ((1 *)) rwork-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itauq) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itaup) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8 var-9))
                  (setf ierr var-10))
                (zlacpy "L" n n vt ldvt
                 (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                       ((1 *)) work-%offset%)
                 ldwrkr)
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9)
                    (zungbr "Q" n n n
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                           ((1 *)) work-%offset%)
                     ldwrkr
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itauq) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8))
                  (setf ierr var-9))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9)
                    (zungbr "P" n n n vt ldvt
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itaup) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8))
                  (setf ierr var-9))
                (setf irwork (f2cl-lib:int-add ie n))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14)
                    (zbdsqr "U" n n n 0 s
                     (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                           ((1 *)) rwork-%offset%)
                     vt ldvt
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                           ((1 *)) work-%offset%)
                     ldwrkr cdum 1
                     (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                           ((1 *)) rwork-%offset%)
                     info)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8 var-9 var-10 var-11 var-12 var-13))
                  (setf info var-14))
                (setf iu itauq)
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i ldwrku))
                              ((> i m) nil)
                  (tagbody
                    (setf chunk
                            (min
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-sub m i) 1))
                             (the f2cl-lib:integer4 ldwrku)))
                    (zgemm "N" "N" chunk n n cone
                     (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i 1)
                                           ((1 lda) (1 *)) a-%offset%)
                     lda
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                           ((1 *)) work-%offset%)
                     ldwrkr czero
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                           ((1 *)) work-%offset%)
                     ldwrku)
                    (zlacpy "F" chunk n
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                           ((1 *)) work-%offset%)
                     ldwrku
                     (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i 1)
                                           ((1 lda) (1 *)) a-%offset%)
                     lda)
                   label20)))
               (t (setf itau 1) (setf iwork (f2cl-lib:int-add itau n))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                    (zgeqrf m n a lda
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itau) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                  (setf ierr var-7))
                (zlacpy "U" n n a lda vt ldvt)
                (if (> n 1)
                    (zlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1)
                     czero czero
                     (f2cl-lib:array-slice vt-%data% f2cl-lib:complex16 (2 1)
                                           ((1 ldvt) (1 *)) vt-%offset%)
                     ldvt))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                    (zungqr m n n a lda
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itau) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                  (setf ierr var-8))
                (setf ie 1) (setf itauq itau)
                (setf itaup (f2cl-lib:int-add itauq n))
                (setf iwork (f2cl-lib:int-add itaup n))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10)
                    (zgebrd n n vt ldvt s
                     (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                           ((1 *)) rwork-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itauq) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itaup) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8 var-9))
                  (setf ierr var-10))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13)
                    (zunmbr "Q" "R" "N" m n n vt ldvt
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itauq) ((1 *)) work-%offset%)
                     a lda
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8 var-9 var-10 var-11 var-12))
                  (setf ierr var-13))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9)
                    (zungbr "P" n n n vt ldvt
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itaup) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8))
                  (setf ierr var-9))
                (setf irwork (f2cl-lib:int-add ie n))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14)
                    (zbdsqr "U" n n m 0 s
                     (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                           ((1 *)) rwork-%offset%)
                     vt ldvt a lda cdum 1
                     (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                           ((1 *)) rwork-%offset%)
                     info)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8 var-9 var-10 var-11 var-12 var-13))
                  (setf info var-14)))))
             (wntus
              (cond
               (wntvn
                (cond
                 ((>= lwork
                      (f2cl-lib:int-add (f2cl-lib:int-mul n n)
                                        (f2cl-lib:int-mul 3 n)))
                  (setf ir 1)
                  (cond
                   ((>= lwork
                        (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul lda n)))
                    (setf ldwrkr lda))
                   (t (setf ldwrkr n)))
                  (setf itau (f2cl-lib:int-add ir (f2cl-lib:int-mul ldwrkr n)))
                  (setf iwork (f2cl-lib:int-add itau n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgeqrf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "U" n n a lda
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                         ((1 *)) work-%offset%)
                   ldwrkr)
                  (zlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1)
                   czero czero
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                         ((+ ir 1)) ((1 *)) work-%offset%)
                   ldwrkr)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zungqr m n n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq n))
                  (setf iwork (f2cl-lib:int-add itaup n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd n n
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (ir) ((1 *)) work-%offset%)
                       ldwrkr s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "Q" n n n
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (ir) ((1 *)) work-%offset%)
                       ldwrkr
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (setf irwork (f2cl-lib:int-add ie n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" n 0 n 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       cdum 1
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (ir) ((1 *)) work-%offset%)
                       ldwrkr cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14))
                  (zgemm "N" "N" m n n cone a lda
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                         ((1 *)) work-%offset%)
                   ldwrkr czero u ldu))
                 (t (setf itau 1) (setf iwork (f2cl-lib:int-add itau n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgeqrf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "L" m n a lda u ldu)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zungqr m n n u ldu
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq n))
                  (setf iwork (f2cl-lib:int-add itaup n))
                  (zlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1)
                   czero czero
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (2 1)
                                         ((1 lda) (1 *)) a-%offset%)
                   lda)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd n n a lda s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13)
                      (zunmbr "Q" "R" "N" m n n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       u ldu
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12))
                    (setf ierr var-13))
                  (setf irwork (f2cl-lib:int-add ie n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" n 0 m 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       cdum 1 u ldu cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14)))))
               (wntvo
                (cond
                 ((>= lwork
                      (f2cl-lib:int-add (f2cl-lib:int-mul 2 n n)
                                        (f2cl-lib:int-mul 3 n)))
                  (setf iu 1)
                  (cond
                   ((>= lwork
                        (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul 2 lda n)))
                    (setf ldwrku lda)
                    (setf ir (f2cl-lib:int-add iu (f2cl-lib:int-mul ldwrku n)))
                    (setf ldwrkr lda))
                   ((>= lwork
                        (f2cl-lib:int-add wrkbl
                                          (f2cl-lib:int-mul
                                           (f2cl-lib:int-add lda n) n)))
                    (setf ldwrku lda)
                    (setf ir (f2cl-lib:int-add iu (f2cl-lib:int-mul ldwrku n)))
                    (setf ldwrkr n))
                   (t (setf ldwrku n)
                    (setf ir (f2cl-lib:int-add iu (f2cl-lib:int-mul ldwrku n)))
                    (setf ldwrkr n)))
                  (setf itau (f2cl-lib:int-add ir (f2cl-lib:int-mul ldwrkr n)))
                  (setf iwork (f2cl-lib:int-add itau n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgeqrf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "U" n n a lda
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku)
                  (zlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1)
                   czero czero
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                         ((+ iu 1)) ((1 *)) work-%offset%)
                   ldwrku)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zungqr m n n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq n))
                  (setf iwork (f2cl-lib:int-add itaup n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd n n
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (zlacpy "U" n n
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                         ((1 *)) work-%offset%)
                   ldwrkr)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "Q" n n n
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "P" n n n
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (ir) ((1 *)) work-%offset%)
                       ldwrkr
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (setf irwork (f2cl-lib:int-add ie n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" n n n 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (ir) ((1 *)) work-%offset%)
                       ldwrkr
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14))
                  (zgemm "N" "N" m n n cone a lda
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku czero u ldu)
                  (zlacpy "F" n n
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                         ((1 *)) work-%offset%)
                   ldwrkr a lda))
                 (t (setf itau 1) (setf iwork (f2cl-lib:int-add itau n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgeqrf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "L" m n a lda u ldu)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zungqr m n n u ldu
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq n))
                  (setf iwork (f2cl-lib:int-add itaup n))
                  (zlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1)
                   czero czero
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (2 1)
                                         ((1 lda) (1 *)) a-%offset%)
                   lda)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd n n a lda s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13)
                      (zunmbr "Q" "R" "N" m n n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       u ldu
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12))
                    (setf ierr var-13))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "P" n n n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (setf irwork (f2cl-lib:int-add ie n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" n n m 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       a lda u ldu cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14)))))
               (wntvas
                (cond
                 ((>= lwork
                      (f2cl-lib:int-add (f2cl-lib:int-mul n n)
                                        (f2cl-lib:int-mul 3 n)))
                  (setf iu 1)
                  (cond
                   ((>= lwork
                        (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul lda n)))
                    (setf ldwrku lda))
                   (t (setf ldwrku n)))
                  (setf itau (f2cl-lib:int-add iu (f2cl-lib:int-mul ldwrku n)))
                  (setf iwork (f2cl-lib:int-add itau n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgeqrf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "U" n n a lda
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku)
                  (zlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1)
                   czero czero
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                         ((+ iu 1)) ((1 *)) work-%offset%)
                   ldwrku)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zungqr m n n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq n))
                  (setf iwork (f2cl-lib:int-add itaup n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd n n
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (zlacpy "U" n n
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku vt ldvt)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "Q" n n n
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "P" n n n vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (setf irwork (f2cl-lib:int-add ie n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" n n n 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14))
                  (zgemm "N" "N" m n n cone a lda
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku czero u ldu))
                 (t (setf itau 1) (setf iwork (f2cl-lib:int-add itau n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgeqrf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "L" m n a lda u ldu)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zungqr m n n u ldu
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (zlacpy "U" n n a lda vt ldvt)
                  (if (> n 1)
                      (zlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1)
                       czero czero
                       (f2cl-lib:array-slice vt-%data% f2cl-lib:complex16 (2 1)
                                             ((1 ldvt) (1 *)) vt-%offset%)
                       ldvt))
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq n))
                  (setf iwork (f2cl-lib:int-add itaup n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd n n vt ldvt s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13)
                      (zunmbr "Q" "R" "N" m n n vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       u ldu
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12))
                    (setf ierr var-13))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "P" n n n vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (setf irwork (f2cl-lib:int-add ie n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" n n m 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       vt ldvt u ldu cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14)))))))
             (wntua
              (cond
               (wntvn
                (cond
                 ((>= lwork
                      (f2cl-lib:int-add (f2cl-lib:int-mul n n)
                                        (max
                                         (the f2cl-lib:integer4
                                              (f2cl-lib:int-add n m))
                                         (the f2cl-lib:integer4
                                              (f2cl-lib:int-mul 3 n)))))
                  (setf ir 1)
                  (cond
                   ((>= lwork
                        (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul lda n)))
                    (setf ldwrkr lda))
                   (t (setf ldwrkr n)))
                  (setf itau (f2cl-lib:int-add ir (f2cl-lib:int-mul ldwrkr n)))
                  (setf iwork (f2cl-lib:int-add itau n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgeqrf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "L" m n a lda u ldu)
                  (zlacpy "U" n n a lda
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                         ((1 *)) work-%offset%)
                   ldwrkr)
                  (zlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1)
                   czero czero
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                         ((+ ir 1)) ((1 *)) work-%offset%)
                   ldwrkr)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zungqr m m n u ldu
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq n))
                  (setf iwork (f2cl-lib:int-add itaup n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd n n
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (ir) ((1 *)) work-%offset%)
                       ldwrkr s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "Q" n n n
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (ir) ((1 *)) work-%offset%)
                       ldwrkr
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (setf irwork (f2cl-lib:int-add ie n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" n 0 n 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       cdum 1
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (ir) ((1 *)) work-%offset%)
                       ldwrkr cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14))
                  (zgemm "N" "N" m n n cone u ldu
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                         ((1 *)) work-%offset%)
                   ldwrkr czero a lda)
                  (zlacpy "F" m n a lda u ldu))
                 (t (setf itau 1) (setf iwork (f2cl-lib:int-add itau n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgeqrf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "L" m n a lda u ldu)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zungqr m m n u ldu
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq n))
                  (setf iwork (f2cl-lib:int-add itaup n))
                  (zlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1)
                   czero czero
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (2 1)
                                         ((1 lda) (1 *)) a-%offset%)
                   lda)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd n n a lda s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13)
                      (zunmbr "Q" "R" "N" m n n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       u ldu
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12))
                    (setf ierr var-13))
                  (setf irwork (f2cl-lib:int-add ie n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" n 0 m 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       cdum 1 u ldu cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14)))))
               (wntvo
                (cond
                 ((>= lwork
                      (f2cl-lib:int-add (f2cl-lib:int-mul 2 n n)
                                        (max
                                         (the f2cl-lib:integer4
                                              (f2cl-lib:int-add n m))
                                         (the f2cl-lib:integer4
                                              (f2cl-lib:int-mul 3 n)))))
                  (setf iu 1)
                  (cond
                   ((>= lwork
                        (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul 2 lda n)))
                    (setf ldwrku lda)
                    (setf ir (f2cl-lib:int-add iu (f2cl-lib:int-mul ldwrku n)))
                    (setf ldwrkr lda))
                   ((>= lwork
                        (f2cl-lib:int-add wrkbl
                                          (f2cl-lib:int-mul
                                           (f2cl-lib:int-add lda n) n)))
                    (setf ldwrku lda)
                    (setf ir (f2cl-lib:int-add iu (f2cl-lib:int-mul ldwrku n)))
                    (setf ldwrkr n))
                   (t (setf ldwrku n)
                    (setf ir (f2cl-lib:int-add iu (f2cl-lib:int-mul ldwrku n)))
                    (setf ldwrkr n)))
                  (setf itau (f2cl-lib:int-add ir (f2cl-lib:int-mul ldwrkr n)))
                  (setf iwork (f2cl-lib:int-add itau n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgeqrf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "L" m n a lda u ldu)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zungqr m m n u ldu
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (zlacpy "U" n n a lda
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku)
                  (zlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1)
                   czero czero
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                         ((+ iu 1)) ((1 *)) work-%offset%)
                   ldwrku)
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq n))
                  (setf iwork (f2cl-lib:int-add itaup n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd n n
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (zlacpy "U" n n
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                         ((1 *)) work-%offset%)
                   ldwrkr)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "Q" n n n
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "P" n n n
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (ir) ((1 *)) work-%offset%)
                       ldwrkr
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (setf irwork (f2cl-lib:int-add ie n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" n n n 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (ir) ((1 *)) work-%offset%)
                       ldwrkr
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14))
                  (zgemm "N" "N" m n n cone u ldu
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku czero a lda)
                  (zlacpy "F" m n a lda u ldu)
                  (zlacpy "F" n n
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                         ((1 *)) work-%offset%)
                   ldwrkr a lda))
                 (t (setf itau 1) (setf iwork (f2cl-lib:int-add itau n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgeqrf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "L" m n a lda u ldu)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zungqr m m n u ldu
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq n))
                  (setf iwork (f2cl-lib:int-add itaup n))
                  (zlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1)
                   czero czero
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (2 1)
                                         ((1 lda) (1 *)) a-%offset%)
                   lda)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd n n a lda s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13)
                      (zunmbr "Q" "R" "N" m n n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       u ldu
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12))
                    (setf ierr var-13))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "P" n n n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (setf irwork (f2cl-lib:int-add ie n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" n n m 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       a lda u ldu cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14)))))
               (wntvas
                (cond
                 ((>= lwork
                      (f2cl-lib:int-add (f2cl-lib:int-mul n n)
                                        (max
                                         (the f2cl-lib:integer4
                                              (f2cl-lib:int-add n m))
                                         (the f2cl-lib:integer4
                                              (f2cl-lib:int-mul 3 n)))))
                  (setf iu 1)
                  (cond
                   ((>= lwork
                        (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul lda n)))
                    (setf ldwrku lda))
                   (t (setf ldwrku n)))
                  (setf itau (f2cl-lib:int-add iu (f2cl-lib:int-mul ldwrku n)))
                  (setf iwork (f2cl-lib:int-add itau n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgeqrf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "L" m n a lda u ldu)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zungqr m m n u ldu
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (zlacpy "U" n n a lda
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku)
                  (zlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1)
                   czero czero
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                         ((+ iu 1)) ((1 *)) work-%offset%)
                   ldwrku)
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq n))
                  (setf iwork (f2cl-lib:int-add itaup n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd n n
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (zlacpy "U" n n
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku vt ldvt)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "Q" n n n
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "P" n n n vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (setf irwork (f2cl-lib:int-add ie n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" n n n 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14))
                  (zgemm "N" "N" m n n cone u ldu
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku czero a lda)
                  (zlacpy "F" m n a lda u ldu))
                 (t (setf itau 1) (setf iwork (f2cl-lib:int-add itau n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgeqrf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "L" m n a lda u ldu)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zungqr m m n u ldu
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (zlacpy "U" n n a lda vt ldvt)
                  (if (> n 1)
                      (zlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1)
                       czero czero
                       (f2cl-lib:array-slice vt-%data% f2cl-lib:complex16 (2 1)
                                             ((1 ldvt) (1 *)) vt-%offset%)
                       ldvt))
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq n))
                  (setf iwork (f2cl-lib:int-add itaup n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd n n vt ldvt s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13)
                      (zunmbr "Q" "R" "N" m n n vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       u ldu
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12))
                    (setf ierr var-13))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "P" n n n vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (setf irwork (f2cl-lib:int-add ie n))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" n n m 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       vt ldvt u ldu cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14)))))))))
           (t (setf ie 1) (setf itauq 1)
            (setf itaup (f2cl-lib:int-add itauq n))
            (setf iwork (f2cl-lib:int-add itaup n))
            (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10)
                (zgebrd m n a lda s
                 (f2cl-lib:array-slice rwork-%data% double-float (ie) ((1 *))
                                       rwork-%offset%)
                 (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (itauq)
                                       ((1 *)) work-%offset%)
                 (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (itaup)
                                       ((1 *)) work-%offset%)
                 (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iwork)
                                       ((1 *)) work-%offset%)
                 (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
              (declare
               (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                var-9))
              (setf ierr var-10))
            (cond
             (wntuas (zlacpy "L" m n a lda u ldu)
              (if wntus
                  (setf ncu n))
              (if wntua
                  (setf ncu m))
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
                  (zungbr "Q" m ncu n u ldu
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (itauq)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iwork)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                  var-8))
                (setf ierr var-9))))
            (cond
             (wntvas (zlacpy "U" n n a lda vt ldvt)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
                  (zungbr "P" n n n vt ldvt
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (itaup)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iwork)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                  var-8))
                (setf ierr var-9))))
            (cond
             (wntuo
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
                  (zungbr "Q" m n n a lda
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (itauq)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iwork)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                  var-8))
                (setf ierr var-9))))
            (cond
             (wntvo
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
                  (zungbr "P" n n n a lda
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (itaup)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iwork)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                  var-8))
                (setf ierr var-9))))
            (setf irwork (f2cl-lib:int-add ie n))
            (if (or wntuas wntuo)
                (setf nru m))
            (if wntun
                (setf nru 0))
            (if (or wntvas wntvo)
                (setf ncvt n))
            (if wntvn
                (setf ncvt 0))
            (cond
             ((and (not wntuo) (not wntvo))
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                   var-10 var-11 var-12 var-13 var-14)
                  (zbdsqr "U" n ncvt nru 0 s
                   (f2cl-lib:array-slice rwork-%data% double-float (ie) ((1 *))
                                         rwork-%offset%)
                   vt ldvt u ldu cdum 1
                   (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                         ((1 *)) rwork-%offset%)
                   info)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                  var-9 var-10 var-11 var-12 var-13))
                (setf info var-14)))
             ((and (not wntuo) wntvo)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                   var-10 var-11 var-12 var-13 var-14)
                  (zbdsqr "U" n ncvt nru 0 s
                   (f2cl-lib:array-slice rwork-%data% double-float (ie) ((1 *))
                                         rwork-%offset%)
                   a lda u ldu cdum 1
                   (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                         ((1 *)) rwork-%offset%)
                   info)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                  var-9 var-10 var-11 var-12 var-13))
                (setf info var-14)))
             (t
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                   var-10 var-11 var-12 var-13 var-14)
                  (zbdsqr "U" n ncvt nru 0 s
                   (f2cl-lib:array-slice rwork-%data% double-float (ie) ((1 *))
                                         rwork-%offset%)
                   vt ldvt a lda cdum 1
                   (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                         ((1 *)) rwork-%offset%)
                   info)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                  var-9 var-10 var-11 var-12 var-13))
                (setf info var-14)))))))
         (t
          (cond
           ((>= n mnthr)
            (cond
             (wntvn (setf itau 1) (setf iwork (f2cl-lib:int-add itau m))
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                  (zgelqf m n a lda
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (itau)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iwork)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                (setf ierr var-7))
              (zlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1) czero
               czero
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (1 2)
                                     ((1 lda) (1 *)) a-%offset%)
               lda)
              (setf ie 1) (setf itauq 1)
              (setf itaup (f2cl-lib:int-add itauq m))
              (setf iwork (f2cl-lib:int-add itaup m))
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                   var-10)
                  (zgebrd m m a lda s
                   (f2cl-lib:array-slice rwork-%data% double-float (ie) ((1 *))
                                         rwork-%offset%)
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (itauq)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (itaup)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iwork)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                  var-9))
                (setf ierr var-10))
              (cond
               ((or wntuo wntuas)
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9)
                    (zungbr "Q" m m m a lda
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itauq) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8))
                  (setf ierr var-9))))
              (setf irwork (f2cl-lib:int-add ie m)) (setf nru 0)
              (if (or wntuo wntuas)
                  (setf nru m))
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                   var-10 var-11 var-12 var-13 var-14)
                  (zbdsqr "U" m 0 nru 0 s
                   (f2cl-lib:array-slice rwork-%data% double-float (ie) ((1 *))
                                         rwork-%offset%)
                   cdum 1 a lda cdum 1
                   (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                         ((1 *)) rwork-%offset%)
                   info)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                  var-9 var-10 var-11 var-12 var-13))
                (setf info var-14))
              (if wntuas
                  (zlacpy "F" m m a lda u ldu)))
             ((and wntvo wntun)
              (cond
               ((>= lwork
                    (f2cl-lib:int-add (f2cl-lib:int-mul m m)
                                      (f2cl-lib:int-mul 3 m)))
                (setf ir 1)
                (cond
                 ((>= lwork
                      (f2cl-lib:int-add
                       (max (the f2cl-lib:integer4 wrkbl)
                            (the f2cl-lib:integer4 (f2cl-lib:int-mul lda n)))
                       (f2cl-lib:int-mul lda m)))
                  (setf ldwrku lda) (setf chunk n) (setf ldwrkr lda))
                 ((>= lwork
                      (f2cl-lib:int-add
                       (max (the f2cl-lib:integer4 wrkbl)
                            (the f2cl-lib:integer4 (f2cl-lib:int-mul lda n)))
                       (f2cl-lib:int-mul m m)))
                  (setf ldwrku lda) (setf chunk n) (setf ldwrkr m))
                 (t (setf ldwrku m)
                  (setf chunk
                          (the f2cl-lib:integer4
                               (truncate (- lwork (* m m)) m)))
                  (setf ldwrkr m)))
                (setf itau (f2cl-lib:int-add ir (f2cl-lib:int-mul ldwrkr m)))
                (setf iwork (f2cl-lib:int-add itau m))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                    (zgelqf m n a lda
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itau) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                  (setf ierr var-7))
                (zlacpy "L" m m a lda
                 (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                       ((1 *)) work-%offset%)
                 ldwrkr)
                (zlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1) czero
                 czero
                 (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                       ((+ ir ldwrkr)) ((1 *)) work-%offset%)
                 ldwrkr)
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                    (zunglq m n m a lda
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itau) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                  (setf ierr var-8))
                (setf ie 1) (setf itauq itau)
                (setf itaup (f2cl-lib:int-add itauq m))
                (setf iwork (f2cl-lib:int-add itaup m))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10)
                    (zgebrd m m
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                           ((1 *)) work-%offset%)
                     ldwrkr s
                     (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                           ((1 *)) rwork-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itauq) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itaup) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8 var-9))
                  (setf ierr var-10))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9)
                    (zungbr "P" m m m
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                           ((1 *)) work-%offset%)
                     ldwrkr
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itaup) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8))
                  (setf ierr var-9))
                (setf irwork (f2cl-lib:int-add ie m))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14)
                    (zbdsqr "U" m m 0 0 s
                     (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                           ((1 *)) rwork-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                           ((1 *)) work-%offset%)
                     ldwrkr cdum 1 cdum 1
                     (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                           ((1 *)) rwork-%offset%)
                     info)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8 var-9 var-10 var-11 var-12 var-13))
                  (setf info var-14))
                (setf iu itauq)
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i chunk))
                              ((> i n) nil)
                  (tagbody
                    (setf blk
                            (min
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1))
                             (the f2cl-lib:integer4 chunk)))
                    (zgemm "N" "N" m blk m cone
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                           ((1 *)) work-%offset%)
                     ldwrkr
                     (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (1 i)
                                           ((1 lda) (1 *)) a-%offset%)
                     lda czero
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                           ((1 *)) work-%offset%)
                     ldwrku)
                    (zlacpy "F" m blk
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                           ((1 *)) work-%offset%)
                     ldwrku
                     (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (1 i)
                                           ((1 lda) (1 *)) a-%offset%)
                     lda)
                   label30)))
               (t (setf ie 1) (setf itauq 1)
                (setf itaup (f2cl-lib:int-add itauq m))
                (setf iwork (f2cl-lib:int-add itaup m))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10)
                    (zgebrd m n a lda s
                     (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                           ((1 *)) rwork-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itauq) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itaup) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8 var-9))
                  (setf ierr var-10))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9)
                    (zungbr "P" m n m a lda
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itaup) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8))
                  (setf ierr var-9))
                (setf irwork (f2cl-lib:int-add ie m))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14)
                    (zbdsqr "L" m n 0 0 s
                     (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                           ((1 *)) rwork-%offset%)
                     a lda cdum 1 cdum 1
                     (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                           ((1 *)) rwork-%offset%)
                     info)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8 var-9 var-10 var-11 var-12 var-13))
                  (setf info var-14)))))
             ((and wntvo wntuas)
              (cond
               ((>= lwork
                    (f2cl-lib:int-add (f2cl-lib:int-mul m m)
                                      (f2cl-lib:int-mul 3 m)))
                (setf ir 1)
                (cond
                 ((>= lwork
                      (f2cl-lib:int-add
                       (max (the f2cl-lib:integer4 wrkbl)
                            (the f2cl-lib:integer4 (f2cl-lib:int-mul lda n)))
                       (f2cl-lib:int-mul lda m)))
                  (setf ldwrku lda) (setf chunk n) (setf ldwrkr lda))
                 ((>= lwork
                      (f2cl-lib:int-add
                       (max (the f2cl-lib:integer4 wrkbl)
                            (the f2cl-lib:integer4 (f2cl-lib:int-mul lda n)))
                       (f2cl-lib:int-mul m m)))
                  (setf ldwrku lda) (setf chunk n) (setf ldwrkr m))
                 (t (setf ldwrku m)
                  (setf chunk
                          (the f2cl-lib:integer4
                               (truncate (- lwork (* m m)) m)))
                  (setf ldwrkr m)))
                (setf itau (f2cl-lib:int-add ir (f2cl-lib:int-mul ldwrkr m)))
                (setf iwork (f2cl-lib:int-add itau m))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                    (zgelqf m n a lda
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itau) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                  (setf ierr var-7))
                (zlacpy "L" m m a lda u ldu)
                (zlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1) czero
                 czero
                 (f2cl-lib:array-slice u-%data% f2cl-lib:complex16 (1 2)
                                       ((1 ldu) (1 *)) u-%offset%)
                 ldu)
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                    (zunglq m n m a lda
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itau) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                  (setf ierr var-8))
                (setf ie 1) (setf itauq itau)
                (setf itaup (f2cl-lib:int-add itauq m))
                (setf iwork (f2cl-lib:int-add itaup m))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10)
                    (zgebrd m m u ldu s
                     (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                           ((1 *)) rwork-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itauq) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itaup) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8 var-9))
                  (setf ierr var-10))
                (zlacpy "U" m m u ldu
                 (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                       ((1 *)) work-%offset%)
                 ldwrkr)
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9)
                    (zungbr "P" m m m
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                           ((1 *)) work-%offset%)
                     ldwrkr
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itaup) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8))
                  (setf ierr var-9))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9)
                    (zungbr "Q" m m m u ldu
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itauq) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8))
                  (setf ierr var-9))
                (setf irwork (f2cl-lib:int-add ie m))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14)
                    (zbdsqr "U" m m m 0 s
                     (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                           ((1 *)) rwork-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                           ((1 *)) work-%offset%)
                     ldwrkr u ldu cdum 1
                     (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                           ((1 *)) rwork-%offset%)
                     info)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8 var-9 var-10 var-11 var-12 var-13))
                  (setf info var-14))
                (setf iu itauq)
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i chunk))
                              ((> i n) nil)
                  (tagbody
                    (setf blk
                            (min
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1))
                             (the f2cl-lib:integer4 chunk)))
                    (zgemm "N" "N" m blk m cone
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                           ((1 *)) work-%offset%)
                     ldwrkr
                     (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (1 i)
                                           ((1 lda) (1 *)) a-%offset%)
                     lda czero
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                           ((1 *)) work-%offset%)
                     ldwrku)
                    (zlacpy "F" m blk
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                           ((1 *)) work-%offset%)
                     ldwrku
                     (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (1 i)
                                           ((1 lda) (1 *)) a-%offset%)
                     lda)
                   label40)))
               (t (setf itau 1) (setf iwork (f2cl-lib:int-add itau m))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                    (zgelqf m n a lda
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itau) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                  (setf ierr var-7))
                (zlacpy "L" m m a lda u ldu)
                (zlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1) czero
                 czero
                 (f2cl-lib:array-slice u-%data% f2cl-lib:complex16 (1 2)
                                       ((1 ldu) (1 *)) u-%offset%)
                 ldu)
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                    (zunglq m n m a lda
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itau) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                  (setf ierr var-8))
                (setf ie 1) (setf itauq itau)
                (setf itaup (f2cl-lib:int-add itauq m))
                (setf iwork (f2cl-lib:int-add itaup m))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10)
                    (zgebrd m m u ldu s
                     (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                           ((1 *)) rwork-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itauq) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itaup) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8 var-9))
                  (setf ierr var-10))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13)
                    (zunmbr "P" "L" "C" m n m u ldu
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itaup) ((1 *)) work-%offset%)
                     a lda
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8 var-9 var-10 var-11 var-12))
                  (setf ierr var-13))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9)
                    (zungbr "Q" m m m u ldu
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (itauq) ((1 *)) work-%offset%)
                     (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                           (iwork) ((1 *)) work-%offset%)
                     (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8))
                  (setf ierr var-9))
                (setf irwork (f2cl-lib:int-add ie m))
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14)
                    (zbdsqr "U" m n m 0 s
                     (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                           ((1 *)) rwork-%offset%)
                     a lda u ldu cdum 1
                     (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                           ((1 *)) rwork-%offset%)
                     info)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                    var-8 var-9 var-10 var-11 var-12 var-13))
                  (setf info var-14)))))
             (wntvs
              (cond
               (wntun
                (cond
                 ((>= lwork
                      (f2cl-lib:int-add (f2cl-lib:int-mul m m)
                                        (f2cl-lib:int-mul 3 m)))
                  (setf ir 1)
                  (cond
                   ((>= lwork
                        (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul lda m)))
                    (setf ldwrkr lda))
                   (t (setf ldwrkr m)))
                  (setf itau (f2cl-lib:int-add ir (f2cl-lib:int-mul ldwrkr m)))
                  (setf iwork (f2cl-lib:int-add itau m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgelqf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "L" m m a lda
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                         ((1 *)) work-%offset%)
                   ldwrkr)
                  (zlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1)
                   czero czero
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                         ((+ ir ldwrkr)) ((1 *)) work-%offset%)
                   ldwrkr)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zunglq m n m a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq m))
                  (setf iwork (f2cl-lib:int-add itaup m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd m m
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (ir) ((1 *)) work-%offset%)
                       ldwrkr s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "P" m m m
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (ir) ((1 *)) work-%offset%)
                       ldwrkr
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (setf irwork (f2cl-lib:int-add ie m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" m m 0 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (ir) ((1 *)) work-%offset%)
                       ldwrkr cdum 1 cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14))
                  (zgemm "N" "N" m n m cone
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                         ((1 *)) work-%offset%)
                   ldwrkr a lda czero vt ldvt))
                 (t (setf itau 1) (setf iwork (f2cl-lib:int-add itau m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgelqf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "U" m n a lda vt ldvt)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zunglq m n m vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq m))
                  (setf iwork (f2cl-lib:int-add itaup m))
                  (zlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1)
                   czero czero
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (1 2)
                                         ((1 lda) (1 *)) a-%offset%)
                   lda)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd m m a lda s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13)
                      (zunmbr "P" "L" "C" m n m a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12))
                    (setf ierr var-13))
                  (setf irwork (f2cl-lib:int-add ie m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" m n 0 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       vt ldvt cdum 1 cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14)))))
               (wntuo
                (cond
                 ((>= lwork
                      (f2cl-lib:int-add (f2cl-lib:int-mul 2 m m)
                                        (f2cl-lib:int-mul 3 m)))
                  (setf iu 1)
                  (cond
                   ((>= lwork
                        (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul 2 lda m)))
                    (setf ldwrku lda)
                    (setf ir (f2cl-lib:int-add iu (f2cl-lib:int-mul ldwrku m)))
                    (setf ldwrkr lda))
                   ((>= lwork
                        (f2cl-lib:int-add wrkbl
                                          (f2cl-lib:int-mul
                                           (f2cl-lib:int-add lda m) m)))
                    (setf ldwrku lda)
                    (setf ir (f2cl-lib:int-add iu (f2cl-lib:int-mul ldwrku m)))
                    (setf ldwrkr m))
                   (t (setf ldwrku m)
                    (setf ir (f2cl-lib:int-add iu (f2cl-lib:int-mul ldwrku m)))
                    (setf ldwrkr m)))
                  (setf itau (f2cl-lib:int-add ir (f2cl-lib:int-mul ldwrkr m)))
                  (setf iwork (f2cl-lib:int-add itau m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgelqf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "L" m m a lda
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku)
                  (zlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1)
                   czero czero
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                         ((+ iu ldwrku)) ((1 *)) work-%offset%)
                   ldwrku)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zunglq m n m a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq m))
                  (setf iwork (f2cl-lib:int-add itaup m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd m m
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (zlacpy "L" m m
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                         ((1 *)) work-%offset%)
                   ldwrkr)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "P" m m m
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "Q" m m m
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (ir) ((1 *)) work-%offset%)
                       ldwrkr
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (setf irwork (f2cl-lib:int-add ie m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" m m m 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (ir) ((1 *)) work-%offset%)
                       ldwrkr cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14))
                  (zgemm "N" "N" m n m cone
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku a lda czero vt ldvt)
                  (zlacpy "F" m m
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                         ((1 *)) work-%offset%)
                   ldwrkr a lda))
                 (t (setf itau 1) (setf iwork (f2cl-lib:int-add itau m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgelqf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "U" m n a lda vt ldvt)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zunglq m n m vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq m))
                  (setf iwork (f2cl-lib:int-add itaup m))
                  (zlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1)
                   czero czero
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (1 2)
                                         ((1 lda) (1 *)) a-%offset%)
                   lda)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd m m a lda s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13)
                      (zunmbr "P" "L" "C" m n m a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12))
                    (setf ierr var-13))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "Q" m m m a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (setf irwork (f2cl-lib:int-add ie m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" m n m 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       vt ldvt a lda cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14)))))
               (wntuas
                (cond
                 ((>= lwork
                      (f2cl-lib:int-add (f2cl-lib:int-mul m m)
                                        (f2cl-lib:int-mul 3 m)))
                  (setf iu 1)
                  (cond
                   ((>= lwork
                        (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul lda m)))
                    (setf ldwrku lda))
                   (t (setf ldwrku m)))
                  (setf itau (f2cl-lib:int-add iu (f2cl-lib:int-mul ldwrku m)))
                  (setf iwork (f2cl-lib:int-add itau m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgelqf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "L" m m a lda
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku)
                  (zlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1)
                   czero czero
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                         ((+ iu ldwrku)) ((1 *)) work-%offset%)
                   ldwrku)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zunglq m n m a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq m))
                  (setf iwork (f2cl-lib:int-add itaup m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd m m
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (zlacpy "L" m m
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku u ldu)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "P" m m m
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "Q" m m m u ldu
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (setf irwork (f2cl-lib:int-add ie m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" m m m 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku u ldu cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14))
                  (zgemm "N" "N" m n m cone
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku a lda czero vt ldvt))
                 (t (setf itau 1) (setf iwork (f2cl-lib:int-add itau m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgelqf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "U" m n a lda vt ldvt)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zunglq m n m vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (zlacpy "L" m m a lda u ldu)
                  (zlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1)
                   czero czero
                   (f2cl-lib:array-slice u-%data% f2cl-lib:complex16 (1 2)
                                         ((1 ldu) (1 *)) u-%offset%)
                   ldu)
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq m))
                  (setf iwork (f2cl-lib:int-add itaup m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd m m u ldu s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13)
                      (zunmbr "P" "L" "C" m n m u ldu
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12))
                    (setf ierr var-13))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "Q" m m m u ldu
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (setf irwork (f2cl-lib:int-add ie m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" m n m 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       vt ldvt u ldu cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14)))))))
             (wntva
              (cond
               (wntun
                (cond
                 ((>= lwork
                      (f2cl-lib:int-add (f2cl-lib:int-mul m m)
                                        (max
                                         (the f2cl-lib:integer4
                                              (f2cl-lib:int-add n m))
                                         (the f2cl-lib:integer4
                                              (f2cl-lib:int-mul 3 m)))))
                  (setf ir 1)
                  (cond
                   ((>= lwork
                        (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul lda m)))
                    (setf ldwrkr lda))
                   (t (setf ldwrkr m)))
                  (setf itau (f2cl-lib:int-add ir (f2cl-lib:int-mul ldwrkr m)))
                  (setf iwork (f2cl-lib:int-add itau m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgelqf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "U" m n a lda vt ldvt)
                  (zlacpy "L" m m a lda
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                         ((1 *)) work-%offset%)
                   ldwrkr)
                  (zlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1)
                   czero czero
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                         ((+ ir ldwrkr)) ((1 *)) work-%offset%)
                   ldwrkr)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zunglq n n m vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq m))
                  (setf iwork (f2cl-lib:int-add itaup m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd m m
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (ir) ((1 *)) work-%offset%)
                       ldwrkr s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "P" m m m
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (ir) ((1 *)) work-%offset%)
                       ldwrkr
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (setf irwork (f2cl-lib:int-add ie m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" m m 0 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (ir) ((1 *)) work-%offset%)
                       ldwrkr cdum 1 cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14))
                  (zgemm "N" "N" m n m cone
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                         ((1 *)) work-%offset%)
                   ldwrkr vt ldvt czero a lda)
                  (zlacpy "F" m n a lda vt ldvt))
                 (t (setf itau 1) (setf iwork (f2cl-lib:int-add itau m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgelqf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "U" m n a lda vt ldvt)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zunglq n n m vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq m))
                  (setf iwork (f2cl-lib:int-add itaup m))
                  (zlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1)
                   czero czero
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (1 2)
                                         ((1 lda) (1 *)) a-%offset%)
                   lda)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd m m a lda s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13)
                      (zunmbr "P" "L" "C" m n m a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12))
                    (setf ierr var-13))
                  (setf irwork (f2cl-lib:int-add ie m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" m n 0 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       vt ldvt cdum 1 cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14)))))
               (wntuo
                (cond
                 ((>= lwork
                      (f2cl-lib:int-add (f2cl-lib:int-mul 2 m m)
                                        (max
                                         (the f2cl-lib:integer4
                                              (f2cl-lib:int-add n m))
                                         (the f2cl-lib:integer4
                                              (f2cl-lib:int-mul 3 m)))))
                  (setf iu 1)
                  (cond
                   ((>= lwork
                        (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul 2 lda m)))
                    (setf ldwrku lda)
                    (setf ir (f2cl-lib:int-add iu (f2cl-lib:int-mul ldwrku m)))
                    (setf ldwrkr lda))
                   ((>= lwork
                        (f2cl-lib:int-add wrkbl
                                          (f2cl-lib:int-mul
                                           (f2cl-lib:int-add lda m) m)))
                    (setf ldwrku lda)
                    (setf ir (f2cl-lib:int-add iu (f2cl-lib:int-mul ldwrku m)))
                    (setf ldwrkr m))
                   (t (setf ldwrku m)
                    (setf ir (f2cl-lib:int-add iu (f2cl-lib:int-mul ldwrku m)))
                    (setf ldwrkr m)))
                  (setf itau (f2cl-lib:int-add ir (f2cl-lib:int-mul ldwrkr m)))
                  (setf iwork (f2cl-lib:int-add itau m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgelqf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "U" m n a lda vt ldvt)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zunglq n n m vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (zlacpy "L" m m a lda
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku)
                  (zlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1)
                   czero czero
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                         ((+ iu ldwrku)) ((1 *)) work-%offset%)
                   ldwrku)
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq m))
                  (setf iwork (f2cl-lib:int-add itaup m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd m m
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (zlacpy "L" m m
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                         ((1 *)) work-%offset%)
                   ldwrkr)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "P" m m m
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "Q" m m m
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (ir) ((1 *)) work-%offset%)
                       ldwrkr
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (setf irwork (f2cl-lib:int-add ie m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" m m m 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (ir) ((1 *)) work-%offset%)
                       ldwrkr cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14))
                  (zgemm "N" "N" m n m cone
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku vt ldvt czero a lda)
                  (zlacpy "F" m n a lda vt ldvt)
                  (zlacpy "F" m m
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (ir)
                                         ((1 *)) work-%offset%)
                   ldwrkr a lda))
                 (t (setf itau 1) (setf iwork (f2cl-lib:int-add itau m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgelqf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "U" m n a lda vt ldvt)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zunglq n n m vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq m))
                  (setf iwork (f2cl-lib:int-add itaup m))
                  (zlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1)
                   czero czero
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (1 2)
                                         ((1 lda) (1 *)) a-%offset%)
                   lda)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd m m a lda s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13)
                      (zunmbr "P" "L" "C" m n m a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12))
                    (setf ierr var-13))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "Q" m m m a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (setf irwork (f2cl-lib:int-add ie m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" m n m 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       vt ldvt a lda cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14)))))
               (wntuas
                (cond
                 ((>= lwork
                      (f2cl-lib:int-add (f2cl-lib:int-mul m m)
                                        (max
                                         (the f2cl-lib:integer4
                                              (f2cl-lib:int-add n m))
                                         (the f2cl-lib:integer4
                                              (f2cl-lib:int-mul 3 m)))))
                  (setf iu 1)
                  (cond
                   ((>= lwork
                        (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul lda m)))
                    (setf ldwrku lda))
                   (t (setf ldwrku m)))
                  (setf itau (f2cl-lib:int-add iu (f2cl-lib:int-mul ldwrku m)))
                  (setf iwork (f2cl-lib:int-add itau m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgelqf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "U" m n a lda vt ldvt)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zunglq n n m vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (zlacpy "L" m m a lda
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku)
                  (zlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1)
                   czero czero
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                         ((+ iu ldwrku)) ((1 *)) work-%offset%)
                   ldwrku)
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq m))
                  (setf iwork (f2cl-lib:int-add itaup m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd m m
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (zlacpy "L" m m
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku u ldu)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "P" m m m
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "Q" m m m u ldu
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (setf irwork (f2cl-lib:int-add ie m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" m m m 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iu) ((1 *)) work-%offset%)
                       ldwrku u ldu cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14))
                  (zgemm "N" "N" m n m cone
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iu)
                                         ((1 *)) work-%offset%)
                   ldwrku vt ldvt czero a lda)
                  (zlacpy "F" m n a lda vt ldvt))
                 (t (setf itau 1) (setf iwork (f2cl-lib:int-add itau m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (zgelqf m n a lda
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                    (setf ierr var-7))
                  (zlacpy "U" m n a lda vt ldvt)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                      (zunglq n n m vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itau) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                    (setf ierr var-8))
                  (zlacpy "L" m m a lda u ldu)
                  (zlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1)
                   czero czero
                   (f2cl-lib:array-slice u-%data% f2cl-lib:complex16 (1 2)
                                         ((1 ldu) (1 *)) u-%offset%)
                   ldu)
                  (setf ie 1) (setf itauq itau)
                  (setf itaup (f2cl-lib:int-add itauq m))
                  (setf iwork (f2cl-lib:int-add itaup m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                      (zgebrd m m u ldu s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9))
                    (setf ierr var-10))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13)
                      (zunmbr "P" "L" "C" m n m u ldu
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itaup) ((1 *)) work-%offset%)
                       vt ldvt
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12))
                    (setf ierr var-13))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                      (zungbr "Q" m m m u ldu
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (itauq) ((1 *)) work-%offset%)
                       (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                             (iwork) ((1 *)) work-%offset%)
                       (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                       ierr)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8))
                    (setf ierr var-9))
                  (setf irwork (f2cl-lib:int-add ie m))
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11 var-12 var-13 var-14)
                      (zbdsqr "U" m n m 0 s
                       (f2cl-lib:array-slice rwork-%data% double-float (ie)
                                             ((1 *)) rwork-%offset%)
                       vt ldvt u ldu cdum 1
                       (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                             ((1 *)) rwork-%offset%)
                       info)
                    (declare
                     (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                      var-8 var-9 var-10 var-11 var-12 var-13))
                    (setf info var-14)))))))))
           (t (setf ie 1) (setf itauq 1)
            (setf itaup (f2cl-lib:int-add itauq m))
            (setf iwork (f2cl-lib:int-add itaup m))
            (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10)
                (zgebrd m n a lda s
                 (f2cl-lib:array-slice rwork-%data% double-float (ie) ((1 *))
                                       rwork-%offset%)
                 (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (itauq)
                                       ((1 *)) work-%offset%)
                 (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (itaup)
                                       ((1 *)) work-%offset%)
                 (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iwork)
                                       ((1 *)) work-%offset%)
                 (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
              (declare
               (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                var-9))
              (setf ierr var-10))
            (cond
             (wntuas (zlacpy "L" m m a lda u ldu)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
                  (zungbr "Q" m m n u ldu
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (itauq)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iwork)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                  var-8))
                (setf ierr var-9))))
            (cond
             (wntvas (zlacpy "U" m n a lda vt ldvt)
              (if wntva
                  (setf nrvt n))
              (if wntvs
                  (setf nrvt m))
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
                  (zungbr "P" nrvt n m vt ldvt
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (itaup)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iwork)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                  var-8))
                (setf ierr var-9))))
            (cond
             (wntuo
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
                  (zungbr "Q" m m n a lda
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (itauq)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iwork)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                  var-8))
                (setf ierr var-9))))
            (cond
             (wntvo
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
                  (zungbr "P" m n m a lda
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (itaup)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (iwork)
                                         ((1 *)) work-%offset%)
                   (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                  var-8))
                (setf ierr var-9))))
            (setf irwork (f2cl-lib:int-add ie m))
            (if (or wntuas wntuo)
                (setf nru m))
            (if wntun
                (setf nru 0))
            (if (or wntvas wntvo)
                (setf ncvt n))
            (if wntvn
                (setf ncvt 0))
            (cond
             ((and (not wntuo) (not wntvo))
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                   var-10 var-11 var-12 var-13 var-14)
                  (zbdsqr "L" m ncvt nru 0 s
                   (f2cl-lib:array-slice rwork-%data% double-float (ie) ((1 *))
                                         rwork-%offset%)
                   vt ldvt u ldu cdum 1
                   (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                         ((1 *)) rwork-%offset%)
                   info)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                  var-9 var-10 var-11 var-12 var-13))
                (setf info var-14)))
             ((and (not wntuo) wntvo)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                   var-10 var-11 var-12 var-13 var-14)
                  (zbdsqr "L" m ncvt nru 0 s
                   (f2cl-lib:array-slice rwork-%data% double-float (ie) ((1 *))
                                         rwork-%offset%)
                   a lda u ldu cdum 1
                   (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                         ((1 *)) rwork-%offset%)
                   info)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                  var-9 var-10 var-11 var-12 var-13))
                (setf info var-14)))
             (t
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                   var-10 var-11 var-12 var-13 var-14)
                  (zbdsqr "L" m ncvt nru 0 s
                   (f2cl-lib:array-slice rwork-%data% double-float (ie) ((1 *))
                                         rwork-%offset%)
                   vt ldvt a lda cdum 1
                   (f2cl-lib:array-slice rwork-%data% double-float (irwork)
                                         ((1 *)) rwork-%offset%)
                   info)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                  var-9 var-10 var-11 var-12 var-13))
                (setf info var-14))))))))
        (cond
         ((= iscl 1)
          (if (> anrm bignum)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
                  (dlascl "G" 0 0 bignum anrm minmn 1 s minmn ierr)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                  var-8))
                (setf ierr var-9)))
          (if (and (/= info 0) (> anrm bignum))
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
                  (dlascl "G" 0 0 bignum anrm (f2cl-lib:int-sub minmn 1) 1
                   (f2cl-lib:array-slice rwork-%data% double-float (ie) ((1 *))
                                         rwork-%offset%)
                   minmn ierr)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                  var-8))
                (setf ierr var-9)))
          (if (< anrm smlnum)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
                  (dlascl "G" 0 0 smlnum anrm minmn 1 s minmn ierr)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                  var-8))
                (setf ierr var-9)))
          (if (and (/= info 0) (< anrm smlnum))
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
                  (dlascl "G" 0 0 smlnum anrm (f2cl-lib:int-sub minmn 1) 1
                   (f2cl-lib:array-slice rwork-%data% double-float (ie) ((1 *))
                                         rwork-%offset%)
                   minmn ierr)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                  var-8))
                (setf ierr var-9)))))
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                (coerce maxwrk 'f2cl-lib:complex16))
        (go end_label)
       end_label
        (return
         (values nil nil nil nil nil nil nil nil nil nil nil nil nil nil
                 info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zgesvd
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((string 1) (string 1)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil nil nil nil
                                              nil nil nil nil nil nil
                                              fortran-to-lisp::info)
                                            :calls
                                            '(fortran-to-lisp::dlascl
                                              fortran-to-lisp::zunmbr
                                              fortran-to-lisp::zgemm
                                              fortran-to-lisp::zlacpy
                                              fortran-to-lisp::zbdsqr
                                              fortran-to-lisp::zlaset
                                              fortran-to-lisp::zlascl
                                              fortran-to-lisp::zlange
                                              fortran-to-lisp::dlamch
                                              fortran-to-lisp::xerbla
                                              fortran-to-lisp::zunglq
                                              fortran-to-lisp::zgelqf
                                              fortran-to-lisp::zungbr
                                              fortran-to-lisp::zgebrd
                                              fortran-to-lisp::zungqr
                                              fortran-to-lisp::zgeqrf
                                              fortran-to-lisp::ilaenv
                                              fortran-to-lisp::lsame))))

