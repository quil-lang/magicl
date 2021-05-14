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


(let* ((zero 0.0d0) (one 1.0d0) (cone (f2cl-lib:cmplx 1.0d0 0.0d0)))
  (declare (type (double-float 0.0d0 0.0d0) zero)
           (type (double-float 1.0d0 1.0d0) one)
           (type (f2cl-lib:complex16) cone)
           (ignorable zero one cone))
  (defun zheev (jobz uplo n a lda w work lwork rwork info)
    (declare (type (array double-float (*)) rwork w)
             (type (array f2cl-lib:complex16 (*)) work a)
             (type (f2cl-lib:integer4) info lwork lda n)
             (type (string 1) uplo jobz))
    (f2cl-lib:with-multi-array-data
        ((jobz character jobz-%data% jobz-%offset%)
         (uplo character uplo-%data% uplo-%offset%)
         (a f2cl-lib:complex16 a-%data% a-%offset%)
         (work f2cl-lib:complex16 work-%data% work-%offset%)
         (w double-float w-%data% w-%offset%)
         (rwork double-float rwork-%data% rwork-%offset%))
      (prog ((anrm 0.0d0) (bignum 0.0d0) (eps 0.0d0) (rmax 0.0d0) (rmin 0.0d0)
             (safmin 0.0d0) (sigma 0.0d0) (smlnum 0.0d0) (iinfo 0) (imax 0)
             (inde 0) (indtau 0) (indwrk 0) (iscale 0) (llwork 0) (lwkopt 0)
             (nb 0) (lower nil) (lquery nil) (wantz nil))
        (declare
         (type (double-float) anrm bignum eps rmax rmin safmin sigma smlnum)
         (type (f2cl-lib:integer4) iinfo imax inde indtau indwrk iscale llwork
          lwkopt nb)
         (type f2cl-lib:logical lower lquery wantz))
        (setf wantz (lsame jobz "V"))
        (setf lower (lsame uplo "L"))
        (setf lquery (coerce (= lwork -1) 'f2cl-lib:logical))
        (setf info 0)
        (cond ((not (or wantz (lsame jobz "N"))) (setf info -1))
              ((not (or lower (lsame uplo "U"))) (setf info -2))
              ((< n 0) (setf info -3))
              ((< lda
                  (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
               (setf info -5)))
        (cond
         ((= info 0) (setf nb (ilaenv 1 "ZHETRD" uplo n -1 -1 -1))
          (setf lwkopt
                  (max (the f2cl-lib:integer4 1)
                       (the f2cl-lib:integer4
                            (f2cl-lib:int-mul (f2cl-lib:int-add nb 1) n))))
          (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                  (coerce lwkopt 'f2cl-lib:complex16))
          (if (and
               (< lwork
                  (max (the f2cl-lib:integer4 1)
                       (the f2cl-lib:integer4
                            (f2cl-lib:int-sub (f2cl-lib:int-mul 2 n) 1))))
               (not lquery))
              (setf info -8))))
        (cond
         ((/= info 0) (xerbla "ZHEEV " (f2cl-lib:int-sub info)) (go end_label))
         (lquery (go end_label)))
        (cond ((= n 0) (go end_label)))
        (cond
         ((= n 1)
          (setf (f2cl-lib:fref w-%data% (1) ((1 *)) w-%offset%)
                  (coerce
                   (realpart
                    (f2cl-lib:fref a-%data% (1 1) ((1 lda) (1 *)) a-%offset%))
                   'double-float))
          (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                  (coerce 1 'f2cl-lib:complex16))
          (if wantz
              (setf (f2cl-lib:fref a-%data% (1 1) ((1 lda) (1 *)) a-%offset%)
                      cone))
          (go end_label)))
        (setf safmin (dlamch "S"))
        (setf eps (dlamch "P"))
        (setf smlnum (/ safmin eps))
        (setf bignum (/ one smlnum))
        (setf rmin (f2cl-lib:fsqrt smlnum))
        (setf rmax (f2cl-lib:fsqrt bignum))
        (setf anrm (zlanhe "M" uplo n a lda rwork))
        (setf iscale 0)
        (cond
         ((and (> anrm zero) (< anrm rmin)) (setf iscale 1)
          (setf sigma (/ rmin anrm)))
         ((> anrm rmax) (setf iscale 1) (setf sigma (/ rmax anrm))))
        (if (= iscale 1)
            (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
                (zlascl uplo 0 0 one sigma n n a lda info)
              (declare
               (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8))
              (setf info var-9)))
        (setf inde 1)
        (setf indtau 1)
        (setf indwrk (f2cl-lib:int-add indtau n))
        (setf llwork (f2cl-lib:int-add (f2cl-lib:int-sub lwork indwrk) 1))
        (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
            (zhetrd uplo n a lda w
             (f2cl-lib:array-slice rwork-%data% double-float (inde) ((1 *))
                                   rwork-%offset%)
             (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (indtau)
                                   ((1 *)) work-%offset%)
             (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (indwrk)
                                   ((1 *)) work-%offset%)
             llwork iinfo)
          (declare
           (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8))
          (setf iinfo var-9))
        (cond
         ((not wantz)
          (multiple-value-bind (var-0 var-1 var-2 var-3)
              (dsterf n w
               (f2cl-lib:array-slice rwork-%data% double-float (inde) ((1 *))
                                     rwork-%offset%)
               info)
            (declare (ignore var-0 var-1 var-2))
            (setf info var-3)))
         (t
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
              (zungtr uplo n a lda
               (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (indtau)
                                     ((1 *)) work-%offset%)
               (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 (indwrk)
                                     ((1 *)) work-%offset%)
               llwork iinfo)
            (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
            (setf iinfo var-7))
          (setf indwrk (f2cl-lib:int-add inde n))
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
              (zsteqr jobz n w
               (f2cl-lib:array-slice rwork-%data% double-float (inde) ((1 *))
                                     rwork-%offset%)
               a lda
               (f2cl-lib:array-slice rwork-%data% double-float (indwrk) ((1 *))
                                     rwork-%offset%)
               info)
            (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
            (setf info var-7))))
        (cond
         ((= iscale 1)
          (cond ((= info 0) (setf imax n))
                (t (setf imax (f2cl-lib:int-sub info 1))))
          (dscal imax (/ one sigma) w 1)))
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                (coerce lwkopt 'f2cl-lib:complex16))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zheev fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((string 1) (string 1)
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil nil nil nil
                                              nil fortran-to-lisp::info)
                                            :calls
                                            '(fortran-to-lisp::dscal
                                              fortran-to-lisp::zsteqr
                                              fortran-to-lisp::zungtr
                                              fortran-to-lisp::dsterf
                                              fortran-to-lisp::zhetrd
                                              fortran-to-lisp::zlascl
                                              fortran-to-lisp::zlanhe
                                              fortran-to-lisp::dlamch
                                              fortran-to-lisp::xerbla
                                              fortran-to-lisp::ilaenv
                                              fortran-to-lisp::lsame))))

