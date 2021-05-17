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


(let* ((two 2.0d0) (one 1.0d0) (zero 0.0d0))
  (declare (type (double-float 2.0d0 2.0d0) two)
           (type (double-float 1.0d0 1.0d0) one)
           (type (double-float 0.0d0 0.0d0) zero)
           (ignorable two one zero))
  (defun zlarfgp (n alpha x incx tau)
    (declare (type (array f2cl-lib:complex16 (*)) x)
             (type (f2cl-lib:complex16) tau alpha)
             (type (f2cl-lib:integer4) incx n))
    (f2cl-lib:with-multi-array-data
        ((x f2cl-lib:complex16 x-%data% x-%offset%))
      (prog ((savealpha #C(0.0d0 0.0d0)) (alphi 0.0d0) (alphr 0.0d0)
             (beta 0.0d0) (bignum 0.0d0) (smlnum 0.0d0) (xnorm 0.0d0) (j 0)
             (knt 0))
        (declare (type (f2cl-lib:complex16) savealpha)
                 (type (double-float) alphi alphr beta bignum smlnum xnorm)
                 (type (f2cl-lib:integer4) j knt))
        (cond
         ((<= n 0) (setf tau (coerce zero 'f2cl-lib:complex16))
          (go end_label)))
        (setf xnorm (dznrm2 (f2cl-lib:int-sub n 1) x incx))
        (setf alphr (f2cl-lib:dble alpha))
        (setf alphi (f2cl-lib:dimag alpha))
        (cond
         ((= xnorm zero)
          (cond
           ((= alphi zero)
            (cond
             ((>= alphr zero) (setf tau (coerce zero 'f2cl-lib:complex16)))
             (t (setf tau (coerce two 'f2cl-lib:complex16))
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j (f2cl-lib:int-add n (f2cl-lib:int-sub 1)))
                             nil)
                (tagbody
                  (setf (f2cl-lib:fref x-%data%
                                       ((f2cl-lib:int-add 1
                                                          (f2cl-lib:int-mul
                                                           (f2cl-lib:int-sub j
                                                                             1)
                                                           incx)))
                                       ((1 *)) x-%offset%)
                          (coerce zero 'f2cl-lib:complex16))
                 label100000))
              (setf alpha (- alpha)))))
           (t (setf xnorm (dlapy2 alphr alphi))
            (setf tau
                    (f2cl-lib:dcmplx (+ one (/ (- alphr) xnorm))
                                     (/ (- alphi) xnorm)))
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j (f2cl-lib:int-add n (f2cl-lib:int-sub 1))) nil)
              (tagbody
                (setf (f2cl-lib:fref x-%data%
                                     ((f2cl-lib:int-add 1
                                                        (f2cl-lib:int-mul
                                                         (f2cl-lib:int-sub j 1)
                                                         incx)))
                                     ((1 *)) x-%offset%)
                        (coerce zero 'f2cl-lib:complex16))
               label100001))
            (setf alpha (coerce xnorm 'f2cl-lib:complex16)))))
         (t (setf beta (f2cl-lib:sign (dlapy3 alphr alphi xnorm) alphr))
          (setf smlnum (/ (dlamch "S") (dlamch "E")))
          (setf bignum (/ one smlnum)) (setf knt 0)
          (cond
           ((< (abs beta) smlnum)
            (tagbody
             label10
              (setf knt (f2cl-lib:int-add knt 1))
              (zdscal (f2cl-lib:int-sub n 1) bignum x incx)
              (setf beta (* beta bignum))
              (setf alphi (* alphi bignum))
              (setf alphr (* alphr bignum))
              (if (< (abs beta) smlnum)
                  (go label10))
              (setf xnorm (dznrm2 (f2cl-lib:int-sub n 1) x incx))
              (setf alpha (f2cl-lib:dcmplx alphr alphi))
              (setf beta (f2cl-lib:sign (dlapy3 alphr alphi xnorm) alphr)))))
          (setf savealpha alpha) (setf alpha (+ alpha beta))
          (cond
           ((< beta zero) (setf beta (- beta)) (setf tau (/ (- alpha) beta)))
           (t (setf alphr (* alphi (/ alphi (f2cl-lib:dble alpha))))
            (setf alphr (+ alphr (* xnorm (/ xnorm (f2cl-lib:dble alpha)))))
            (setf tau (f2cl-lib:dcmplx (/ alphr beta) (/ (- alphi) beta)))
            (setf alpha (f2cl-lib:dcmplx (- alphr) alphi))))
          (setf alpha (zladiv (f2cl-lib:dcmplx one) alpha))
          (cond
           ((<= (abs tau) smlnum) (setf alphr (f2cl-lib:dble savealpha))
            (setf alphi (f2cl-lib:dimag savealpha))
            (cond
             ((= alphi zero)
              (cond
               ((>= alphr zero) (setf tau (coerce zero 'f2cl-lib:complex16)))
               (t (setf tau (coerce two 'f2cl-lib:complex16))
                (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                              ((> j (f2cl-lib:int-add n (f2cl-lib:int-sub 1)))
                               nil)
                  (tagbody
                    (setf (f2cl-lib:fref x-%data%
                                         ((f2cl-lib:int-add 1
                                                            (f2cl-lib:int-mul
                                                             (f2cl-lib:int-sub
                                                              j 1)
                                                             incx)))
                                         ((1 *)) x-%offset%)
                            (coerce zero 'f2cl-lib:complex16))
                   label100002))
                (setf beta (coerce (realpart (- savealpha)) 'double-float)))))
             (t (setf xnorm (dlapy2 alphr alphi))
              (setf tau
                      (f2cl-lib:dcmplx (+ one (/ (- alphr) xnorm))
                                       (/ (- alphi) xnorm)))
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j (f2cl-lib:int-add n (f2cl-lib:int-sub 1)))
                             nil)
                (tagbody
                  (setf (f2cl-lib:fref x-%data%
                                       ((f2cl-lib:int-add 1
                                                          (f2cl-lib:int-mul
                                                           (f2cl-lib:int-sub j
                                                                             1)
                                                           incx)))
                                       ((1 *)) x-%offset%)
                          (coerce zero 'f2cl-lib:complex16))
                 label100003))
              (setf beta xnorm))))
           (t (zscal (f2cl-lib:int-sub n 1) alpha x incx)))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j knt) nil)
            (tagbody (setf beta (* beta smlnum)) label20))
          (setf alpha (coerce beta 'f2cl-lib:complex16))))
        (go end_label)
       end_label
        (return (values nil alpha nil nil tau))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zlarfgp
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (fortran-to-lisp::complex16)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::complex16))
                                            :return-values
                                            '(nil fortran-to-lisp::alpha nil
                                              nil fortran-to-lisp::tau)
                                            :calls
                                            '(fortran-to-lisp::zscal
                                              fortran-to-lisp::zdscal
                                              fortran-to-lisp::dznrm2
                                              fortran-to-lisp::zladiv
                                              fortran-to-lisp::dlamch
                                              fortran-to-lisp::dlapy3
                                              fortran-to-lisp::dlapy2))))

