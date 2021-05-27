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


(let ((zero 0.0d0)
      (one 1.0d0)
      (two 2.0d0)
      (gam 4096.0d0)
      (gamsq 1.6777216d7)
      (rgamsq 5.9604645d-8))
  (declare (type (double-float) zero one two gam gamsq rgamsq))
  (defun drotmg (dd1 dd2 dx1 dy1 dparam)
    (declare (type (array double-float (5)) dparam)
             (type (double-float) dy1 dx1 dd2 dd1))
    (f2cl-lib:with-multi-array-data
        ((dparam double-float dparam-%data% dparam-%offset%))
      (prog ((dflag 0.0d0) (dh11 0.0d0) (dh12 0.0d0) (dh21 0.0d0) (dh22 0.0d0)
             (dp1 0.0d0) (dp2 0.0d0) (dq1 0.0d0) (dq2 0.0d0) (dtemp 0.0d0)
             (du 0.0d0))
        (declare
         (type (double-float) du dtemp dq2 dq1 dp2 dp1 dh22 dh21 dh12 dh11
          dflag))
        (cond
         ((< dd1 zero) (setf dflag (- one)) (setf dh11 zero) (setf dh12 zero)
          (setf dh21 zero) (setf dh22 zero) (setf dd1 zero) (setf dd2 zero)
          (setf dx1 zero))
         (t (setf dp2 (* dd2 dy1))
          (cond
           ((= dp2 zero) (setf dflag (- two))
            (setf (f2cl-lib:fref dparam-%data% (1) ((1 5)) dparam-%offset%)
                    dflag)
            (go end_label)))
          (setf dp1 (* dd1 dx1)) (setf dq2 (* dp2 dy1)) (setf dq1 (* dp1 dx1))
          (cond
           ((> (f2cl-lib:dabs dq1) (f2cl-lib:dabs dq2))
            (setf dh21 (/ (- dy1) dx1)) (setf dh12 (/ dp2 dp1))
            (setf du (- one (* dh12 dh21)))
            (cond
             ((> du zero) (setf dflag zero) (setf dd1 (/ dd1 du))
              (setf dd2 (/ dd2 du)) (setf dx1 (* dx1 du)))))
           (t
            (cond
             ((< dq2 zero) (setf dflag (- one)) (setf dh11 zero)
              (setf dh12 zero) (setf dh21 zero) (setf dh22 zero)
              (setf dd1 zero) (setf dd2 zero) (setf dx1 zero))
             (t (setf dflag one) (setf dh11 (/ dp1 dp2))
              (setf dh22 (/ dx1 dy1)) (setf du (+ one (* dh11 dh22)))
              (setf dtemp (/ dd2 du)) (setf dd2 (/ dd1 du)) (setf dd1 dtemp)
              (setf dx1 (* dy1 du))))))
          (cond
           ((/= dd1 zero)
            (tagbody
             label100000
              (if (not (or (<= dd1 rgamsq) (>= dd1 gamsq)))
                  (go label100001))
              (cond
               ((= dflag zero) (setf dh11 one) (setf dh22 one)
                (setf dflag (- one)))
               (t (setf dh21 (- one)) (setf dh12 one) (setf dflag (- one))))
              (cond
               ((<= dd1 rgamsq) (setf dd1 (* dd1 (expt gam 2)))
                (setf dx1 (/ dx1 gam)) (setf dh11 (/ dh11 gam))
                (setf dh12 (/ dh12 gam)))
               (t (setf dd1 (/ dd1 (expt gam 2))) (setf dx1 (* dx1 gam))
                (setf dh11 (* dh11 gam)) (setf dh12 (* dh12 gam))))
              (go label100000)
             label100001)))
          (cond
           ((/= dd2 zero)
            (tagbody
             label100002
              (if (not
                   (or (<= (f2cl-lib:dabs dd2) rgamsq)
                       (>= (f2cl-lib:dabs dd2) gamsq)))
                  (go label100003))
              (cond
               ((= dflag zero) (setf dh11 one) (setf dh22 one)
                (setf dflag (- one)))
               (t (setf dh21 (- one)) (setf dh12 one) (setf dflag (- one))))
              (cond
               ((<= (f2cl-lib:dabs dd2) rgamsq) (setf dd2 (* dd2 (expt gam 2)))
                (setf dh21 (/ dh21 gam)) (setf dh22 (/ dh22 gam)))
               (t (setf dd2 (/ dd2 (expt gam 2))) (setf dh21 (* dh21 gam))
                (setf dh22 (* dh22 gam))))
              (go label100002)
             label100003)))))
        (cond
         ((< dflag zero)
          (setf (f2cl-lib:fref dparam-%data% (2) ((1 5)) dparam-%offset%) dh11)
          (setf (f2cl-lib:fref dparam-%data% (3) ((1 5)) dparam-%offset%) dh21)
          (setf (f2cl-lib:fref dparam-%data% (4) ((1 5)) dparam-%offset%) dh12)
          (setf (f2cl-lib:fref dparam-%data% (5) ((1 5)) dparam-%offset%)
                  dh22))
         ((= dflag zero)
          (setf (f2cl-lib:fref dparam-%data% (3) ((1 5)) dparam-%offset%) dh21)
          (setf (f2cl-lib:fref dparam-%data% (4) ((1 5)) dparam-%offset%)
                  dh12))
         (t
          (setf (f2cl-lib:fref dparam-%data% (2) ((1 5)) dparam-%offset%) dh11)
          (setf (f2cl-lib:fref dparam-%data% (5) ((1 5)) dparam-%offset%)
                  dh22)))
        (setf (f2cl-lib:fref dparam-%data% (1) ((1 5)) dparam-%offset%) dflag)
        (go end_label)
       end_label
        (return (values dd1 dd2 dx1 nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::drotmg
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((double-float) (double-float)
                                              (double-float) (double-float)
                                              (array double-float (5)))
                                            :return-values
                                            '(fortran-to-lisp::dd1
                                              fortran-to-lisp::dd2
                                              fortran-to-lisp::dx1 nil nil)
                                            :calls 'nil)))

