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


(let* ((one (f2cl-lib:cmplx 1.0d0 0.0d0))
       (zero (f2cl-lib:cmplx 0.0d0 0.0d0))
       (half (f2cl-lib:cmplx 0.5d0 0.0d0)))
  (declare (type (f2cl-lib:complex16) one)
           (type (f2cl-lib:complex16) zero)
           (type (f2cl-lib:complex16) half)
           (ignorable one zero half))
  (defun zhetd2 (uplo n a lda d e tau info)
    (declare (type (array double-float (*)) e d)
             (type (array f2cl-lib:complex16 (*)) tau a)
             (type (f2cl-lib:integer4) info lda n)
             (type (string 1) uplo))
    (f2cl-lib:with-multi-array-data
        ((uplo character uplo-%data% uplo-%offset%)
         (a f2cl-lib:complex16 a-%data% a-%offset%)
         (tau f2cl-lib:complex16 tau-%data% tau-%offset%)
         (d double-float d-%data% d-%offset%)
         (e double-float e-%data% e-%offset%))
      (prog ((alpha #C(0.0d0 0.0d0)) (taui #C(0.0d0 0.0d0)) (i 0) (upper nil))
        (declare (type (f2cl-lib:complex16) alpha taui)
                 (type (f2cl-lib:integer4) i)
                 (type f2cl-lib:logical upper))
        (setf info 0)
        (setf upper (lsame uplo "U"))
        (cond ((and (not upper) (not (lsame uplo "L"))) (setf info -1))
              ((< n 0) (setf info -2))
              ((< lda
                  (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
               (setf info -4)))
        (cond
         ((/= info 0) (xerbla "ZHETD2" (f2cl-lib:int-sub info))
          (go end_label)))
        (if (<= n 0)
            (go end_label))
        (cond
         (upper
          (setf (f2cl-lib:fref a-%data% (n n) ((1 lda) (1 *)) a-%offset%)
                  (coerce
                   (f2cl-lib:dble
                    (f2cl-lib:fref a-%data% (n n) ((1 lda) (1 *)) a-%offset%))
                   'f2cl-lib:complex16))
          (f2cl-lib:fdo (i (f2cl-lib:int-add n (f2cl-lib:int-sub 1))
                         (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                        ((> i 1) nil)
            (tagbody
              (setf alpha
                      (f2cl-lib:fref a-%data% (i (f2cl-lib:int-add i 1))
                                     ((1 lda) (1 *)) a-%offset%))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (zlarfg i alpha
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                         (1 (f2cl-lib:int-add i 1))
                                         ((1 lda) (1 *)) a-%offset%)
                   1 taui)
                (declare (ignore var-0 var-2 var-3))
                (setf alpha var-1)
                (setf taui var-4))
              (setf (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%)
                      (coerce (realpart alpha) 'double-float))
              (cond
               ((/= taui zero)
                (setf (f2cl-lib:fref a-%data% (i (f2cl-lib:int-add i 1))
                                     ((1 lda) (1 *)) a-%offset%)
                        one)
                (zhemv uplo i taui a lda
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                       (1 (f2cl-lib:int-add i 1))
                                       ((1 lda) (1 *)) a-%offset%)
                 1 zero tau 1)
                (setf alpha
                        (* (- half) taui
                           (zdotc i tau 1
                            (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                                  (1 (f2cl-lib:int-add i 1))
                                                  ((1 lda) (1 *)) a-%offset%)
                            1)))
                (zaxpy i alpha
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                       (1 (f2cl-lib:int-add i 1))
                                       ((1 lda) (1 *)) a-%offset%)
                 1 tau 1)
                (zher2 uplo i (- one)
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                       (1 (f2cl-lib:int-add i 1))
                                       ((1 lda) (1 *)) a-%offset%)
                 1 tau 1 a lda))
               (t
                (setf (f2cl-lib:fref a-%data% (i i) ((1 lda) (1 *)) a-%offset%)
                        (coerce
                         (f2cl-lib:dble
                          (f2cl-lib:fref a-%data% (i i) ((1 lda) (1 *))
                                         a-%offset%))
                         'f2cl-lib:complex16))))
              (setf (f2cl-lib:fref a-%data% (i (f2cl-lib:int-add i 1))
                                   ((1 lda) (1 *)) a-%offset%)
                      (coerce (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%)
                              'f2cl-lib:complex16))
              (setf (f2cl-lib:fref d-%data% ((f2cl-lib:int-add i 1)) ((1 *))
                                   d-%offset%)
                      (coerce
                       (realpart
                        (f2cl-lib:fref a-%data%
                                       ((f2cl-lib:int-add i 1)
                                        (f2cl-lib:int-add i 1))
                                       ((1 lda) (1 *)) a-%offset%))
                       'double-float))
              (setf (f2cl-lib:fref tau-%data% (i) ((1 *)) tau-%offset%) taui)
             label10))
          (setf (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%)
                  (coerce
                   (realpart
                    (f2cl-lib:fref a-%data% (1 1) ((1 lda) (1 *)) a-%offset%))
                   'double-float)))
         (t
          (setf (f2cl-lib:fref a-%data% (1 1) ((1 lda) (1 *)) a-%offset%)
                  (coerce
                   (f2cl-lib:dble
                    (f2cl-lib:fref a-%data% (1 1) ((1 lda) (1 *)) a-%offset%))
                   'f2cl-lib:complex16))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i (f2cl-lib:int-add n (f2cl-lib:int-sub 1))) nil)
            (tagbody
              (setf alpha
                      (f2cl-lib:fref a-%data% ((f2cl-lib:int-add i 1) i)
                                     ((1 lda) (1 *)) a-%offset%))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (zlarfg (f2cl-lib:int-sub n i) alpha
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                         ((min (f2cl-lib:int-add i 2) n) i)
                                         ((1 lda) (1 *)) a-%offset%)
                   1 taui)
                (declare (ignore var-0 var-2 var-3))
                (setf alpha var-1)
                (setf taui var-4))
              (setf (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%)
                      (coerce (realpart alpha) 'double-float))
              (cond
               ((/= taui zero)
                (setf (f2cl-lib:fref a-%data% ((f2cl-lib:int-add i 1) i)
                                     ((1 lda) (1 *)) a-%offset%)
                        one)
                (zhemv uplo (f2cl-lib:int-sub n i) taui
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                       ((+ i 1) (f2cl-lib:int-add i 1))
                                       ((1 lda) (1 *)) a-%offset%)
                 lda
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 ((+ i 1) i)
                                       ((1 lda) (1 *)) a-%offset%)
                 1 zero
                 (f2cl-lib:array-slice tau-%data% f2cl-lib:complex16 (i)
                                       ((1 *)) tau-%offset%)
                 1)
                (setf alpha
                        (* (- half) taui
                           (zdotc (f2cl-lib:int-sub n i)
                            (f2cl-lib:array-slice tau-%data% f2cl-lib:complex16
                                                  (i) ((1 *)) tau-%offset%)
                            1
                            (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                                  ((+ i 1) i) ((1 lda) (1 *))
                                                  a-%offset%)
                            1)))
                (zaxpy (f2cl-lib:int-sub n i) alpha
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 ((+ i 1) i)
                                       ((1 lda) (1 *)) a-%offset%)
                 1
                 (f2cl-lib:array-slice tau-%data% f2cl-lib:complex16 (i)
                                       ((1 *)) tau-%offset%)
                 1)
                (zher2 uplo (f2cl-lib:int-sub n i) (- one)
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 ((+ i 1) i)
                                       ((1 lda) (1 *)) a-%offset%)
                 1
                 (f2cl-lib:array-slice tau-%data% f2cl-lib:complex16 (i)
                                       ((1 *)) tau-%offset%)
                 1
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                       ((+ i 1) (f2cl-lib:int-add i 1))
                                       ((1 lda) (1 *)) a-%offset%)
                 lda))
               (t
                (setf (f2cl-lib:fref a-%data%
                                     ((f2cl-lib:int-add i 1)
                                      (f2cl-lib:int-add i 1))
                                     ((1 lda) (1 *)) a-%offset%)
                        (coerce
                         (f2cl-lib:dble
                          (f2cl-lib:fref a-%data%
                                         ((f2cl-lib:int-add i 1)
                                          (f2cl-lib:int-add i 1))
                                         ((1 lda) (1 *)) a-%offset%))
                         'f2cl-lib:complex16))))
              (setf (f2cl-lib:fref a-%data% ((f2cl-lib:int-add i 1) i)
                                   ((1 lda) (1 *)) a-%offset%)
                      (coerce (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%)
                              'f2cl-lib:complex16))
              (setf (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                      (coerce
                       (realpart
                        (f2cl-lib:fref a-%data% (i i) ((1 lda) (1 *))
                                       a-%offset%))
                       'double-float))
              (setf (f2cl-lib:fref tau-%data% (i) ((1 *)) tau-%offset%) taui)
             label20))
          (setf (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%)
                  (coerce
                   (realpart
                    (f2cl-lib:fref a-%data% (n n) ((1 lda) (1 *)) a-%offset%))
                   'double-float))))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zhetd2
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((string 1)
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (array double-float (*))
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil nil nil
                                              fortran-to-lisp::info)
                                            :calls
                                            '(fortran-to-lisp::zher2
                                              fortran-to-lisp::zaxpy
                                              fortran-to-lisp::zdotc
                                              fortran-to-lisp::zhemv
                                              fortran-to-lisp::zlarfg
                                              fortran-to-lisp::xerbla
                                              fortran-to-lisp::lsame))))

