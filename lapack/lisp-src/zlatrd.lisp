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


(let* ((zero (f2cl-lib:cmplx 0.0d0 0.0d0))
       (one (f2cl-lib:cmplx 1.0d0 0.0d0))
       (half (f2cl-lib:cmplx 0.5d0 0.0d0)))
  (declare (type (f2cl-lib:complex16) zero)
           (type (f2cl-lib:complex16) one)
           (type (f2cl-lib:complex16) half)
           (ignorable zero one half))
  (defun zlatrd (uplo n nb a lda e tau w ldw)
    (declare (type (array double-float (*)) e)
             (type (array f2cl-lib:complex16 (*)) w tau a)
             (type (f2cl-lib:integer4) ldw lda nb n)
             (type (string 1) uplo))
    (f2cl-lib:with-multi-array-data
        ((uplo character uplo-%data% uplo-%offset%)
         (a f2cl-lib:complex16 a-%data% a-%offset%)
         (tau f2cl-lib:complex16 tau-%data% tau-%offset%)
         (w f2cl-lib:complex16 w-%data% w-%offset%)
         (e double-float e-%data% e-%offset%))
      (prog ((alpha #C(0.0d0 0.0d0)) (i 0) (iw 0))
        (declare (type (f2cl-lib:complex16) alpha)
                 (type (f2cl-lib:integer4) i iw))
        (if (<= n 0)
            (go end_label))
        (cond
         ((lsame uplo "U")
          (f2cl-lib:fdo (i n (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                        ((> i (f2cl-lib:int-add n (f2cl-lib:int-sub nb) 1)) nil)
            (tagbody
              (setf iw (f2cl-lib:int-add (f2cl-lib:int-sub i n) nb))
              (cond
               ((< i n)
                (setf (f2cl-lib:fref a-%data% (i i) ((1 lda) (1 *)) a-%offset%)
                        (coerce
                         (f2cl-lib:dble
                          (f2cl-lib:fref a-%data% (i i) ((1 lda) (1 *))
                                         a-%offset%))
                         'f2cl-lib:complex16))
                (zlacgv (f2cl-lib:int-sub n i)
                 (f2cl-lib:array-slice w-%data% f2cl-lib:complex16
                                       (i (f2cl-lib:int-add iw 1))
                                       ((1 ldw) (1 *)) w-%offset%)
                 ldw)
                (zgemv "N" i (f2cl-lib:int-sub n i) (- one)
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                       (1 (f2cl-lib:int-add i 1))
                                       ((1 lda) (1 *)) a-%offset%)
                 lda
                 (f2cl-lib:array-slice w-%data% f2cl-lib:complex16
                                       (i (f2cl-lib:int-add iw 1))
                                       ((1 ldw) (1 *)) w-%offset%)
                 ldw one
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (1 i)
                                       ((1 lda) (1 *)) a-%offset%)
                 1)
                (zlacgv (f2cl-lib:int-sub n i)
                 (f2cl-lib:array-slice w-%data% f2cl-lib:complex16
                                       (i (f2cl-lib:int-add iw 1))
                                       ((1 ldw) (1 *)) w-%offset%)
                 ldw)
                (zlacgv (f2cl-lib:int-sub n i)
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                       (i (f2cl-lib:int-add i 1))
                                       ((1 lda) (1 *)) a-%offset%)
                 lda)
                (zgemv "N" i (f2cl-lib:int-sub n i) (- one)
                 (f2cl-lib:array-slice w-%data% f2cl-lib:complex16
                                       (1 (f2cl-lib:int-add iw 1))
                                       ((1 ldw) (1 *)) w-%offset%)
                 ldw
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                       (i (f2cl-lib:int-add i 1))
                                       ((1 lda) (1 *)) a-%offset%)
                 lda one
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (1 i)
                                       ((1 lda) (1 *)) a-%offset%)
                 1)
                (zlacgv (f2cl-lib:int-sub n i)
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                       (i (f2cl-lib:int-add i 1))
                                       ((1 lda) (1 *)) a-%offset%)
                 lda)
                (setf (f2cl-lib:fref a-%data% (i i) ((1 lda) (1 *)) a-%offset%)
                        (coerce
                         (f2cl-lib:dble
                          (f2cl-lib:fref a-%data% (i i) ((1 lda) (1 *))
                                         a-%offset%))
                         'f2cl-lib:complex16))))
              (cond
               ((> i 1)
                (setf alpha
                        (f2cl-lib:fref a-%data% ((f2cl-lib:int-sub i 1) i)
                                       ((1 lda) (1 *)) a-%offset%))
                (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                    (zlarfg (f2cl-lib:int-sub i 1) alpha
                     (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (1 i)
                                           ((1 lda) (1 *)) a-%offset%)
                     1
                     (f2cl-lib:fref tau-%data% ((f2cl-lib:int-sub i 1)) ((1 *))
                                    tau-%offset%))
                  (declare (ignore var-0 var-2 var-3))
                  (setf alpha var-1)
                  (setf (f2cl-lib:fref tau-%data% ((f2cl-lib:int-sub i 1))
                                       ((1 *)) tau-%offset%)
                          var-4))
                (setf (f2cl-lib:fref e-%data% ((f2cl-lib:int-sub i 1)) ((1 *))
                                     e-%offset%)
                        (coerce (realpart alpha) 'double-float))
                (setf (f2cl-lib:fref a-%data% ((f2cl-lib:int-sub i 1) i)
                                     ((1 lda) (1 *)) a-%offset%)
                        one)
                (zhemv "U" (f2cl-lib:int-sub i 1) one a lda
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (1 i)
                                       ((1 lda) (1 *)) a-%offset%)
                 1 zero
                 (f2cl-lib:array-slice w-%data% f2cl-lib:complex16 (1 iw)
                                       ((1 ldw) (1 *)) w-%offset%)
                 1)
                (cond
                 ((< i n)
                  (zgemv "C" (f2cl-lib:int-sub i 1) (f2cl-lib:int-sub n i) one
                   (f2cl-lib:array-slice w-%data% f2cl-lib:complex16
                                         (1 (f2cl-lib:int-add iw 1))
                                         ((1 ldw) (1 *)) w-%offset%)
                   ldw
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (1 i)
                                         ((1 lda) (1 *)) a-%offset%)
                   1 zero
                   (f2cl-lib:array-slice w-%data% f2cl-lib:complex16
                                         ((+ i 1) iw) ((1 ldw) (1 *))
                                         w-%offset%)
                   1)
                  (zgemv "N" (f2cl-lib:int-sub i 1) (f2cl-lib:int-sub n i)
                   (- one)
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                         (1 (f2cl-lib:int-add i 1))
                                         ((1 lda) (1 *)) a-%offset%)
                   lda
                   (f2cl-lib:array-slice w-%data% f2cl-lib:complex16
                                         ((+ i 1) iw) ((1 ldw) (1 *))
                                         w-%offset%)
                   1 one
                   (f2cl-lib:array-slice w-%data% f2cl-lib:complex16 (1 iw)
                                         ((1 ldw) (1 *)) w-%offset%)
                   1)
                  (zgemv "C" (f2cl-lib:int-sub i 1) (f2cl-lib:int-sub n i) one
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                         (1 (f2cl-lib:int-add i 1))
                                         ((1 lda) (1 *)) a-%offset%)
                   lda
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (1 i)
                                         ((1 lda) (1 *)) a-%offset%)
                   1 zero
                   (f2cl-lib:array-slice w-%data% f2cl-lib:complex16
                                         ((+ i 1) iw) ((1 ldw) (1 *))
                                         w-%offset%)
                   1)
                  (zgemv "N" (f2cl-lib:int-sub i 1) (f2cl-lib:int-sub n i)
                   (- one)
                   (f2cl-lib:array-slice w-%data% f2cl-lib:complex16
                                         (1 (f2cl-lib:int-add iw 1))
                                         ((1 ldw) (1 *)) w-%offset%)
                   ldw
                   (f2cl-lib:array-slice w-%data% f2cl-lib:complex16
                                         ((+ i 1) iw) ((1 ldw) (1 *))
                                         w-%offset%)
                   1 one
                   (f2cl-lib:array-slice w-%data% f2cl-lib:complex16 (1 iw)
                                         ((1 ldw) (1 *)) w-%offset%)
                   1)))
                (zscal (f2cl-lib:int-sub i 1)
                 (f2cl-lib:fref tau-%data% ((f2cl-lib:int-sub i 1)) ((1 *))
                                tau-%offset%)
                 (f2cl-lib:array-slice w-%data% f2cl-lib:complex16 (1 iw)
                                       ((1 ldw) (1 *)) w-%offset%)
                 1)
                (setf alpha
                        (* (- half)
                           (f2cl-lib:fref tau-%data% ((f2cl-lib:int-sub i 1))
                                          ((1 *)) tau-%offset%)
                           (zdotc (f2cl-lib:int-sub i 1)
                            (f2cl-lib:array-slice w-%data% f2cl-lib:complex16
                                                  (1 iw) ((1 ldw) (1 *))
                                                  w-%offset%)
                            1
                            (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                                  (1 i) ((1 lda) (1 *))
                                                  a-%offset%)
                            1)))
                (zaxpy (f2cl-lib:int-sub i 1) alpha
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (1 i)
                                       ((1 lda) (1 *)) a-%offset%)
                 1
                 (f2cl-lib:array-slice w-%data% f2cl-lib:complex16 (1 iw)
                                       ((1 ldw) (1 *)) w-%offset%)
                 1)))
             label10)))
         (t
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i nb) nil)
            (tagbody
              (setf (f2cl-lib:fref a-%data% (i i) ((1 lda) (1 *)) a-%offset%)
                      (coerce
                       (f2cl-lib:dble
                        (f2cl-lib:fref a-%data% (i i) ((1 lda) (1 *))
                                       a-%offset%))
                       'f2cl-lib:complex16))
              (zlacgv (f2cl-lib:int-sub i 1)
               (f2cl-lib:array-slice w-%data% f2cl-lib:complex16 (i 1)
                                     ((1 ldw) (1 *)) w-%offset%)
               ldw)
              (zgemv "N" (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1)
               (f2cl-lib:int-sub i 1) (- one)
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i 1)
                                     ((1 lda) (1 *)) a-%offset%)
               lda
               (f2cl-lib:array-slice w-%data% f2cl-lib:complex16 (i 1)
                                     ((1 ldw) (1 *)) w-%offset%)
               ldw one
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i i)
                                     ((1 lda) (1 *)) a-%offset%)
               1)
              (zlacgv (f2cl-lib:int-sub i 1)
               (f2cl-lib:array-slice w-%data% f2cl-lib:complex16 (i 1)
                                     ((1 ldw) (1 *)) w-%offset%)
               ldw)
              (zlacgv (f2cl-lib:int-sub i 1)
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i 1)
                                     ((1 lda) (1 *)) a-%offset%)
               lda)
              (zgemv "N" (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1)
               (f2cl-lib:int-sub i 1) (- one)
               (f2cl-lib:array-slice w-%data% f2cl-lib:complex16 (i 1)
                                     ((1 ldw) (1 *)) w-%offset%)
               ldw
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i 1)
                                     ((1 lda) (1 *)) a-%offset%)
               lda one
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i i)
                                     ((1 lda) (1 *)) a-%offset%)
               1)
              (zlacgv (f2cl-lib:int-sub i 1)
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i 1)
                                     ((1 lda) (1 *)) a-%offset%)
               lda)
              (setf (f2cl-lib:fref a-%data% (i i) ((1 lda) (1 *)) a-%offset%)
                      (coerce
                       (f2cl-lib:dble
                        (f2cl-lib:fref a-%data% (i i) ((1 lda) (1 *))
                                       a-%offset%))
                       'f2cl-lib:complex16))
              (cond
               ((< i n)
                (setf alpha
                        (f2cl-lib:fref a-%data% ((f2cl-lib:int-add i 1) i)
                                       ((1 lda) (1 *)) a-%offset%))
                (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                    (zlarfg (f2cl-lib:int-sub n i) alpha
                     (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                           ((min (f2cl-lib:int-add i 2) n) i)
                                           ((1 lda) (1 *)) a-%offset%)
                     1 (f2cl-lib:fref tau-%data% (i) ((1 *)) tau-%offset%))
                  (declare (ignore var-0 var-2 var-3))
                  (setf alpha var-1)
                  (setf (f2cl-lib:fref tau-%data% (i) ((1 *)) tau-%offset%)
                          var-4))
                (setf (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%)
                        (coerce (realpart alpha) 'double-float))
                (setf (f2cl-lib:fref a-%data% ((f2cl-lib:int-add i 1) i)
                                     ((1 lda) (1 *)) a-%offset%)
                        one)
                (zhemv "L" (f2cl-lib:int-sub n i) one
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                       ((+ i 1) (f2cl-lib:int-add i 1))
                                       ((1 lda) (1 *)) a-%offset%)
                 lda
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 ((+ i 1) i)
                                       ((1 lda) (1 *)) a-%offset%)
                 1 zero
                 (f2cl-lib:array-slice w-%data% f2cl-lib:complex16 ((+ i 1) i)
                                       ((1 ldw) (1 *)) w-%offset%)
                 1)
                (zgemv "C" (f2cl-lib:int-sub n i) (f2cl-lib:int-sub i 1) one
                 (f2cl-lib:array-slice w-%data% f2cl-lib:complex16 ((+ i 1) 1)
                                       ((1 ldw) (1 *)) w-%offset%)
                 ldw
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 ((+ i 1) i)
                                       ((1 lda) (1 *)) a-%offset%)
                 1 zero
                 (f2cl-lib:array-slice w-%data% f2cl-lib:complex16 (1 i)
                                       ((1 ldw) (1 *)) w-%offset%)
                 1)
                (zgemv "N" (f2cl-lib:int-sub n i) (f2cl-lib:int-sub i 1)
                 (- one)
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 ((+ i 1) 1)
                                       ((1 lda) (1 *)) a-%offset%)
                 lda
                 (f2cl-lib:array-slice w-%data% f2cl-lib:complex16 (1 i)
                                       ((1 ldw) (1 *)) w-%offset%)
                 1 one
                 (f2cl-lib:array-slice w-%data% f2cl-lib:complex16 ((+ i 1) i)
                                       ((1 ldw) (1 *)) w-%offset%)
                 1)
                (zgemv "C" (f2cl-lib:int-sub n i) (f2cl-lib:int-sub i 1) one
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 ((+ i 1) 1)
                                       ((1 lda) (1 *)) a-%offset%)
                 lda
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 ((+ i 1) i)
                                       ((1 lda) (1 *)) a-%offset%)
                 1 zero
                 (f2cl-lib:array-slice w-%data% f2cl-lib:complex16 (1 i)
                                       ((1 ldw) (1 *)) w-%offset%)
                 1)
                (zgemv "N" (f2cl-lib:int-sub n i) (f2cl-lib:int-sub i 1)
                 (- one)
                 (f2cl-lib:array-slice w-%data% f2cl-lib:complex16 ((+ i 1) 1)
                                       ((1 ldw) (1 *)) w-%offset%)
                 ldw
                 (f2cl-lib:array-slice w-%data% f2cl-lib:complex16 (1 i)
                                       ((1 ldw) (1 *)) w-%offset%)
                 1 one
                 (f2cl-lib:array-slice w-%data% f2cl-lib:complex16 ((+ i 1) i)
                                       ((1 ldw) (1 *)) w-%offset%)
                 1)
                (zscal (f2cl-lib:int-sub n i)
                 (f2cl-lib:fref tau-%data% (i) ((1 *)) tau-%offset%)
                 (f2cl-lib:array-slice w-%data% f2cl-lib:complex16 ((+ i 1) i)
                                       ((1 ldw) (1 *)) w-%offset%)
                 1)
                (setf alpha
                        (* (- half)
                           (f2cl-lib:fref tau-%data% (i) ((1 *)) tau-%offset%)
                           (zdotc (f2cl-lib:int-sub n i)
                            (f2cl-lib:array-slice w-%data% f2cl-lib:complex16
                                                  ((+ i 1) i) ((1 ldw) (1 *))
                                                  w-%offset%)
                            1
                            (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                                  ((+ i 1) i) ((1 lda) (1 *))
                                                  a-%offset%)
                            1)))
                (zaxpy (f2cl-lib:int-sub n i) alpha
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 ((+ i 1) i)
                                       ((1 lda) (1 *)) a-%offset%)
                 1
                 (f2cl-lib:array-slice w-%data% f2cl-lib:complex16 ((+ i 1) i)
                                       ((1 ldw) (1 *)) w-%offset%)
                 1)))
             label20))))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zlatrd
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((string 1)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil nil nil nil
                                              nil)
                                            :calls
                                            '(fortran-to-lisp::zaxpy
                                              fortran-to-lisp::zdotc
                                              fortran-to-lisp::zscal
                                              fortran-to-lisp::zhemv
                                              fortran-to-lisp::zlarfg
                                              fortran-to-lisp::zgemv
                                              fortran-to-lisp::zlacgv
                                              fortran-to-lisp::lsame))))

