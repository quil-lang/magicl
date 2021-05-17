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


(let* ((zero (f2cl-lib:cmplx 0.0d0 0.0d0)) (one (f2cl-lib:cmplx 1.0d0 0.0d0)))
  (declare (type (f2cl-lib:complex16) zero)
           (type (f2cl-lib:complex16) one)
           (ignorable zero one))
  (defun zgebd2 (m n a lda d e tauq taup work info)
    (declare (type (array double-float (*)) e d)
             (type (array f2cl-lib:complex16 (*)) work taup tauq a)
             (type (f2cl-lib:integer4) info lda n m))
    (f2cl-lib:with-multi-array-data
        ((a f2cl-lib:complex16 a-%data% a-%offset%)
         (tauq f2cl-lib:complex16 tauq-%data% tauq-%offset%)
         (taup f2cl-lib:complex16 taup-%data% taup-%offset%)
         (work f2cl-lib:complex16 work-%data% work-%offset%)
         (d double-float d-%data% d-%offset%)
         (e double-float e-%data% e-%offset%))
      (prog ((alpha #C(0.0d0 0.0d0)) (i 0))
        (declare (type (f2cl-lib:complex16) alpha)
                 (type (f2cl-lib:integer4) i))
        (setf info 0)
        (cond ((< m 0) (setf info -1)) ((< n 0) (setf info -2))
              ((< lda
                  (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)))
               (setf info -4)))
        (cond
         ((< info 0) (xerbla "ZGEBD2" (f2cl-lib:int-sub info)) (go end_label)))
        (cond
         ((>= m n)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf alpha
                      (f2cl-lib:fref a-%data% (i i) ((1 lda) (1 *))
                                     a-%offset%))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (zlarfg (f2cl-lib:int-add (f2cl-lib:int-sub m i) 1) alpha
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                         ((min (f2cl-lib:int-add i 1) m) i)
                                         ((1 lda) (1 *)) a-%offset%)
                   1 (f2cl-lib:fref tauq-%data% (i) ((1 *)) tauq-%offset%))
                (declare (ignore var-0 var-2 var-3))
                (setf alpha var-1)
                (setf (f2cl-lib:fref tauq-%data% (i) ((1 *)) tauq-%offset%)
                        var-4))
              (setf (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                      (coerce (realpart alpha) 'double-float))
              (setf (f2cl-lib:fref a-%data% (i i) ((1 lda) (1 *)) a-%offset%)
                      one)
              (if (< i n)
                  (zlarf "L" (f2cl-lib:int-add (f2cl-lib:int-sub m i) 1)
                   (f2cl-lib:int-sub n i)
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i i)
                                         ((1 lda) (1 *)) a-%offset%)
                   1
                   (f2cl-lib:dconjg
                    (f2cl-lib:fref tauq-%data% (i) ((1 *)) tauq-%offset%))
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                         (i (f2cl-lib:int-add i 1))
                                         ((1 lda) (1 *)) a-%offset%)
                   lda work))
              (setf (f2cl-lib:fref a-%data% (i i) ((1 lda) (1 *)) a-%offset%)
                      (coerce (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                              'f2cl-lib:complex16))
              (cond
               ((< i n)
                (zlacgv (f2cl-lib:int-sub n i)
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                       (i (f2cl-lib:int-add i 1))
                                       ((1 lda) (1 *)) a-%offset%)
                 lda)
                (setf alpha
                        (f2cl-lib:fref a-%data% (i (f2cl-lib:int-add i 1))
                                       ((1 lda) (1 *)) a-%offset%))
                (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                    (zlarfg (f2cl-lib:int-sub n i) alpha
                     (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                           (i
                                            (min
                                             (the f2cl-lib:integer4
                                                  (f2cl-lib:int-add i 2))
                                             (the f2cl-lib:integer4 n)))
                                           ((1 lda) (1 *)) a-%offset%)
                     lda (f2cl-lib:fref taup-%data% (i) ((1 *)) taup-%offset%))
                  (declare (ignore var-0 var-2 var-3))
                  (setf alpha var-1)
                  (setf (f2cl-lib:fref taup-%data% (i) ((1 *)) taup-%offset%)
                          var-4))
                (setf (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%)
                        (coerce (realpart alpha) 'double-float))
                (setf (f2cl-lib:fref a-%data% (i (f2cl-lib:int-add i 1))
                                     ((1 lda) (1 *)) a-%offset%)
                        one)
                (zlarf "R" (f2cl-lib:int-sub m i) (f2cl-lib:int-sub n i)
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                       (i (f2cl-lib:int-add i 1))
                                       ((1 lda) (1 *)) a-%offset%)
                 lda (f2cl-lib:fref taup-%data% (i) ((1 *)) taup-%offset%)
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                       ((+ i 1) (f2cl-lib:int-add i 1))
                                       ((1 lda) (1 *)) a-%offset%)
                 lda work)
                (zlacgv (f2cl-lib:int-sub n i)
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                       (i (f2cl-lib:int-add i 1))
                                       ((1 lda) (1 *)) a-%offset%)
                 lda)
                (setf (f2cl-lib:fref a-%data% (i (f2cl-lib:int-add i 1))
                                     ((1 lda) (1 *)) a-%offset%)
                        (coerce (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%)
                                'f2cl-lib:complex16)))
               (t
                (setf (f2cl-lib:fref taup-%data% (i) ((1 *)) taup-%offset%)
                        zero)))
             label10)))
         (t
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (zlacgv (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1)
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i i)
                                     ((1 lda) (1 *)) a-%offset%)
               lda)
              (setf alpha
                      (f2cl-lib:fref a-%data% (i i) ((1 lda) (1 *))
                                     a-%offset%))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (zlarfg (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1) alpha
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                         (i
                                          (min
                                           (the f2cl-lib:integer4
                                                (f2cl-lib:int-add i 1))
                                           (the f2cl-lib:integer4 n)))
                                         ((1 lda) (1 *)) a-%offset%)
                   lda (f2cl-lib:fref taup-%data% (i) ((1 *)) taup-%offset%))
                (declare (ignore var-0 var-2 var-3))
                (setf alpha var-1)
                (setf (f2cl-lib:fref taup-%data% (i) ((1 *)) taup-%offset%)
                        var-4))
              (setf (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                      (coerce (realpart alpha) 'double-float))
              (setf (f2cl-lib:fref a-%data% (i i) ((1 lda) (1 *)) a-%offset%)
                      one)
              (if (< i m)
                  (zlarf "R" (f2cl-lib:int-sub m i)
                   (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1)
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i i)
                                         ((1 lda) (1 *)) a-%offset%)
                   lda (f2cl-lib:fref taup-%data% (i) ((1 *)) taup-%offset%)
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                         ((+ i 1) i) ((1 lda) (1 *))
                                         a-%offset%)
                   lda work))
              (zlacgv (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1)
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i i)
                                     ((1 lda) (1 *)) a-%offset%)
               lda)
              (setf (f2cl-lib:fref a-%data% (i i) ((1 lda) (1 *)) a-%offset%)
                      (coerce (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                              'f2cl-lib:complex16))
              (cond
               ((< i m)
                (setf alpha
                        (f2cl-lib:fref a-%data% ((f2cl-lib:int-add i 1) i)
                                       ((1 lda) (1 *)) a-%offset%))
                (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                    (zlarfg (f2cl-lib:int-sub m i) alpha
                     (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                           ((min (f2cl-lib:int-add i 2) m) i)
                                           ((1 lda) (1 *)) a-%offset%)
                     1 (f2cl-lib:fref tauq-%data% (i) ((1 *)) tauq-%offset%))
                  (declare (ignore var-0 var-2 var-3))
                  (setf alpha var-1)
                  (setf (f2cl-lib:fref tauq-%data% (i) ((1 *)) tauq-%offset%)
                          var-4))
                (setf (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%)
                        (coerce (realpart alpha) 'double-float))
                (setf (f2cl-lib:fref a-%data% ((f2cl-lib:int-add i 1) i)
                                     ((1 lda) (1 *)) a-%offset%)
                        one)
                (zlarf "L" (f2cl-lib:int-sub m i) (f2cl-lib:int-sub n i)
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 ((+ i 1) i)
                                       ((1 lda) (1 *)) a-%offset%)
                 1
                 (f2cl-lib:dconjg
                  (f2cl-lib:fref tauq-%data% (i) ((1 *)) tauq-%offset%))
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                       ((+ i 1) (f2cl-lib:int-add i 1))
                                       ((1 lda) (1 *)) a-%offset%)
                 lda work)
                (setf (f2cl-lib:fref a-%data% ((f2cl-lib:int-add i 1) i)
                                     ((1 lda) (1 *)) a-%offset%)
                        (coerce (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%)
                                'f2cl-lib:complex16)))
               (t
                (setf (f2cl-lib:fref tauq-%data% (i) ((1 *)) tauq-%offset%)
                        zero)))
             label20))))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zgebd2
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (array double-float (*))
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil nil nil nil
                                              nil fortran-to-lisp::info)
                                            :calls
                                            '(fortran-to-lisp::zlacgv
                                              fortran-to-lisp::zlarf
                                              fortran-to-lisp::zlarfg
                                              fortran-to-lisp::xerbla))))

