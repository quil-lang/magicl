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


(let* ((one (f2cl-lib:cmplx 1.0d0 0.0d0)))
  (declare (type (f2cl-lib:complex16) one)
           (ignorable one))
  (defun zgelq2 (m n a lda tau work info)
    (declare (type (array f2cl-lib:complex16 (*)) work tau a)
             (type (f2cl-lib:integer4) info lda n m))
    (f2cl-lib:with-multi-array-data
        ((a f2cl-lib:complex16 a-%data% a-%offset%)
         (tau f2cl-lib:complex16 tau-%data% tau-%offset%)
         (work f2cl-lib:complex16 work-%data% work-%offset%))
      (prog ((alpha #C(0.0d0 0.0d0)) (i 0) (k 0))
        (declare (type (f2cl-lib:complex16) alpha)
                 (type (f2cl-lib:integer4) i k))
        (setf info 0)
        (cond ((< m 0) (setf info -1)) ((< n 0) (setf info -2))
              ((< lda
                  (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)))
               (setf info -4)))
        (cond
         ((/= info 0) (xerbla "ZGELQ2" (f2cl-lib:int-sub info))
          (go end_label)))
        (setf k (min (the f2cl-lib:integer4 m) (the f2cl-lib:integer4 n)))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i k) nil)
          (tagbody
            (zlacgv (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1)
             (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i i)
                                   ((1 lda) (1 *)) a-%offset%)
             lda)
            (setf alpha
                    (f2cl-lib:fref a-%data% (i i) ((1 lda) (1 *)) a-%offset%))
            (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                (zlarfg (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1) alpha
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                       (i
                                        (min
                                         (the f2cl-lib:integer4
                                              (f2cl-lib:int-add i 1))
                                         (the f2cl-lib:integer4 n)))
                                       ((1 lda) (1 *)) a-%offset%)
                 lda (f2cl-lib:fref tau-%data% (i) ((1 *)) tau-%offset%))
              (declare (ignore var-0 var-2 var-3))
              (setf alpha var-1)
              (setf (f2cl-lib:fref tau-%data% (i) ((1 *)) tau-%offset%) var-4))
            (cond
             ((< i m)
              (setf (f2cl-lib:fref a-%data% (i i) ((1 lda) (1 *)) a-%offset%)
                      one)
              (zlarf "R" (f2cl-lib:int-sub m i)
               (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1)
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i i)
                                     ((1 lda) (1 *)) a-%offset%)
               lda (f2cl-lib:fref tau-%data% (i) ((1 *)) tau-%offset%)
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 ((+ i 1) i)
                                     ((1 lda) (1 *)) a-%offset%)
               lda work)))
            (setf (f2cl-lib:fref a-%data% (i i) ((1 lda) (1 *)) a-%offset%)
                    alpha)
            (zlacgv (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1)
             (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i i)
                                   ((1 lda) (1 *)) a-%offset%)
             lda)
           label10))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zgelq2
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil nil
                                              fortran-to-lisp::info)
                                            :calls
                                            '(fortran-to-lisp::zlarf
                                              fortran-to-lisp::zlarfg
                                              fortran-to-lisp::zlacgv
                                              fortran-to-lisp::xerbla))))

