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


(let* ((one (f2cl-lib:cmplx 1.0d0 0.0d0)) (zero (f2cl-lib:cmplx 0.0d0 0.0d0)))
  (declare (type (f2cl-lib:complex16) one)
           (type (f2cl-lib:complex16) zero)
           (ignorable one zero))
  (defun zgetf2 (m n a lda ipiv info)
    (declare (type (array f2cl-lib:integer4 (*)) ipiv)
             (type (array f2cl-lib:complex16 (*)) a)
             (type (f2cl-lib:integer4) info lda n m))
    (f2cl-lib:with-multi-array-data
        ((a f2cl-lib:complex16 a-%data% a-%offset%)
         (ipiv f2cl-lib:integer4 ipiv-%data% ipiv-%offset%))
      (prog ((i 0) (j 0) (jp 0) (sfmin 0.0d0))
        (declare (type (f2cl-lib:integer4) i j jp)
                 (type (double-float) sfmin))
        (setf info 0)
        (cond ((< m 0) (setf info -1)) ((< n 0) (setf info -2))
              ((< lda
                  (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)))
               (setf info -4)))
        (cond
         ((/= info 0) (xerbla "ZGETF2" (f2cl-lib:int-sub info))
          (go end_label)))
        (if (or (= m 0) (= n 0))
            (go end_label))
        (setf sfmin (dlamch "S"))
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j
                          (min (the f2cl-lib:integer4 m)
                               (the f2cl-lib:integer4 n)))
                       nil)
          (tagbody
            (setf jp
                    (f2cl-lib:int-add (f2cl-lib:int-sub j 1)
                                      (izamax
                                       (f2cl-lib:int-add (f2cl-lib:int-sub m j)
                                                         1)
                                       (f2cl-lib:array-slice a-%data%
                                                             f2cl-lib:complex16
                                                             (j j)
                                                             ((1 lda) (1 *))
                                                             a-%offset%)
                                       1)))
            (setf (f2cl-lib:fref ipiv-%data% (j) ((1 *)) ipiv-%offset%) jp)
            (cond
             ((/= (f2cl-lib:fref a (jp j) ((1 lda) (1 *))) zero)
              (if (/= jp j)
                  (zswap n
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (j 1)
                                         ((1 lda) (1 *)) a-%offset%)
                   lda
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (jp 1)
                                         ((1 lda) (1 *)) a-%offset%)
                   lda))
              (cond
               ((< j m)
                (cond
                 ((>= (abs (f2cl-lib:fref a (j j) ((1 lda) (1 *)))) sfmin)
                  (zscal (f2cl-lib:int-sub m j)
                   (/ one
                      (f2cl-lib:fref a-%data% (j j) ((1 lda) (1 *))
                                     a-%offset%))
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                         ((+ j 1) j) ((1 lda) (1 *))
                                         a-%offset%)
                   1))
                 (t
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i
                                    (f2cl-lib:int-add m (f2cl-lib:int-sub j)))
                                 nil)
                    (tagbody
                      (setf (f2cl-lib:fref a-%data% ((f2cl-lib:int-add j i) j)
                                           ((1 lda) (1 *)) a-%offset%)
                              (/
                               (f2cl-lib:fref a-%data%
                                              ((f2cl-lib:int-add j i) j)
                                              ((1 lda) (1 *)) a-%offset%)
                               (f2cl-lib:fref a-%data% (j j) ((1 lda) (1 *))
                                              a-%offset%)))
                     label20)))))))
             ((= info 0) (setf info j)))
            (cond
             ((< j (min (the f2cl-lib:integer4 m) (the f2cl-lib:integer4 n)))
              (zgeru (f2cl-lib:int-sub m j) (f2cl-lib:int-sub n j) (- one)
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 ((+ j 1) j)
                                     ((1 lda) (1 *)) a-%offset%)
               1
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                     (j (f2cl-lib:int-add j 1)) ((1 lda) (1 *))
                                     a-%offset%)
               lda
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                     ((+ j 1) (f2cl-lib:int-add j 1))
                                     ((1 lda) (1 *)) a-%offset%)
               lda)))
           label10))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zgetf2
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::integer4
                                               (*))
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil
                                              fortran-to-lisp::info)
                                            :calls
                                            '(fortran-to-lisp::zgeru
                                              fortran-to-lisp::zscal
                                              fortran-to-lisp::zswap
                                              fortran-to-lisp::izamax
                                              fortran-to-lisp::dlamch
                                              fortran-to-lisp::xerbla))))

