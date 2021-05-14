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
  (defun zungtr (uplo n a lda tau work lwork info)
    (declare (type (array f2cl-lib:complex16 (*)) work tau a)
             (type (f2cl-lib:integer4) info lwork lda n)
             (type (string 1) uplo))
    (f2cl-lib:with-multi-array-data
        ((uplo character uplo-%data% uplo-%offset%)
         (a f2cl-lib:complex16 a-%data% a-%offset%)
         (tau f2cl-lib:complex16 tau-%data% tau-%offset%)
         (work f2cl-lib:complex16 work-%data% work-%offset%))
      (prog ((i 0) (iinfo 0) (j 0) (lwkopt 0) (nb 0) (lquery nil) (upper nil))
        (declare (type (f2cl-lib:integer4) i iinfo j lwkopt nb)
                 (type f2cl-lib:logical lquery upper))
        (setf info 0)
        (setf lquery (coerce (= lwork -1) 'f2cl-lib:logical))
        (setf upper (lsame uplo "U"))
        (cond ((and (not upper) (not (lsame uplo "L"))) (setf info -1))
              ((< n 0) (setf info -2))
              ((< lda
                  (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
               (setf info -4))
              ((and
                (< lwork
                   (max (the f2cl-lib:integer4 1)
                        (the f2cl-lib:integer4
                             (f2cl-lib:int-add n (f2cl-lib:int-sub 1)))))
                (not lquery))
               (setf info -7)))
        (cond
         ((= info 0)
          (cond
           (upper
            (setf nb
                    (ilaenv 1 "ZUNGQL" " " (f2cl-lib:int-sub n 1)
                     (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1) -1)))
           (t
            (setf nb
                    (ilaenv 1 "ZUNGQR" " " (f2cl-lib:int-sub n 1)
                     (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1) -1))))
          (setf lwkopt
                  (f2cl-lib:int-mul
                   (max (the f2cl-lib:integer4 1)
                        (the f2cl-lib:integer4 (f2cl-lib:int-sub n 1)))
                   nb))
          (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                  (coerce lwkopt 'f2cl-lib:complex16))))
        (cond
         ((/= info 0) (xerbla "ZUNGTR" (f2cl-lib:int-sub info)) (go end_label))
         (lquery (go end_label)))
        (cond
         ((= n 0)
          (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                  (coerce 1 'f2cl-lib:complex16))
          (go end_label)))
        (cond
         (upper
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j (f2cl-lib:int-add n (f2cl-lib:int-sub 1))) nil)
            (tagbody
              (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                            ((> i (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                             nil)
                (tagbody
                  (setf (f2cl-lib:fref a-%data% (i j) ((1 lda) (1 *))
                                       a-%offset%)
                          (f2cl-lib:fref a-%data% (i (f2cl-lib:int-add j 1))
                                         ((1 lda) (1 *)) a-%offset%))
                 label10))
              (setf (f2cl-lib:fref a-%data% (n j) ((1 lda) (1 *)) a-%offset%)
                      zero)
             label20))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i (f2cl-lib:int-add n (f2cl-lib:int-sub 1))) nil)
            (tagbody
              (setf (f2cl-lib:fref a-%data% (i n) ((1 lda) (1 *)) a-%offset%)
                      zero)
             label30))
          (setf (f2cl-lib:fref a-%data% (n n) ((1 lda) (1 *)) a-%offset%) one)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
              (zungql (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1)
               (f2cl-lib:int-sub n 1) a lda tau work lwork iinfo)
            (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
            (setf iinfo var-8)))
         (t
          (f2cl-lib:fdo (j n (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                        ((> j 2) nil)
            (tagbody
              (setf (f2cl-lib:fref a-%data% (1 j) ((1 lda) (1 *)) a-%offset%)
                      zero)
              (f2cl-lib:fdo (i (f2cl-lib:int-add j 1) (f2cl-lib:int-add i 1))
                            ((> i n) nil)
                (tagbody
                  (setf (f2cl-lib:fref a-%data% (i j) ((1 lda) (1 *))
                                       a-%offset%)
                          (f2cl-lib:fref a-%data% (i (f2cl-lib:int-sub j 1))
                                         ((1 lda) (1 *)) a-%offset%))
                 label40))
             label50))
          (setf (f2cl-lib:fref a-%data% (1 1) ((1 lda) (1 *)) a-%offset%) one)
          (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf (f2cl-lib:fref a-%data% (i 1) ((1 lda) (1 *)) a-%offset%)
                      zero)
             label60))
          (cond
           ((> n 1)
            (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                (zungqr (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1)
                 (f2cl-lib:int-sub n 1)
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (2 2)
                                       ((1 lda) (1 *)) a-%offset%)
                 lda tau work lwork iinfo)
              (declare
               (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
              (setf iinfo var-8))))))
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                (coerce lwkopt 'f2cl-lib:complex16))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zungtr
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((string 1)
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil nil nil
                                              fortran-to-lisp::info)
                                            :calls
                                            '(fortran-to-lisp::zungqr
                                              fortran-to-lisp::zungql
                                              fortran-to-lisp::xerbla
                                              fortran-to-lisp::ilaenv
                                              fortran-to-lisp::lsame))))

