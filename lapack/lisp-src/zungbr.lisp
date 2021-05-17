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
  (defun zungbr (vect m n k a lda tau work lwork info)
    (declare (type (array f2cl-lib:complex16 (*)) work tau a)
             (type (f2cl-lib:integer4) info lwork lda k n m)
             (type (string 1) vect))
    (f2cl-lib:with-multi-array-data
        ((vect character vect-%data% vect-%offset%)
         (a f2cl-lib:complex16 a-%data% a-%offset%)
         (tau f2cl-lib:complex16 tau-%data% tau-%offset%)
         (work f2cl-lib:complex16 work-%data% work-%offset%))
      (prog ((i 0) (iinfo 0) (j 0) (lwkopt 0) (mn 0) (lquery nil) (wantq nil))
        (declare (type (f2cl-lib:integer4) i iinfo j lwkopt mn)
                 (type f2cl-lib:logical lquery wantq))
        (setf info 0)
        (setf wantq (lsame vect "Q"))
        (setf mn (min (the f2cl-lib:integer4 m) (the f2cl-lib:integer4 n)))
        (setf lquery (coerce (= lwork -1) 'f2cl-lib:logical))
        (cond ((and (not wantq) (not (lsame vect "P"))) (setf info -1))
              ((< m 0) (setf info -2))
              ((or (< n 0)
                   (and wantq
                        (or (> n m)
                            (< n
                               (min (the f2cl-lib:integer4 m)
                                    (the f2cl-lib:integer4 k)))))
                   (and (not wantq)
                        (or (> m n)
                            (< m
                               (min (the f2cl-lib:integer4 n)
                                    (the f2cl-lib:integer4 k))))))
               (setf info -3))
              ((< k 0) (setf info -4))
              ((< lda
                  (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)))
               (setf info -6))
              ((and
                (< lwork
                   (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 mn)))
                (not lquery))
               (setf info -9)))
        (cond
         ((= info 0)
          (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                  (coerce 1 'f2cl-lib:complex16))
          (cond
           (wantq
            (cond
             ((>= m k)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                  (zungqr m n k a lda tau work -1 iinfo)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                (setf iinfo var-8)))
             (t
              (cond
               ((> m 1)
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                    (zungqr (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1)
                     (f2cl-lib:int-sub m 1)
                     (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (2 2)
                                           ((1 lda) (1 *)) a-%offset%)
                     lda tau work -1 iinfo)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                  (setf iinfo var-8)))))))
           (t
            (cond
             ((< k n)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                  (zunglq m n k a lda tau work -1 iinfo)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                (setf iinfo var-8)))
             (t
              (cond
               ((> n 1)
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                    (zunglq (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1)
                     (f2cl-lib:int-sub n 1)
                     (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (2 2)
                                           ((1 lda) (1 *)) a-%offset%)
                     lda tau work -1 iinfo)
                  (declare
                   (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                  (setf iinfo var-8))))))))
          (setf lwkopt
                  (f2cl-lib:int
                   (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)))
          (setf lwkopt
                  (max (the f2cl-lib:integer4 lwkopt)
                       (the f2cl-lib:integer4 mn)))))
        (cond
         ((/= info 0) (xerbla "ZUNGBR" (f2cl-lib:int-sub info)) (go end_label))
         (lquery
          (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                  (coerce lwkopt 'f2cl-lib:complex16))
          (go end_label)))
        (cond
         ((or (= m 0) (= n 0))
          (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                  (coerce 1 'f2cl-lib:complex16))
          (go end_label)))
        (cond
         (wantq
          (cond
           ((>= m k)
            (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                (zungqr m n k a lda tau work lwork iinfo)
              (declare
               (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
              (setf iinfo var-8)))
           (t
            (f2cl-lib:fdo (j m (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                          ((> j 2) nil)
              (tagbody
                (setf (f2cl-lib:fref a-%data% (1 j) ((1 lda) (1 *)) a-%offset%)
                        zero)
                (f2cl-lib:fdo (i (f2cl-lib:int-add j 1) (f2cl-lib:int-add i 1))
                              ((> i m) nil)
                  (tagbody
                    (setf (f2cl-lib:fref a-%data% (i j) ((1 lda) (1 *))
                                         a-%offset%)
                            (f2cl-lib:fref a-%data% (i (f2cl-lib:int-sub j 1))
                                           ((1 lda) (1 *)) a-%offset%))
                   label10))
               label20))
            (setf (f2cl-lib:fref a-%data% (1 1) ((1 lda) (1 *)) a-%offset%)
                    one)
            (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                          ((> i m) nil)
              (tagbody
                (setf (f2cl-lib:fref a-%data% (i 1) ((1 lda) (1 *)) a-%offset%)
                        zero)
               label30))
            (cond
             ((> m 1)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                  (zungqr (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1)
                   (f2cl-lib:int-sub m 1)
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (2 2)
                                         ((1 lda) (1 *)) a-%offset%)
                   lda tau work lwork iinfo)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                (setf iinfo var-8)))))))
         (t
          (cond
           ((< k n)
            (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                (zunglq m n k a lda tau work lwork iinfo)
              (declare
               (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
              (setf iinfo var-8)))
           (t
            (setf (f2cl-lib:fref a-%data% (1 1) ((1 lda) (1 *)) a-%offset%)
                    one)
            (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (setf (f2cl-lib:fref a-%data% (i 1) ((1 lda) (1 *)) a-%offset%)
                        zero)
               label40))
            (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (f2cl-lib:fdo (i (f2cl-lib:int-add j (f2cl-lib:int-sub 1))
                               (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                              ((> i 2) nil)
                  (tagbody
                    (setf (f2cl-lib:fref a-%data% (i j) ((1 lda) (1 *))
                                         a-%offset%)
                            (f2cl-lib:fref a-%data% ((f2cl-lib:int-sub i 1) j)
                                           ((1 lda) (1 *)) a-%offset%))
                   label50))
                (setf (f2cl-lib:fref a-%data% (1 j) ((1 lda) (1 *)) a-%offset%)
                        zero)
               label60))
            (cond
             ((> n 1)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                  (zunglq (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1)
                   (f2cl-lib:int-sub n 1)
                   (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (2 2)
                                         ((1 lda) (1 *)) a-%offset%)
                   lda tau work lwork iinfo)
                (declare
                 (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
                (setf iinfo var-8))))))))
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                (coerce lwkopt 'f2cl-lib:complex16))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zungbr
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((string 1)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
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
                                            '(nil nil nil nil nil nil nil nil
                                              nil fortran-to-lisp::info)
                                            :calls
                                            '(fortran-to-lisp::xerbla
                                              fortran-to-lisp::zunglq
                                              fortran-to-lisp::zungqr
                                              fortran-to-lisp::lsame))))

