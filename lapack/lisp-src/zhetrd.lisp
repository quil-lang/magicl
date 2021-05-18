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


(let* ((one 1.0d0) (cone (f2cl-lib:cmplx 1.0d0 0.0d0)))
  (declare (type (double-float 1.0d0 1.0d0) one)
           (type (f2cl-lib:complex16) cone)
           (ignorable one cone))
  (defun zhetrd (uplo n a lda d e tau work lwork info)
    (declare (type (array double-float (*)) e d)
             (type (array f2cl-lib:complex16 (*)) work tau a)
             (type (f2cl-lib:integer4) info lwork lda n)
             (type (string 1) uplo))
    (f2cl-lib:with-multi-array-data
        ((uplo character uplo-%data% uplo-%offset%)
         (a f2cl-lib:complex16 a-%data% a-%offset%)
         (tau f2cl-lib:complex16 tau-%data% tau-%offset%)
         (work f2cl-lib:complex16 work-%data% work-%offset%)
         (d double-float d-%data% d-%offset%)
         (e double-float e-%data% e-%offset%))
      (prog ((i 0) (iinfo 0) (iws 0) (j 0) (kk 0) (ldwork 0) (lwkopt 0) (nb 0)
             (nbmin 0) (nx 0) (lquery nil) (upper nil))
        (declare
         (type (f2cl-lib:integer4) i iinfo iws j kk ldwork lwkopt nb nbmin nx)
         (type f2cl-lib:logical lquery upper))
        (setf info 0)
        (setf upper (lsame uplo "U"))
        (setf lquery (coerce (= lwork -1) 'f2cl-lib:logical))
        (cond ((and (not upper) (not (lsame uplo "L"))) (setf info -1))
              ((< n 0) (setf info -2))
              ((< lda
                  (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
               (setf info -4))
              ((and (< lwork 1) (not lquery)) (setf info -9)))
        (cond
         ((= info 0) (setf nb (ilaenv 1 "ZHETRD" uplo n -1 -1 -1))
          (setf lwkopt (f2cl-lib:int-mul n nb))
          (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                  (coerce lwkopt 'f2cl-lib:complex16))))
        (cond
         ((/= info 0) (xerbla "ZHETRD" (f2cl-lib:int-sub info)) (go end_label))
         (lquery (go end_label)))
        (cond
         ((= n 0)
          (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                  (coerce 1 'f2cl-lib:complex16))
          (go end_label)))
        (setf nx n)
        (setf iws 1)
        (cond
         ((and (> nb 1) (< nb n))
          (setf nx
                  (max (the f2cl-lib:integer4 nb)
                       (the f2cl-lib:integer4
                            (ilaenv 3 "ZHETRD" uplo n -1 -1 -1))))
          (cond
           ((< nx n) (setf ldwork n) (setf iws (f2cl-lib:int-mul ldwork nb))
            (cond
             ((< lwork iws)
              (setf nb (max (the f2cl-lib:integer4 (truncate lwork ldwork)) 1))
              (setf nbmin (ilaenv 2 "ZHETRD" uplo n -1 -1 -1))
              (if (< nb nbmin)
                  (setf nx n)))))
           (t (setf nx n))))
         (t (setf nb 1)))
        (cond
         (upper
          (setf kk
                  (- n
                     (*
                      (the f2cl-lib:integer4
                           (truncate (- (+ (- n nx) nb) 1) nb))
                      nb)))
          (f2cl-lib:fdo (i (f2cl-lib:int-add n (f2cl-lib:int-sub nb) 1)
                         (f2cl-lib:int-add i (f2cl-lib:int-sub nb)))
                        ((> i (f2cl-lib:int-add kk 1)) nil)
            (tagbody
              (zlatrd uplo (f2cl-lib:int-sub (f2cl-lib:int-add i nb) 1) nb a
               lda e tau work ldwork)
              (zher2k uplo "N" (f2cl-lib:int-sub i 1) nb (- cone)
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (1 i)
                                     ((1 lda) (1 *)) a-%offset%)
               lda work ldwork one a lda)
              (f2cl-lib:fdo (j i (f2cl-lib:int-add j 1))
                            ((> j (f2cl-lib:int-add i nb (f2cl-lib:int-sub 1)))
                             nil)
                (tagbody
                  (setf (f2cl-lib:fref a-%data% ((f2cl-lib:int-sub j 1) j)
                                       ((1 lda) (1 *)) a-%offset%)
                          (coerce
                           (f2cl-lib:fref e-%data% ((f2cl-lib:int-sub j 1))
                                          ((1 *)) e-%offset%)
                           'f2cl-lib:complex16))
                  (setf (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%)
                          (coerce
                           (realpart
                            (f2cl-lib:fref a-%data% (j j) ((1 lda) (1 *))
                                           a-%offset%))
                           'double-float))
                 label10))
             label20))
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
              (zhetd2 uplo kk a lda d e tau iinfo)
            (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
            (setf iinfo var-7)))
         (t
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i nb))
                        ((> i (f2cl-lib:int-add n (f2cl-lib:int-sub nx))) nil)
            (tagbody
              (zlatrd uplo (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1) nb
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i i)
                                     ((1 lda) (1 *)) a-%offset%)
               lda
               (f2cl-lib:array-slice e-%data% double-float (i) ((1 *))
                                     e-%offset%)
               (f2cl-lib:array-slice tau-%data% f2cl-lib:complex16 (i) ((1 *))
                                     tau-%offset%)
               work ldwork)
              (zher2k uplo "N" (f2cl-lib:int-add (f2cl-lib:int-sub n i nb) 1)
               nb (- cone)
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 ((+ i nb) i)
                                     ((1 lda) (1 *)) a-%offset%)
               lda
               (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 ((+ nb 1))
                                     ((1 *)) work-%offset%)
               ldwork one
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                     ((+ i nb) (f2cl-lib:int-add i nb))
                                     ((1 lda) (1 *)) a-%offset%)
               lda)
              (f2cl-lib:fdo (j i (f2cl-lib:int-add j 1))
                            ((> j (f2cl-lib:int-add i nb (f2cl-lib:int-sub 1)))
                             nil)
                (tagbody
                  (setf (f2cl-lib:fref a-%data% ((f2cl-lib:int-add j 1) j)
                                       ((1 lda) (1 *)) a-%offset%)
                          (coerce
                           (f2cl-lib:fref e-%data% (j) ((1 *)) e-%offset%)
                           'f2cl-lib:complex16))
                  (setf (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%)
                          (coerce
                           (realpart
                            (f2cl-lib:fref a-%data% (j j) ((1 lda) (1 *))
                                           a-%offset%))
                           'double-float))
                 label30))
             label40))
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
              (zhetd2 uplo (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1)
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i i)
                                     ((1 lda) (1 *)) a-%offset%)
               lda
               (f2cl-lib:array-slice d-%data% double-float (i) ((1 *))
                                     d-%offset%)
               (f2cl-lib:array-slice e-%data% double-float (i) ((1 *))
                                     e-%offset%)
               (f2cl-lib:array-slice tau-%data% f2cl-lib:complex16 (i) ((1 *))
                                     tau-%offset%)
               iinfo)
            (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
            (setf iinfo var-7))))
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                (coerce lwkopt 'f2cl-lib:complex16))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zhetrd
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
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil nil nil nil
                                              nil fortran-to-lisp::info)
                                            :calls
                                            '(fortran-to-lisp::zhetd2
                                              fortran-to-lisp::zher2k
                                              fortran-to-lisp::zlatrd
                                              fortran-to-lisp::xerbla
                                              fortran-to-lisp::ilaenv
                                              fortran-to-lisp::lsame))))

