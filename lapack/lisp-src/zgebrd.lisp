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
  (defun zgebrd (m n a lda d e tauq taup work lwork info)
    (declare (type (array double-float (*)) e d)
             (type (array f2cl-lib:complex16 (*)) work taup tauq a)
             (type (f2cl-lib:integer4) info lwork lda n m))
    (f2cl-lib:with-multi-array-data
        ((a f2cl-lib:complex16 a-%data% a-%offset%)
         (tauq f2cl-lib:complex16 tauq-%data% tauq-%offset%)
         (taup f2cl-lib:complex16 taup-%data% taup-%offset%)
         (work f2cl-lib:complex16 work-%data% work-%offset%)
         (d double-float d-%data% d-%offset%)
         (e double-float e-%data% e-%offset%))
      (prog ((ws 0.0d0) (i 0) (iinfo 0) (j 0) (ldwrkx 0) (ldwrky 0) (lwkopt 0)
             (minmn 0) (nb 0) (nbmin 0) (nx 0) (lquery nil))
        (declare (type (double-float) ws)
                 (type (f2cl-lib:integer4) i iinfo j ldwrkx ldwrky lwkopt minmn
                  nb nbmin nx)
                 (type f2cl-lib:logical lquery))
        (setf info 0)
        (setf nb
                (max (the f2cl-lib:integer4 1)
                     (the f2cl-lib:integer4
                          (ilaenv 1 "ZGEBRD" " " m n -1 -1))))
        (setf lwkopt (f2cl-lib:int-mul (f2cl-lib:int-add m n) nb))
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                (coerce (f2cl-lib:dble lwkopt) 'f2cl-lib:complex16))
        (setf lquery (coerce (= lwork -1) 'f2cl-lib:logical))
        (cond ((< m 0) (setf info -1)) ((< n 0) (setf info -2))
              ((< lda
                  (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)))
               (setf info -4))
              ((and
                (< lwork
                   (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)
                        (the f2cl-lib:integer4 n)))
                (not lquery))
               (setf info -10)))
        (cond
         ((< info 0) (xerbla "ZGEBRD" (f2cl-lib:int-sub info)) (go end_label))
         (lquery (go end_label)))
        (setf minmn (min (the f2cl-lib:integer4 m) (the f2cl-lib:integer4 n)))
        (cond
         ((= minmn 0)
          (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                  (coerce 1 'f2cl-lib:complex16))
          (go end_label)))
        (setf ws
                (coerce
                 (the f2cl-lib:integer4
                      (max (the f2cl-lib:integer4 m)
                           (the f2cl-lib:integer4 n)))
                 'double-float))
        (setf ldwrkx m)
        (setf ldwrky n)
        (cond
         ((and (> nb 1) (< nb minmn))
          (setf nx
                  (max (the f2cl-lib:integer4 nb)
                       (the f2cl-lib:integer4
                            (ilaenv 3 "ZGEBRD" " " m n -1 -1))))
          (cond
           ((< nx minmn)
            (setf ws
                    (coerce
                     (the f2cl-lib:integer4
                          (f2cl-lib:int-mul (f2cl-lib:int-add m n) nb))
                     'double-float))
            (cond
             ((< lwork ws) (setf nbmin (ilaenv 2 "ZGEBRD" " " m n -1 -1))
              (cond
               ((>= lwork (f2cl-lib:int-mul (f2cl-lib:int-add m n) nbmin))
                (setf nb (the f2cl-lib:integer4 (truncate lwork (+ m n)))))
               (t (setf nb 1) (setf nx minmn))))))))
         (t (setf nx minmn)))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i nb))
                      ((> i (f2cl-lib:int-add minmn (f2cl-lib:int-sub nx))) nil)
          (tagbody
            (zlabrd (f2cl-lib:int-add (f2cl-lib:int-sub m i) 1)
             (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1) nb
             (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i i)
                                   ((1 lda) (1 *)) a-%offset%)
             lda
             (f2cl-lib:array-slice d-%data% double-float (i) ((1 *))
                                   d-%offset%)
             (f2cl-lib:array-slice e-%data% double-float (i) ((1 *))
                                   e-%offset%)
             (f2cl-lib:array-slice tauq-%data% f2cl-lib:complex16 (i) ((1 *))
                                   tauq-%offset%)
             (f2cl-lib:array-slice taup-%data% f2cl-lib:complex16 (i) ((1 *))
                                   taup-%offset%)
             work ldwrkx
             (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                   ((+ (f2cl-lib:int-mul ldwrkx nb) 1)) ((1 *))
                                   work-%offset%)
             ldwrky)
            (zgemm "N" "C" (f2cl-lib:int-add (f2cl-lib:int-sub m i nb) 1)
             (f2cl-lib:int-add (f2cl-lib:int-sub n i nb) 1) nb (- one)
             (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 ((+ i nb) i)
                                   ((1 lda) (1 *)) a-%offset%)
             lda
             (f2cl-lib:array-slice work-%data% f2cl-lib:complex16
                                   ((+ (f2cl-lib:int-mul ldwrkx nb) nb 1))
                                   ((1 *)) work-%offset%)
             ldwrky one
             (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                   ((+ i nb) (f2cl-lib:int-add i nb))
                                   ((1 lda) (1 *)) a-%offset%)
             lda)
            (zgemm "N" "N" (f2cl-lib:int-add (f2cl-lib:int-sub m i nb) 1)
             (f2cl-lib:int-add (f2cl-lib:int-sub n i nb) 1) nb (- one)
             (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 ((+ nb 1))
                                   ((1 *)) work-%offset%)
             ldwrkx
             (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                   (i (f2cl-lib:int-add i nb)) ((1 lda) (1 *))
                                   a-%offset%)
             lda one
             (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                   ((+ i nb) (f2cl-lib:int-add i nb))
                                   ((1 lda) (1 *)) a-%offset%)
             lda)
            (cond
             ((>= m n)
              (f2cl-lib:fdo (j i (f2cl-lib:int-add j 1))
                            ((> j (f2cl-lib:int-add i nb (f2cl-lib:int-sub 1)))
                             nil)
                (tagbody
                  (setf (f2cl-lib:fref a-%data% (j j) ((1 lda) (1 *))
                                       a-%offset%)
                          (coerce
                           (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%)
                           'f2cl-lib:complex16))
                  (setf (f2cl-lib:fref a-%data% (j (f2cl-lib:int-add j 1))
                                       ((1 lda) (1 *)) a-%offset%)
                          (coerce
                           (f2cl-lib:fref e-%data% (j) ((1 *)) e-%offset%)
                           'f2cl-lib:complex16))
                 label10)))
             (t
              (f2cl-lib:fdo (j i (f2cl-lib:int-add j 1))
                            ((> j (f2cl-lib:int-add i nb (f2cl-lib:int-sub 1)))
                             nil)
                (tagbody
                  (setf (f2cl-lib:fref a-%data% (j j) ((1 lda) (1 *))
                                       a-%offset%)
                          (coerce
                           (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%)
                           'f2cl-lib:complex16))
                  (setf (f2cl-lib:fref a-%data% ((f2cl-lib:int-add j 1) j)
                                       ((1 lda) (1 *)) a-%offset%)
                          (coerce
                           (f2cl-lib:fref e-%data% (j) ((1 *)) e-%offset%)
                           'f2cl-lib:complex16))
                 label20))))
           label30))
        (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
            (zgebd2 (f2cl-lib:int-add (f2cl-lib:int-sub m i) 1)
             (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1)
             (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i i)
                                   ((1 lda) (1 *)) a-%offset%)
             lda
             (f2cl-lib:array-slice d-%data% double-float (i) ((1 *))
                                   d-%offset%)
             (f2cl-lib:array-slice e-%data% double-float (i) ((1 *))
                                   e-%offset%)
             (f2cl-lib:array-slice tauq-%data% f2cl-lib:complex16 (i) ((1 *))
                                   tauq-%offset%)
             (f2cl-lib:array-slice taup-%data% f2cl-lib:complex16 (i) ((1 *))
                                   taup-%offset%)
             work iinfo)
          (declare
           (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8))
          (setf iinfo var-9))
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                (coerce ws 'f2cl-lib:complex16))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zgebrd
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
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil nil nil nil
                                              nil nil fortran-to-lisp::info)
                                            :calls
                                            '(fortran-to-lisp::zgebd2
                                              fortran-to-lisp::zgemm
                                              fortran-to-lisp::zlabrd
                                              fortran-to-lisp::xerbla
                                              fortran-to-lisp::ilaenv))))

