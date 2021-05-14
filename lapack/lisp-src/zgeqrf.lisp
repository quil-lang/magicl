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


(defun zgeqrf (m n a lda tau work lwork info)
  (declare (type (array f2cl-lib:complex16 (*)) work tau a)
           (type (f2cl-lib:integer4) info lwork lda n m))
  (f2cl-lib:with-multi-array-data
      ((a f2cl-lib:complex16 a-%data% a-%offset%)
       (tau f2cl-lib:complex16 tau-%data% tau-%offset%)
       (work f2cl-lib:complex16 work-%data% work-%offset%))
    (prog ((i 0) (ib 0) (iinfo 0) (iws 0) (k 0) (ldwork 0) (lwkopt 0) (nb 0)
           (nbmin 0) (nx 0) (lquery nil))
      (declare (type f2cl-lib:logical lquery)
               (type (f2cl-lib:integer4) nx nbmin nb lwkopt ldwork k iws iinfo
                ib i))
      (setf info 0)
      (setf nb (ilaenv 1 "ZGEQRF" " " m n -1 -1))
      (setf lwkopt (f2cl-lib:int-mul n nb))
      (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
              (coerce lwkopt 'f2cl-lib:complex16))
      (setf lquery (coerce (= lwork -1) 'f2cl-lib:logical))
      (cond ((< m 0) (setf info -1)) ((< n 0) (setf info -2))
            ((< lda (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)))
             (setf info -4))
            ((and
              (< lwork
                 (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
              (not lquery))
             (setf info -7)))
      (cond
       ((/= info 0) (xerbla "ZGEQRF" (f2cl-lib:int-sub info)) (go end_label))
       (lquery (go end_label)))
      (setf k (min (the f2cl-lib:integer4 m) (the f2cl-lib:integer4 n)))
      (cond
       ((= k 0)
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                (coerce 1 'f2cl-lib:complex16))
        (go end_label)))
      (setf nbmin 2)
      (setf nx 0)
      (setf iws n)
      (cond
       ((and (> nb 1) (< nb k))
        (setf nx
                (max (the f2cl-lib:integer4 0)
                     (the f2cl-lib:integer4
                          (ilaenv 3 "ZGEQRF" " " m n -1 -1))))
        (cond
         ((< nx k) (setf ldwork n) (setf iws (f2cl-lib:int-mul ldwork nb))
          (cond
           ((< lwork iws)
            (setf nb (the f2cl-lib:integer4 (truncate lwork ldwork)))
            (setf nbmin
                    (max (the f2cl-lib:integer4 2)
                         (the f2cl-lib:integer4
                              (ilaenv 2 "ZGEQRF" " " m n -1 -1))))))))))
      (cond
       ((and (>= nb nbmin) (< nb k) (< nx k))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i nb))
                      ((> i (f2cl-lib:int-add k (f2cl-lib:int-sub nx))) nil)
          (tagbody
            (setf ib
                    (min
                     (the f2cl-lib:integer4
                          (f2cl-lib:int-add (f2cl-lib:int-sub k i) 1))
                     (the f2cl-lib:integer4 nb)))
            (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                (zgeqr2 (f2cl-lib:int-add (f2cl-lib:int-sub m i) 1) ib
                 (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i i)
                                       ((1 lda) (1 *)) a-%offset%)
                 lda
                 (f2cl-lib:array-slice tau-%data% f2cl-lib:complex16 (i)
                                       ((1 *)) tau-%offset%)
                 work iinfo)
              (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5))
              (setf iinfo var-6))
            (cond
             ((<= (f2cl-lib:int-add i ib) n)
              (zlarft "F" "C" (f2cl-lib:int-add (f2cl-lib:int-sub m i) 1) ib
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i i)
                                     ((1 lda) (1 *)) a-%offset%)
               lda
               (f2cl-lib:array-slice tau-%data% f2cl-lib:complex16 (i) ((1 *))
                                     tau-%offset%)
               work ldwork)
              (zlarfb "L" "C" "F" "C"
               (f2cl-lib:int-add (f2cl-lib:int-sub m i) 1)
               (f2cl-lib:int-add (f2cl-lib:int-sub n i ib) 1) ib
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i i)
                                     ((1 lda) (1 *)) a-%offset%)
               lda work ldwork
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16
                                     (i (f2cl-lib:int-add i ib))
                                     ((1 lda) (1 *)) a-%offset%)
               lda
               (f2cl-lib:array-slice work-%data% f2cl-lib:complex16 ((+ ib 1))
                                     ((1 *)) work-%offset%)
               ldwork)))
           label10)))
       (t (setf i 1)))
      (if (<= i k)
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
              (zgeqr2 (f2cl-lib:int-add (f2cl-lib:int-sub m i) 1)
               (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1)
               (f2cl-lib:array-slice a-%data% f2cl-lib:complex16 (i i)
                                     ((1 lda) (1 *)) a-%offset%)
               lda
               (f2cl-lib:array-slice tau-%data% f2cl-lib:complex16 (i) ((1 *))
                                     tau-%offset%)
               work iinfo)
            (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5))
            (setf iinfo var-6)))
      (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
              (coerce iws 'f2cl-lib:complex16))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil info)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zgeqrf
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
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil nil nil
                                              fortran-to-lisp::info)
                                            :calls
                                            '(fortran-to-lisp::zlarfb
                                              fortran-to-lisp::zlarft
                                              fortran-to-lisp::zgeqr2
                                              fortran-to-lisp::xerbla
                                              fortran-to-lisp::ilaenv))))

