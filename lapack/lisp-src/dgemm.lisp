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


(let* ((one 1.0d0) (zero 0.0d0))
  (declare (type (double-float 1.0d0 1.0d0) one)
           (type (double-float 0.0d0 0.0d0) zero)
           (ignorable one zero))
  (defun dgemm (transa transb m n k alpha a lda b ldb$ beta c ldc)
    (declare (type (array double-float (*)) c b a)
             (type (double-float) beta alpha)
             (type (f2cl-lib:integer4) ldc ldb$ lda k n m)
             (type (string 1) transb transa))
    (f2cl-lib:with-multi-array-data
        ((transa character transa-%data% transa-%offset%)
         (transb character transb-%data% transb-%offset%)
         (a double-float a-%data% a-%offset%)
         (b double-float b-%data% b-%offset%)
         (c double-float c-%data% c-%offset%))
      (prog ((nota nil) (notb nil) (i 0) (info 0) (j 0) (l 0) (ncola 0)
             (nrowa 0) (nrowb 0) (temp 0.0d0))
        (declare (type f2cl-lib:logical nota notb)
                 (type (f2cl-lib:integer4) i info j l ncola nrowa nrowb)
                 (type (double-float) temp))
        (setf nota (lsame transa "N"))
        (setf notb (lsame transb "N"))
        (cond (nota (setf nrowa m) (setf ncola k))
              (t (setf nrowa k) (setf ncola m)))
        (cond (notb (setf nrowb k)) (t (setf nrowb n)))
        (setf info 0)
        (cond
         ((and (not nota) (not (lsame transa "C")) (not (lsame transa "T")))
          (setf info 1))
         ((and (not notb) (not (lsame transb "C")) (not (lsame transb "T")))
          (setf info 2))
         ((< m 0) (setf info 3)) ((< n 0) (setf info 4))
         ((< k 0) (setf info 5))
         ((< lda (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 nrowa)))
          (setf info 8))
         ((< ldb$
             (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 nrowb)))
          (setf info 10))
         ((< ldc (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)))
          (setf info 13)))
        (cond ((/= info 0) (xerbla "DGEMM " info) (go end_label)))
        (if (or (= m 0) (= n 0) (and (or (= alpha zero) (= k 0)) (= beta one)))
            (go end_label))
        (cond
         ((= alpha zero)
          (cond
           ((= beta zero)
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i m) nil)
                  (tagbody
                    (setf (f2cl-lib:fref c-%data% (i j) ((1 ldc) (1 *))
                                         c-%offset%)
                            zero)
                   label10))
               label20)))
           (t
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i m) nil)
                  (tagbody
                    (setf (f2cl-lib:fref c-%data% (i j) ((1 ldc) (1 *))
                                         c-%offset%)
                            (* beta
                               (f2cl-lib:fref c-%data% (i j) ((1 ldc) (1 *))
                                              c-%offset%)))
                   label30))
               label40))))
          (go end_label)))
        (cond
         (notb
          (cond
           (nota
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (cond
                 ((= beta zero)
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i m) nil)
                    (tagbody
                      (setf (f2cl-lib:fref c-%data% (i j) ((1 ldc) (1 *))
                                           c-%offset%)
                              zero)
                     label50)))
                 ((/= beta one)
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i m) nil)
                    (tagbody
                      (setf (f2cl-lib:fref c-%data% (i j) ((1 ldc) (1 *))
                                           c-%offset%)
                              (* beta
                                 (f2cl-lib:fref c-%data% (i j) ((1 ldc) (1 *))
                                                c-%offset%)))
                     label60))))
                (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                              ((> l k) nil)
                  (tagbody
                    (cond
                     ((/= (f2cl-lib:fref b (l j) ((1 ldb$) (1 *))) zero)
                      (setf temp
                              (* alpha
                                 (f2cl-lib:fref b-%data% (l j) ((1 ldb$) (1 *))
                                                b-%offset%)))
                      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                    ((> i m) nil)
                        (tagbody
                          (setf (f2cl-lib:fref c-%data% (i j) ((1 ldc) (1 *))
                                               c-%offset%)
                                  (+
                                   (f2cl-lib:fref c-%data% (i j)
                                                  ((1 ldc) (1 *)) c-%offset%)
                                   (* temp
                                      (f2cl-lib:fref a-%data% (i l)
                                                     ((1 lda) (1 *))
                                                     a-%offset%))))
                         label70))))
                   label80))
               label90)))
           (t
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i m) nil)
                  (tagbody
                    (setf temp zero)
                    (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                                  ((> l k) nil)
                      (tagbody
                        (setf temp
                                (+ temp
                                   (*
                                    (f2cl-lib:fref a-%data% (l i)
                                                   ((1 lda) (1 *)) a-%offset%)
                                    (f2cl-lib:fref b-%data% (l j)
                                                   ((1 ldb$) (1 *))
                                                   b-%offset%))))
                       label100))
                    (cond
                     ((= beta zero)
                      (setf (f2cl-lib:fref c-%data% (i j) ((1 ldc) (1 *))
                                           c-%offset%)
                              (* alpha temp)))
                     (t
                      (setf (f2cl-lib:fref c-%data% (i j) ((1 ldc) (1 *))
                                           c-%offset%)
                              (+ (* alpha temp)
                                 (* beta
                                    (f2cl-lib:fref c-%data% (i j)
                                                   ((1 ldc) (1 *))
                                                   c-%offset%))))))
                   label110))
               label120)))))
         (t
          (cond
           (nota
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (cond
                 ((= beta zero)
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i m) nil)
                    (tagbody
                      (setf (f2cl-lib:fref c-%data% (i j) ((1 ldc) (1 *))
                                           c-%offset%)
                              zero)
                     label130)))
                 ((/= beta one)
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i m) nil)
                    (tagbody
                      (setf (f2cl-lib:fref c-%data% (i j) ((1 ldc) (1 *))
                                           c-%offset%)
                              (* beta
                                 (f2cl-lib:fref c-%data% (i j) ((1 ldc) (1 *))
                                                c-%offset%)))
                     label140))))
                (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                              ((> l k) nil)
                  (tagbody
                    (cond
                     ((/= (f2cl-lib:fref b (j l) ((1 ldb$) (1 *))) zero)
                      (setf temp
                              (* alpha
                                 (f2cl-lib:fref b-%data% (j l) ((1 ldb$) (1 *))
                                                b-%offset%)))
                      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                    ((> i m) nil)
                        (tagbody
                          (setf (f2cl-lib:fref c-%data% (i j) ((1 ldc) (1 *))
                                               c-%offset%)
                                  (+
                                   (f2cl-lib:fref c-%data% (i j)
                                                  ((1 ldc) (1 *)) c-%offset%)
                                   (* temp
                                      (f2cl-lib:fref a-%data% (i l)
                                                     ((1 lda) (1 *))
                                                     a-%offset%))))
                         label150))))
                   label160))
               label170)))
           (t
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i m) nil)
                  (tagbody
                    (setf temp zero)
                    (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                                  ((> l k) nil)
                      (tagbody
                        (setf temp
                                (+ temp
                                   (*
                                    (f2cl-lib:fref a-%data% (l i)
                                                   ((1 lda) (1 *)) a-%offset%)
                                    (f2cl-lib:fref b-%data% (j l)
                                                   ((1 ldb$) (1 *))
                                                   b-%offset%))))
                       label180))
                    (cond
                     ((= beta zero)
                      (setf (f2cl-lib:fref c-%data% (i j) ((1 ldc) (1 *))
                                           c-%offset%)
                              (* alpha temp)))
                     (t
                      (setf (f2cl-lib:fref c-%data% (i j) ((1 ldc) (1 *))
                                           c-%offset%)
                              (+ (* alpha temp)
                                 (* beta
                                    (f2cl-lib:fref c-%data% (i j)
                                                   ((1 ldc) (1 *))
                                                   c-%offset%))))))
                   label190))
               label200))))))
        (go end_label)
       end_label
        (return
         (values nil nil nil nil nil nil nil nil nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dgemm fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((string 1) (string 1)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (double-float)
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4)
                                              (double-float)
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil nil nil nil
                                              nil nil nil nil nil)
                                            :calls
                                            '(fortran-to-lisp::xerbla
                                              fortran-to-lisp::lsame))))

