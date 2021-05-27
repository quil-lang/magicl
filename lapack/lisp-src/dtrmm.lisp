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
  (defun dtrmm (side uplo transa diag m n alpha a lda b ldb$)
    (declare (type (array double-float (*)) b a)
             (type (double-float) alpha)
             (type (f2cl-lib:integer4) ldb$ lda n m)
             (type (string 1) diag transa uplo side))
    (f2cl-lib:with-multi-array-data
        ((side character side-%data% side-%offset%)
         (uplo character uplo-%data% uplo-%offset%)
         (transa character transa-%data% transa-%offset%)
         (diag character diag-%data% diag-%offset%)
         (a double-float a-%data% a-%offset%)
         (b double-float b-%data% b-%offset%))
      (prog ((lside nil) (nounit nil) (upper nil) (i 0) (info 0) (j 0) (k 0)
             (nrowa 0) (temp 0.0d0))
        (declare (type f2cl-lib:logical lside nounit upper)
                 (type (f2cl-lib:integer4) i info j k nrowa)
                 (type (double-float) temp))
        (setf lside (lsame side "L"))
        (cond (lside (setf nrowa m)) (t (setf nrowa n)))
        (setf nounit (lsame diag "N"))
        (setf upper (lsame uplo "U"))
        (setf info 0)
        (cond ((and (not lside) (not (lsame side "R"))) (setf info 1))
              ((and (not upper) (not (lsame uplo "L"))) (setf info 2))
              ((and (not (lsame transa "N")) (not (lsame transa "T"))
                    (not (lsame transa "C")))
               (setf info 3))
              ((and (not (lsame diag "U")) (not (lsame diag "N")))
               (setf info 4))
              ((< m 0) (setf info 5)) ((< n 0) (setf info 6))
              ((< lda
                  (max (the f2cl-lib:integer4 1)
                       (the f2cl-lib:integer4 nrowa)))
               (setf info 9))
              ((< ldb$
                  (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)))
               (setf info 11)))
        (cond ((/= info 0) (xerbla "DTRMM " info) (go end_label)))
        (if (or (= m 0) (= n 0))
            (go end_label))
        (cond
         ((= alpha zero)
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                            ((> i m) nil)
                (tagbody
                  (setf (f2cl-lib:fref b-%data% (i j) ((1 ldb$) (1 *))
                                       b-%offset%)
                          zero)
                 label10))
             label20))
          (go end_label)))
        (cond
         (lside
          (cond
           ((lsame transa "N")
            (cond
             (upper
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                ((> k m) nil)
                    (tagbody
                      (cond
                       ((/= (f2cl-lib:fref b (k j) ((1 ldb$) (1 *))) zero)
                        (setf temp
                                (* alpha
                                   (f2cl-lib:fref b-%data% (k j)
                                                  ((1 ldb$) (1 *))
                                                  b-%offset%)))
                        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                      ((> i
                                          (f2cl-lib:int-add k
                                                            (f2cl-lib:int-sub
                                                             1)))
                                       nil)
                          (tagbody
                            (setf (f2cl-lib:fref b-%data% (i j)
                                                 ((1 ldb$) (1 *)) b-%offset%)
                                    (+
                                     (f2cl-lib:fref b-%data% (i j)
                                                    ((1 ldb$) (1 *))
                                                    b-%offset%)
                                     (* temp
                                        (f2cl-lib:fref a-%data% (i k)
                                                       ((1 lda) (1 *))
                                                       a-%offset%))))
                           label30))
                        (if nounit
                            (setf temp
                                    (* temp
                                       (f2cl-lib:fref a-%data% (k k)
                                                      ((1 lda) (1 *))
                                                      a-%offset%))))
                        (setf (f2cl-lib:fref b-%data% (k j) ((1 ldb$) (1 *))
                                             b-%offset%)
                                temp)))
                     label40))
                 label50)))
             (t
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (f2cl-lib:fdo (k m (f2cl-lib:int-add k (f2cl-lib:int-sub 1)))
                                ((> k 1) nil)
                    (tagbody
                      (cond
                       ((/= (f2cl-lib:fref b (k j) ((1 ldb$) (1 *))) zero)
                        (setf temp
                                (* alpha
                                   (f2cl-lib:fref b-%data% (k j)
                                                  ((1 ldb$) (1 *))
                                                  b-%offset%)))
                        (setf (f2cl-lib:fref b-%data% (k j) ((1 ldb$) (1 *))
                                             b-%offset%)
                                temp)
                        (if nounit
                            (setf (f2cl-lib:fref b-%data% (k j)
                                                 ((1 ldb$) (1 *)) b-%offset%)
                                    (*
                                     (f2cl-lib:fref b-%data% (k j)
                                                    ((1 ldb$) (1 *))
                                                    b-%offset%)
                                     (f2cl-lib:fref a-%data% (k k)
                                                    ((1 lda) (1 *))
                                                    a-%offset%))))
                        (f2cl-lib:fdo (i (f2cl-lib:int-add k 1)
                                       (f2cl-lib:int-add i 1))
                                      ((> i m) nil)
                          (tagbody
                            (setf (f2cl-lib:fref b-%data% (i j)
                                                 ((1 ldb$) (1 *)) b-%offset%)
                                    (+
                                     (f2cl-lib:fref b-%data% (i j)
                                                    ((1 ldb$) (1 *))
                                                    b-%offset%)
                                     (* temp
                                        (f2cl-lib:fref a-%data% (i k)
                                                       ((1 lda) (1 *))
                                                       a-%offset%))))
                           label60))))
                     label70))
                 label80)))))
           (t
            (cond
             (upper
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (f2cl-lib:fdo (i m (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                                ((> i 1) nil)
                    (tagbody
                      (setf temp
                              (f2cl-lib:fref b-%data% (i j) ((1 ldb$) (1 *))
                                             b-%offset%))
                      (if nounit
                          (setf temp
                                  (* temp
                                     (f2cl-lib:fref a-%data% (i i)
                                                    ((1 lda) (1 *))
                                                    a-%offset%))))
                      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                    ((> k
                                        (f2cl-lib:int-add i
                                                          (f2cl-lib:int-sub
                                                           1)))
                                     nil)
                        (tagbody
                          (setf temp
                                  (+ temp
                                     (*
                                      (f2cl-lib:fref a-%data% (k i)
                                                     ((1 lda) (1 *))
                                                     a-%offset%)
                                      (f2cl-lib:fref b-%data% (k j)
                                                     ((1 ldb$) (1 *))
                                                     b-%offset%))))
                         label90))
                      (setf (f2cl-lib:fref b-%data% (i j) ((1 ldb$) (1 *))
                                           b-%offset%)
                              (* alpha temp))
                     label100))
                 label110)))
             (t
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i m) nil)
                    (tagbody
                      (setf temp
                              (f2cl-lib:fref b-%data% (i j) ((1 ldb$) (1 *))
                                             b-%offset%))
                      (if nounit
                          (setf temp
                                  (* temp
                                     (f2cl-lib:fref a-%data% (i i)
                                                    ((1 lda) (1 *))
                                                    a-%offset%))))
                      (f2cl-lib:fdo (k (f2cl-lib:int-add i 1)
                                     (f2cl-lib:int-add k 1))
                                    ((> k m) nil)
                        (tagbody
                          (setf temp
                                  (+ temp
                                     (*
                                      (f2cl-lib:fref a-%data% (k i)
                                                     ((1 lda) (1 *))
                                                     a-%offset%)
                                      (f2cl-lib:fref b-%data% (k j)
                                                     ((1 ldb$) (1 *))
                                                     b-%offset%))))
                         label120))
                      (setf (f2cl-lib:fref b-%data% (i j) ((1 ldb$) (1 *))
                                           b-%offset%)
                              (* alpha temp))
                     label130))
                 label140)))))))
         (t
          (cond
           ((lsame transa "N")
            (cond
             (upper
              (f2cl-lib:fdo (j n (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                            ((> j 1) nil)
                (tagbody
                  (setf temp alpha)
                  (if nounit
                      (setf temp
                              (* temp
                                 (f2cl-lib:fref a-%data% (j j) ((1 lda) (1 *))
                                                a-%offset%))))
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i m) nil)
                    (tagbody
                      (setf (f2cl-lib:fref b-%data% (i j) ((1 ldb$) (1 *))
                                           b-%offset%)
                              (* temp
                                 (f2cl-lib:fref b-%data% (i j) ((1 ldb$) (1 *))
                                                b-%offset%)))
                     label150))
                  (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                ((> k
                                    (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                                 nil)
                    (tagbody
                      (cond
                       ((/= (f2cl-lib:fref a (k j) ((1 lda) (1 *))) zero)
                        (setf temp
                                (* alpha
                                   (f2cl-lib:fref a-%data% (k j)
                                                  ((1 lda) (1 *)) a-%offset%)))
                        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                      ((> i m) nil)
                          (tagbody
                            (setf (f2cl-lib:fref b-%data% (i j)
                                                 ((1 ldb$) (1 *)) b-%offset%)
                                    (+
                                     (f2cl-lib:fref b-%data% (i j)
                                                    ((1 ldb$) (1 *))
                                                    b-%offset%)
                                     (* temp
                                        (f2cl-lib:fref b-%data% (i k)
                                                       ((1 ldb$) (1 *))
                                                       b-%offset%))))
                           label160))))
                     label170))
                 label180)))
             (t
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (setf temp alpha)
                  (if nounit
                      (setf temp
                              (* temp
                                 (f2cl-lib:fref a-%data% (j j) ((1 lda) (1 *))
                                                a-%offset%))))
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i m) nil)
                    (tagbody
                      (setf (f2cl-lib:fref b-%data% (i j) ((1 ldb$) (1 *))
                                           b-%offset%)
                              (* temp
                                 (f2cl-lib:fref b-%data% (i j) ((1 ldb$) (1 *))
                                                b-%offset%)))
                     label190))
                  (f2cl-lib:fdo (k (f2cl-lib:int-add j 1)
                                 (f2cl-lib:int-add k 1))
                                ((> k n) nil)
                    (tagbody
                      (cond
                       ((/= (f2cl-lib:fref a (k j) ((1 lda) (1 *))) zero)
                        (setf temp
                                (* alpha
                                   (f2cl-lib:fref a-%data% (k j)
                                                  ((1 lda) (1 *)) a-%offset%)))
                        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                      ((> i m) nil)
                          (tagbody
                            (setf (f2cl-lib:fref b-%data% (i j)
                                                 ((1 ldb$) (1 *)) b-%offset%)
                                    (+
                                     (f2cl-lib:fref b-%data% (i j)
                                                    ((1 ldb$) (1 *))
                                                    b-%offset%)
                                     (* temp
                                        (f2cl-lib:fref b-%data% (i k)
                                                       ((1 ldb$) (1 *))
                                                       b-%offset%))))
                           label200))))
                     label210))
                 label220)))))
           (t
            (cond
             (upper
              (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                            ((> k n) nil)
                (tagbody
                  (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                ((> j
                                    (f2cl-lib:int-add k (f2cl-lib:int-sub 1)))
                                 nil)
                    (tagbody
                      (cond
                       ((/= (f2cl-lib:fref a (j k) ((1 lda) (1 *))) zero)
                        (setf temp
                                (* alpha
                                   (f2cl-lib:fref a-%data% (j k)
                                                  ((1 lda) (1 *)) a-%offset%)))
                        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                      ((> i m) nil)
                          (tagbody
                            (setf (f2cl-lib:fref b-%data% (i j)
                                                 ((1 ldb$) (1 *)) b-%offset%)
                                    (+
                                     (f2cl-lib:fref b-%data% (i j)
                                                    ((1 ldb$) (1 *))
                                                    b-%offset%)
                                     (* temp
                                        (f2cl-lib:fref b-%data% (i k)
                                                       ((1 ldb$) (1 *))
                                                       b-%offset%))))
                           label230))))
                     label240))
                  (setf temp alpha)
                  (if nounit
                      (setf temp
                              (* temp
                                 (f2cl-lib:fref a-%data% (k k) ((1 lda) (1 *))
                                                a-%offset%))))
                  (cond
                   ((/= temp one)
                    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                  ((> i m) nil)
                      (tagbody
                        (setf (f2cl-lib:fref b-%data% (i k) ((1 ldb$) (1 *))
                                             b-%offset%)
                                (* temp
                                   (f2cl-lib:fref b-%data% (i k)
                                                  ((1 ldb$) (1 *))
                                                  b-%offset%)))
                       label250))))
                 label260)))
             (t
              (f2cl-lib:fdo (k n (f2cl-lib:int-add k (f2cl-lib:int-sub 1)))
                            ((> k 1) nil)
                (tagbody
                  (f2cl-lib:fdo (j (f2cl-lib:int-add k 1)
                                 (f2cl-lib:int-add j 1))
                                ((> j n) nil)
                    (tagbody
                      (cond
                       ((/= (f2cl-lib:fref a (j k) ((1 lda) (1 *))) zero)
                        (setf temp
                                (* alpha
                                   (f2cl-lib:fref a-%data% (j k)
                                                  ((1 lda) (1 *)) a-%offset%)))
                        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                      ((> i m) nil)
                          (tagbody
                            (setf (f2cl-lib:fref b-%data% (i j)
                                                 ((1 ldb$) (1 *)) b-%offset%)
                                    (+
                                     (f2cl-lib:fref b-%data% (i j)
                                                    ((1 ldb$) (1 *))
                                                    b-%offset%)
                                     (* temp
                                        (f2cl-lib:fref b-%data% (i k)
                                                       ((1 ldb$) (1 *))
                                                       b-%offset%))))
                           label270))))
                     label280))
                  (setf temp alpha)
                  (if nounit
                      (setf temp
                              (* temp
                                 (f2cl-lib:fref a-%data% (k k) ((1 lda) (1 *))
                                                a-%offset%))))
                  (cond
                   ((/= temp one)
                    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                  ((> i m) nil)
                      (tagbody
                        (setf (f2cl-lib:fref b-%data% (i k) ((1 ldb$) (1 *))
                                             b-%offset%)
                                (* temp
                                   (f2cl-lib:fref b-%data% (i k)
                                                  ((1 ldb$) (1 *))
                                                  b-%offset%)))
                       label290))))
                 label300))))))))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dtrmm fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((string 1) (string 1) (string 1)
                                              (string 1)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (double-float)
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil nil nil nil
                                              nil nil nil)
                                            :calls
                                            '(fortran-to-lisp::xerbla
                                              fortran-to-lisp::lsame))))

