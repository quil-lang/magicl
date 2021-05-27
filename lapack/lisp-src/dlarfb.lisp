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


(let* ((one 1.0d0))
  (declare (type (double-float 1.0d0 1.0d0) one)
           (ignorable one))
  (defun dlarfb (side trans direct storev m n k v ldv t$ ldt c ldc work ldwork)
    (declare (type (array double-float (*)) work c t$ v)
             (type (f2cl-lib:integer4) ldwork ldc ldt ldv k n m)
             (type (string 1) storev direct trans side))
    (f2cl-lib:with-multi-array-data
        ((side character side-%data% side-%offset%)
         (trans character trans-%data% trans-%offset%)
         (direct character direct-%data% direct-%offset%)
         (storev character storev-%data% storev-%offset%)
         (v double-float v-%data% v-%offset%)
         (t$ double-float t$-%data% t$-%offset%)
         (c double-float c-%data% c-%offset%)
         (work double-float work-%data% work-%offset%))
      (prog ((i 0) (j 0)
             (transt
              (make-array '(1) :element-type 'character :initial-element #\ )))
        (declare (type (f2cl-lib:integer4) i j)
                 (type (string 1) transt))
        (if (or (<= m 0) (<= n 0))
            (go end_label))
        (cond
         ((lsame trans "N") (f2cl-lib:f2cl-set-string transt "T" (string 1)))
         (t (f2cl-lib:f2cl-set-string transt "N" (string 1))))
        (cond
         ((lsame storev "C")
          (cond
           ((lsame direct "F")
            (cond
             ((lsame side "L")
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j k) nil)
                (tagbody
                  (dcopy n
                   (f2cl-lib:array-slice c-%data% double-float (j 1)
                                         ((1 ldc) (1 *)) c-%offset%)
                   ldc
                   (f2cl-lib:array-slice work-%data% double-float (1 j)
                                         ((1 ldwork) (1 *)) work-%offset%)
                   1)
                 label10))
              (dtrmm "R" "L" "N" "U" n k one v ldv work ldwork)
              (cond
               ((> m k)
                (dgemm "T" "N" n k (f2cl-lib:int-sub m k) one
                 (f2cl-lib:array-slice c-%data% double-float ((+ k 1) 1)
                                       ((1 ldc) (1 *)) c-%offset%)
                 ldc
                 (f2cl-lib:array-slice v-%data% double-float ((+ k 1) 1)
                                       ((1 ldv) (1 *)) v-%offset%)
                 ldv one work ldwork)))
              (dtrmm "R" "U" transt "N" n k one t$ ldt work ldwork)
              (cond
               ((> m k)
                (dgemm "N" "T" (f2cl-lib:int-sub m k) n k (- one)
                 (f2cl-lib:array-slice v-%data% double-float ((+ k 1) 1)
                                       ((1 ldv) (1 *)) v-%offset%)
                 ldv work ldwork one
                 (f2cl-lib:array-slice c-%data% double-float ((+ k 1) 1)
                                       ((1 ldc) (1 *)) c-%offset%)
                 ldc)))
              (dtrmm "R" "L" "T" "U" n k one v ldv work ldwork)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j k) nil)
                (tagbody
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i n) nil)
                    (tagbody
                      (setf (f2cl-lib:fref c-%data% (j i) ((1 ldc) (1 *))
                                           c-%offset%)
                              (-
                               (f2cl-lib:fref c-%data% (j i) ((1 ldc) (1 *))
                                              c-%offset%)
                               (f2cl-lib:fref work-%data% (i j)
                                              ((1 ldwork) (1 *))
                                              work-%offset%)))
                     label20))
                 label30)))
             ((lsame side "R")
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j k) nil)
                (tagbody
                  (dcopy m
                   (f2cl-lib:array-slice c-%data% double-float (1 j)
                                         ((1 ldc) (1 *)) c-%offset%)
                   1
                   (f2cl-lib:array-slice work-%data% double-float (1 j)
                                         ((1 ldwork) (1 *)) work-%offset%)
                   1)
                 label40))
              (dtrmm "R" "L" "N" "U" m k one v ldv work ldwork)
              (cond
               ((> n k)
                (dgemm "N" "N" m k (f2cl-lib:int-sub n k) one
                 (f2cl-lib:array-slice c-%data% double-float
                                       (1 (f2cl-lib:int-add k 1))
                                       ((1 ldc) (1 *)) c-%offset%)
                 ldc
                 (f2cl-lib:array-slice v-%data% double-float ((+ k 1) 1)
                                       ((1 ldv) (1 *)) v-%offset%)
                 ldv one work ldwork)))
              (dtrmm "R" "U" trans "N" m k one t$ ldt work ldwork)
              (cond
               ((> n k)
                (dgemm "N" "T" m (f2cl-lib:int-sub n k) k (- one) work ldwork
                 (f2cl-lib:array-slice v-%data% double-float ((+ k 1) 1)
                                       ((1 ldv) (1 *)) v-%offset%)
                 ldv one
                 (f2cl-lib:array-slice c-%data% double-float
                                       (1 (f2cl-lib:int-add k 1))
                                       ((1 ldc) (1 *)) c-%offset%)
                 ldc)))
              (dtrmm "R" "L" "T" "U" m k one v ldv work ldwork)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j k) nil)
                (tagbody
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i m) nil)
                    (tagbody
                      (setf (f2cl-lib:fref c-%data% (i j) ((1 ldc) (1 *))
                                           c-%offset%)
                              (-
                               (f2cl-lib:fref c-%data% (i j) ((1 ldc) (1 *))
                                              c-%offset%)
                               (f2cl-lib:fref work-%data% (i j)
                                              ((1 ldwork) (1 *))
                                              work-%offset%)))
                     label50))
                 label60)))))
           (t
            (cond
             ((lsame side "L")
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j k) nil)
                (tagbody
                  (dcopy n
                   (f2cl-lib:array-slice c-%data% double-float
                                         ((+ m (f2cl-lib:int-sub k) j) 1)
                                         ((1 ldc) (1 *)) c-%offset%)
                   ldc
                   (f2cl-lib:array-slice work-%data% double-float (1 j)
                                         ((1 ldwork) (1 *)) work-%offset%)
                   1)
                 label70))
              (dtrmm "R" "U" "N" "U" n k one
               (f2cl-lib:array-slice v-%data% double-float
                                     ((+ m (f2cl-lib:int-sub k) 1) 1)
                                     ((1 ldv) (1 *)) v-%offset%)
               ldv work ldwork)
              (cond
               ((> m k)
                (dgemm "T" "N" n k (f2cl-lib:int-sub m k) one c ldc v ldv one
                 work ldwork)))
              (dtrmm "R" "L" transt "N" n k one t$ ldt work ldwork)
              (cond
               ((> m k)
                (dgemm "N" "T" (f2cl-lib:int-sub m k) n k (- one) v ldv work
                 ldwork one c ldc)))
              (dtrmm "R" "U" "T" "U" n k one
               (f2cl-lib:array-slice v-%data% double-float
                                     ((+ m (f2cl-lib:int-sub k) 1) 1)
                                     ((1 ldv) (1 *)) v-%offset%)
               ldv work ldwork)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j k) nil)
                (tagbody
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i n) nil)
                    (tagbody
                      (setf (f2cl-lib:fref c-%data%
                                           ((f2cl-lib:int-add
                                             (f2cl-lib:int-sub m k) j)
                                            i)
                                           ((1 ldc) (1 *)) c-%offset%)
                              (-
                               (f2cl-lib:fref c-%data%
                                              ((f2cl-lib:int-add
                                                (f2cl-lib:int-sub m k) j)
                                               i)
                                              ((1 ldc) (1 *)) c-%offset%)
                               (f2cl-lib:fref work-%data% (i j)
                                              ((1 ldwork) (1 *))
                                              work-%offset%)))
                     label80))
                 label90)))
             ((lsame side "R")
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j k) nil)
                (tagbody
                  (dcopy m
                   (f2cl-lib:array-slice c-%data% double-float
                                         (1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-sub n k) j))
                                         ((1 ldc) (1 *)) c-%offset%)
                   1
                   (f2cl-lib:array-slice work-%data% double-float (1 j)
                                         ((1 ldwork) (1 *)) work-%offset%)
                   1)
                 label100))
              (dtrmm "R" "U" "N" "U" m k one
               (f2cl-lib:array-slice v-%data% double-float
                                     ((+ n (f2cl-lib:int-sub k) 1) 1)
                                     ((1 ldv) (1 *)) v-%offset%)
               ldv work ldwork)
              (cond
               ((> n k)
                (dgemm "N" "N" m k (f2cl-lib:int-sub n k) one c ldc v ldv one
                 work ldwork)))
              (dtrmm "R" "L" trans "N" m k one t$ ldt work ldwork)
              (cond
               ((> n k)
                (dgemm "N" "T" m (f2cl-lib:int-sub n k) k (- one) work ldwork v
                 ldv one c ldc)))
              (dtrmm "R" "U" "T" "U" m k one
               (f2cl-lib:array-slice v-%data% double-float
                                     ((+ n (f2cl-lib:int-sub k) 1) 1)
                                     ((1 ldv) (1 *)) v-%offset%)
               ldv work ldwork)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j k) nil)
                (tagbody
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i m) nil)
                    (tagbody
                      (setf (f2cl-lib:fref c-%data%
                                           (i
                                            (f2cl-lib:int-add
                                             (f2cl-lib:int-sub n k) j))
                                           ((1 ldc) (1 *)) c-%offset%)
                              (-
                               (f2cl-lib:fref c-%data%
                                              (i
                                               (f2cl-lib:int-add
                                                (f2cl-lib:int-sub n k) j))
                                              ((1 ldc) (1 *)) c-%offset%)
                               (f2cl-lib:fref work-%data% (i j)
                                              ((1 ldwork) (1 *))
                                              work-%offset%)))
                     label110))
                 label120)))))))
         ((lsame storev "R")
          (cond
           ((lsame direct "F")
            (cond
             ((lsame side "L")
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j k) nil)
                (tagbody
                  (dcopy n
                   (f2cl-lib:array-slice c-%data% double-float (j 1)
                                         ((1 ldc) (1 *)) c-%offset%)
                   ldc
                   (f2cl-lib:array-slice work-%data% double-float (1 j)
                                         ((1 ldwork) (1 *)) work-%offset%)
                   1)
                 label130))
              (dtrmm "R" "U" "T" "U" n k one v ldv work ldwork)
              (cond
               ((> m k)
                (dgemm "T" "T" n k (f2cl-lib:int-sub m k) one
                 (f2cl-lib:array-slice c-%data% double-float ((+ k 1) 1)
                                       ((1 ldc) (1 *)) c-%offset%)
                 ldc
                 (f2cl-lib:array-slice v-%data% double-float
                                       (1 (f2cl-lib:int-add k 1))
                                       ((1 ldv) (1 *)) v-%offset%)
                 ldv one work ldwork)))
              (dtrmm "R" "U" transt "N" n k one t$ ldt work ldwork)
              (cond
               ((> m k)
                (dgemm "T" "T" (f2cl-lib:int-sub m k) n k (- one)
                 (f2cl-lib:array-slice v-%data% double-float
                                       (1 (f2cl-lib:int-add k 1))
                                       ((1 ldv) (1 *)) v-%offset%)
                 ldv work ldwork one
                 (f2cl-lib:array-slice c-%data% double-float ((+ k 1) 1)
                                       ((1 ldc) (1 *)) c-%offset%)
                 ldc)))
              (dtrmm "R" "U" "N" "U" n k one v ldv work ldwork)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j k) nil)
                (tagbody
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i n) nil)
                    (tagbody
                      (setf (f2cl-lib:fref c-%data% (j i) ((1 ldc) (1 *))
                                           c-%offset%)
                              (-
                               (f2cl-lib:fref c-%data% (j i) ((1 ldc) (1 *))
                                              c-%offset%)
                               (f2cl-lib:fref work-%data% (i j)
                                              ((1 ldwork) (1 *))
                                              work-%offset%)))
                     label140))
                 label150)))
             ((lsame side "R")
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j k) nil)
                (tagbody
                  (dcopy m
                   (f2cl-lib:array-slice c-%data% double-float (1 j)
                                         ((1 ldc) (1 *)) c-%offset%)
                   1
                   (f2cl-lib:array-slice work-%data% double-float (1 j)
                                         ((1 ldwork) (1 *)) work-%offset%)
                   1)
                 label160))
              (dtrmm "R" "U" "T" "U" m k one v ldv work ldwork)
              (cond
               ((> n k)
                (dgemm "N" "T" m k (f2cl-lib:int-sub n k) one
                 (f2cl-lib:array-slice c-%data% double-float
                                       (1 (f2cl-lib:int-add k 1))
                                       ((1 ldc) (1 *)) c-%offset%)
                 ldc
                 (f2cl-lib:array-slice v-%data% double-float
                                       (1 (f2cl-lib:int-add k 1))
                                       ((1 ldv) (1 *)) v-%offset%)
                 ldv one work ldwork)))
              (dtrmm "R" "U" trans "N" m k one t$ ldt work ldwork)
              (cond
               ((> n k)
                (dgemm "N" "N" m (f2cl-lib:int-sub n k) k (- one) work ldwork
                 (f2cl-lib:array-slice v-%data% double-float
                                       (1 (f2cl-lib:int-add k 1))
                                       ((1 ldv) (1 *)) v-%offset%)
                 ldv one
                 (f2cl-lib:array-slice c-%data% double-float
                                       (1 (f2cl-lib:int-add k 1))
                                       ((1 ldc) (1 *)) c-%offset%)
                 ldc)))
              (dtrmm "R" "U" "N" "U" m k one v ldv work ldwork)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j k) nil)
                (tagbody
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i m) nil)
                    (tagbody
                      (setf (f2cl-lib:fref c-%data% (i j) ((1 ldc) (1 *))
                                           c-%offset%)
                              (-
                               (f2cl-lib:fref c-%data% (i j) ((1 ldc) (1 *))
                                              c-%offset%)
                               (f2cl-lib:fref work-%data% (i j)
                                              ((1 ldwork) (1 *))
                                              work-%offset%)))
                     label170))
                 label180)))))
           (t
            (cond
             ((lsame side "L")
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j k) nil)
                (tagbody
                  (dcopy n
                   (f2cl-lib:array-slice c-%data% double-float
                                         ((+ m (f2cl-lib:int-sub k) j) 1)
                                         ((1 ldc) (1 *)) c-%offset%)
                   ldc
                   (f2cl-lib:array-slice work-%data% double-float (1 j)
                                         ((1 ldwork) (1 *)) work-%offset%)
                   1)
                 label190))
              (dtrmm "R" "L" "T" "U" n k one
               (f2cl-lib:array-slice v-%data% double-float
                                     (1
                                      (f2cl-lib:int-add (f2cl-lib:int-sub m k)
                                                        1))
                                     ((1 ldv) (1 *)) v-%offset%)
               ldv work ldwork)
              (cond
               ((> m k)
                (dgemm "T" "T" n k (f2cl-lib:int-sub m k) one c ldc v ldv one
                 work ldwork)))
              (dtrmm "R" "L" transt "N" n k one t$ ldt work ldwork)
              (cond
               ((> m k)
                (dgemm "T" "T" (f2cl-lib:int-sub m k) n k (- one) v ldv work
                 ldwork one c ldc)))
              (dtrmm "R" "L" "N" "U" n k one
               (f2cl-lib:array-slice v-%data% double-float
                                     (1
                                      (f2cl-lib:int-add (f2cl-lib:int-sub m k)
                                                        1))
                                     ((1 ldv) (1 *)) v-%offset%)
               ldv work ldwork)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j k) nil)
                (tagbody
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i n) nil)
                    (tagbody
                      (setf (f2cl-lib:fref c-%data%
                                           ((f2cl-lib:int-add
                                             (f2cl-lib:int-sub m k) j)
                                            i)
                                           ((1 ldc) (1 *)) c-%offset%)
                              (-
                               (f2cl-lib:fref c-%data%
                                              ((f2cl-lib:int-add
                                                (f2cl-lib:int-sub m k) j)
                                               i)
                                              ((1 ldc) (1 *)) c-%offset%)
                               (f2cl-lib:fref work-%data% (i j)
                                              ((1 ldwork) (1 *))
                                              work-%offset%)))
                     label200))
                 label210)))
             ((lsame side "R")
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j k) nil)
                (tagbody
                  (dcopy m
                   (f2cl-lib:array-slice c-%data% double-float
                                         (1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-sub n k) j))
                                         ((1 ldc) (1 *)) c-%offset%)
                   1
                   (f2cl-lib:array-slice work-%data% double-float (1 j)
                                         ((1 ldwork) (1 *)) work-%offset%)
                   1)
                 label220))
              (dtrmm "R" "L" "T" "U" m k one
               (f2cl-lib:array-slice v-%data% double-float
                                     (1
                                      (f2cl-lib:int-add (f2cl-lib:int-sub n k)
                                                        1))
                                     ((1 ldv) (1 *)) v-%offset%)
               ldv work ldwork)
              (cond
               ((> n k)
                (dgemm "N" "T" m k (f2cl-lib:int-sub n k) one c ldc v ldv one
                 work ldwork)))
              (dtrmm "R" "L" trans "N" m k one t$ ldt work ldwork)
              (cond
               ((> n k)
                (dgemm "N" "N" m (f2cl-lib:int-sub n k) k (- one) work ldwork v
                 ldv one c ldc)))
              (dtrmm "R" "L" "N" "U" m k one
               (f2cl-lib:array-slice v-%data% double-float
                                     (1
                                      (f2cl-lib:int-add (f2cl-lib:int-sub n k)
                                                        1))
                                     ((1 ldv) (1 *)) v-%offset%)
               ldv work ldwork)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j k) nil)
                (tagbody
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i m) nil)
                    (tagbody
                      (setf (f2cl-lib:fref c-%data%
                                           (i
                                            (f2cl-lib:int-add
                                             (f2cl-lib:int-sub n k) j))
                                           ((1 ldc) (1 *)) c-%offset%)
                              (-
                               (f2cl-lib:fref c-%data%
                                              (i
                                               (f2cl-lib:int-add
                                                (f2cl-lib:int-sub n k) j))
                                              ((1 ldc) (1 *)) c-%offset%)
                               (f2cl-lib:fref work-%data% (i j)
                                              ((1 ldwork) (1 *))
                                              work-%offset%)))
                     label230))
                 label240))))))))
        (go end_label)
       end_label
        (return
         (values nil nil nil nil nil nil nil nil nil nil nil nil nil nil
                 nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlarfb
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((string 1) (string 1) (string 1)
                                              (string 1)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil nil nil nil
                                              nil nil nil nil nil nil nil)
                                            :calls
                                            '(fortran-to-lisp::dgemm
                                              fortran-to-lisp::dtrmm
                                              fortran-to-lisp::dcopy
                                              fortran-to-lisp::lsame))))

