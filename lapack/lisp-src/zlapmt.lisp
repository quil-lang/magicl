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


(defun zlapmt (forwrd m n x ldx k)
  (declare (type (array f2cl-lib:integer4 (*)) k)
           (type (array f2cl-lib:complex16 (*)) x)
           (type (f2cl-lib:integer4) ldx n m)
           (type f2cl-lib:logical forwrd))
  (f2cl-lib:with-multi-array-data
      ((x f2cl-lib:complex16 x-%data% x-%offset%)
       (k f2cl-lib:integer4 k-%data% k-%offset%))
    (prog ((temp #C(0.0d0 0.0d0)) (i 0) (ii 0) (in 0) (j 0))
      (declare (type (f2cl-lib:integer4) j in ii i)
               (type (f2cl-lib:complex16) temp))
      (if (<= n 1)
          (go end_label))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf (f2cl-lib:fref k-%data% (i) ((1 *)) k-%offset%)
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref k-%data% (i) ((1 *)) k-%offset%)))
         label10))
      (cond
       (forwrd
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (if (> (f2cl-lib:fref k-%data% (i) ((1 *)) k-%offset%) 0)
                (go label40))
            (setf j i)
            (setf (f2cl-lib:fref k-%data% (j) ((1 *)) k-%offset%)
                    (f2cl-lib:int-sub
                     (f2cl-lib:fref k-%data% (j) ((1 *)) k-%offset%)))
            (setf in (f2cl-lib:fref k-%data% (j) ((1 *)) k-%offset%))
           label20
            (if (> (f2cl-lib:fref k-%data% (in) ((1 *)) k-%offset%) 0)
                (go label40))
            (f2cl-lib:fdo (ii 1 (f2cl-lib:int-add ii 1))
                          ((> ii m) nil)
              (tagbody
                (setf temp
                        (f2cl-lib:fref x-%data% (ii j) ((1 ldx) (1 *))
                                       x-%offset%))
                (setf (f2cl-lib:fref x-%data% (ii j) ((1 ldx) (1 *))
                                     x-%offset%)
                        (f2cl-lib:fref x-%data% (ii in) ((1 ldx) (1 *))
                                       x-%offset%))
                (setf (f2cl-lib:fref x-%data% (ii in) ((1 ldx) (1 *))
                                     x-%offset%)
                        temp)
               label30))
            (setf (f2cl-lib:fref k-%data% (in) ((1 *)) k-%offset%)
                    (f2cl-lib:int-sub
                     (f2cl-lib:fref k-%data% (in) ((1 *)) k-%offset%)))
            (setf j in)
            (setf in (f2cl-lib:fref k-%data% (in) ((1 *)) k-%offset%))
            (go label20)
           label40
           label50)))
       (t
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (if (> (f2cl-lib:fref k-%data% (i) ((1 *)) k-%offset%) 0)
                (go label80))
            (setf (f2cl-lib:fref k-%data% (i) ((1 *)) k-%offset%)
                    (f2cl-lib:int-sub
                     (f2cl-lib:fref k-%data% (i) ((1 *)) k-%offset%)))
            (setf j (f2cl-lib:fref k-%data% (i) ((1 *)) k-%offset%))
           label60
            (if (= j i)
                (go label80))
            (f2cl-lib:fdo (ii 1 (f2cl-lib:int-add ii 1))
                          ((> ii m) nil)
              (tagbody
                (setf temp
                        (f2cl-lib:fref x-%data% (ii i) ((1 ldx) (1 *))
                                       x-%offset%))
                (setf (f2cl-lib:fref x-%data% (ii i) ((1 ldx) (1 *))
                                     x-%offset%)
                        (f2cl-lib:fref x-%data% (ii j) ((1 ldx) (1 *))
                                       x-%offset%))
                (setf (f2cl-lib:fref x-%data% (ii j) ((1 ldx) (1 *))
                                     x-%offset%)
                        temp)
               label70))
            (setf (f2cl-lib:fref k-%data% (j) ((1 *)) k-%offset%)
                    (f2cl-lib:int-sub
                     (f2cl-lib:fref k-%data% (j) ((1 *)) k-%offset%)))
            (setf j (f2cl-lib:fref k-%data% (j) ((1 *)) k-%offset%))
            (go label60)
           label80
           label90))))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zlapmt
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '(fortran-to-lisp::logical
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::integer4
                                               (*)))
                                            :return-values
                                            '(nil nil nil nil nil nil) :calls
                                            'nil)))

