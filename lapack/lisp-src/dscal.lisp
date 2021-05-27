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


(defun dscal (n da dx incx)
  (declare (type (array double-float (*)) dx)
           (type (double-float) da)
           (type (f2cl-lib:integer4) incx n))
  (f2cl-lib:with-multi-array-data
      ((dx double-float dx-%data% dx-%offset%))
    (prog ((i 0) (m 0) (mp1 0) (nincx 0))
      (declare (type (f2cl-lib:integer4) nincx mp1 m i))
      (if (or (<= n 0) (<= incx 0))
          (go end_label))
      (cond
       ((= incx 1) (setf m (mod n 5))
        (cond
         ((/= m 0)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)
                      (* da (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)))
             label100000))
          (if (< n 5)
              (go end_label))))
        (setf mp1 (f2cl-lib:int-add m 1))
        (f2cl-lib:fdo (i mp1 (f2cl-lib:int-add i 5))
                      ((> i n) nil)
          (tagbody
            (setf (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)
                    (* da (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)))
            (setf (f2cl-lib:fref dx-%data% ((f2cl-lib:int-add i 1)) ((1 *))
                                 dx-%offset%)
                    (* da
                       (f2cl-lib:fref dx-%data% ((f2cl-lib:int-add i 1))
                                      ((1 *)) dx-%offset%)))
            (setf (f2cl-lib:fref dx-%data% ((f2cl-lib:int-add i 2)) ((1 *))
                                 dx-%offset%)
                    (* da
                       (f2cl-lib:fref dx-%data% ((f2cl-lib:int-add i 2))
                                      ((1 *)) dx-%offset%)))
            (setf (f2cl-lib:fref dx-%data% ((f2cl-lib:int-add i 3)) ((1 *))
                                 dx-%offset%)
                    (* da
                       (f2cl-lib:fref dx-%data% ((f2cl-lib:int-add i 3))
                                      ((1 *)) dx-%offset%)))
            (setf (f2cl-lib:fref dx-%data% ((f2cl-lib:int-add i 4)) ((1 *))
                                 dx-%offset%)
                    (* da
                       (f2cl-lib:fref dx-%data% ((f2cl-lib:int-add i 4))
                                      ((1 *)) dx-%offset%)))
           label100001)))
       (t (setf nincx (f2cl-lib:int-mul n incx))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i incx))
                      ((> i nincx) nil)
          (tagbody
            (setf (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)
                    (* da (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)))
           label100002))))
      (go end_label)
     end_label
      (return (values nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dscal fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (double-float)
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4))
                                            :return-values '(nil nil nil nil)
                                            :calls 'nil)))

