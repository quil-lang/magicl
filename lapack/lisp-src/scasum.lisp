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


(defun scasum (n cx incx)
  (declare (type (array f2cl-lib:complex8 (*)) cx)
           (type (f2cl-lib:integer4) incx n))
  (f2cl-lib:with-multi-array-data
      ((cx f2cl-lib:complex8 cx-%data% cx-%offset%))
    (prog ((i 0) (nincx 0) (stemp 0.0) (scasum 0.0))
      (declare (type (single-float) scasum stemp)
               (type (f2cl-lib:integer4) nincx i))
      (setf scasum 0.0)
      (setf stemp 0.0)
      (if (or (<= n 0) (<= incx 0))
          (go end_label))
      (cond
       ((= incx 1)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (setf stemp
                    (+ stemp
                       (abs
                        (f2cl-lib:freal
                         (f2cl-lib:fref cx-%data% (i) ((1 *)) cx-%offset%)))
                       (abs
                        (f2cl-lib:aimag
                         (f2cl-lib:fref cx-%data% (i) ((1 *)) cx-%offset%)))))
           label100000)))
       (t (setf nincx (f2cl-lib:int-mul n incx))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i incx))
                      ((> i nincx) nil)
          (tagbody
            (setf stemp
                    (+ stemp
                       (abs
                        (f2cl-lib:freal
                         (f2cl-lib:fref cx-%data% (i) ((1 *)) cx-%offset%)))
                       (abs
                        (f2cl-lib:aimag
                         (f2cl-lib:fref cx-%data% (i) ((1 *)) cx-%offset%)))))
           label100001))))
      (setf scasum stemp)
      (go end_label)
     end_label
      (return (values scasum nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::scasum
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::complex8
                                               (*))
                                              (fortran-to-lisp::integer4))
                                            :return-values '(nil nil nil)
                                            :calls 'nil)))

