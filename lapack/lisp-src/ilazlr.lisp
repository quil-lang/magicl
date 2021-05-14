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


(let* ((zero (f2cl-lib:cmplx 0.0d0 0.0d0)))
  (declare (type (f2cl-lib:complex16) zero)
           (ignorable zero))
  (defun ilazlr (m n a lda)
    (declare (type (array f2cl-lib:complex16 (*)) a)
             (type (f2cl-lib:integer4) lda n m))
    (f2cl-lib:with-multi-array-data
        ((a f2cl-lib:complex16 a-%data% a-%offset%))
      (prog ((i 0) (j 0) (ilazlr 0))
        (declare (type (f2cl-lib:integer4) i j ilazlr))
        (cond ((= m 0) (setf ilazlr m))
              ((or (/= (f2cl-lib:fref a (m 1) ((1 lda) (1 *))) zero)
                   (/= (f2cl-lib:fref a (m n) ((1 lda) (1 *))) zero))
               (setf ilazlr m))
              (t (setf ilazlr 0)
               (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                             ((> j n) nil)
                 (tagbody
                   (setf i m)
                  label100001
                   (if (not
                        (and
                         (/=
                          (f2cl-lib:fref a-%data% (i j) ((1 lda) (1 *))
                                         a-%offset%)
                          zero)
                         (>= i 1)))
                       (go label100002))
                   (setf i (f2cl-lib:int-sub i 1))
                   (cond ((= i 0) (go f2cl-lib::exit)))
                   (go label100001)
                  label100002
                   (setf ilazlr
                           (max (the f2cl-lib:integer4 ilazlr)
                                (the f2cl-lib:integer4 i)))
                  label100000))))
        (go end_label)
       end_label
        (return (values ilazlr nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::ilazlr
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4))
                                            :return-values '(nil nil nil nil)
                                            :calls 'nil)))

