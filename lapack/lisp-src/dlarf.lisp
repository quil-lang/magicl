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
  (defun dlarf (side m n v incv tau c ldc work)
    (declare (type (double-float) tau)
             (type (array double-float (*)) work c v)
             (type (f2cl-lib:integer4) ldc incv n m)
             (type (string 1) side))
    (f2cl-lib:with-multi-array-data
        ((side character side-%data% side-%offset%)
         (v double-float v-%data% v-%offset%)
         (c double-float c-%data% c-%offset%)
         (work double-float work-%data% work-%offset%))
      (prog ()
        (declare)
        (cond
         ((lsame side "L")
          (cond
           ((/= tau zero) (dgemv "T" m n one c ldc v incv zero work 1)
            (dger m n (- tau) v incv work 1 c ldc))))
         (t
          (cond
           ((/= tau zero) (dgemv "N" m n one c ldc v incv zero work 1)
            (dger m n (- tau) work 1 v incv c ldc)))))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlarf fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((string 1)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4)
                                              (double-float)
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*)))
                                            :return-values
                                            '(nil nil nil nil nil nil nil nil
                                              nil)
                                            :calls
                                            '(fortran-to-lisp::dger
                                              fortran-to-lisp::dgemv
                                              fortran-to-lisp::lsame))))

