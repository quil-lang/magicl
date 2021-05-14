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


(defun zladiv (x y)
  (declare (type (f2cl-lib:complex16) y x))
  (prog ((zi 0.0d0) (zr 0.0d0) (zladiv #C(0.0d0 0.0d0)))
    (declare (type (f2cl-lib:complex16) zladiv)
             (type (double-float) zr zi))
    (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
        (dladiv (f2cl-lib:dble x) (f2cl-lib:dimag x) (f2cl-lib:dble y)
         (f2cl-lib:dimag y) zr zi)
      (declare (ignore var-0 var-1 var-2 var-3))
      (setf zr var-4)
      (setf zi var-5))
    (setf zladiv (f2cl-lib:dcmplx zr zi))
    (go end_label)
   end_label
    (return (values zladiv nil nil))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zladiv
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::complex16)
                                              (fortran-to-lisp::complex16))
                                            :return-values '(nil nil) :calls
                                            '(fortran-to-lisp::dladiv))))

