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


(defun zrotg (ca cb c s)
  (declare (type (double-float) c)
           (type (f2cl-lib:complex16) s cb ca))
  (prog ((norm 0.0d0) (scale 0.0d0) (alpha #C(0.0d0 0.0d0)))
    (declare (type (f2cl-lib:complex16) alpha)
             (type (double-float) scale norm))
    (cond
     ((= (f2cl-lib:cdabs ca) 0.0d0) (setf c 0.0d0)
      (setf s (f2cl-lib:cmplx 1.0d0 0.0d0)) (setf ca cb))
     (t
      (setf scale
              (coerce (+ (f2cl-lib:cdabs ca) (f2cl-lib:cdabs cb))
                      'double-float))
      (setf norm
              (* scale
                 (f2cl-lib:dsqrt
                  (+
                   (expt (f2cl-lib:cdabs (/ ca (f2cl-lib:dcmplx scale 0.0d0)))
                         2)
                   (expt (f2cl-lib:cdabs (/ cb (f2cl-lib:dcmplx scale 0.0d0)))
                         2)))))
      (setf alpha (/ ca (f2cl-lib:cdabs ca)))
      (setf c (/ (f2cl-lib:cdabs ca) norm))
      (setf s (/ (* alpha (f2cl-lib:dconjg cb)) norm))
      (setf ca (* alpha norm))))
    (go end_label)
   end_label
    (return (values ca nil c s))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zrotg fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::complex16)
                                              (fortran-to-lisp::complex16)
                                              (double-float)
                                              (fortran-to-lisp::complex16))
                                            :return-values
                                            '(fortran-to-lisp::ca nil
                                              fortran-to-lisp::c
                                              fortran-to-lisp::s)
                                            :calls 'nil)))

