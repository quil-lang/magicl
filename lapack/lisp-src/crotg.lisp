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


(defun crotg (ca cb c s)
  (declare (type (single-float) c)
           (type (f2cl-lib:complex8) s cb ca))
  (prog ((norm 0.0) (scale 0.0) (alpha #C(0.0 0.0)))
    (declare (type (f2cl-lib:complex8) alpha)
             (type (single-float) scale norm))
    (cond
     ((= (f2cl-lib:cabs ca) 0.0) (setf c 0.0) (setf s (f2cl-lib:cmplx 1.0 0.0))
      (setf ca cb))
     (t
      (setf scale
              (coerce (realpart (+ (f2cl-lib:cabs ca) (f2cl-lib:cabs cb)))
                      'single-float))
      (setf norm
              (coerce
               (realpart
                (* scale
                   (f2cl-lib:fsqrt
                    (+ (expt (f2cl-lib:cabs (/ ca scale)) 2)
                       (expt (f2cl-lib:cabs (/ cb scale)) 2)))))
               'single-float))
      (setf alpha (/ ca (f2cl-lib:cabs ca)))
      (setf c (coerce (realpart (/ (f2cl-lib:cabs ca) norm)) 'single-float))
      (setf s (/ (* alpha (f2cl-lib:conjg cb)) norm))
      (setf ca (* alpha norm))))
    (go end_label)
   end_label
    (return (values ca nil c s))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::crotg fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::complex8)
                                              (fortran-to-lisp::complex8)
                                              (single-float)
                                              (fortran-to-lisp::complex8))
                                            :return-values
                                            '(fortran-to-lisp::ca nil
                                              fortran-to-lisp::c
                                              fortran-to-lisp::s)
                                            :calls 'nil)))

