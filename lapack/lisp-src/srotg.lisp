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


(defun srotg (sa sb c s)
  (declare (type (single-float) s c sb sa))
  (prog ((r 0.0) (roe 0.0) (scale 0.0) (z 0.0))
    (declare (type (single-float) z scale roe r))
    (setf roe sb)
    (if (> (abs sa) (abs sb))
        (setf roe sa))
    (setf scale (+ (abs sa) (abs sb)))
    (cond ((= scale 0.0) (setf c 1.0) (setf s 0.0) (setf r 0.0) (setf z 0.0))
          (t
           (setf r
                   (* scale
                      (f2cl-lib:fsqrt
                       (+ (expt (/ sa scale) 2) (expt (/ sb scale) 2)))))
           (setf r (* (f2cl-lib:sign 1.0 roe) r)) (setf c (/ sa r))
           (setf s (/ sb r)) (setf z 1.0)
           (if (> (abs sa) (abs sb))
               (setf z s))
           (if (and (>= (abs sb) (abs sa)) (/= c 0.0))
               (setf z (/ 1.0 c)))))
    (setf sa r)
    (setf sb z)
    (go end_label)
   end_label
    (return (values sa sb c s))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::srotg fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((single-float) (single-float)
                                              (single-float) (single-float))
                                            :return-values
                                            '(fortran-to-lisp::sa
                                              fortran-to-lisp::sb
                                              fortran-to-lisp::c
                                              fortran-to-lisp::s)
                                            :calls 'nil)))

