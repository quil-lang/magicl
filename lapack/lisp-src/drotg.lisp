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


(defun drotg (da db c s)
  (declare (type (double-float) s c db da))
  (prog ((r 0.0d0) (roe 0.0d0) (scale 0.0d0) (z 0.0d0))
    (declare (type (double-float) z scale roe r))
    (setf roe db)
    (if (> (f2cl-lib:dabs da) (f2cl-lib:dabs db))
        (setf roe da))
    (setf scale (+ (f2cl-lib:dabs da) (f2cl-lib:dabs db)))
    (cond
     ((= scale 0.0d0) (setf c 1.0d0) (setf s 0.0d0) (setf r 0.0d0)
      (setf z 0.0d0))
     (t
      (setf r
              (* scale
                 (f2cl-lib:dsqrt
                  (+ (expt (/ da scale) 2) (expt (/ db scale) 2)))))
      (setf r (* (f2cl-lib:dsign 1.0d0 roe) r)) (setf c (/ da r))
      (setf s (/ db r)) (setf z 1.0d0)
      (if (> (f2cl-lib:dabs da) (f2cl-lib:dabs db))
          (setf z s))
      (if (and (>= (f2cl-lib:dabs db) (f2cl-lib:dabs da)) (/= c 0.0d0))
          (setf z (/ 1.0d0 c)))))
    (setf da r)
    (setf db z)
    (go end_label)
   end_label
    (return (values da db c s))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::drotg fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((double-float) (double-float)
                                              (double-float) (double-float))
                                            :return-values
                                            '(fortran-to-lisp::da
                                              fortran-to-lisp::db
                                              fortran-to-lisp::c
                                              fortran-to-lisp::s)
                                            :calls 'nil)))

