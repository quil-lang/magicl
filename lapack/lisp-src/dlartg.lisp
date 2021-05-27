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


(let* ((zero 0.0d0) (one 1.0d0) (two 2.0d0))
  (declare (type (double-float 0.0d0 0.0d0) zero)
           (type (double-float 1.0d0 1.0d0) one)
           (type (double-float 2.0d0 2.0d0) two)
           (ignorable zero one two))
  (let ((first$ nil) (safmn2 0.0d0) (safmin 0.0d0) (safmx2 0.0d0))
    (declare (type f2cl-lib:logical first$)
             (type (double-float) safmn2 safmin safmx2))
    (setq first$ f2cl-lib:%true%)
    (defun dlartg (f g cs sn r)
      (declare (type (double-float) r sn cs g f))
      (prog ((eps 0.0d0) (f1 0.0d0) (g1 0.0d0) (scale 0.0d0) (i 0) (count$ 0))
        (declare (type (double-float) eps f1 g1 scale)
                 (type (f2cl-lib:integer4) count$ i))
        (cond
         (first$ (setf first$ f2cl-lib:%false%) (setf safmin (dlamch "S"))
          (setf eps (dlamch "E"))
          (setf safmn2
                  (expt (dlamch "B")
                        (f2cl-lib:int
                         (/
                          (/ (f2cl-lib:flog (/ safmin eps))
                             (f2cl-lib:flog (dlamch "B")))
                          two))))
          (setf safmx2 (/ one safmn2))))
        (cond ((= g zero) (setf cs one) (setf sn zero) (setf r f))
              ((= f zero) (setf cs zero) (setf sn one) (setf r g))
              (t (setf f1 f) (setf g1 g) (setf scale (max (abs f1) (abs g1)))
               (cond
                ((>= scale safmx2)
                 (tagbody
                   (setf count$ 0)
                  label10
                   (setf count$ (f2cl-lib:int-add count$ 1))
                   (setf f1 (* f1 safmn2))
                   (setf g1 (* g1 safmn2))
                   (setf scale (max (abs f1) (abs g1)))
                   (if (>= scale safmx2)
                       (go label10))
                   (setf r (f2cl-lib:fsqrt (+ (expt f1 2) (expt g1 2))))
                   (setf cs (/ f1 r))
                   (setf sn (/ g1 r))
                   (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                 ((> i count$) nil)
                     (tagbody (setf r (* r safmx2)) label20))))
                ((<= scale safmn2)
                 (tagbody
                   (setf count$ 0)
                  label30
                   (setf count$ (f2cl-lib:int-add count$ 1))
                   (setf f1 (* f1 safmx2))
                   (setf g1 (* g1 safmx2))
                   (setf scale (max (abs f1) (abs g1)))
                   (if (<= scale safmn2)
                       (go label30))
                   (setf r (f2cl-lib:fsqrt (+ (expt f1 2) (expt g1 2))))
                   (setf cs (/ f1 r))
                   (setf sn (/ g1 r))
                   (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                 ((> i count$) nil)
                     (tagbody (setf r (* r safmn2)) label40))))
                (t (setf r (f2cl-lib:fsqrt (+ (expt f1 2) (expt g1 2))))
                 (setf cs (/ f1 r)) (setf sn (/ g1 r))))
               (cond
                ((and (> (abs f) (abs g)) (< cs zero)) (setf cs (- cs))
                 (setf sn (- sn)) (setf r (- r))))))
        (go end_label)
       end_label
        (return (values nil nil cs sn r))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlartg
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((double-float) (double-float)
                                              (double-float) (double-float)
                                              (double-float))
                                            :return-values
                                            '(nil nil fortran-to-lisp::cs
                                              fortran-to-lisp::sn
                                              fortran-to-lisp::r)
                                            :calls '(fortran-to-lisp::dlamch))))

