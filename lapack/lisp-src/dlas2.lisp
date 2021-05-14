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
  (defun dlas2 (f g h ssmin ssmax)
    (declare (type (double-float) ssmax ssmin h g f))
    (prog ((as 0.0d0) (at 0.0d0) (au 0.0d0) (c 0.0d0) (fa 0.0d0) (fhmn 0.0d0)
           (fhmx 0.0d0) (ga 0.0d0) (ha 0.0d0))
      (declare (type (double-float) as at au c fa fhmn fhmx ga ha))
      (setf fa (abs f))
      (setf ga (abs g))
      (setf ha (abs h))
      (setf fhmn (min fa ha))
      (setf fhmx (max fa ha))
      (cond
       ((= fhmn zero) (setf ssmin zero)
        (cond ((= fhmx zero) (setf ssmax ga))
              (t
               (setf ssmax
                       (* (max fhmx ga)
                          (f2cl-lib:fsqrt
                           (+ one
                              (expt (/ (min fhmx ga) (max fhmx ga)) 2))))))))
       (t
        (cond
         ((< ga fhmx) (setf as (+ one (/ fhmn fhmx)))
          (setf at (/ (- fhmx fhmn) fhmx)) (setf au (expt (/ ga fhmx) 2))
          (setf c
                  (/ two
                     (+ (f2cl-lib:fsqrt (+ (* as as) au))
                        (f2cl-lib:fsqrt (+ (* at at) au)))))
          (setf ssmin (* fhmn c)) (setf ssmax (/ fhmx c)))
         (t (setf au (/ fhmx ga))
          (cond ((= au zero) (setf ssmin (/ (* fhmn fhmx) ga)) (setf ssmax ga))
                (t (setf as (+ one (/ fhmn fhmx)))
                 (setf at (/ (- fhmx fhmn) fhmx))
                 (setf c
                         (/ one
                            (+ (f2cl-lib:fsqrt (+ one (expt (* as au) 2)))
                               (f2cl-lib:fsqrt (+ one (expt (* at au) 2))))))
                 (setf ssmin (* fhmn c au)) (setf ssmin (+ ssmin ssmin))
                 (setf ssmax (/ ga (+ c c)))))))))
      (go end_label)
     end_label
      (return (values nil nil nil ssmin ssmax)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlas2 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((double-float) (double-float)
                                              (double-float) (double-float)
                                              (double-float))
                                            :return-values
                                            '(nil nil nil
                                              fortran-to-lisp::ssmin
                                              fortran-to-lisp::ssmax)
                                            :calls 'nil)))

