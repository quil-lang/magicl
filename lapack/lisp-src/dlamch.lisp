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
  (defun dlamch (cmach)
    (declare (type (string 1) cmach))
    (f2cl-lib:with-multi-array-data
        ((cmach character cmach-%data% cmach-%offset%))
      (prog ((rnd 0.0d0) (eps 0.0d0) (sfmin 0.0d0) (small 0.0d0) (rmach 0.0d0)
             (a 0.0d0) (b 0.0d0) (dlamch 0.0d0))
        (declare (type (double-float) rnd eps sfmin small rmach a b dlamch))
        (setf rnd one)
        (cond
         ((= one rnd)
          (setf eps
                  (coerce
                   (*
                    (multiple-value-bind (ret-val var-0)
                        (epsilon zero)
                      (declare (ignore))
                      (when var-0 (setf zero var-0))
                      ret-val)
                    0.5)
                   'double-float)))
         (t
          (setf eps
                  (coerce
                   (multiple-value-bind (ret-val var-0)
                       (epsilon zero)
                     (declare (ignore))
                     (when var-0 (setf zero var-0))
                     ret-val)
                   'double-float))))
        (cond ((lsame cmach "E") (setf rmach eps))
              ((lsame cmach "S")
               (setf sfmin
                       (coerce
                        (multiple-value-bind (ret-val var-0)
                            (tiny zero)
                          (declare (ignore))
                          (when var-0 (setf zero var-0))
                          ret-val)
                        'double-float))
               (setf small
                       (/ one
                          (multiple-value-bind (ret-val var-0)
                              (huge zero)
                            (declare (ignore))
                            (when var-0 (setf zero var-0))
                            ret-val)))
               (cond ((>= small sfmin) (setf sfmin (* small (+ one eps)))))
               (setf rmach sfmin))
              ((lsame cmach "B")
               (setf rmach
                       (coerce
                        (multiple-value-bind (ret-val var-0)
                            (radix zero)
                          (declare (ignore))
                          (when var-0 (setf zero var-0))
                          ret-val)
                        'double-float)))
              ((lsame cmach "P")
               (setf rmach
                       (* eps
                          (multiple-value-bind (ret-val var-0)
                              (radix zero)
                            (declare (ignore))
                            (when var-0 (setf zero var-0))
                            ret-val))))
              ((lsame cmach "N")
               (setf rmach
                       (coerce
                        (multiple-value-bind (ret-val var-0)
                            (digits zero)
                          (declare (ignore))
                          (when var-0 (setf zero var-0))
                          ret-val)
                        'double-float)))
              ((lsame cmach "R") (setf rmach rnd))
              ((lsame cmach "M")
               (setf rmach
                       (coerce
                        (the f2cl-lib:integer4
                             (multiple-value-bind (ret-val var-0)
                                 (minexponent zero)
                               (declare (ignore))
                               (when var-0 (setf zero var-0))
                               ret-val))
                        'double-float)))
              ((lsame cmach "U")
               (setf rmach
                       (coerce
                        (multiple-value-bind (ret-val var-0)
                            (tiny zero)
                          (declare (ignore))
                          (when var-0 (setf zero var-0))
                          ret-val)
                        'double-float)))
              ((lsame cmach "L")
               (setf rmach
                       (coerce
                        (the f2cl-lib:integer4
                             (multiple-value-bind (ret-val var-0)
                                 (maxexponent zero)
                               (declare (ignore))
                               (when var-0 (setf zero var-0))
                               ret-val))
                        'double-float)))
              ((lsame cmach "O")
               (setf rmach
                       (coerce
                        (multiple-value-bind (ret-val var-0)
                            (huge zero)
                          (declare (ignore))
                          (when var-0 (setf zero var-0))
                          ret-val)
                        'double-float)))
              (t (setf rmach zero)))
        (setf dlamch rmach)
        (go end_label)
       end_label
        (return (values dlamch nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlamch
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types '((string 1))
                                            :return-values '(nil) :calls
                                            '(fortran-to-lisp::lsame))))

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


(defun dlamc3 (a b)
  (declare (type (double-float) b a))
  (prog ((dlamc3 0.0d0))
    (declare (type (double-float) dlamc3))
    (setf dlamc3 (+ a b))
    (go end_label)
   end_label
    (return (values dlamc3 nil nil))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlamc3
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((double-float) (double-float))
                                            :return-values '(nil nil) :calls
                                            'nil)))

