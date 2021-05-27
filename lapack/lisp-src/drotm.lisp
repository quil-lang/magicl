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


(let ((zero 0.0d0) (two 2.0d0))
  (declare (type (double-float) zero two))
  (defun drotm (n dx incx dy incy dparam)
    (declare (type (array double-float (5)) dparam)
             (type (array double-float (*)) dy dx)
             (type (f2cl-lib:integer4) incy incx n))
    (f2cl-lib:with-multi-array-data
        ((dx double-float dx-%data% dx-%offset%)
         (dy double-float dy-%data% dy-%offset%)
         (dparam double-float dparam-%data% dparam-%offset%))
      (prog ((i 0) (kx 0) (ky 0) (nsteps 0) (dflag 0.0d0) (dh11 0.0d0)
             (dh12 0.0d0) (dh21 0.0d0) (dh22 0.0d0) (w 0.0d0) (z 0.0d0))
        (declare (type (double-float) z w dh22 dh21 dh12 dh11 dflag)
                 (type (f2cl-lib:integer4) nsteps ky kx i))
        (setf dflag (f2cl-lib:fref dparam-%data% (1) ((1 5)) dparam-%offset%))
        (if (or (<= n 0) (= (+ dflag two) zero))
            (go end_label))
        (cond
         ((and (= incx incy) (> incx 0))
          (setf nsteps (f2cl-lib:int-mul n incx))
          (cond
           ((< dflag zero)
            (setf dh11
                    (f2cl-lib:fref dparam-%data% (2) ((1 5)) dparam-%offset%))
            (setf dh12
                    (f2cl-lib:fref dparam-%data% (4) ((1 5)) dparam-%offset%))
            (setf dh21
                    (f2cl-lib:fref dparam-%data% (3) ((1 5)) dparam-%offset%))
            (setf dh22
                    (f2cl-lib:fref dparam-%data% (5) ((1 5)) dparam-%offset%))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i incx))
                          ((> i nsteps) nil)
              (tagbody
                (setf w (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%))
                (setf z (f2cl-lib:fref dy-%data% (i) ((1 *)) dy-%offset%))
                (setf (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)
                        (+ (* w dh11) (* z dh12)))
                (setf (f2cl-lib:fref dy-%data% (i) ((1 *)) dy-%offset%)
                        (+ (* w dh21) (* z dh22)))
               label100000)))
           ((= dflag zero)
            (setf dh12
                    (f2cl-lib:fref dparam-%data% (4) ((1 5)) dparam-%offset%))
            (setf dh21
                    (f2cl-lib:fref dparam-%data% (3) ((1 5)) dparam-%offset%))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i incx))
                          ((> i nsteps) nil)
              (tagbody
                (setf w (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%))
                (setf z (f2cl-lib:fref dy-%data% (i) ((1 *)) dy-%offset%))
                (setf (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)
                        (+ w (* z dh12)))
                (setf (f2cl-lib:fref dy-%data% (i) ((1 *)) dy-%offset%)
                        (+ (* w dh21) z))
               label100001)))
           (t
            (setf dh11
                    (f2cl-lib:fref dparam-%data% (2) ((1 5)) dparam-%offset%))
            (setf dh22
                    (f2cl-lib:fref dparam-%data% (5) ((1 5)) dparam-%offset%))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i incx))
                          ((> i nsteps) nil)
              (tagbody
                (setf w (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%))
                (setf z (f2cl-lib:fref dy-%data% (i) ((1 *)) dy-%offset%))
                (setf (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)
                        (+ (* w dh11) z))
                (setf (f2cl-lib:fref dy-%data% (i) ((1 *)) dy-%offset%)
                        (- (* dh22 z) w))
               label100002)))))
         (t (setf kx 1) (setf ky 1)
          (if (< incx 0)
              (setf kx
                      (f2cl-lib:int-add 1
                                        (f2cl-lib:int-mul
                                         (f2cl-lib:int-sub 1 n) incx))))
          (if (< incy 0)
              (setf ky
                      (f2cl-lib:int-add 1
                                        (f2cl-lib:int-mul
                                         (f2cl-lib:int-sub 1 n) incy))))
          (cond
           ((< dflag zero)
            (setf dh11
                    (f2cl-lib:fref dparam-%data% (2) ((1 5)) dparam-%offset%))
            (setf dh12
                    (f2cl-lib:fref dparam-%data% (4) ((1 5)) dparam-%offset%))
            (setf dh21
                    (f2cl-lib:fref dparam-%data% (3) ((1 5)) dparam-%offset%))
            (setf dh22
                    (f2cl-lib:fref dparam-%data% (5) ((1 5)) dparam-%offset%))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (setf w (f2cl-lib:fref dx-%data% (kx) ((1 *)) dx-%offset%))
                (setf z (f2cl-lib:fref dy-%data% (ky) ((1 *)) dy-%offset%))
                (setf (f2cl-lib:fref dx-%data% (kx) ((1 *)) dx-%offset%)
                        (+ (* w dh11) (* z dh12)))
                (setf (f2cl-lib:fref dy-%data% (ky) ((1 *)) dy-%offset%)
                        (+ (* w dh21) (* z dh22)))
                (setf kx (f2cl-lib:int-add kx incx))
                (setf ky (f2cl-lib:int-add ky incy))
               label100003)))
           ((= dflag zero)
            (setf dh12
                    (f2cl-lib:fref dparam-%data% (4) ((1 5)) dparam-%offset%))
            (setf dh21
                    (f2cl-lib:fref dparam-%data% (3) ((1 5)) dparam-%offset%))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (setf w (f2cl-lib:fref dx-%data% (kx) ((1 *)) dx-%offset%))
                (setf z (f2cl-lib:fref dy-%data% (ky) ((1 *)) dy-%offset%))
                (setf (f2cl-lib:fref dx-%data% (kx) ((1 *)) dx-%offset%)
                        (+ w (* z dh12)))
                (setf (f2cl-lib:fref dy-%data% (ky) ((1 *)) dy-%offset%)
                        (+ (* w dh21) z))
                (setf kx (f2cl-lib:int-add kx incx))
                (setf ky (f2cl-lib:int-add ky incy))
               label100004)))
           (t
            (setf dh11
                    (f2cl-lib:fref dparam-%data% (2) ((1 5)) dparam-%offset%))
            (setf dh22
                    (f2cl-lib:fref dparam-%data% (5) ((1 5)) dparam-%offset%))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (setf w (f2cl-lib:fref dx-%data% (kx) ((1 *)) dx-%offset%))
                (setf z (f2cl-lib:fref dy-%data% (ky) ((1 *)) dy-%offset%))
                (setf (f2cl-lib:fref dx-%data% (kx) ((1 *)) dx-%offset%)
                        (+ (* w dh11) z))
                (setf (f2cl-lib:fref dy-%data% (ky) ((1 *)) dy-%offset%)
                        (- (* dh22 z) w))
                (setf kx (f2cl-lib:int-add kx incx))
                (setf ky (f2cl-lib:int-add ky incy))
               label100005))))))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::drotm fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4)
                                              (array double-float (5)))
                                            :return-values
                                            '(nil nil nil nil nil nil) :calls
                                            'nil)))

