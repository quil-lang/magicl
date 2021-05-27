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


(let ((zero 0.0) (two 2.0))
  (declare (type (single-float) zero two))
  (defun srotm (n sx incx sy incy sparam)
    (declare (type (array single-float (5)) sparam)
             (type (array single-float (*)) sy sx)
             (type (f2cl-lib:integer4) incy incx n))
    (f2cl-lib:with-multi-array-data
        ((sx single-float sx-%data% sx-%offset%)
         (sy single-float sy-%data% sy-%offset%)
         (sparam single-float sparam-%data% sparam-%offset%))
      (prog ((i 0) (kx 0) (ky 0) (nsteps 0) (sflag 0.0) (sh11 0.0) (sh12 0.0)
             (sh21 0.0) (sh22 0.0) (w 0.0) (z 0.0))
        (declare (type (single-float) z w sh22 sh21 sh12 sh11 sflag)
                 (type (f2cl-lib:integer4) nsteps ky kx i))
        (setf sflag (f2cl-lib:fref sparam-%data% (1) ((1 5)) sparam-%offset%))
        (if (or (<= n 0) (= (+ sflag two) zero))
            (go end_label))
        (cond
         ((and (= incx incy) (> incx 0))
          (setf nsteps (f2cl-lib:int-mul n incx))
          (cond
           ((< sflag zero)
            (setf sh11
                    (f2cl-lib:fref sparam-%data% (2) ((1 5)) sparam-%offset%))
            (setf sh12
                    (f2cl-lib:fref sparam-%data% (4) ((1 5)) sparam-%offset%))
            (setf sh21
                    (f2cl-lib:fref sparam-%data% (3) ((1 5)) sparam-%offset%))
            (setf sh22
                    (f2cl-lib:fref sparam-%data% (5) ((1 5)) sparam-%offset%))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i incx))
                          ((> i nsteps) nil)
              (tagbody
                (setf w (f2cl-lib:fref sx-%data% (i) ((1 *)) sx-%offset%))
                (setf z (f2cl-lib:fref sy-%data% (i) ((1 *)) sy-%offset%))
                (setf (f2cl-lib:fref sx-%data% (i) ((1 *)) sx-%offset%)
                        (+ (* w sh11) (* z sh12)))
                (setf (f2cl-lib:fref sy-%data% (i) ((1 *)) sy-%offset%)
                        (+ (* w sh21) (* z sh22)))
               label100000)))
           ((= sflag zero)
            (setf sh12
                    (f2cl-lib:fref sparam-%data% (4) ((1 5)) sparam-%offset%))
            (setf sh21
                    (f2cl-lib:fref sparam-%data% (3) ((1 5)) sparam-%offset%))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i incx))
                          ((> i nsteps) nil)
              (tagbody
                (setf w (f2cl-lib:fref sx-%data% (i) ((1 *)) sx-%offset%))
                (setf z (f2cl-lib:fref sy-%data% (i) ((1 *)) sy-%offset%))
                (setf (f2cl-lib:fref sx-%data% (i) ((1 *)) sx-%offset%)
                        (+ w (* z sh12)))
                (setf (f2cl-lib:fref sy-%data% (i) ((1 *)) sy-%offset%)
                        (+ (* w sh21) z))
               label100001)))
           (t
            (setf sh11
                    (f2cl-lib:fref sparam-%data% (2) ((1 5)) sparam-%offset%))
            (setf sh22
                    (f2cl-lib:fref sparam-%data% (5) ((1 5)) sparam-%offset%))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i incx))
                          ((> i nsteps) nil)
              (tagbody
                (setf w (f2cl-lib:fref sx-%data% (i) ((1 *)) sx-%offset%))
                (setf z (f2cl-lib:fref sy-%data% (i) ((1 *)) sy-%offset%))
                (setf (f2cl-lib:fref sx-%data% (i) ((1 *)) sx-%offset%)
                        (+ (* w sh11) z))
                (setf (f2cl-lib:fref sy-%data% (i) ((1 *)) sy-%offset%)
                        (- (* sh22 z) w))
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
           ((< sflag zero)
            (setf sh11
                    (f2cl-lib:fref sparam-%data% (2) ((1 5)) sparam-%offset%))
            (setf sh12
                    (f2cl-lib:fref sparam-%data% (4) ((1 5)) sparam-%offset%))
            (setf sh21
                    (f2cl-lib:fref sparam-%data% (3) ((1 5)) sparam-%offset%))
            (setf sh22
                    (f2cl-lib:fref sparam-%data% (5) ((1 5)) sparam-%offset%))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (setf w (f2cl-lib:fref sx-%data% (kx) ((1 *)) sx-%offset%))
                (setf z (f2cl-lib:fref sy-%data% (ky) ((1 *)) sy-%offset%))
                (setf (f2cl-lib:fref sx-%data% (kx) ((1 *)) sx-%offset%)
                        (+ (* w sh11) (* z sh12)))
                (setf (f2cl-lib:fref sy-%data% (ky) ((1 *)) sy-%offset%)
                        (+ (* w sh21) (* z sh22)))
                (setf kx (f2cl-lib:int-add kx incx))
                (setf ky (f2cl-lib:int-add ky incy))
               label100003)))
           ((= sflag zero)
            (setf sh12
                    (f2cl-lib:fref sparam-%data% (4) ((1 5)) sparam-%offset%))
            (setf sh21
                    (f2cl-lib:fref sparam-%data% (3) ((1 5)) sparam-%offset%))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (setf w (f2cl-lib:fref sx-%data% (kx) ((1 *)) sx-%offset%))
                (setf z (f2cl-lib:fref sy-%data% (ky) ((1 *)) sy-%offset%))
                (setf (f2cl-lib:fref sx-%data% (kx) ((1 *)) sx-%offset%)
                        (+ w (* z sh12)))
                (setf (f2cl-lib:fref sy-%data% (ky) ((1 *)) sy-%offset%)
                        (+ (* w sh21) z))
                (setf kx (f2cl-lib:int-add kx incx))
                (setf ky (f2cl-lib:int-add ky incy))
               label100004)))
           (t
            (setf sh11
                    (f2cl-lib:fref sparam-%data% (2) ((1 5)) sparam-%offset%))
            (setf sh22
                    (f2cl-lib:fref sparam-%data% (5) ((1 5)) sparam-%offset%))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (setf w (f2cl-lib:fref sx-%data% (kx) ((1 *)) sx-%offset%))
                (setf z (f2cl-lib:fref sy-%data% (ky) ((1 *)) sy-%offset%))
                (setf (f2cl-lib:fref sx-%data% (kx) ((1 *)) sx-%offset%)
                        (+ (* w sh11) z))
                (setf (f2cl-lib:fref sy-%data% (ky) ((1 *)) sy-%offset%)
                        (- (* sh22 z) w))
                (setf kx (f2cl-lib:int-add kx incx))
                (setf ky (f2cl-lib:int-add ky incy))
               label100005))))))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::srotm fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (array single-float (*))
                                              (fortran-to-lisp::integer4)
                                              (array single-float (*))
                                              (fortran-to-lisp::integer4)
                                              (array single-float (5)))
                                            :return-values
                                            '(nil nil nil nil nil nil) :calls
                                            'nil)))

