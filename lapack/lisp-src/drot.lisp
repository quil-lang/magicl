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


(defun drot (n dx incx dy incy c s)
  (declare (type (double-float) s c)
           (type (array double-float (*)) dy dx)
           (type (f2cl-lib:integer4) incy incx n))
  (f2cl-lib:with-multi-array-data
      ((dx double-float dx-%data% dx-%offset%)
       (dy double-float dy-%data% dy-%offset%))
    (prog ((i 0) (ix 0) (iy 0) (dtemp 0.0d0))
      (declare (type (double-float) dtemp)
               (type (f2cl-lib:integer4) iy ix i))
      (if (<= n 0)
          (go end_label))
      (cond
       ((and (= incx 1) (= incy 1))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (setf dtemp
                    (+ (* c (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%))
                       (* s
                          (f2cl-lib:fref dy-%data% (i) ((1 *)) dy-%offset%))))
            (setf (f2cl-lib:fref dy-%data% (i) ((1 *)) dy-%offset%)
                    (- (* c (f2cl-lib:fref dy-%data% (i) ((1 *)) dy-%offset%))
                       (* s
                          (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%))))
            (setf (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%) dtemp)
           label100000)))
       (t (setf ix 1) (setf iy 1)
        (if (< incx 0)
            (setf ix
                    (f2cl-lib:int-add
                     (f2cl-lib:int-mul (f2cl-lib:int-sub 1 n) incx) 1)))
        (if (< incy 0)
            (setf iy
                    (f2cl-lib:int-add
                     (f2cl-lib:int-mul (f2cl-lib:int-sub 1 n) incy) 1)))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (setf dtemp
                    (+ (* c (f2cl-lib:fref dx-%data% (ix) ((1 *)) dx-%offset%))
                       (* s
                          (f2cl-lib:fref dy-%data% (iy) ((1 *)) dy-%offset%))))
            (setf (f2cl-lib:fref dy-%data% (iy) ((1 *)) dy-%offset%)
                    (- (* c (f2cl-lib:fref dy-%data% (iy) ((1 *)) dy-%offset%))
                       (* s
                          (f2cl-lib:fref dx-%data% (ix) ((1 *)) dx-%offset%))))
            (setf (f2cl-lib:fref dx-%data% (ix) ((1 *)) dx-%offset%) dtemp)
            (setf ix (f2cl-lib:int-add ix incx))
            (setf iy (f2cl-lib:int-add iy incy))
           label100001))))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::drot fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4)
                                              (double-float) (double-float))
                                            :return-values
                                            '(nil nil nil nil nil nil nil)
                                            :calls 'nil)))

