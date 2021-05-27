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


(defun dsdot (n sx incx sy incy)
  (declare (type (array single-float (*)) sy sx)
           (type (f2cl-lib:integer4) incy incx n))
  (f2cl-lib:with-multi-array-data
      ((sx single-float sx-%data% sx-%offset%)
       (sy single-float sy-%data% sy-%offset%))
    (prog ((i 0) (kx 0) (ky 0) (ns 0) (dsdot 0.0d0))
      (declare (type (double-float) dsdot)
               (type (f2cl-lib:integer4) ns ky kx i))
      (setf dsdot 0.0d0)
      (if (<= n 0)
          (go end_label))
      (cond
       ((and (= incx incy) (> incx 0)) (setf ns (f2cl-lib:int-mul n incx))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i incx))
                      ((> i ns) nil)
          (tagbody
            (setf dsdot
                    (+ dsdot
                       (*
                        (f2cl-lib:dble
                         (f2cl-lib:fref sx-%data% (i) ((1 *)) sx-%offset%))
                        (f2cl-lib:dble
                         (f2cl-lib:fref sy-%data% (i) ((1 *)) sy-%offset%)))))
           label100000)))
       (t (setf kx 1) (setf ky 1)
        (if (< incx 0)
            (setf kx
                    (f2cl-lib:int-add 1
                                      (f2cl-lib:int-mul (f2cl-lib:int-sub 1 n)
                                                        incx))))
        (if (< incy 0)
            (setf ky
                    (f2cl-lib:int-add 1
                                      (f2cl-lib:int-mul (f2cl-lib:int-sub 1 n)
                                                        incy))))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (setf dsdot
                    (+ dsdot
                       (*
                        (f2cl-lib:dble
                         (f2cl-lib:fref sx-%data% (kx) ((1 *)) sx-%offset%))
                        (f2cl-lib:dble
                         (f2cl-lib:fref sy-%data% (ky) ((1 *)) sy-%offset%)))))
            (setf kx (f2cl-lib:int-add kx incx))
            (setf ky (f2cl-lib:int-add ky incy))
           label100001))))
      (go end_label)
     end_label
      (return (values dsdot nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dsdot fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (array single-float (*))
                                              (fortran-to-lisp::integer4)
                                              (array single-float (*))
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil) :calls
                                            'nil)))

