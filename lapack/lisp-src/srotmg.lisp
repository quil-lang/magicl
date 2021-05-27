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


(let ((zero 0.0)
      (one 1.0)
      (two 2.0)
      (gam 4096.0)
      (gamsq 1.67772e7)
      (rgamsq 5.96046e-8))
  (declare (type (single-float) zero one two gam gamsq rgamsq))
  (defun srotmg (sd1 sd2 sx1 sy1 sparam)
    (declare (type (array single-float (5)) sparam)
             (type (single-float) sy1 sx1 sd2 sd1))
    (f2cl-lib:with-multi-array-data
        ((sparam single-float sparam-%data% sparam-%offset%))
      (prog ((sflag 0.0) (sh11 0.0) (sh12 0.0) (sh21 0.0) (sh22 0.0) (sp1 0.0)
             (sp2 0.0) (sq1 0.0) (sq2 0.0) (stemp 0.0) (su 0.0))
        (declare
         (type (single-float) su stemp sq2 sq1 sp2 sp1 sh22 sh21 sh12 sh11
          sflag))
        (cond
         ((< sd1 zero) (setf sflag (- one)) (setf sh11 zero) (setf sh12 zero)
          (setf sh21 zero) (setf sh22 zero) (setf sd1 zero) (setf sd2 zero)
          (setf sx1 zero))
         (t (setf sp2 (* sd2 sy1))
          (cond
           ((= sp2 zero) (setf sflag (- two))
            (setf (f2cl-lib:fref sparam-%data% (1) ((1 5)) sparam-%offset%)
                    sflag)
            (go end_label)))
          (setf sp1 (* sd1 sx1)) (setf sq2 (* sp2 sy1)) (setf sq1 (* sp1 sx1))
          (cond
           ((> (abs sq1) (abs sq2)) (setf sh21 (/ (- sy1) sx1))
            (setf sh12 (/ sp2 sp1)) (setf su (- one (* sh12 sh21)))
            (cond
             ((> su zero) (setf sflag zero) (setf sd1 (/ sd1 su))
              (setf sd2 (/ sd2 su)) (setf sx1 (* sx1 su)))))
           (t
            (cond
             ((< sq2 zero) (setf sflag (- one)) (setf sh11 zero)
              (setf sh12 zero) (setf sh21 zero) (setf sh22 zero)
              (setf sd1 zero) (setf sd2 zero) (setf sx1 zero))
             (t (setf sflag one) (setf sh11 (/ sp1 sp2))
              (setf sh22 (/ sx1 sy1)) (setf su (+ one (* sh11 sh22)))
              (setf stemp (/ sd2 su)) (setf sd2 (/ sd1 su)) (setf sd1 stemp)
              (setf sx1 (* sy1 su))))))
          (cond
           ((/= sd1 zero)
            (tagbody
             label100000
              (if (not (or (<= sd1 rgamsq) (>= sd1 gamsq)))
                  (go label100001))
              (cond
               ((= sflag zero) (setf sh11 one) (setf sh22 one)
                (setf sflag (- one)))
               (t (setf sh21 (- one)) (setf sh12 one) (setf sflag (- one))))
              (cond
               ((<= sd1 rgamsq) (setf sd1 (* sd1 (expt gam 2)))
                (setf sx1 (/ sx1 gam)) (setf sh11 (/ sh11 gam))
                (setf sh12 (/ sh12 gam)))
               (t (setf sd1 (/ sd1 (expt gam 2))) (setf sx1 (* sx1 gam))
                (setf sh11 (* sh11 gam)) (setf sh12 (* sh12 gam))))
              (go label100000)
             label100001)))
          (cond
           ((/= sd2 zero)
            (tagbody
             label100002
              (if (not (or (<= (abs sd2) rgamsq) (>= (abs sd2) gamsq)))
                  (go label100003))
              (cond
               ((= sflag zero) (setf sh11 one) (setf sh22 one)
                (setf sflag (- one)))
               (t (setf sh21 (- one)) (setf sh12 one) (setf sflag (- one))))
              (cond
               ((<= (abs sd2) rgamsq) (setf sd2 (* sd2 (expt gam 2)))
                (setf sh21 (/ sh21 gam)) (setf sh22 (/ sh22 gam)))
               (t (setf sd2 (/ sd2 (expt gam 2))) (setf sh21 (* sh21 gam))
                (setf sh22 (* sh22 gam))))
              (go label100002)
             label100003)))))
        (cond
         ((< sflag zero)
          (setf (f2cl-lib:fref sparam-%data% (2) ((1 5)) sparam-%offset%) sh11)
          (setf (f2cl-lib:fref sparam-%data% (3) ((1 5)) sparam-%offset%) sh21)
          (setf (f2cl-lib:fref sparam-%data% (4) ((1 5)) sparam-%offset%) sh12)
          (setf (f2cl-lib:fref sparam-%data% (5) ((1 5)) sparam-%offset%)
                  sh22))
         ((= sflag zero)
          (setf (f2cl-lib:fref sparam-%data% (3) ((1 5)) sparam-%offset%) sh21)
          (setf (f2cl-lib:fref sparam-%data% (4) ((1 5)) sparam-%offset%)
                  sh12))
         (t
          (setf (f2cl-lib:fref sparam-%data% (2) ((1 5)) sparam-%offset%) sh11)
          (setf (f2cl-lib:fref sparam-%data% (5) ((1 5)) sparam-%offset%)
                  sh22)))
        (setf (f2cl-lib:fref sparam-%data% (1) ((1 5)) sparam-%offset%) sflag)
        (go end_label)
       end_label
        (return (values sd1 sd2 sx1 nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::srotmg
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((single-float) (single-float)
                                              (single-float) (single-float)
                                              (array single-float (5)))
                                            :return-values
                                            '(fortran-to-lisp::sd1
                                              fortran-to-lisp::sd2
                                              fortran-to-lisp::sx1 nil nil)
                                            :calls 'nil)))

