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


(let* ((one 1.0d0) (two 2.0d0) (zero 0.0d0) (half 0.5d0))
  (declare (type (double-float 1.0d0 1.0d0) one)
           (type (double-float 2.0d0 2.0d0) two)
           (type (double-float 0.0d0 0.0d0) zero)
           (type (double-float 0.5d0 0.5d0) half)
           (ignorable one two zero half))
  (defun dlaev2 (a b c rt1 rt2 cs1 sn1)
    (declare (type (double-float) sn1 cs1 rt2 rt1 c b a))
    (prog ((ab 0.0d0) (acmn 0.0d0) (acmx 0.0d0) (acs 0.0d0) (adf 0.0d0)
           (cs 0.0d0) (ct 0.0d0) (df 0.0d0) (rt 0.0d0) (sm 0.0d0) (tb 0.0d0)
           (tn 0.0d0) (sgn1 0) (sgn2 0))
      (declare (type (double-float) ab acmn acmx acs adf cs ct df rt sm tb tn)
               (type (f2cl-lib:integer4) sgn1 sgn2))
      (setf sm (+ a c))
      (setf df (- a c))
      (setf adf (abs df))
      (setf tb (+ b b))
      (setf ab (abs tb))
      (cond ((> (abs a) (abs c)) (setf acmx a) (setf acmn c))
            (t (setf acmx c) (setf acmn a)))
      (cond
       ((> adf ab)
        (setf rt (* adf (f2cl-lib:fsqrt (+ one (expt (/ ab adf) 2))))))
       ((< adf ab)
        (setf rt (* ab (f2cl-lib:fsqrt (+ one (expt (/ adf ab) 2))))))
       (t (setf rt (* ab (f2cl-lib:fsqrt two)))))
      (cond
       ((< sm zero) (setf rt1 (* half (- sm rt))) (setf sgn1 -1)
        (setf rt2 (- (* (/ acmx rt1) acmn) (* (/ b rt1) b))))
       ((> sm zero) (setf rt1 (* half (+ sm rt))) (setf sgn1 1)
        (setf rt2 (- (* (/ acmx rt1) acmn) (* (/ b rt1) b))))
       (t (setf rt1 (* half rt)) (setf rt2 (* (- half) rt)) (setf sgn1 1)))
      (cond ((>= df zero) (setf cs (+ df rt)) (setf sgn2 1))
            (t (setf cs (- df rt)) (setf sgn2 -1)))
      (setf acs (abs cs))
      (cond
       ((> acs ab) (setf ct (/ (- tb) cs))
        (setf sn1 (/ one (f2cl-lib:fsqrt (+ one (* ct ct)))))
        (setf cs1 (* ct sn1)))
       (t
        (cond ((= ab zero) (setf cs1 one) (setf sn1 zero))
              (t (setf tn (/ (- cs) tb))
               (setf cs1 (/ one (f2cl-lib:fsqrt (+ one (* tn tn)))))
               (setf sn1 (* tn cs1))))))
      (cond ((= sgn1 sgn2) (setf tn cs1) (setf cs1 (- sn1)) (setf sn1 tn)))
      (go end_label)
     end_label
      (return (values nil nil nil rt1 rt2 cs1 sn1)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlaev2
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((double-float) (double-float)
                                              (double-float) (double-float)
                                              (double-float) (double-float)
                                              (double-float))
                                            :return-values
                                            '(nil nil nil fortran-to-lisp::rt1
                                              fortran-to-lisp::rt2
                                              fortran-to-lisp::cs1
                                              fortran-to-lisp::sn1)
                                            :calls 'nil)))

