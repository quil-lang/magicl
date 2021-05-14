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


(let* ((maxit 20)
       (zero 0.0d0)
       (one 1.0d0)
       (two 2.0d0)
       (three 3.0d0)
       (four 4.0d0)
       (eight 8.0d0))
  (declare (type (f2cl-lib:integer4 20 20) maxit)
           (type (double-float 0.0d0 0.0d0) zero)
           (type (double-float 1.0d0 1.0d0) one)
           (type (double-float 2.0d0 2.0d0) two)
           (type (double-float 3.0d0 3.0d0) three)
           (type (double-float 4.0d0 4.0d0) four)
           (type (double-float 8.0d0 8.0d0) eight)
           (ignorable maxit zero one two three four eight))
  (let ((first$ nil)
        (eps 0.0d0)
        (sminv2 0.0d0)
        (small2 0.0d0)
        (sminv1 0.0d0)
        (small1 0.0d0))
    (declare (type f2cl-lib:logical first$)
             (type (double-float) eps sminv2 small2 sminv1 small1))
    (setq first$ f2cl-lib:%true%)
    (defun dlaed6 (kniter orgati rho d z finit tau info)
      (declare (type (array double-float (3)) z d)
               (type (double-float) tau finit rho)
               (type f2cl-lib:logical orgati)
               (type (f2cl-lib:integer4) info kniter))
      (f2cl-lib:with-multi-array-data
          ((d double-float d-%data% d-%offset%)
           (z double-float z-%data% z-%offset%))
        (prog ((a 0.0d0) (b 0.0d0) (base 0.0d0) (c 0.0d0) (ddf 0.0d0)
               (df 0.0d0) (erretm 0.0d0) (eta 0.0d0) (f 0.0d0) (fc 0.0d0)
               (sclfac 0.0d0) (sclinv 0.0d0) (temp 0.0d0) (temp1 0.0d0)
               (temp2 0.0d0) (temp3 0.0d0) (temp4 0.0d0) (i 0) (iter 0)
               (niter 0) (scale nil)
               (dscale (make-array 3 :element-type 'double-float))
               (zscale (make-array 3 :element-type 'double-float)))
          (declare
           (type (double-float) a b base c ddf df erretm eta f fc sclfac sclinv
            temp temp1 temp2 temp3 temp4)
           (type (f2cl-lib:integer4) i iter niter)
           (type f2cl-lib:logical scale)
           (type (array double-float (3)) dscale zscale))
          (setf info 0)
          (setf niter 1)
          (setf tau zero)
          (cond
           ((= kniter 2)
            (cond
             (orgati
              (setf temp
                      (/
                       (- (f2cl-lib:fref d-%data% (3) ((1 3)) d-%offset%)
                          (f2cl-lib:fref d-%data% (2) ((1 3)) d-%offset%))
                       two))
              (setf c
                      (+ rho
                         (/ (f2cl-lib:fref z-%data% (1) ((1 3)) z-%offset%)
                            (- (f2cl-lib:fref d-%data% (1) ((1 3)) d-%offset%)
                               (f2cl-lib:fref d-%data% (2) ((1 3)) d-%offset%)
                               temp))))
              (setf a
                      (+
                       (* c
                          (+ (f2cl-lib:fref d-%data% (2) ((1 3)) d-%offset%)
                             (f2cl-lib:fref d-%data% (3) ((1 3)) d-%offset%)))
                       (f2cl-lib:fref z-%data% (2) ((1 3)) z-%offset%)
                       (f2cl-lib:fref z-%data% (3) ((1 3)) z-%offset%)))
              (setf b
                      (+
                       (* c (f2cl-lib:fref d-%data% (2) ((1 3)) d-%offset%)
                          (f2cl-lib:fref d-%data% (3) ((1 3)) d-%offset%))
                       (* (f2cl-lib:fref z-%data% (2) ((1 3)) z-%offset%)
                          (f2cl-lib:fref d-%data% (3) ((1 3)) d-%offset%))
                       (* (f2cl-lib:fref z-%data% (3) ((1 3)) z-%offset%)
                          (f2cl-lib:fref d-%data% (2) ((1 3)) d-%offset%)))))
             (t
              (setf temp
                      (/
                       (- (f2cl-lib:fref d-%data% (1) ((1 3)) d-%offset%)
                          (f2cl-lib:fref d-%data% (2) ((1 3)) d-%offset%))
                       two))
              (setf c
                      (+ rho
                         (/ (f2cl-lib:fref z-%data% (3) ((1 3)) z-%offset%)
                            (- (f2cl-lib:fref d-%data% (3) ((1 3)) d-%offset%)
                               (f2cl-lib:fref d-%data% (2) ((1 3)) d-%offset%)
                               temp))))
              (setf a
                      (+
                       (* c
                          (+ (f2cl-lib:fref d-%data% (1) ((1 3)) d-%offset%)
                             (f2cl-lib:fref d-%data% (2) ((1 3)) d-%offset%)))
                       (f2cl-lib:fref z-%data% (1) ((1 3)) z-%offset%)
                       (f2cl-lib:fref z-%data% (2) ((1 3)) z-%offset%)))
              (setf b
                      (+
                       (* c (f2cl-lib:fref d-%data% (1) ((1 3)) d-%offset%)
                          (f2cl-lib:fref d-%data% (2) ((1 3)) d-%offset%))
                       (* (f2cl-lib:fref z-%data% (1) ((1 3)) z-%offset%)
                          (f2cl-lib:fref d-%data% (2) ((1 3)) d-%offset%))
                       (* (f2cl-lib:fref z-%data% (2) ((1 3)) z-%offset%)
                          (f2cl-lib:fref d-%data% (1) ((1 3)) d-%offset%))))))
            (setf temp (max (abs a) (abs b) (abs c))) (setf a (/ a temp))
            (setf b (/ b temp)) (setf c (/ c temp))
            (cond ((= c zero) (setf tau (/ b a)))
                  ((<= a zero)
                   (setf tau
                           (/
                            (- a
                               (f2cl-lib:fsqrt
                                (abs (+ (* a a) (* (- four) b c)))))
                            (* two c))))
                  (t
                   (setf tau
                           (/ (* two b)
                              (+ a
                                 (f2cl-lib:fsqrt
                                  (abs (+ (* a a) (* (- four) b c)))))))))
            (setf temp
                    (+ rho
                       (/ (f2cl-lib:fref z-%data% (1) ((1 3)) z-%offset%)
                          (- (f2cl-lib:fref d-%data% (1) ((1 3)) d-%offset%)
                             tau))
                       (/ (f2cl-lib:fref z-%data% (2) ((1 3)) z-%offset%)
                          (- (f2cl-lib:fref d-%data% (2) ((1 3)) d-%offset%)
                             tau))
                       (/ (f2cl-lib:fref z-%data% (3) ((1 3)) z-%offset%)
                          (- (f2cl-lib:fref d-%data% (3) ((1 3)) d-%offset%)
                             tau))))
            (if (<= (abs finit) (abs temp))
                (setf tau zero))))
          (cond
           (first$ (setf eps (dlamch "E")) (setf base (dlamch "B"))
            (setf small1
                    (expt base
                          (f2cl-lib:int
                           (/
                            (/ (f2cl-lib:flog (dlamch "S"))
                               (f2cl-lib:flog base))
                            three))))
            (setf sminv1 (/ one small1)) (setf small2 (* small1 small1))
            (setf sminv2 (* sminv1 sminv1)) (setf first$ f2cl-lib:%false%)))
          (cond
           (orgati
            (setf temp
                    (min
                     (abs
                      (- (f2cl-lib:fref d-%data% (2) ((1 3)) d-%offset%) tau))
                     (abs
                      (- (f2cl-lib:fref d-%data% (3) ((1 3)) d-%offset%)
                         tau)))))
           (t
            (setf temp
                    (min
                     (abs
                      (- (f2cl-lib:fref d-%data% (1) ((1 3)) d-%offset%) tau))
                     (abs
                      (- (f2cl-lib:fref d-%data% (2) ((1 3)) d-%offset%)
                         tau))))))
          (setf scale f2cl-lib:%false%)
          (cond
           ((<= temp small1) (setf scale f2cl-lib:%true%)
            (cond ((<= temp small2) (setf sclfac sminv2) (setf sclinv small2))
                  (t (setf sclfac sminv1) (setf sclinv small1)))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i 3) nil)
              (tagbody
                (setf (f2cl-lib:fref dscale (i) ((1 3)))
                        (* (f2cl-lib:fref d-%data% (i) ((1 3)) d-%offset%)
                           sclfac))
                (setf (f2cl-lib:fref zscale (i) ((1 3)))
                        (* (f2cl-lib:fref z-%data% (i) ((1 3)) z-%offset%)
                           sclfac))
               label10))
            (setf tau (* tau sclfac)))
           (t
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i 3) nil)
              (tagbody
                (setf (f2cl-lib:fref dscale (i) ((1 3)))
                        (f2cl-lib:fref d-%data% (i) ((1 3)) d-%offset%))
                (setf (f2cl-lib:fref zscale (i) ((1 3)))
                        (f2cl-lib:fref z-%data% (i) ((1 3)) z-%offset%))
               label20))))
          (setf fc zero)
          (setf df zero)
          (setf ddf zero)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i 3) nil)
            (tagbody
              (setf temp (/ one (- (f2cl-lib:fref dscale (i) ((1 3))) tau)))
              (setf temp1 (* (f2cl-lib:fref zscale (i) ((1 3))) temp))
              (setf temp2 (* temp1 temp))
              (setf temp3 (* temp2 temp))
              (setf fc (+ fc (/ temp1 (f2cl-lib:fref dscale (i) ((1 3))))))
              (setf df (+ df temp2))
              (setf ddf (+ ddf temp3))
             label30))
          (setf f (+ finit (* tau fc)))
          (if (<= (abs f) zero)
              (go label60))
          (setf iter (f2cl-lib:int-add niter 1))
          (f2cl-lib:fdo (niter iter (f2cl-lib:int-add niter 1))
                        ((> niter maxit) nil)
            (tagbody
              (cond
               (orgati (setf temp1 (- (f2cl-lib:fref dscale (2) ((1 3))) tau))
                (setf temp2 (- (f2cl-lib:fref dscale (3) ((1 3))) tau)))
               (t (setf temp1 (- (f2cl-lib:fref dscale (1) ((1 3))) tau))
                (setf temp2 (- (f2cl-lib:fref dscale (2) ((1 3))) tau))))
              (setf a (+ (* (+ temp1 temp2) f) (* (- temp1) temp2 df)))
              (setf b (* temp1 temp2 f))
              (setf c (+ (- f (* (+ temp1 temp2) df)) (* temp1 temp2 ddf)))
              (setf temp (max (abs a) (abs b) (abs c)))
              (setf a (/ a temp))
              (setf b (/ b temp))
              (setf c (/ c temp))
              (cond ((= c zero) (setf eta (/ b a)))
                    ((<= a zero)
                     (setf eta
                             (/
                              (- a
                                 (f2cl-lib:fsqrt
                                  (abs (+ (* a a) (* (- four) b c)))))
                              (* two c))))
                    (t
                     (setf eta
                             (/ (* two b)
                                (+ a
                                   (f2cl-lib:fsqrt
                                    (abs (+ (* a a) (* (- four) b c)))))))))
              (cond ((>= (* f eta) zero) (setf eta (/ (- f) df))))
              (setf temp (+ eta tau))
              (cond
               (orgati
                (if (and (> eta zero)
                         (>= temp (f2cl-lib:fref dscale (3) ((1 3)))))
                    (setf eta
                            (/ (- (f2cl-lib:fref dscale (3) ((1 3))) tau)
                               two)))
                (if (and (< eta zero)
                         (<= temp (f2cl-lib:fref dscale (2) ((1 3)))))
                    (setf eta
                            (/ (- (f2cl-lib:fref dscale (2) ((1 3))) tau)
                               two))))
               (t
                (if (and (> eta zero)
                         (>= temp (f2cl-lib:fref dscale (2) ((1 3)))))
                    (setf eta
                            (/ (- (f2cl-lib:fref dscale (2) ((1 3))) tau)
                               two)))
                (if (and (< eta zero)
                         (<= temp (f2cl-lib:fref dscale (1) ((1 3)))))
                    (setf eta
                            (/ (- (f2cl-lib:fref dscale (1) ((1 3))) tau)
                               two)))))
              (setf tau (+ tau eta))
              (setf fc zero)
              (setf erretm zero)
              (setf df zero)
              (setf ddf zero)
              (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                            ((> i 3) nil)
                (tagbody
                  (setf temp
                          (/ one (- (f2cl-lib:fref dscale (i) ((1 3))) tau)))
                  (setf temp1 (* (f2cl-lib:fref zscale (i) ((1 3))) temp))
                  (setf temp2 (* temp1 temp))
                  (setf temp3 (* temp2 temp))
                  (setf temp4 (/ temp1 (f2cl-lib:fref dscale (i) ((1 3)))))
                  (setf fc (+ fc temp4))
                  (setf erretm (+ erretm (abs temp4)))
                  (setf df (+ df temp2))
                  (setf ddf (+ ddf temp3))
                 label40))
              (setf f (+ finit (* tau fc)))
              (setf erretm
                      (+ (* eight (+ (abs finit) (* (abs tau) erretm)))
                         (* (abs tau) df)))
              (if (<= (abs f) (* eps erretm))
                  (go label60))
             label50))
          (setf info 1)
         label60
          (if scale
              (setf tau (* tau sclinv)))
          (go end_label)
         end_label
          (return (values nil nil nil nil nil nil tau info)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlaed6
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              fortran-to-lisp::logical
                                              (double-float)
                                              (array double-float (3))
                                              (array double-float (3))
                                              (double-float) (double-float)
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil nil
                                              fortran-to-lisp::tau
                                              fortran-to-lisp::info)
                                            :calls '(fortran-to-lisp::dlamch))))

