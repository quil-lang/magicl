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


(let* ((realone 1.0d0) (one (f2cl-lib:cmplx 1.0d0 0.0d0)))
  (declare (type (double-float 1.0d0 1.0d0) realone)
           (type (f2cl-lib:complex16) one)
           (ignorable realone one))
  (defun zunbdb
         (trans signs m p q x11 ldx11 x12 ldx12 x21 ldx21 x22 ldx22 theta phi
          taup1 taup2 tauq1 tauq2 work lwork info)
    (declare (type (array double-float (*)) phi theta)
             (type (array f2cl-lib:complex16 (*)) work tauq2 tauq1 taup2 taup1
              x22 x21 x12 x11)
             (type (f2cl-lib:integer4) info lwork ldx22 ldx21 ldx12 ldx11 q p
              m)
             (type (string 1) signs trans))
    (f2cl-lib:with-multi-array-data
        ((trans character trans-%data% trans-%offset%)
         (signs character signs-%data% signs-%offset%)
         (x11 f2cl-lib:complex16 x11-%data% x11-%offset%)
         (x12 f2cl-lib:complex16 x12-%data% x12-%offset%)
         (x21 f2cl-lib:complex16 x21-%data% x21-%offset%)
         (x22 f2cl-lib:complex16 x22-%data% x22-%offset%)
         (taup1 f2cl-lib:complex16 taup1-%data% taup1-%offset%)
         (taup2 f2cl-lib:complex16 taup2-%data% taup2-%offset%)
         (tauq1 f2cl-lib:complex16 tauq1-%data% tauq1-%offset%)
         (tauq2 f2cl-lib:complex16 tauq2-%data% tauq2-%offset%)
         (work f2cl-lib:complex16 work-%data% work-%offset%)
         (theta double-float theta-%data% theta-%offset%)
         (phi double-float phi-%data% phi-%offset%))
      (prog ((z1 0.0d0) (z2 0.0d0) (z3 0.0d0) (z4 0.0d0) (i 0) (lworkmin 0)
             (lworkopt 0) (colmajor nil) (lquery nil))
        (declare (type (double-float) z1 z2 z3 z4)
                 (type (f2cl-lib:integer4) i lworkmin lworkopt)
                 (type f2cl-lib:logical colmajor lquery))
        (setf info 0)
        (setf colmajor (not (lsame trans "T")))
        (cond
         ((not (lsame signs "O")) (setf z1 realone) (setf z2 realone)
          (setf z3 realone) (setf z4 realone))
         (t (setf z1 realone) (setf z2 (- realone)) (setf z3 realone)
          (setf z4 (- realone))))
        (setf lquery (coerce (= lwork -1) 'f2cl-lib:logical))
        (cond ((< m 0) (setf info -3)) ((or (< p 0) (> p m)) (setf info -4))
              ((or (< q 0) (> q p)
                   (> q (f2cl-lib:int-add m (f2cl-lib:int-sub p)))
                   (> q (f2cl-lib:int-add m (f2cl-lib:int-sub q))))
               (setf info -5))
              ((and colmajor
                    (< ldx11
                       (max (the f2cl-lib:integer4 1)
                            (the f2cl-lib:integer4 p))))
               (setf info -7))
              ((and (not colmajor)
                    (< ldx11
                       (max (the f2cl-lib:integer4 1)
                            (the f2cl-lib:integer4 q))))
               (setf info -7))
              ((and colmajor
                    (< ldx12
                       (max (the f2cl-lib:integer4 1)
                            (the f2cl-lib:integer4 p))))
               (setf info -9))
              ((and (not colmajor)
                    (< ldx12
                       (max (the f2cl-lib:integer4 1)
                            (the f2cl-lib:integer4
                                 (f2cl-lib:int-add m (f2cl-lib:int-sub q))))))
               (setf info -9))
              ((and colmajor
                    (< ldx21
                       (max (the f2cl-lib:integer4 1)
                            (the f2cl-lib:integer4
                                 (f2cl-lib:int-add m (f2cl-lib:int-sub p))))))
               (setf info -11))
              ((and (not colmajor)
                    (< ldx21
                       (max (the f2cl-lib:integer4 1)
                            (the f2cl-lib:integer4 q))))
               (setf info -11))
              ((and colmajor
                    (< ldx22
                       (max (the f2cl-lib:integer4 1)
                            (the f2cl-lib:integer4
                                 (f2cl-lib:int-add m (f2cl-lib:int-sub p))))))
               (setf info -13))
              ((and (not colmajor)
                    (< ldx22
                       (max (the f2cl-lib:integer4 1)
                            (the f2cl-lib:integer4
                                 (f2cl-lib:int-add m (f2cl-lib:int-sub q))))))
               (setf info -13)))
        (cond
         ((= info 0) (setf lworkopt (f2cl-lib:int-sub m q))
          (setf lworkmin (f2cl-lib:int-sub m q))
          (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                  (coerce lworkopt 'f2cl-lib:complex16))
          (cond ((and (< lwork lworkmin) (not lquery)) (setf info -21)))))
        (cond
         ((/= info 0) (xerbla "xORBDB" (f2cl-lib:int-sub info)) (go end_label))
         (lquery (go end_label)))
        (cond
         (colmajor
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i q) nil)
            (tagbody
              (cond
               ((= i 1)
                (zscal (f2cl-lib:int-add (f2cl-lib:int-sub p i) 1)
                 (f2cl-lib:dcmplx z1 0.0d0)
                 (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16 (i i)
                                       ((1 ldx11) (1 *)) x11-%offset%)
                 1))
               (t
                (zscal (f2cl-lib:int-add (f2cl-lib:int-sub p i) 1)
                 (f2cl-lib:dcmplx
                  (* z1
                     (cos
                      (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub i 1))
                                     ((1 *)) phi-%offset%)))
                  0.0d0)
                 (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16 (i i)
                                       ((1 ldx11) (1 *)) x11-%offset%)
                 1)
                (zaxpy (f2cl-lib:int-add (f2cl-lib:int-sub p i) 1)
                 (f2cl-lib:dcmplx
                  (* (- z1) z3 z4
                     (sin
                      (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub i 1))
                                     ((1 *)) phi-%offset%)))
                  0.0d0)
                 (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16
                                       (i (f2cl-lib:int-sub i 1))
                                       ((1 ldx12) (1 *)) x12-%offset%)
                 1
                 (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16 (i i)
                                       ((1 ldx11) (1 *)) x11-%offset%)
                 1)))
              (cond
               ((= i 1)
                (zscal (f2cl-lib:int-add (f2cl-lib:int-sub m p i) 1)
                 (f2cl-lib:dcmplx z2 0.0d0)
                 (f2cl-lib:array-slice x21-%data% f2cl-lib:complex16 (i i)
                                       ((1 ldx21) (1 *)) x21-%offset%)
                 1))
               (t
                (zscal (f2cl-lib:int-add (f2cl-lib:int-sub m p i) 1)
                 (f2cl-lib:dcmplx
                  (* z2
                     (cos
                      (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub i 1))
                                     ((1 *)) phi-%offset%)))
                  0.0d0)
                 (f2cl-lib:array-slice x21-%data% f2cl-lib:complex16 (i i)
                                       ((1 ldx21) (1 *)) x21-%offset%)
                 1)
                (zaxpy (f2cl-lib:int-add (f2cl-lib:int-sub m p i) 1)
                 (f2cl-lib:dcmplx
                  (* (- z2) z3 z4
                     (sin
                      (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub i 1))
                                     ((1 *)) phi-%offset%)))
                  0.0d0)
                 (f2cl-lib:array-slice x22-%data% f2cl-lib:complex16
                                       (i (f2cl-lib:int-sub i 1))
                                       ((1 ldx22) (1 *)) x22-%offset%)
                 1
                 (f2cl-lib:array-slice x21-%data% f2cl-lib:complex16 (i i)
                                       ((1 ldx21) (1 *)) x21-%offset%)
                 1)))
              (setf (f2cl-lib:fref theta-%data% (i) ((1 *)) theta-%offset%)
                      (f2cl-lib:atan2
                       (dznrm2 (f2cl-lib:int-add (f2cl-lib:int-sub m p i) 1)
                        (f2cl-lib:array-slice x21-%data% f2cl-lib:complex16
                                              (i i) ((1 ldx21) (1 *))
                                              x21-%offset%)
                        1)
                       (dznrm2 (f2cl-lib:int-add (f2cl-lib:int-sub p i) 1)
                        (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16
                                              (i i) ((1 ldx11) (1 *))
                                              x11-%offset%)
                        1)))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (zlarfgp (f2cl-lib:int-add (f2cl-lib:int-sub p i) 1)
                   (f2cl-lib:fref x11-%data% (i i) ((1 ldx11) (1 *))
                                  x11-%offset%)
                   (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16
                                         ((+ i 1) i) ((1 ldx11) (1 *))
                                         x11-%offset%)
                   1 (f2cl-lib:fref taup1-%data% (i) ((1 *)) taup1-%offset%))
                (declare (ignore var-0 var-2 var-3))
                (setf (f2cl-lib:fref x11-%data% (i i) ((1 ldx11) (1 *))
                                     x11-%offset%)
                        var-1)
                (setf (f2cl-lib:fref taup1-%data% (i) ((1 *)) taup1-%offset%)
                        var-4))
              (setf (f2cl-lib:fref x11-%data% (i i) ((1 ldx11) (1 *))
                                   x11-%offset%)
                      one)
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (zlarfgp (f2cl-lib:int-add (f2cl-lib:int-sub m p i) 1)
                   (f2cl-lib:fref x21-%data% (i i) ((1 ldx21) (1 *))
                                  x21-%offset%)
                   (f2cl-lib:array-slice x21-%data% f2cl-lib:complex16
                                         ((+ i 1) i) ((1 ldx21) (1 *))
                                         x21-%offset%)
                   1 (f2cl-lib:fref taup2-%data% (i) ((1 *)) taup2-%offset%))
                (declare (ignore var-0 var-2 var-3))
                (setf (f2cl-lib:fref x21-%data% (i i) ((1 ldx21) (1 *))
                                     x21-%offset%)
                        var-1)
                (setf (f2cl-lib:fref taup2-%data% (i) ((1 *)) taup2-%offset%)
                        var-4))
              (setf (f2cl-lib:fref x21-%data% (i i) ((1 ldx21) (1 *))
                                   x21-%offset%)
                      one)
              (zlarf "L" (f2cl-lib:int-add (f2cl-lib:int-sub p i) 1)
               (f2cl-lib:int-sub q i)
               (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx11) (1 *)) x11-%offset%)
               1
               (f2cl-lib:dconjg
                (f2cl-lib:fref taup1-%data% (i) ((1 *)) taup1-%offset%))
               (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16
                                     (i (f2cl-lib:int-add i 1))
                                     ((1 ldx11) (1 *)) x11-%offset%)
               ldx11 work)
              (zlarf "L" (f2cl-lib:int-add (f2cl-lib:int-sub p i) 1)
               (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
               (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx11) (1 *)) x11-%offset%)
               1
               (f2cl-lib:dconjg
                (f2cl-lib:fref taup1-%data% (i) ((1 *)) taup1-%offset%))
               (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx12) (1 *)) x12-%offset%)
               ldx12 work)
              (zlarf "L" (f2cl-lib:int-add (f2cl-lib:int-sub m p i) 1)
               (f2cl-lib:int-sub q i)
               (f2cl-lib:array-slice x21-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx21) (1 *)) x21-%offset%)
               1
               (f2cl-lib:dconjg
                (f2cl-lib:fref taup2-%data% (i) ((1 *)) taup2-%offset%))
               (f2cl-lib:array-slice x21-%data% f2cl-lib:complex16
                                     (i (f2cl-lib:int-add i 1))
                                     ((1 ldx21) (1 *)) x21-%offset%)
               ldx21 work)
              (zlarf "L" (f2cl-lib:int-add (f2cl-lib:int-sub m p i) 1)
               (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
               (f2cl-lib:array-slice x21-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx21) (1 *)) x21-%offset%)
               1
               (f2cl-lib:dconjg
                (f2cl-lib:fref taup2-%data% (i) ((1 *)) taup2-%offset%))
               (f2cl-lib:array-slice x22-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx22) (1 *)) x22-%offset%)
               ldx22 work)
              (cond
               ((< i q)
                (zscal (f2cl-lib:int-sub q i)
                 (f2cl-lib:dcmplx
                  (* (- z1) z3
                     (sin
                      (f2cl-lib:fref theta-%data% (i) ((1 *)) theta-%offset%)))
                  0.0d0)
                 (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16
                                       (i (f2cl-lib:int-add i 1))
                                       ((1 ldx11) (1 *)) x11-%offset%)
                 ldx11)
                (zaxpy (f2cl-lib:int-sub q i)
                 (f2cl-lib:dcmplx
                  (* z2 z3
                     (cos
                      (f2cl-lib:fref theta-%data% (i) ((1 *)) theta-%offset%)))
                  0.0d0)
                 (f2cl-lib:array-slice x21-%data% f2cl-lib:complex16
                                       (i (f2cl-lib:int-add i 1))
                                       ((1 ldx21) (1 *)) x21-%offset%)
                 ldx21
                 (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16
                                       (i (f2cl-lib:int-add i 1))
                                       ((1 ldx11) (1 *)) x11-%offset%)
                 ldx11)))
              (zscal (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
               (f2cl-lib:dcmplx
                (* (- z1) z4
                   (sin
                    (f2cl-lib:fref theta-%data% (i) ((1 *)) theta-%offset%)))
                0.0d0)
               (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx12) (1 *)) x12-%offset%)
               ldx12)
              (zaxpy (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
               (f2cl-lib:dcmplx
                (* z2 z4
                   (cos
                    (f2cl-lib:fref theta-%data% (i) ((1 *)) theta-%offset%)))
                0.0d0)
               (f2cl-lib:array-slice x22-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx22) (1 *)) x22-%offset%)
               ldx22
               (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx12) (1 *)) x12-%offset%)
               ldx12)
              (if (< i q)
                  (setf (f2cl-lib:fref phi-%data% (i) ((1 *)) phi-%offset%)
                          (f2cl-lib:atan2
                           (dznrm2 (f2cl-lib:int-sub q i)
                            (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16
                                                  (i (f2cl-lib:int-add i 1))
                                                  ((1 ldx11) (1 *))
                                                  x11-%offset%)
                            ldx11)
                           (dznrm2
                            (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
                            (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16
                                                  (i i) ((1 ldx12) (1 *))
                                                  x12-%offset%)
                            ldx12))))
              (cond
               ((< i q)
                (zlacgv (f2cl-lib:int-sub q i)
                 (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16
                                       (i (f2cl-lib:int-add i 1))
                                       ((1 ldx11) (1 *)) x11-%offset%)
                 ldx11)
                (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                    (zlarfgp (f2cl-lib:int-sub q i)
                     (f2cl-lib:fref x11-%data% (i (f2cl-lib:int-add i 1))
                                    ((1 ldx11) (1 *)) x11-%offset%)
                     (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16
                                           (i (f2cl-lib:int-add i 2))
                                           ((1 ldx11) (1 *)) x11-%offset%)
                     ldx11
                     (f2cl-lib:fref tauq1-%data% (i) ((1 *)) tauq1-%offset%))
                  (declare (ignore var-0 var-2 var-3))
                  (setf (f2cl-lib:fref x11-%data% (i (f2cl-lib:int-add i 1))
                                       ((1 ldx11) (1 *)) x11-%offset%)
                          var-1)
                  (setf (f2cl-lib:fref tauq1-%data% (i) ((1 *)) tauq1-%offset%)
                          var-4))
                (setf (f2cl-lib:fref x11-%data% (i (f2cl-lib:int-add i 1))
                                     ((1 ldx11) (1 *)) x11-%offset%)
                        one)))
              (zlacgv (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
               (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx12) (1 *)) x12-%offset%)
               ldx12)
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (zlarfgp (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
                   (f2cl-lib:fref x12-%data% (i i) ((1 ldx12) (1 *))
                                  x12-%offset%)
                   (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16
                                         (i (f2cl-lib:int-add i 1))
                                         ((1 ldx12) (1 *)) x12-%offset%)
                   ldx12
                   (f2cl-lib:fref tauq2-%data% (i) ((1 *)) tauq2-%offset%))
                (declare (ignore var-0 var-2 var-3))
                (setf (f2cl-lib:fref x12-%data% (i i) ((1 ldx12) (1 *))
                                     x12-%offset%)
                        var-1)
                (setf (f2cl-lib:fref tauq2-%data% (i) ((1 *)) tauq2-%offset%)
                        var-4))
              (setf (f2cl-lib:fref x12-%data% (i i) ((1 ldx12) (1 *))
                                   x12-%offset%)
                      one)
              (cond
               ((< i q)
                (zlarf "R" (f2cl-lib:int-sub p i) (f2cl-lib:int-sub q i)
                 (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16
                                       (i (f2cl-lib:int-add i 1))
                                       ((1 ldx11) (1 *)) x11-%offset%)
                 ldx11 (f2cl-lib:fref tauq1-%data% (i) ((1 *)) tauq1-%offset%)
                 (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16
                                       ((+ i 1) (f2cl-lib:int-add i 1))
                                       ((1 ldx11) (1 *)) x11-%offset%)
                 ldx11 work)
                (zlarf "R" (f2cl-lib:int-sub m p i) (f2cl-lib:int-sub q i)
                 (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16
                                       (i (f2cl-lib:int-add i 1))
                                       ((1 ldx11) (1 *)) x11-%offset%)
                 ldx11 (f2cl-lib:fref tauq1-%data% (i) ((1 *)) tauq1-%offset%)
                 (f2cl-lib:array-slice x21-%data% f2cl-lib:complex16
                                       ((+ i 1) (f2cl-lib:int-add i 1))
                                       ((1 ldx21) (1 *)) x21-%offset%)
                 ldx21 work)))
              (zlarf "R" (f2cl-lib:int-sub p i)
               (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
               (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx12) (1 *)) x12-%offset%)
               ldx12 (f2cl-lib:fref tauq2-%data% (i) ((1 *)) tauq2-%offset%)
               (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16 ((+ i 1) i)
                                     ((1 ldx12) (1 *)) x12-%offset%)
               ldx12 work)
              (zlarf "R" (f2cl-lib:int-sub m p i)
               (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
               (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx12) (1 *)) x12-%offset%)
               ldx12 (f2cl-lib:fref tauq2-%data% (i) ((1 *)) tauq2-%offset%)
               (f2cl-lib:array-slice x22-%data% f2cl-lib:complex16 ((+ i 1) i)
                                     ((1 ldx22) (1 *)) x22-%offset%)
               ldx22 work)
              (if (< i q)
                  (zlacgv (f2cl-lib:int-sub q i)
                   (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16
                                         (i (f2cl-lib:int-add i 1))
                                         ((1 ldx11) (1 *)) x11-%offset%)
                   ldx11))
              (zlacgv (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
               (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx12) (1 *)) x12-%offset%)
               ldx12)
             label100000))
          (f2cl-lib:fdo (i (f2cl-lib:int-add q 1) (f2cl-lib:int-add i 1))
                        ((> i p) nil)
            (tagbody
              (zscal (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
               (f2cl-lib:dcmplx (* (- z1) z4) 0.0d0)
               (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx12) (1 *)) x12-%offset%)
               ldx12)
              (zlacgv (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
               (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx12) (1 *)) x12-%offset%)
               ldx12)
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (zlarfgp (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
                   (f2cl-lib:fref x12-%data% (i i) ((1 ldx12) (1 *))
                                  x12-%offset%)
                   (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16
                                         (i (f2cl-lib:int-add i 1))
                                         ((1 ldx12) (1 *)) x12-%offset%)
                   ldx12
                   (f2cl-lib:fref tauq2-%data% (i) ((1 *)) tauq2-%offset%))
                (declare (ignore var-0 var-2 var-3))
                (setf (f2cl-lib:fref x12-%data% (i i) ((1 ldx12) (1 *))
                                     x12-%offset%)
                        var-1)
                (setf (f2cl-lib:fref tauq2-%data% (i) ((1 *)) tauq2-%offset%)
                        var-4))
              (setf (f2cl-lib:fref x12-%data% (i i) ((1 ldx12) (1 *))
                                   x12-%offset%)
                      one)
              (zlarf "R" (f2cl-lib:int-sub p i)
               (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
               (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx12) (1 *)) x12-%offset%)
               ldx12 (f2cl-lib:fref tauq2-%data% (i) ((1 *)) tauq2-%offset%)
               (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16 ((+ i 1) i)
                                     ((1 ldx12) (1 *)) x12-%offset%)
               ldx12 work)
              (if (>= (f2cl-lib:int-sub m p q) 1)
                  (zlarf "R" (f2cl-lib:int-sub m p q)
                   (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
                   (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16 (i i)
                                         ((1 ldx12) (1 *)) x12-%offset%)
                   ldx12
                   (f2cl-lib:fref tauq2-%data% (i) ((1 *)) tauq2-%offset%)
                   (f2cl-lib:array-slice x22-%data% f2cl-lib:complex16
                                         ((+ q 1) i) ((1 ldx22) (1 *))
                                         x22-%offset%)
                   ldx22 work))
              (zlacgv (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
               (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx12) (1 *)) x12-%offset%)
               ldx12)
             label100001))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i
                            (f2cl-lib:int-add m (f2cl-lib:int-sub p)
                                              (f2cl-lib:int-sub q)))
                         nil)
            (tagbody
              (zscal (f2cl-lib:int-add (f2cl-lib:int-sub m p q i) 1)
               (f2cl-lib:dcmplx (* z2 z4) 0.0d0)
               (f2cl-lib:array-slice x22-%data% f2cl-lib:complex16
                                     ((+ q i) (f2cl-lib:int-add p i))
                                     ((1 ldx22) (1 *)) x22-%offset%)
               ldx22)
              (zlacgv (f2cl-lib:int-add (f2cl-lib:int-sub m p q i) 1)
               (f2cl-lib:array-slice x22-%data% f2cl-lib:complex16
                                     ((+ q i) (f2cl-lib:int-add p i))
                                     ((1 ldx22) (1 *)) x22-%offset%)
               ldx22)
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (zlarfgp (f2cl-lib:int-add (f2cl-lib:int-sub m p q i) 1)
                   (f2cl-lib:fref x22-%data%
                                  ((f2cl-lib:int-add q i)
                                   (f2cl-lib:int-add p i))
                                  ((1 ldx22) (1 *)) x22-%offset%)
                   (f2cl-lib:array-slice x22-%data% f2cl-lib:complex16
                                         ((+ q i) (f2cl-lib:int-add p i 1))
                                         ((1 ldx22) (1 *)) x22-%offset%)
                   ldx22
                   (f2cl-lib:fref tauq2-%data% ((f2cl-lib:int-add p i)) ((1 *))
                                  tauq2-%offset%))
                (declare (ignore var-0 var-2 var-3))
                (setf (f2cl-lib:fref x22-%data%
                                     ((f2cl-lib:int-add q i)
                                      (f2cl-lib:int-add p i))
                                     ((1 ldx22) (1 *)) x22-%offset%)
                        var-1)
                (setf (f2cl-lib:fref tauq2-%data% ((f2cl-lib:int-add p i))
                                     ((1 *)) tauq2-%offset%)
                        var-4))
              (setf (f2cl-lib:fref x22-%data%
                                   ((f2cl-lib:int-add q i)
                                    (f2cl-lib:int-add p i))
                                   ((1 ldx22) (1 *)) x22-%offset%)
                      one)
              (zlarf "R" (f2cl-lib:int-sub m p q i)
               (f2cl-lib:int-add (f2cl-lib:int-sub m p q i) 1)
               (f2cl-lib:array-slice x22-%data% f2cl-lib:complex16
                                     ((+ q i) (f2cl-lib:int-add p i))
                                     ((1 ldx22) (1 *)) x22-%offset%)
               ldx22
               (f2cl-lib:fref tauq2-%data% ((f2cl-lib:int-add p i)) ((1 *))
                              tauq2-%offset%)
               (f2cl-lib:array-slice x22-%data% f2cl-lib:complex16
                                     ((+ q i 1) (f2cl-lib:int-add p i))
                                     ((1 ldx22) (1 *)) x22-%offset%)
               ldx22 work)
              (zlacgv (f2cl-lib:int-add (f2cl-lib:int-sub m p q i) 1)
               (f2cl-lib:array-slice x22-%data% f2cl-lib:complex16
                                     ((+ q i) (f2cl-lib:int-add p i))
                                     ((1 ldx22) (1 *)) x22-%offset%)
               ldx22)
             label100002)))
         (t
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i q) nil)
            (tagbody
              (cond
               ((= i 1)
                (zscal (f2cl-lib:int-add (f2cl-lib:int-sub p i) 1)
                 (f2cl-lib:dcmplx z1 0.0d0)
                 (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16 (i i)
                                       ((1 ldx11) (1 *)) x11-%offset%)
                 ldx11))
               (t
                (zscal (f2cl-lib:int-add (f2cl-lib:int-sub p i) 1)
                 (f2cl-lib:dcmplx
                  (* z1
                     (cos
                      (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub i 1))
                                     ((1 *)) phi-%offset%)))
                  0.0d0)
                 (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16 (i i)
                                       ((1 ldx11) (1 *)) x11-%offset%)
                 ldx11)
                (zaxpy (f2cl-lib:int-add (f2cl-lib:int-sub p i) 1)
                 (f2cl-lib:dcmplx
                  (* (- z1) z3 z4
                     (sin
                      (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub i 1))
                                     ((1 *)) phi-%offset%)))
                  0.0d0)
                 (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16
                                       ((+ i (f2cl-lib:int-sub 1)) i)
                                       ((1 ldx12) (1 *)) x12-%offset%)
                 ldx12
                 (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16 (i i)
                                       ((1 ldx11) (1 *)) x11-%offset%)
                 ldx11)))
              (cond
               ((= i 1)
                (zscal (f2cl-lib:int-add (f2cl-lib:int-sub m p i) 1)
                 (f2cl-lib:dcmplx z2 0.0d0)
                 (f2cl-lib:array-slice x21-%data% f2cl-lib:complex16 (i i)
                                       ((1 ldx21) (1 *)) x21-%offset%)
                 ldx21))
               (t
                (zscal (f2cl-lib:int-add (f2cl-lib:int-sub m p i) 1)
                 (f2cl-lib:dcmplx
                  (* z2
                     (cos
                      (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub i 1))
                                     ((1 *)) phi-%offset%)))
                  0.0d0)
                 (f2cl-lib:array-slice x21-%data% f2cl-lib:complex16 (i i)
                                       ((1 ldx21) (1 *)) x21-%offset%)
                 ldx21)
                (zaxpy (f2cl-lib:int-add (f2cl-lib:int-sub m p i) 1)
                 (f2cl-lib:dcmplx
                  (* (- z2) z3 z4
                     (sin
                      (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub i 1))
                                     ((1 *)) phi-%offset%)))
                  0.0d0)
                 (f2cl-lib:array-slice x22-%data% f2cl-lib:complex16
                                       ((+ i (f2cl-lib:int-sub 1)) i)
                                       ((1 ldx22) (1 *)) x22-%offset%)
                 ldx22
                 (f2cl-lib:array-slice x21-%data% f2cl-lib:complex16 (i i)
                                       ((1 ldx21) (1 *)) x21-%offset%)
                 ldx21)))
              (setf (f2cl-lib:fref theta-%data% (i) ((1 *)) theta-%offset%)
                      (f2cl-lib:atan2
                       (dznrm2 (f2cl-lib:int-add (f2cl-lib:int-sub m p i) 1)
                        (f2cl-lib:array-slice x21-%data% f2cl-lib:complex16
                                              (i i) ((1 ldx21) (1 *))
                                              x21-%offset%)
                        ldx21)
                       (dznrm2 (f2cl-lib:int-add (f2cl-lib:int-sub p i) 1)
                        (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16
                                              (i i) ((1 ldx11) (1 *))
                                              x11-%offset%)
                        ldx11)))
              (zlacgv (f2cl-lib:int-add (f2cl-lib:int-sub p i) 1)
               (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx11) (1 *)) x11-%offset%)
               ldx11)
              (zlacgv (f2cl-lib:int-add (f2cl-lib:int-sub m p i) 1)
               (f2cl-lib:array-slice x21-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx21) (1 *)) x21-%offset%)
               ldx21)
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (zlarfgp (f2cl-lib:int-add (f2cl-lib:int-sub p i) 1)
                   (f2cl-lib:fref x11-%data% (i i) ((1 ldx11) (1 *))
                                  x11-%offset%)
                   (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16
                                         (i (f2cl-lib:int-add i 1))
                                         ((1 ldx11) (1 *)) x11-%offset%)
                   ldx11
                   (f2cl-lib:fref taup1-%data% (i) ((1 *)) taup1-%offset%))
                (declare (ignore var-0 var-2 var-3))
                (setf (f2cl-lib:fref x11-%data% (i i) ((1 ldx11) (1 *))
                                     x11-%offset%)
                        var-1)
                (setf (f2cl-lib:fref taup1-%data% (i) ((1 *)) taup1-%offset%)
                        var-4))
              (setf (f2cl-lib:fref x11-%data% (i i) ((1 ldx11) (1 *))
                                   x11-%offset%)
                      one)
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (zlarfgp (f2cl-lib:int-add (f2cl-lib:int-sub m p i) 1)
                   (f2cl-lib:fref x21-%data% (i i) ((1 ldx21) (1 *))
                                  x21-%offset%)
                   (f2cl-lib:array-slice x21-%data% f2cl-lib:complex16
                                         (i (f2cl-lib:int-add i 1))
                                         ((1 ldx21) (1 *)) x21-%offset%)
                   ldx21
                   (f2cl-lib:fref taup2-%data% (i) ((1 *)) taup2-%offset%))
                (declare (ignore var-0 var-2 var-3))
                (setf (f2cl-lib:fref x21-%data% (i i) ((1 ldx21) (1 *))
                                     x21-%offset%)
                        var-1)
                (setf (f2cl-lib:fref taup2-%data% (i) ((1 *)) taup2-%offset%)
                        var-4))
              (setf (f2cl-lib:fref x21-%data% (i i) ((1 ldx21) (1 *))
                                   x21-%offset%)
                      one)
              (zlarf "R" (f2cl-lib:int-sub q i)
               (f2cl-lib:int-add (f2cl-lib:int-sub p i) 1)
               (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx11) (1 *)) x11-%offset%)
               ldx11 (f2cl-lib:fref taup1-%data% (i) ((1 *)) taup1-%offset%)
               (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16 ((+ i 1) i)
                                     ((1 ldx11) (1 *)) x11-%offset%)
               ldx11 work)
              (zlarf "R" (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
               (f2cl-lib:int-add (f2cl-lib:int-sub p i) 1)
               (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx11) (1 *)) x11-%offset%)
               ldx11 (f2cl-lib:fref taup1-%data% (i) ((1 *)) taup1-%offset%)
               (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx12) (1 *)) x12-%offset%)
               ldx12 work)
              (zlarf "R" (f2cl-lib:int-sub q i)
               (f2cl-lib:int-add (f2cl-lib:int-sub m p i) 1)
               (f2cl-lib:array-slice x21-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx21) (1 *)) x21-%offset%)
               ldx21 (f2cl-lib:fref taup2-%data% (i) ((1 *)) taup2-%offset%)
               (f2cl-lib:array-slice x21-%data% f2cl-lib:complex16 ((+ i 1) i)
                                     ((1 ldx21) (1 *)) x21-%offset%)
               ldx21 work)
              (zlarf "R" (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
               (f2cl-lib:int-add (f2cl-lib:int-sub m p i) 1)
               (f2cl-lib:array-slice x21-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx21) (1 *)) x21-%offset%)
               ldx21 (f2cl-lib:fref taup2-%data% (i) ((1 *)) taup2-%offset%)
               (f2cl-lib:array-slice x22-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx22) (1 *)) x22-%offset%)
               ldx22 work)
              (zlacgv (f2cl-lib:int-add (f2cl-lib:int-sub p i) 1)
               (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx11) (1 *)) x11-%offset%)
               ldx11)
              (zlacgv (f2cl-lib:int-add (f2cl-lib:int-sub m p i) 1)
               (f2cl-lib:array-slice x21-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx21) (1 *)) x21-%offset%)
               ldx21)
              (cond
               ((< i q)
                (zscal (f2cl-lib:int-sub q i)
                 (f2cl-lib:dcmplx
                  (* (- z1) z3
                     (sin
                      (f2cl-lib:fref theta-%data% (i) ((1 *)) theta-%offset%)))
                  0.0d0)
                 (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16
                                       ((+ i 1) i) ((1 ldx11) (1 *))
                                       x11-%offset%)
                 1)
                (zaxpy (f2cl-lib:int-sub q i)
                 (f2cl-lib:dcmplx
                  (* z2 z3
                     (cos
                      (f2cl-lib:fref theta-%data% (i) ((1 *)) theta-%offset%)))
                  0.0d0)
                 (f2cl-lib:array-slice x21-%data% f2cl-lib:complex16
                                       ((+ i 1) i) ((1 ldx21) (1 *))
                                       x21-%offset%)
                 1
                 (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16
                                       ((+ i 1) i) ((1 ldx11) (1 *))
                                       x11-%offset%)
                 1)))
              (zscal (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
               (f2cl-lib:dcmplx
                (* (- z1) z4
                   (sin
                    (f2cl-lib:fref theta-%data% (i) ((1 *)) theta-%offset%)))
                0.0d0)
               (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx12) (1 *)) x12-%offset%)
               1)
              (zaxpy (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
               (f2cl-lib:dcmplx
                (* z2 z4
                   (cos
                    (f2cl-lib:fref theta-%data% (i) ((1 *)) theta-%offset%)))
                0.0d0)
               (f2cl-lib:array-slice x22-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx22) (1 *)) x22-%offset%)
               1
               (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx12) (1 *)) x12-%offset%)
               1)
              (if (< i q)
                  (setf (f2cl-lib:fref phi-%data% (i) ((1 *)) phi-%offset%)
                          (f2cl-lib:atan2
                           (dznrm2 (f2cl-lib:int-sub q i)
                            (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16
                                                  ((+ i 1) i) ((1 ldx11) (1 *))
                                                  x11-%offset%)
                            1)
                           (dznrm2
                            (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
                            (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16
                                                  (i i) ((1 ldx12) (1 *))
                                                  x12-%offset%)
                            1))))
              (cond
               ((< i q)
                (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                    (zlarfgp (f2cl-lib:int-sub q i)
                     (f2cl-lib:fref x11-%data% ((f2cl-lib:int-add i 1) i)
                                    ((1 ldx11) (1 *)) x11-%offset%)
                     (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16
                                           ((+ i 2) i) ((1 ldx11) (1 *))
                                           x11-%offset%)
                     1 (f2cl-lib:fref tauq1-%data% (i) ((1 *)) tauq1-%offset%))
                  (declare (ignore var-0 var-2 var-3))
                  (setf (f2cl-lib:fref x11-%data% ((f2cl-lib:int-add i 1) i)
                                       ((1 ldx11) (1 *)) x11-%offset%)
                          var-1)
                  (setf (f2cl-lib:fref tauq1-%data% (i) ((1 *)) tauq1-%offset%)
                          var-4))
                (setf (f2cl-lib:fref x11-%data% ((f2cl-lib:int-add i 1) i)
                                     ((1 ldx11) (1 *)) x11-%offset%)
                        one)))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (zlarfgp (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
                   (f2cl-lib:fref x12-%data% (i i) ((1 ldx12) (1 *))
                                  x12-%offset%)
                   (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16
                                         ((+ i 1) i) ((1 ldx12) (1 *))
                                         x12-%offset%)
                   1 (f2cl-lib:fref tauq2-%data% (i) ((1 *)) tauq2-%offset%))
                (declare (ignore var-0 var-2 var-3))
                (setf (f2cl-lib:fref x12-%data% (i i) ((1 ldx12) (1 *))
                                     x12-%offset%)
                        var-1)
                (setf (f2cl-lib:fref tauq2-%data% (i) ((1 *)) tauq2-%offset%)
                        var-4))
              (setf (f2cl-lib:fref x12-%data% (i i) ((1 ldx12) (1 *))
                                   x12-%offset%)
                      one)
              (cond
               ((< i q)
                (zlarf "L" (f2cl-lib:int-sub q i) (f2cl-lib:int-sub p i)
                 (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16
                                       ((+ i 1) i) ((1 ldx11) (1 *))
                                       x11-%offset%)
                 1
                 (f2cl-lib:dconjg
                  (f2cl-lib:fref tauq1-%data% (i) ((1 *)) tauq1-%offset%))
                 (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16
                                       ((+ i 1) (f2cl-lib:int-add i 1))
                                       ((1 ldx11) (1 *)) x11-%offset%)
                 ldx11 work)
                (zlarf "L" (f2cl-lib:int-sub q i) (f2cl-lib:int-sub m p i)
                 (f2cl-lib:array-slice x11-%data% f2cl-lib:complex16
                                       ((+ i 1) i) ((1 ldx11) (1 *))
                                       x11-%offset%)
                 1
                 (f2cl-lib:dconjg
                  (f2cl-lib:fref tauq1-%data% (i) ((1 *)) tauq1-%offset%))
                 (f2cl-lib:array-slice x21-%data% f2cl-lib:complex16
                                       ((+ i 1) (f2cl-lib:int-add i 1))
                                       ((1 ldx21) (1 *)) x21-%offset%)
                 ldx21 work)))
              (zlarf "L" (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
               (f2cl-lib:int-sub p i)
               (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx12) (1 *)) x12-%offset%)
               1
               (f2cl-lib:dconjg
                (f2cl-lib:fref tauq2-%data% (i) ((1 *)) tauq2-%offset%))
               (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16
                                     (i (f2cl-lib:int-add i 1))
                                     ((1 ldx12) (1 *)) x12-%offset%)
               ldx12 work)
              (zlarf "L" (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
               (f2cl-lib:int-sub m p i)
               (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx12) (1 *)) x12-%offset%)
               1
               (f2cl-lib:dconjg
                (f2cl-lib:fref tauq2-%data% (i) ((1 *)) tauq2-%offset%))
               (f2cl-lib:array-slice x22-%data% f2cl-lib:complex16
                                     (i (f2cl-lib:int-add i 1))
                                     ((1 ldx22) (1 *)) x22-%offset%)
               ldx22 work)
             label100003))
          (f2cl-lib:fdo (i (f2cl-lib:int-add q 1) (f2cl-lib:int-add i 1))
                        ((> i p) nil)
            (tagbody
              (zscal (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
               (f2cl-lib:dcmplx (* (- z1) z4) 0.0d0)
               (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx12) (1 *)) x12-%offset%)
               1)
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (zlarfgp (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
                   (f2cl-lib:fref x12-%data% (i i) ((1 ldx12) (1 *))
                                  x12-%offset%)
                   (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16
                                         ((+ i 1) i) ((1 ldx12) (1 *))
                                         x12-%offset%)
                   1 (f2cl-lib:fref tauq2-%data% (i) ((1 *)) tauq2-%offset%))
                (declare (ignore var-0 var-2 var-3))
                (setf (f2cl-lib:fref x12-%data% (i i) ((1 ldx12) (1 *))
                                     x12-%offset%)
                        var-1)
                (setf (f2cl-lib:fref tauq2-%data% (i) ((1 *)) tauq2-%offset%)
                        var-4))
              (setf (f2cl-lib:fref x12-%data% (i i) ((1 ldx12) (1 *))
                                   x12-%offset%)
                      one)
              (zlarf "L" (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
               (f2cl-lib:int-sub p i)
               (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16 (i i)
                                     ((1 ldx12) (1 *)) x12-%offset%)
               1
               (f2cl-lib:dconjg
                (f2cl-lib:fref tauq2-%data% (i) ((1 *)) tauq2-%offset%))
               (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16
                                     (i (f2cl-lib:int-add i 1))
                                     ((1 ldx12) (1 *)) x12-%offset%)
               ldx12 work)
              (if (>= (f2cl-lib:int-sub m p q) 1)
                  (zlarf "L" (f2cl-lib:int-add (f2cl-lib:int-sub m q i) 1)
                   (f2cl-lib:int-sub m p q)
                   (f2cl-lib:array-slice x12-%data% f2cl-lib:complex16 (i i)
                                         ((1 ldx12) (1 *)) x12-%offset%)
                   1
                   (f2cl-lib:dconjg
                    (f2cl-lib:fref tauq2-%data% (i) ((1 *)) tauq2-%offset%))
                   (f2cl-lib:array-slice x22-%data% f2cl-lib:complex16
                                         (i (f2cl-lib:int-add q 1))
                                         ((1 ldx22) (1 *)) x22-%offset%)
                   ldx22 work))
             label100004))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i
                            (f2cl-lib:int-add m (f2cl-lib:int-sub p)
                                              (f2cl-lib:int-sub q)))
                         nil)
            (tagbody
              (zscal (f2cl-lib:int-add (f2cl-lib:int-sub m p q i) 1)
               (f2cl-lib:dcmplx (* z2 z4) 0.0d0)
               (f2cl-lib:array-slice x22-%data% f2cl-lib:complex16
                                     ((+ p i) (f2cl-lib:int-add q i))
                                     ((1 ldx22) (1 *)) x22-%offset%)
               1)
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (zlarfgp (f2cl-lib:int-add (f2cl-lib:int-sub m p q i) 1)
                   (f2cl-lib:fref x22-%data%
                                  ((f2cl-lib:int-add p i)
                                   (f2cl-lib:int-add q i))
                                  ((1 ldx22) (1 *)) x22-%offset%)
                   (f2cl-lib:array-slice x22-%data% f2cl-lib:complex16
                                         ((+ p i 1) (f2cl-lib:int-add q i))
                                         ((1 ldx22) (1 *)) x22-%offset%)
                   1
                   (f2cl-lib:fref tauq2-%data% ((f2cl-lib:int-add p i)) ((1 *))
                                  tauq2-%offset%))
                (declare (ignore var-0 var-2 var-3))
                (setf (f2cl-lib:fref x22-%data%
                                     ((f2cl-lib:int-add p i)
                                      (f2cl-lib:int-add q i))
                                     ((1 ldx22) (1 *)) x22-%offset%)
                        var-1)
                (setf (f2cl-lib:fref tauq2-%data% ((f2cl-lib:int-add p i))
                                     ((1 *)) tauq2-%offset%)
                        var-4))
              (setf (f2cl-lib:fref x22-%data%
                                   ((f2cl-lib:int-add p i)
                                    (f2cl-lib:int-add q i))
                                   ((1 ldx22) (1 *)) x22-%offset%)
                      one)
              (zlarf "L" (f2cl-lib:int-add (f2cl-lib:int-sub m p q i) 1)
               (f2cl-lib:int-sub m p q i)
               (f2cl-lib:array-slice x22-%data% f2cl-lib:complex16
                                     ((+ p i) (f2cl-lib:int-add q i))
                                     ((1 ldx22) (1 *)) x22-%offset%)
               1
               (f2cl-lib:dconjg
                (f2cl-lib:fref tauq2-%data% ((f2cl-lib:int-add p i)) ((1 *))
                               tauq2-%offset%))
               (f2cl-lib:array-slice x22-%data% f2cl-lib:complex16
                                     ((+ p i) (f2cl-lib:int-add q i 1))
                                     ((1 ldx22) (1 *)) x22-%offset%)
               ldx22 work)
             label100005))))
        (go end_label)
       end_label
        (return
         (values nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
                 nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zunbdb
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((string 1) (string 1)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (array double-float (*))
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil nil nil nil
                                              nil nil nil nil nil nil nil nil
                                              nil nil nil nil nil
                                              fortran-to-lisp::info)
                                            :calls
                                            '(fortran-to-lisp::dznrm2
                                              fortran-to-lisp::zaxpy
                                              fortran-to-lisp::zscal
                                              fortran-to-lisp::xerbla
                                              fortran-to-lisp::lsame
                                              fortran-to-lisp::zlacgv
                                              fortran-to-lisp::zlarf
                                              fortran-to-lisp::zlarfgp))))

