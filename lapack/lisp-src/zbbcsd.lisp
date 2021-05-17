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


(let* ((maxitr 6)
       (hundred 100.0d0)
       (meighth (- 0.125d0))
       (one 1.0d0)
       (piover2 1.5707963267948966d0)
       (ten 10.0d0)
       (zero 0.0d0)
       (negonecomplex (f2cl-lib:cmplx (- 1.0d0) 0.0d0)))
  (declare (type (f2cl-lib:integer4 6 6) maxitr)
           (type (double-float 100.0d0 100.0d0) hundred)
           (type (double-float) meighth)
           (type (double-float 1.0d0 1.0d0) one)
           (type (double-float 1.5707963267948966d0 1.5707963267948966d0)
            piover2)
           (type (double-float 10.0d0 10.0d0) ten)
           (type (double-float 0.0d0 0.0d0) zero)
           (type (f2cl-lib:complex16) negonecomplex)
           (ignorable maxitr hundred meighth one piover2 ten zero
            negonecomplex))
  (defun zbbcsd
         (jobu1 jobu2 jobv1t jobv2t trans m p q theta phi u1 ldu1 u2 ldu2 v1t
          ldv1t v2t ldv2t b11d b11e b12d b12e b21d b21e b22d b22e rwork lrwork
          info)
    (declare (type (array f2cl-lib:complex16 (*)) v2t v1t u2 u1)
             (type (array double-float (*)) rwork b22e b22d b21e b21d b12e b12d
              b11e b11d phi theta)
             (type (f2cl-lib:integer4) info lrwork ldv2t ldv1t ldu2 ldu1 q p m)
             (type (string 1) trans jobv2t jobv1t jobu2 jobu1))
    (f2cl-lib:with-multi-array-data
        ((jobu1 character jobu1-%data% jobu1-%offset%)
         (jobu2 character jobu2-%data% jobu2-%offset%)
         (jobv1t character jobv1t-%data% jobv1t-%offset%)
         (jobv2t character jobv2t-%data% jobv2t-%offset%)
         (trans character trans-%data% trans-%offset%)
         (theta double-float theta-%data% theta-%offset%)
         (phi double-float phi-%data% phi-%offset%)
         (b11d double-float b11d-%data% b11d-%offset%)
         (b11e double-float b11e-%data% b11e-%offset%)
         (b12d double-float b12d-%data% b12d-%offset%)
         (b12e double-float b12e-%data% b12e-%offset%)
         (b21d double-float b21d-%data% b21d-%offset%)
         (b21e double-float b21e-%data% b21e-%offset%)
         (b22d double-float b22d-%data% b22d-%offset%)
         (b22e double-float b22e-%data% b22e-%offset%)
         (rwork double-float rwork-%data% rwork-%offset%)
         (u1 f2cl-lib:complex16 u1-%data% u1-%offset%)
         (u2 f2cl-lib:complex16 u2-%data% u2-%offset%)
         (v1t f2cl-lib:complex16 v1t-%data% v1t-%offset%)
         (v2t f2cl-lib:complex16 v2t-%data% v2t-%offset%))
      (prog ((b11bulge 0.0d0) (b12bulge 0.0d0) (b21bulge 0.0d0)
             (b22bulge 0.0d0) (dummy 0.0d0) (eps 0.0d0) (mu 0.0d0) (nu 0.0d0)
             (r 0.0d0) (sigma11 0.0d0) (sigma21 0.0d0) (temp 0.0d0)
             (thetamax 0.0d0) (thetamin 0.0d0) (thresh 0.0d0) (tol 0.0d0)
             (tolmul 0.0d0) (unfl 0.0d0) (x1 0.0d0) (x2 0.0d0) (y1 0.0d0)
             (y2 0.0d0) (i 0) (imin 0) (imax 0) (iter 0) (iu1cs 0) (iu1sn 0)
             (iu2cs 0) (iu2sn 0) (iv1tcs 0) (iv1tsn 0) (iv2tcs 0) (iv2tsn 0)
             (j 0) (lrworkmin 0) (lrworkopt 0) (maxit 0) (mini 0)
             (colmajor nil) (lquery nil) (restart11 nil) (restart12 nil)
             (restart21 nil) (restart22 nil) (wantu1 nil) (wantu2 nil)
             (wantv1t nil) (wantv2t nil))
        (declare
         (type (double-float) b11bulge b12bulge b21bulge b22bulge dummy eps mu
          nu r sigma11 sigma21 temp thetamax thetamin thresh tol tolmul unfl x1
          x2 y1 y2)
         (type (f2cl-lib:integer4) i imin imax iter iu1cs iu1sn iu2cs iu2sn
          iv1tcs iv1tsn iv2tcs iv2tsn j lrworkmin lrworkopt maxit mini)
         (type f2cl-lib:logical colmajor lquery restart11 restart12 restart21
          restart22 wantu1 wantu2 wantv1t wantv2t))
        (setf info 0)
        (setf lquery (coerce (= lrwork -1) 'f2cl-lib:logical))
        (setf wantu1 (lsame jobu1 "Y"))
        (setf wantu2 (lsame jobu2 "Y"))
        (setf wantv1t (lsame jobv1t "Y"))
        (setf wantv2t (lsame jobv2t "Y"))
        (setf colmajor (not (lsame trans "T")))
        (cond ((< m 0) (setf info -6)) ((or (< p 0) (> p m)) (setf info -7))
              ((or (< q 0) (> q m)) (setf info -8))
              ((or (> q p) (> q (f2cl-lib:int-add m (f2cl-lib:int-sub p)))
                   (> q (f2cl-lib:int-add m (f2cl-lib:int-sub q))))
               (setf info -8))
              ((and wantu1 (< ldu1 p)) (setf info -12))
              ((and wantu2 (< ldu2 (f2cl-lib:int-add m (f2cl-lib:int-sub p))))
               (setf info -14))
              ((and wantv1t (< ldv1t q)) (setf info -16))
              ((and wantv2t
                    (< ldv2t (f2cl-lib:int-add m (f2cl-lib:int-sub q))))
               (setf info -18)))
        (cond
         ((and (= info 0) (= q 0)) (setf lrworkmin 1)
          (setf (f2cl-lib:fref rwork-%data% (1) ((1 *)) rwork-%offset%)
                  (coerce (the f2cl-lib:integer4 lrworkmin) 'double-float))
          (go end_label)))
        (cond
         ((= info 0) (setf iu1cs 1) (setf iu1sn (f2cl-lib:int-add iu1cs q))
          (setf iu2cs (f2cl-lib:int-add iu1sn q))
          (setf iu2sn (f2cl-lib:int-add iu2cs q))
          (setf iv1tcs (f2cl-lib:int-add iu2sn q))
          (setf iv1tsn (f2cl-lib:int-add iv1tcs q))
          (setf iv2tcs (f2cl-lib:int-add iv1tsn q))
          (setf iv2tsn (f2cl-lib:int-add iv2tcs q))
          (setf lrworkopt (f2cl-lib:int-sub (f2cl-lib:int-add iv2tsn q) 1))
          (setf lrworkmin lrworkopt)
          (setf (f2cl-lib:fref rwork-%data% (1) ((1 *)) rwork-%offset%)
                  (coerce (the f2cl-lib:integer4 lrworkopt) 'double-float))
          (cond ((and (< lrwork lrworkmin) (not lquery)) (setf info -28)))))
        (cond
         ((/= info 0) (xerbla "ZBBCSD" (f2cl-lib:int-sub info)) (go end_label))
         (lquery (go end_label)))
        (setf eps (dlamch "E"))
        (setf unfl (dlamch "S"))
        (setf tolmul (max ten (min hundred (expt eps meighth))))
        (setf tol (* tolmul eps))
        (setf thresh (max tol (* maxitr q q unfl)))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i q) nil)
          (tagbody
            (cond
             ((< (f2cl-lib:fref theta (i) ((1 *))) thresh)
              (setf (f2cl-lib:fref theta-%data% (i) ((1 *)) theta-%offset%)
                      zero))
             ((> (f2cl-lib:fref theta (i) ((1 *))) (+ piover2 (- thresh)))
              (setf (f2cl-lib:fref theta-%data% (i) ((1 *)) theta-%offset%)
                      piover2)))
           label100000))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i (f2cl-lib:int-add q (f2cl-lib:int-sub 1))) nil)
          (tagbody
            (cond
             ((< (f2cl-lib:fref phi (i) ((1 *))) thresh)
              (setf (f2cl-lib:fref phi-%data% (i) ((1 *)) phi-%offset%) zero))
             ((> (f2cl-lib:fref phi (i) ((1 *))) (+ piover2 (- thresh)))
              (setf (f2cl-lib:fref phi-%data% (i) ((1 *)) phi-%offset%)
                      piover2)))
           label100001))
        (setf imax q)
       label100002
        (if (not
             (and (> imax 1)
                  (=
                   (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub imax 1))
                                  ((1 *)) phi-%offset%)
                   zero)))
            (go label100003))
        (setf imax (f2cl-lib:int-sub imax 1))
        (go label100002)
       label100003
        (setf imin (f2cl-lib:int-sub imax 1))
       label100004
        (if (not
             (and (> imin 1)
                  (/=
                   (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub imin 1))
                                  ((1 *)) phi-%offset%)
                   zero)))
            (go label100005))
        (setf imin (f2cl-lib:int-sub imin 1))
        (go label100004)
       label100005
        (setf maxit (f2cl-lib:int-mul maxitr q q))
        (setf iter 0)
       label100006
        (if (not (> imax 1))
            (go label100007))
        (setf (f2cl-lib:fref b11d-%data% (imin) ((1 *)) b11d-%offset%)
                (cos
                 (f2cl-lib:fref theta-%data% (imin) ((1 *)) theta-%offset%)))
        (setf (f2cl-lib:fref b21d-%data% (imin) ((1 *)) b21d-%offset%)
                (-
                 (sin
                  (f2cl-lib:fref theta-%data% (imin) ((1 *)) theta-%offset%))))
        (f2cl-lib:fdo (i imin (f2cl-lib:int-add i 1))
                      ((> i (f2cl-lib:int-add imax (f2cl-lib:int-sub 1))) nil)
          (tagbody
            (setf (f2cl-lib:fref b11e-%data% (i) ((1 *)) b11e-%offset%)
                    (*
                     (-
                      (sin
                       (f2cl-lib:fref theta-%data% (i) ((1 *))
                                      theta-%offset%)))
                     (sin
                      (f2cl-lib:fref phi-%data% (i) ((1 *)) phi-%offset%))))
            (setf (f2cl-lib:fref b11d-%data% ((f2cl-lib:int-add i 1)) ((1 *))
                                 b11d-%offset%)
                    (*
                     (cos
                      (f2cl-lib:fref theta-%data% ((f2cl-lib:int-add i 1))
                                     ((1 *)) theta-%offset%))
                     (cos
                      (f2cl-lib:fref phi-%data% (i) ((1 *)) phi-%offset%))))
            (setf (f2cl-lib:fref b12d-%data% (i) ((1 *)) b12d-%offset%)
                    (*
                     (sin
                      (f2cl-lib:fref theta-%data% (i) ((1 *)) theta-%offset%))
                     (cos
                      (f2cl-lib:fref phi-%data% (i) ((1 *)) phi-%offset%))))
            (setf (f2cl-lib:fref b12e-%data% (i) ((1 *)) b12e-%offset%)
                    (*
                     (cos
                      (f2cl-lib:fref theta-%data% ((f2cl-lib:int-add i 1))
                                     ((1 *)) theta-%offset%))
                     (sin
                      (f2cl-lib:fref phi-%data% (i) ((1 *)) phi-%offset%))))
            (setf (f2cl-lib:fref b21e-%data% (i) ((1 *)) b21e-%offset%)
                    (*
                     (-
                      (cos
                       (f2cl-lib:fref theta-%data% (i) ((1 *))
                                      theta-%offset%)))
                     (sin
                      (f2cl-lib:fref phi-%data% (i) ((1 *)) phi-%offset%))))
            (setf (f2cl-lib:fref b21d-%data% ((f2cl-lib:int-add i 1)) ((1 *))
                                 b21d-%offset%)
                    (*
                     (-
                      (sin
                       (f2cl-lib:fref theta-%data% ((f2cl-lib:int-add i 1))
                                      ((1 *)) theta-%offset%)))
                     (cos
                      (f2cl-lib:fref phi-%data% (i) ((1 *)) phi-%offset%))))
            (setf (f2cl-lib:fref b22d-%data% (i) ((1 *)) b22d-%offset%)
                    (*
                     (cos
                      (f2cl-lib:fref theta-%data% (i) ((1 *)) theta-%offset%))
                     (cos
                      (f2cl-lib:fref phi-%data% (i) ((1 *)) phi-%offset%))))
            (setf (f2cl-lib:fref b22e-%data% (i) ((1 *)) b22e-%offset%)
                    (*
                     (-
                      (sin
                       (f2cl-lib:fref theta-%data% ((f2cl-lib:int-add i 1))
                                      ((1 *)) theta-%offset%)))
                     (sin
                      (f2cl-lib:fref phi-%data% (i) ((1 *)) phi-%offset%))))
           label100008))
        (setf (f2cl-lib:fref b12d-%data% (imax) ((1 *)) b12d-%offset%)
                (sin
                 (f2cl-lib:fref theta-%data% (imax) ((1 *)) theta-%offset%)))
        (setf (f2cl-lib:fref b22d-%data% (imax) ((1 *)) b22d-%offset%)
                (cos
                 (f2cl-lib:fref theta-%data% (imax) ((1 *)) theta-%offset%)))
        (cond
         ((> iter maxit) (setf info 0)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i q) nil)
            (tagbody
              (if (/= (f2cl-lib:fref phi-%data% (i) ((1 *)) phi-%offset%) zero)
                  (setf info (f2cl-lib:int-add info 1)))
             label100009))
          (go end_label)))
        (setf iter (f2cl-lib:int-sub (f2cl-lib:int-add iter imax) imin))
        (setf thetamax
                (f2cl-lib:fref theta-%data% (imin) ((1 *)) theta-%offset%))
        (setf thetamin
                (f2cl-lib:fref theta-%data% (imin) ((1 *)) theta-%offset%))
        (f2cl-lib:fdo (i (f2cl-lib:int-add imin 1) (f2cl-lib:int-add i 1))
                      ((> i imax) nil)
          (tagbody
            (if (> (f2cl-lib:fref theta-%data% (i) ((1 *)) theta-%offset%)
                   thetamax)
                (setf thetamax
                        (f2cl-lib:fref theta-%data% (i) ((1 *))
                                       theta-%offset%)))
            (if (< (f2cl-lib:fref theta-%data% (i) ((1 *)) theta-%offset%)
                   thetamin)
                (setf thetamin
                        (f2cl-lib:fref theta-%data% (i) ((1 *))
                                       theta-%offset%)))
           label100010))
        (cond
         ((> thetamax (+ piover2 (- thresh))) (setf mu zero) (setf nu one))
         ((< thetamin thresh) (setf mu one) (setf nu zero))
         (t
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (dlas2
               (f2cl-lib:fref b11d-%data% ((f2cl-lib:int-sub imax 1)) ((1 *))
                              b11d-%offset%)
               (f2cl-lib:fref b11e-%data% ((f2cl-lib:int-sub imax 1)) ((1 *))
                              b11e-%offset%)
               (f2cl-lib:fref b11d-%data% (imax) ((1 *)) b11d-%offset%) sigma11
               dummy)
            (declare (ignore var-0 var-1 var-2))
            (setf sigma11 var-3)
            (setf dummy var-4))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (dlas2
               (f2cl-lib:fref b21d-%data% ((f2cl-lib:int-sub imax 1)) ((1 *))
                              b21d-%offset%)
               (f2cl-lib:fref b21e-%data% ((f2cl-lib:int-sub imax 1)) ((1 *))
                              b21e-%offset%)
               (f2cl-lib:fref b21d-%data% (imax) ((1 *)) b21d-%offset%) sigma21
               dummy)
            (declare (ignore var-0 var-1 var-2))
            (setf sigma21 var-3)
            (setf dummy var-4))
          (cond
           ((<= sigma11 sigma21) (setf mu sigma11)
            (setf nu (f2cl-lib:fsqrt (- one (expt mu 2))))
            (cond ((< mu thresh) (setf mu zero) (setf nu one))))
           (t (setf nu sigma21) (setf mu (f2cl-lib:fsqrt (- 1.0 (expt nu 2))))
            (cond ((< nu thresh) (setf mu one) (setf nu zero)))))))
        (cond
         ((<= mu nu)
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (dlartgs (f2cl-lib:fref b11d-%data% (imin) ((1 *)) b11d-%offset%)
               (f2cl-lib:fref b11e-%data% (imin) ((1 *)) b11e-%offset%) mu
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iv1tcs imin)
                                                 1))
                              ((1 *)) rwork-%offset%)
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iv1tsn imin)
                                                 1))
                              ((1 *)) rwork-%offset%))
            (declare (ignore var-0 var-1 var-2))
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv1tcs imin) 1))
                                 ((1 *)) rwork-%offset%)
                    var-3)
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv1tsn imin) 1))
                                 ((1 *)) rwork-%offset%)
                    var-4)))
         (t
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (dlartgs (f2cl-lib:fref b21d-%data% (imin) ((1 *)) b21d-%offset%)
               (f2cl-lib:fref b21e-%data% (imin) ((1 *)) b21e-%offset%) nu
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iv1tcs imin)
                                                 1))
                              ((1 *)) rwork-%offset%)
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iv1tsn imin)
                                                 1))
                              ((1 *)) rwork-%offset%))
            (declare (ignore var-0 var-1 var-2))
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv1tcs imin) 1))
                                 ((1 *)) rwork-%offset%)
                    var-3)
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv1tsn imin) 1))
                                 ((1 *)) rwork-%offset%)
                    var-4))))
        (setf temp
                (+
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv1tcs imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b11d-%data% (imin) ((1 *)) b11d-%offset%))
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv1tsn imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b11e-%data% (imin) ((1 *)) b11e-%offset%))))
        (setf (f2cl-lib:fref b11e-%data% (imin) ((1 *)) b11e-%offset%)
                (-
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv1tcs imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b11e-%data% (imin) ((1 *)) b11e-%offset%))
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv1tsn imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b11d-%data% (imin) ((1 *)) b11d-%offset%))))
        (setf (f2cl-lib:fref b11d-%data% (imin) ((1 *)) b11d-%offset%) temp)
        (setf b11bulge
                (*
                 (f2cl-lib:fref rwork-%data%
                                ((f2cl-lib:int-sub
                                  (f2cl-lib:int-add iv1tsn imin) 1))
                                ((1 *)) rwork-%offset%)
                 (f2cl-lib:fref b11d-%data% ((f2cl-lib:int-add imin 1)) ((1 *))
                                b11d-%offset%)))
        (setf (f2cl-lib:fref b11d-%data% ((f2cl-lib:int-add imin 1)) ((1 *))
                             b11d-%offset%)
                (*
                 (f2cl-lib:fref rwork-%data%
                                ((f2cl-lib:int-sub
                                  (f2cl-lib:int-add iv1tcs imin) 1))
                                ((1 *)) rwork-%offset%)
                 (f2cl-lib:fref b11d-%data% ((f2cl-lib:int-add imin 1)) ((1 *))
                                b11d-%offset%)))
        (setf temp
                (+
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv1tcs imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b21d-%data% (imin) ((1 *)) b21d-%offset%))
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv1tsn imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b21e-%data% (imin) ((1 *)) b21e-%offset%))))
        (setf (f2cl-lib:fref b21e-%data% (imin) ((1 *)) b21e-%offset%)
                (-
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv1tcs imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b21e-%data% (imin) ((1 *)) b21e-%offset%))
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv1tsn imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b21d-%data% (imin) ((1 *)) b21d-%offset%))))
        (setf (f2cl-lib:fref b21d-%data% (imin) ((1 *)) b21d-%offset%) temp)
        (setf b21bulge
                (*
                 (f2cl-lib:fref rwork-%data%
                                ((f2cl-lib:int-sub
                                  (f2cl-lib:int-add iv1tsn imin) 1))
                                ((1 *)) rwork-%offset%)
                 (f2cl-lib:fref b21d-%data% ((f2cl-lib:int-add imin 1)) ((1 *))
                                b21d-%offset%)))
        (setf (f2cl-lib:fref b21d-%data% ((f2cl-lib:int-add imin 1)) ((1 *))
                             b21d-%offset%)
                (*
                 (f2cl-lib:fref rwork-%data%
                                ((f2cl-lib:int-sub
                                  (f2cl-lib:int-add iv1tcs imin) 1))
                                ((1 *)) rwork-%offset%)
                 (f2cl-lib:fref b21d-%data% ((f2cl-lib:int-add imin 1)) ((1 *))
                                b21d-%offset%)))
        (setf (f2cl-lib:fref theta-%data% (imin) ((1 *)) theta-%offset%)
                (f2cl-lib:atan2
                 (f2cl-lib:fsqrt
                  (+
                   (expt
                    (f2cl-lib:fref b21d-%data% (imin) ((1 *)) b21d-%offset%) 2)
                   (expt b21bulge 2)))
                 (f2cl-lib:fsqrt
                  (+
                   (expt
                    (f2cl-lib:fref b11d-%data% (imin) ((1 *)) b11d-%offset%) 2)
                   (expt b11bulge 2)))))
        (cond
         ((> (+ (expt (f2cl-lib:fref b11d (imin) ((1 *))) 2) (expt b11bulge 2))
             (expt thresh 2))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (dlartgp b11bulge
               (f2cl-lib:fref b11d-%data% (imin) ((1 *)) b11d-%offset%)
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iu1sn imin)
                                                 1))
                              ((1 *)) rwork-%offset%)
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iu1cs imin)
                                                 1))
                              ((1 *)) rwork-%offset%)
               r)
            (declare (ignore var-0 var-1))
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu1sn imin) 1))
                                 ((1 *)) rwork-%offset%)
                    var-2)
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu1cs imin) 1))
                                 ((1 *)) rwork-%offset%)
                    var-3)
            (setf r var-4)))
         ((<= mu nu)
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (dlartgs (f2cl-lib:fref b11e-%data% (imin) ((1 *)) b11e-%offset%)
               (f2cl-lib:fref b11d-%data% ((f2cl-lib:int-add imin 1)) ((1 *))
                              b11d-%offset%)
               mu
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iu1cs imin)
                                                 1))
                              ((1 *)) rwork-%offset%)
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iu1sn imin)
                                                 1))
                              ((1 *)) rwork-%offset%))
            (declare (ignore var-0 var-1 var-2))
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu1cs imin) 1))
                                 ((1 *)) rwork-%offset%)
                    var-3)
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu1sn imin) 1))
                                 ((1 *)) rwork-%offset%)
                    var-4)))
         (t
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (dlartgs (f2cl-lib:fref b12d-%data% (imin) ((1 *)) b12d-%offset%)
               (f2cl-lib:fref b12e-%data% (imin) ((1 *)) b12e-%offset%) nu
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iu1cs imin)
                                                 1))
                              ((1 *)) rwork-%offset%)
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iu1sn imin)
                                                 1))
                              ((1 *)) rwork-%offset%))
            (declare (ignore var-0 var-1 var-2))
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu1cs imin) 1))
                                 ((1 *)) rwork-%offset%)
                    var-3)
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu1sn imin) 1))
                                 ((1 *)) rwork-%offset%)
                    var-4))))
        (cond
         ((> (+ (expt (f2cl-lib:fref b21d (imin) ((1 *))) 2) (expt b21bulge 2))
             (expt thresh 2))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (dlartgp b21bulge
               (f2cl-lib:fref b21d-%data% (imin) ((1 *)) b21d-%offset%)
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iu2sn imin)
                                                 1))
                              ((1 *)) rwork-%offset%)
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iu2cs imin)
                                                 1))
                              ((1 *)) rwork-%offset%)
               r)
            (declare (ignore var-0 var-1))
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu2sn imin) 1))
                                 ((1 *)) rwork-%offset%)
                    var-2)
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu2cs imin) 1))
                                 ((1 *)) rwork-%offset%)
                    var-3)
            (setf r var-4)))
         ((< nu mu)
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (dlartgs (f2cl-lib:fref b21e-%data% (imin) ((1 *)) b21e-%offset%)
               (f2cl-lib:fref b21d-%data% ((f2cl-lib:int-add imin 1)) ((1 *))
                              b21d-%offset%)
               nu
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iu2cs imin)
                                                 1))
                              ((1 *)) rwork-%offset%)
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iu2sn imin)
                                                 1))
                              ((1 *)) rwork-%offset%))
            (declare (ignore var-0 var-1 var-2))
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu2cs imin) 1))
                                 ((1 *)) rwork-%offset%)
                    var-3)
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu2sn imin) 1))
                                 ((1 *)) rwork-%offset%)
                    var-4)))
         (t
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (dlartgs (f2cl-lib:fref b22d-%data% (imin) ((1 *)) b22d-%offset%)
               (f2cl-lib:fref b22e-%data% (imin) ((1 *)) b22e-%offset%) mu
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iu2cs imin)
                                                 1))
                              ((1 *)) rwork-%offset%)
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iu2sn imin)
                                                 1))
                              ((1 *)) rwork-%offset%))
            (declare (ignore var-0 var-1 var-2))
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu2cs imin) 1))
                                 ((1 *)) rwork-%offset%)
                    var-3)
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu2sn imin) 1))
                                 ((1 *)) rwork-%offset%)
                    var-4))))
        (setf (f2cl-lib:fref rwork-%data%
                             ((f2cl-lib:int-sub (f2cl-lib:int-add iu2cs imin)
                                                1))
                             ((1 *)) rwork-%offset%)
                (-
                 (f2cl-lib:fref rwork-%data%
                                ((f2cl-lib:int-sub
                                  (f2cl-lib:int-add iu2cs imin) 1))
                                ((1 *)) rwork-%offset%)))
        (setf (f2cl-lib:fref rwork-%data%
                             ((f2cl-lib:int-sub (f2cl-lib:int-add iu2sn imin)
                                                1))
                             ((1 *)) rwork-%offset%)
                (-
                 (f2cl-lib:fref rwork-%data%
                                ((f2cl-lib:int-sub
                                  (f2cl-lib:int-add iu2sn imin) 1))
                                ((1 *)) rwork-%offset%)))
        (setf temp
                (+
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu1cs imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b11e-%data% (imin) ((1 *)) b11e-%offset%))
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu1sn imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b11d-%data% ((f2cl-lib:int-add imin 1))
                                 ((1 *)) b11d-%offset%))))
        (setf (f2cl-lib:fref b11d-%data% ((f2cl-lib:int-add imin 1)) ((1 *))
                             b11d-%offset%)
                (-
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu1cs imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b11d-%data% ((f2cl-lib:int-add imin 1))
                                 ((1 *)) b11d-%offset%))
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu1sn imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b11e-%data% (imin) ((1 *)) b11e-%offset%))))
        (setf (f2cl-lib:fref b11e-%data% (imin) ((1 *)) b11e-%offset%) temp)
        (cond
         ((> imax (f2cl-lib:int-add imin 1))
          (setf b11bulge
                  (*
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iu1sn imin) 1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref b11e-%data% ((f2cl-lib:int-add imin 1))
                                  ((1 *)) b11e-%offset%)))
          (setf (f2cl-lib:fref b11e-%data% ((f2cl-lib:int-add imin 1)) ((1 *))
                               b11e-%offset%)
                  (*
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iu1cs imin) 1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref b11e-%data% ((f2cl-lib:int-add imin 1))
                                  ((1 *)) b11e-%offset%)))))
        (setf temp
                (+
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu1cs imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b12d-%data% (imin) ((1 *)) b12d-%offset%))
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu1sn imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b12e-%data% (imin) ((1 *)) b12e-%offset%))))
        (setf (f2cl-lib:fref b12e-%data% (imin) ((1 *)) b12e-%offset%)
                (-
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu1cs imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b12e-%data% (imin) ((1 *)) b12e-%offset%))
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu1sn imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b12d-%data% (imin) ((1 *)) b12d-%offset%))))
        (setf (f2cl-lib:fref b12d-%data% (imin) ((1 *)) b12d-%offset%) temp)
        (setf b12bulge
                (*
                 (f2cl-lib:fref rwork-%data%
                                ((f2cl-lib:int-sub
                                  (f2cl-lib:int-add iu1sn imin) 1))
                                ((1 *)) rwork-%offset%)
                 (f2cl-lib:fref b12d-%data% ((f2cl-lib:int-add imin 1)) ((1 *))
                                b12d-%offset%)))
        (setf (f2cl-lib:fref b12d-%data% ((f2cl-lib:int-add imin 1)) ((1 *))
                             b12d-%offset%)
                (*
                 (f2cl-lib:fref rwork-%data%
                                ((f2cl-lib:int-sub
                                  (f2cl-lib:int-add iu1cs imin) 1))
                                ((1 *)) rwork-%offset%)
                 (f2cl-lib:fref b12d-%data% ((f2cl-lib:int-add imin 1)) ((1 *))
                                b12d-%offset%)))
        (setf temp
                (+
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu2cs imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b21e-%data% (imin) ((1 *)) b21e-%offset%))
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu2sn imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b21d-%data% ((f2cl-lib:int-add imin 1))
                                 ((1 *)) b21d-%offset%))))
        (setf (f2cl-lib:fref b21d-%data% ((f2cl-lib:int-add imin 1)) ((1 *))
                             b21d-%offset%)
                (-
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu2cs imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b21d-%data% ((f2cl-lib:int-add imin 1))
                                 ((1 *)) b21d-%offset%))
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu2sn imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b21e-%data% (imin) ((1 *)) b21e-%offset%))))
        (setf (f2cl-lib:fref b21e-%data% (imin) ((1 *)) b21e-%offset%) temp)
        (cond
         ((> imax (f2cl-lib:int-add imin 1))
          (setf b21bulge
                  (*
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iu2sn imin) 1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref b21e-%data% ((f2cl-lib:int-add imin 1))
                                  ((1 *)) b21e-%offset%)))
          (setf (f2cl-lib:fref b21e-%data% ((f2cl-lib:int-add imin 1)) ((1 *))
                               b21e-%offset%)
                  (*
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iu2cs imin) 1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref b21e-%data% ((f2cl-lib:int-add imin 1))
                                  ((1 *)) b21e-%offset%)))))
        (setf temp
                (+
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu2cs imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b22d-%data% (imin) ((1 *)) b22d-%offset%))
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu2sn imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b22e-%data% (imin) ((1 *)) b22e-%offset%))))
        (setf (f2cl-lib:fref b22e-%data% (imin) ((1 *)) b22e-%offset%)
                (-
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu2cs imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b22e-%data% (imin) ((1 *)) b22e-%offset%))
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iu2sn imin) 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b22d-%data% (imin) ((1 *)) b22d-%offset%))))
        (setf (f2cl-lib:fref b22d-%data% (imin) ((1 *)) b22d-%offset%) temp)
        (setf b22bulge
                (*
                 (f2cl-lib:fref rwork-%data%
                                ((f2cl-lib:int-sub
                                  (f2cl-lib:int-add iu2sn imin) 1))
                                ((1 *)) rwork-%offset%)
                 (f2cl-lib:fref b22d-%data% ((f2cl-lib:int-add imin 1)) ((1 *))
                                b22d-%offset%)))
        (setf (f2cl-lib:fref b22d-%data% ((f2cl-lib:int-add imin 1)) ((1 *))
                             b22d-%offset%)
                (*
                 (f2cl-lib:fref rwork-%data%
                                ((f2cl-lib:int-sub
                                  (f2cl-lib:int-add iu2cs imin) 1))
                                ((1 *)) rwork-%offset%)
                 (f2cl-lib:fref b22d-%data% ((f2cl-lib:int-add imin 1)) ((1 *))
                                b22d-%offset%)))
        (f2cl-lib:fdo (i (f2cl-lib:int-add imin 1) (f2cl-lib:int-add i 1))
                      ((> i (f2cl-lib:int-add imax (f2cl-lib:int-sub 1))) nil)
          (tagbody
            (setf x1
                    (+
                     (*
                      (sin
                       (f2cl-lib:fref theta-%data% ((f2cl-lib:int-sub i 1))
                                      ((1 *)) theta-%offset%))
                      (f2cl-lib:fref b11e-%data% ((f2cl-lib:int-sub i 1))
                                     ((1 *)) b11e-%offset%))
                     (*
                      (cos
                       (f2cl-lib:fref theta-%data% ((f2cl-lib:int-sub i 1))
                                      ((1 *)) theta-%offset%))
                      (f2cl-lib:fref b21e-%data% ((f2cl-lib:int-sub i 1))
                                     ((1 *)) b21e-%offset%))))
            (setf x2
                    (+
                     (*
                      (sin
                       (f2cl-lib:fref theta-%data% ((f2cl-lib:int-sub i 1))
                                      ((1 *)) theta-%offset%))
                      b11bulge)
                     (*
                      (cos
                       (f2cl-lib:fref theta-%data% ((f2cl-lib:int-sub i 1))
                                      ((1 *)) theta-%offset%))
                      b21bulge)))
            (setf y1
                    (+
                     (*
                      (sin
                       (f2cl-lib:fref theta-%data% ((f2cl-lib:int-sub i 1))
                                      ((1 *)) theta-%offset%))
                      (f2cl-lib:fref b12d-%data% ((f2cl-lib:int-sub i 1))
                                     ((1 *)) b12d-%offset%))
                     (*
                      (cos
                       (f2cl-lib:fref theta-%data% ((f2cl-lib:int-sub i 1))
                                      ((1 *)) theta-%offset%))
                      (f2cl-lib:fref b22d-%data% ((f2cl-lib:int-sub i 1))
                                     ((1 *)) b22d-%offset%))))
            (setf y2
                    (+
                     (*
                      (sin
                       (f2cl-lib:fref theta-%data% ((f2cl-lib:int-sub i 1))
                                      ((1 *)) theta-%offset%))
                      b12bulge)
                     (*
                      (cos
                       (f2cl-lib:fref theta-%data% ((f2cl-lib:int-sub i 1))
                                      ((1 *)) theta-%offset%))
                      b22bulge)))
            (setf (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub i 1)) ((1 *))
                                 phi-%offset%)
                    (f2cl-lib:atan2
                     (f2cl-lib:fsqrt (+ (expt x1 2) (expt x2 2)))
                     (f2cl-lib:fsqrt (+ (expt y1 2) (expt y2 2)))))
            (setf restart11
                    (<=
                     (+
                      (expt
                       (f2cl-lib:fref b11e-%data% ((f2cl-lib:int-sub i 1))
                                      ((1 *)) b11e-%offset%)
                       2)
                      (expt b11bulge 2))
                     (expt thresh 2)))
            (setf restart21
                    (<=
                     (+
                      (expt
                       (f2cl-lib:fref b21e-%data% ((f2cl-lib:int-sub i 1))
                                      ((1 *)) b21e-%offset%)
                       2)
                      (expt b21bulge 2))
                     (expt thresh 2)))
            (setf restart12
                    (<=
                     (+
                      (expt
                       (f2cl-lib:fref b12d-%data% ((f2cl-lib:int-sub i 1))
                                      ((1 *)) b12d-%offset%)
                       2)
                      (expt b12bulge 2))
                     (expt thresh 2)))
            (setf restart22
                    (<=
                     (+
                      (expt
                       (f2cl-lib:fref b22d-%data% ((f2cl-lib:int-sub i 1))
                                      ((1 *)) b22d-%offset%)
                       2)
                      (expt b22bulge 2))
                     (expt thresh 2)))
            (cond
             ((and (not restart11) (not restart21))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlartgp x2 x1
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iv1tsn i) 1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iv1tcs i) 1))
                                  ((1 *)) rwork-%offset%)
                   r)
                (declare (ignore var-0 var-1))
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv1tsn i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-2)
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv1tcs i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-3)
                (setf r var-4)))
             ((and (not restart11) restart21)
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlartgp b11bulge
                   (f2cl-lib:fref b11e-%data% ((f2cl-lib:int-sub i 1)) ((1 *))
                                  b11e-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iv1tsn i) 1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iv1tcs i) 1))
                                  ((1 *)) rwork-%offset%)
                   r)
                (declare (ignore var-0 var-1))
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv1tsn i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-2)
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv1tcs i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-3)
                (setf r var-4)))
             ((and restart11 (not restart21))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlartgp b21bulge
                   (f2cl-lib:fref b21e-%data% ((f2cl-lib:int-sub i 1)) ((1 *))
                                  b21e-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iv1tsn i) 1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iv1tcs i) 1))
                                  ((1 *)) rwork-%offset%)
                   r)
                (declare (ignore var-0 var-1))
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv1tsn i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-2)
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv1tcs i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-3)
                (setf r var-4)))
             ((<= mu nu)
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlartgs
                   (f2cl-lib:fref b11d-%data% (i) ((1 *)) b11d-%offset%)
                   (f2cl-lib:fref b11e-%data% (i) ((1 *)) b11e-%offset%) mu
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iv1tcs i) 1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iv1tsn i) 1))
                                  ((1 *)) rwork-%offset%))
                (declare (ignore var-0 var-1 var-2))
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv1tcs i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-3)
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv1tsn i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-4)))
             (t
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlartgs
                   (f2cl-lib:fref b21d-%data% (i) ((1 *)) b21d-%offset%)
                   (f2cl-lib:fref b21e-%data% (i) ((1 *)) b21e-%offset%) nu
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iv1tcs i) 1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iv1tsn i) 1))
                                  ((1 *)) rwork-%offset%))
                (declare (ignore var-0 var-1 var-2))
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv1tcs i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-3)
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv1tsn i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-4))))
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub (f2cl-lib:int-add iv1tcs i)
                                                    1))
                                 ((1 *)) rwork-%offset%)
                    (-
                     (f2cl-lib:fref rwork-%data%
                                    ((f2cl-lib:int-sub
                                      (f2cl-lib:int-add iv1tcs i) 1))
                                    ((1 *)) rwork-%offset%)))
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub (f2cl-lib:int-add iv1tsn i)
                                                    1))
                                 ((1 *)) rwork-%offset%)
                    (-
                     (f2cl-lib:fref rwork-%data%
                                    ((f2cl-lib:int-sub
                                      (f2cl-lib:int-add iv1tsn i) 1))
                                    ((1 *)) rwork-%offset%)))
            (cond
             ((and (not restart12) (not restart22))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlartgp y2 y1
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iv2tsn i) 1 1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iv2tcs i) 1 1))
                                  ((1 *)) rwork-%offset%)
                   r)
                (declare (ignore var-0 var-1))
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv2tsn i) 1 1))
                                     ((1 *)) rwork-%offset%)
                        var-2)
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv2tcs i) 1 1))
                                     ((1 *)) rwork-%offset%)
                        var-3)
                (setf r var-4)))
             ((and (not restart12) restart22)
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlartgp b12bulge
                   (f2cl-lib:fref b12d-%data% ((f2cl-lib:int-sub i 1)) ((1 *))
                                  b12d-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iv2tsn i) 1 1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iv2tcs i) 1 1))
                                  ((1 *)) rwork-%offset%)
                   r)
                (declare (ignore var-0 var-1))
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv2tsn i) 1 1))
                                     ((1 *)) rwork-%offset%)
                        var-2)
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv2tcs i) 1 1))
                                     ((1 *)) rwork-%offset%)
                        var-3)
                (setf r var-4)))
             ((and restart12 (not restart22))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlartgp b22bulge
                   (f2cl-lib:fref b22d-%data% ((f2cl-lib:int-sub i 1)) ((1 *))
                                  b22d-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iv2tsn i) 1 1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iv2tcs i) 1 1))
                                  ((1 *)) rwork-%offset%)
                   r)
                (declare (ignore var-0 var-1))
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv2tsn i) 1 1))
                                     ((1 *)) rwork-%offset%)
                        var-2)
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv2tcs i) 1 1))
                                     ((1 *)) rwork-%offset%)
                        var-3)
                (setf r var-4)))
             ((< nu mu)
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlartgs
                   (f2cl-lib:fref b12e-%data% ((f2cl-lib:int-sub i 1)) ((1 *))
                                  b12e-%offset%)
                   (f2cl-lib:fref b12d-%data% (i) ((1 *)) b12d-%offset%) nu
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iv2tcs i) 1 1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iv2tsn i) 1 1))
                                  ((1 *)) rwork-%offset%))
                (declare (ignore var-0 var-1 var-2))
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv2tcs i) 1 1))
                                     ((1 *)) rwork-%offset%)
                        var-3)
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv2tsn i) 1 1))
                                     ((1 *)) rwork-%offset%)
                        var-4)))
             (t
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlartgs
                   (f2cl-lib:fref b22e-%data% ((f2cl-lib:int-sub i 1)) ((1 *))
                                  b22e-%offset%)
                   (f2cl-lib:fref b22d-%data% (i) ((1 *)) b22d-%offset%) mu
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iv2tcs i) 1 1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add iv2tsn i) 1 1))
                                  ((1 *)) rwork-%offset%))
                (declare (ignore var-0 var-1 var-2))
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv2tcs i) 1 1))
                                     ((1 *)) rwork-%offset%)
                        var-3)
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv2tsn i) 1 1))
                                     ((1 *)) rwork-%offset%)
                        var-4))))
            (setf temp
                    (+
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv1tcs i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b11d-%data% (i) ((1 *)) b11d-%offset%))
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv1tsn i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b11e-%data% (i) ((1 *)) b11e-%offset%))))
            (setf (f2cl-lib:fref b11e-%data% (i) ((1 *)) b11e-%offset%)
                    (-
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv1tcs i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b11e-%data% (i) ((1 *)) b11e-%offset%))
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv1tsn i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b11d-%data% (i) ((1 *)) b11d-%offset%))))
            (setf (f2cl-lib:fref b11d-%data% (i) ((1 *)) b11d-%offset%) temp)
            (setf b11bulge
                    (*
                     (f2cl-lib:fref rwork-%data%
                                    ((f2cl-lib:int-sub
                                      (f2cl-lib:int-add iv1tsn i) 1))
                                    ((1 *)) rwork-%offset%)
                     (f2cl-lib:fref b11d-%data% ((f2cl-lib:int-add i 1))
                                    ((1 *)) b11d-%offset%)))
            (setf (f2cl-lib:fref b11d-%data% ((f2cl-lib:int-add i 1)) ((1 *))
                                 b11d-%offset%)
                    (*
                     (f2cl-lib:fref rwork-%data%
                                    ((f2cl-lib:int-sub
                                      (f2cl-lib:int-add iv1tcs i) 1))
                                    ((1 *)) rwork-%offset%)
                     (f2cl-lib:fref b11d-%data% ((f2cl-lib:int-add i 1))
                                    ((1 *)) b11d-%offset%)))
            (setf temp
                    (+
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv1tcs i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b21d-%data% (i) ((1 *)) b21d-%offset%))
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv1tsn i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b21e-%data% (i) ((1 *)) b21e-%offset%))))
            (setf (f2cl-lib:fref b21e-%data% (i) ((1 *)) b21e-%offset%)
                    (-
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv1tcs i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b21e-%data% (i) ((1 *)) b21e-%offset%))
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv1tsn i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b21d-%data% (i) ((1 *)) b21d-%offset%))))
            (setf (f2cl-lib:fref b21d-%data% (i) ((1 *)) b21d-%offset%) temp)
            (setf b21bulge
                    (*
                     (f2cl-lib:fref rwork-%data%
                                    ((f2cl-lib:int-sub
                                      (f2cl-lib:int-add iv1tsn i) 1))
                                    ((1 *)) rwork-%offset%)
                     (f2cl-lib:fref b21d-%data% ((f2cl-lib:int-add i 1))
                                    ((1 *)) b21d-%offset%)))
            (setf (f2cl-lib:fref b21d-%data% ((f2cl-lib:int-add i 1)) ((1 *))
                                 b21d-%offset%)
                    (*
                     (f2cl-lib:fref rwork-%data%
                                    ((f2cl-lib:int-sub
                                      (f2cl-lib:int-add iv1tcs i) 1))
                                    ((1 *)) rwork-%offset%)
                     (f2cl-lib:fref b21d-%data% ((f2cl-lib:int-add i 1))
                                    ((1 *)) b21d-%offset%)))
            (setf temp
                    (+
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv2tcs i) 1 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b12e-%data% ((f2cl-lib:int-sub i 1))
                                     ((1 *)) b12e-%offset%))
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv2tsn i) 1 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b12d-%data% (i) ((1 *)) b12d-%offset%))))
            (setf (f2cl-lib:fref b12d-%data% (i) ((1 *)) b12d-%offset%)
                    (-
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv2tcs i) 1 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b12d-%data% (i) ((1 *)) b12d-%offset%))
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv2tsn i) 1 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b12e-%data% ((f2cl-lib:int-sub i 1))
                                     ((1 *)) b12e-%offset%))))
            (setf (f2cl-lib:fref b12e-%data% ((f2cl-lib:int-sub i 1)) ((1 *))
                                 b12e-%offset%)
                    temp)
            (setf b12bulge
                    (*
                     (f2cl-lib:fref rwork-%data%
                                    ((f2cl-lib:int-sub
                                      (f2cl-lib:int-add iv2tsn i) 1 1))
                                    ((1 *)) rwork-%offset%)
                     (f2cl-lib:fref b12e-%data% (i) ((1 *)) b12e-%offset%)))
            (setf (f2cl-lib:fref b12e-%data% (i) ((1 *)) b12e-%offset%)
                    (*
                     (f2cl-lib:fref rwork-%data%
                                    ((f2cl-lib:int-sub
                                      (f2cl-lib:int-add iv2tcs i) 1 1))
                                    ((1 *)) rwork-%offset%)
                     (f2cl-lib:fref b12e-%data% (i) ((1 *)) b12e-%offset%)))
            (setf temp
                    (+
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv2tcs i) 1 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b22e-%data% ((f2cl-lib:int-sub i 1))
                                     ((1 *)) b22e-%offset%))
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv2tsn i) 1 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b22d-%data% (i) ((1 *)) b22d-%offset%))))
            (setf (f2cl-lib:fref b22d-%data% (i) ((1 *)) b22d-%offset%)
                    (-
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv2tcs i) 1 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b22d-%data% (i) ((1 *)) b22d-%offset%))
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iv2tsn i) 1 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b22e-%data% ((f2cl-lib:int-sub i 1))
                                     ((1 *)) b22e-%offset%))))
            (setf (f2cl-lib:fref b22e-%data% ((f2cl-lib:int-sub i 1)) ((1 *))
                                 b22e-%offset%)
                    temp)
            (setf b22bulge
                    (*
                     (f2cl-lib:fref rwork-%data%
                                    ((f2cl-lib:int-sub
                                      (f2cl-lib:int-add iv2tsn i) 1 1))
                                    ((1 *)) rwork-%offset%)
                     (f2cl-lib:fref b22e-%data% (i) ((1 *)) b22e-%offset%)))
            (setf (f2cl-lib:fref b22e-%data% (i) ((1 *)) b22e-%offset%)
                    (*
                     (f2cl-lib:fref rwork-%data%
                                    ((f2cl-lib:int-sub
                                      (f2cl-lib:int-add iv2tcs i) 1 1))
                                    ((1 *)) rwork-%offset%)
                     (f2cl-lib:fref b22e-%data% (i) ((1 *)) b22e-%offset%)))
            (setf x1
                    (+
                     (*
                      (cos
                       (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub i 1))
                                      ((1 *)) phi-%offset%))
                      (f2cl-lib:fref b11d-%data% (i) ((1 *)) b11d-%offset%))
                     (*
                      (sin
                       (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub i 1))
                                      ((1 *)) phi-%offset%))
                      (f2cl-lib:fref b12e-%data% ((f2cl-lib:int-sub i 1))
                                     ((1 *)) b12e-%offset%))))
            (setf x2
                    (+
                     (*
                      (cos
                       (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub i 1))
                                      ((1 *)) phi-%offset%))
                      b11bulge)
                     (*
                      (sin
                       (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub i 1))
                                      ((1 *)) phi-%offset%))
                      b12bulge)))
            (setf y1
                    (+
                     (*
                      (cos
                       (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub i 1))
                                      ((1 *)) phi-%offset%))
                      (f2cl-lib:fref b21d-%data% (i) ((1 *)) b21d-%offset%))
                     (*
                      (sin
                       (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub i 1))
                                      ((1 *)) phi-%offset%))
                      (f2cl-lib:fref b22e-%data% ((f2cl-lib:int-sub i 1))
                                     ((1 *)) b22e-%offset%))))
            (setf y2
                    (+
                     (*
                      (cos
                       (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub i 1))
                                      ((1 *)) phi-%offset%))
                      b21bulge)
                     (*
                      (sin
                       (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub i 1))
                                      ((1 *)) phi-%offset%))
                      b22bulge)))
            (setf (f2cl-lib:fref theta-%data% (i) ((1 *)) theta-%offset%)
                    (f2cl-lib:atan2
                     (f2cl-lib:fsqrt (+ (expt y1 2) (expt y2 2)))
                     (f2cl-lib:fsqrt (+ (expt x1 2) (expt x2 2)))))
            (setf restart11
                    (<=
                     (+
                      (expt
                       (f2cl-lib:fref b11d-%data% (i) ((1 *)) b11d-%offset%) 2)
                      (expt b11bulge 2))
                     (expt thresh 2)))
            (setf restart12
                    (<=
                     (+
                      (expt
                       (f2cl-lib:fref b12e-%data% ((f2cl-lib:int-sub i 1))
                                      ((1 *)) b12e-%offset%)
                       2)
                      (expt b12bulge 2))
                     (expt thresh 2)))
            (setf restart21
                    (<=
                     (+
                      (expt
                       (f2cl-lib:fref b21d-%data% (i) ((1 *)) b21d-%offset%) 2)
                      (expt b21bulge 2))
                     (expt thresh 2)))
            (setf restart22
                    (<=
                     (+
                      (expt
                       (f2cl-lib:fref b22e-%data% ((f2cl-lib:int-sub i 1))
                                      ((1 *)) b22e-%offset%)
                       2)
                      (expt b22bulge 2))
                     (expt thresh 2)))
            (cond
             ((and (not restart11) (not restart12))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlartgp x2 x1
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-add iu1sn i)
                                                     1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-add iu1cs i)
                                                     1))
                                  ((1 *)) rwork-%offset%)
                   r)
                (declare (ignore var-0 var-1))
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu1sn i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-2)
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu1cs i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-3)
                (setf r var-4)))
             ((and (not restart11) restart12)
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlartgp b11bulge
                   (f2cl-lib:fref b11d-%data% (i) ((1 *)) b11d-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-add iu1sn i)
                                                     1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-add iu1cs i)
                                                     1))
                                  ((1 *)) rwork-%offset%)
                   r)
                (declare (ignore var-0 var-1))
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu1sn i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-2)
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu1cs i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-3)
                (setf r var-4)))
             ((and restart11 (not restart12))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlartgp b12bulge
                   (f2cl-lib:fref b12e-%data% ((f2cl-lib:int-sub i 1)) ((1 *))
                                  b12e-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-add iu1sn i)
                                                     1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-add iu1cs i)
                                                     1))
                                  ((1 *)) rwork-%offset%)
                   r)
                (declare (ignore var-0 var-1))
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu1sn i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-2)
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu1cs i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-3)
                (setf r var-4)))
             ((<= mu nu)
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlartgs
                   (f2cl-lib:fref b11e-%data% (i) ((1 *)) b11e-%offset%)
                   (f2cl-lib:fref b11d-%data% ((f2cl-lib:int-add i 1)) ((1 *))
                                  b11d-%offset%)
                   mu
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-add iu1cs i)
                                                     1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-add iu1sn i)
                                                     1))
                                  ((1 *)) rwork-%offset%))
                (declare (ignore var-0 var-1 var-2))
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu1cs i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-3)
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu1sn i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-4)))
             (t
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlartgs
                   (f2cl-lib:fref b12d-%data% (i) ((1 *)) b12d-%offset%)
                   (f2cl-lib:fref b12e-%data% (i) ((1 *)) b12e-%offset%) nu
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-add iu1cs i)
                                                     1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-add iu1sn i)
                                                     1))
                                  ((1 *)) rwork-%offset%))
                (declare (ignore var-0 var-1 var-2))
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu1cs i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-3)
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu1sn i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-4))))
            (cond
             ((and (not restart21) (not restart22))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlartgp y2 y1
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-add iu2sn i)
                                                     1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-add iu2cs i)
                                                     1))
                                  ((1 *)) rwork-%offset%)
                   r)
                (declare (ignore var-0 var-1))
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu2sn i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-2)
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu2cs i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-3)
                (setf r var-4)))
             ((and (not restart21) restart22)
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlartgp b21bulge
                   (f2cl-lib:fref b21d-%data% (i) ((1 *)) b21d-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-add iu2sn i)
                                                     1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-add iu2cs i)
                                                     1))
                                  ((1 *)) rwork-%offset%)
                   r)
                (declare (ignore var-0 var-1))
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu2sn i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-2)
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu2cs i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-3)
                (setf r var-4)))
             ((and restart21 (not restart22))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlartgp b22bulge
                   (f2cl-lib:fref b22e-%data% ((f2cl-lib:int-sub i 1)) ((1 *))
                                  b22e-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-add iu2sn i)
                                                     1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-add iu2cs i)
                                                     1))
                                  ((1 *)) rwork-%offset%)
                   r)
                (declare (ignore var-0 var-1))
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu2sn i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-2)
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu2cs i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-3)
                (setf r var-4)))
             ((< nu mu)
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlartgs
                   (f2cl-lib:fref b21e-%data% (i) ((1 *)) b21e-%offset%)
                   (f2cl-lib:fref b21e-%data% ((f2cl-lib:int-add i 1)) ((1 *))
                                  b21e-%offset%)
                   nu
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-add iu2cs i)
                                                     1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-add iu2sn i)
                                                     1))
                                  ((1 *)) rwork-%offset%))
                (declare (ignore var-0 var-1 var-2))
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu2cs i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-3)
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu2sn i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-4)))
             (t
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlartgs
                   (f2cl-lib:fref b22d-%data% (i) ((1 *)) b22d-%offset%)
                   (f2cl-lib:fref b22e-%data% (i) ((1 *)) b22e-%offset%) mu
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-add iu2cs i)
                                                     1))
                                  ((1 *)) rwork-%offset%)
                   (f2cl-lib:fref rwork-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-add iu2sn i)
                                                     1))
                                  ((1 *)) rwork-%offset%))
                (declare (ignore var-0 var-1 var-2))
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu2cs i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-3)
                (setf (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu2sn i) 1))
                                     ((1 *)) rwork-%offset%)
                        var-4))))
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub (f2cl-lib:int-add iu2cs i)
                                                    1))
                                 ((1 *)) rwork-%offset%)
                    (-
                     (f2cl-lib:fref rwork-%data%
                                    ((f2cl-lib:int-sub
                                      (f2cl-lib:int-add iu2cs i) 1))
                                    ((1 *)) rwork-%offset%)))
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub (f2cl-lib:int-add iu2sn i)
                                                    1))
                                 ((1 *)) rwork-%offset%)
                    (-
                     (f2cl-lib:fref rwork-%data%
                                    ((f2cl-lib:int-sub
                                      (f2cl-lib:int-add iu2sn i) 1))
                                    ((1 *)) rwork-%offset%)))
            (setf temp
                    (+
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu1cs i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b11e-%data% (i) ((1 *)) b11e-%offset%))
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu1sn i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b11d-%data% ((f2cl-lib:int-add i 1))
                                     ((1 *)) b11d-%offset%))))
            (setf (f2cl-lib:fref b11d-%data% ((f2cl-lib:int-add i 1)) ((1 *))
                                 b11d-%offset%)
                    (-
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu1cs i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b11d-%data% ((f2cl-lib:int-add i 1))
                                     ((1 *)) b11d-%offset%))
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu1sn i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b11e-%data% (i) ((1 *)) b11e-%offset%))))
            (setf (f2cl-lib:fref b11e-%data% (i) ((1 *)) b11e-%offset%) temp)
            (cond
             ((< i (f2cl-lib:int-add imax (f2cl-lib:int-sub 1)))
              (setf b11bulge
                      (*
                       (f2cl-lib:fref rwork-%data%
                                      ((f2cl-lib:int-sub
                                        (f2cl-lib:int-add iu1sn i) 1))
                                      ((1 *)) rwork-%offset%)
                       (f2cl-lib:fref b11e-%data% ((f2cl-lib:int-add i 1))
                                      ((1 *)) b11e-%offset%)))
              (setf (f2cl-lib:fref b11e-%data% ((f2cl-lib:int-add i 1)) ((1 *))
                                   b11e-%offset%)
                      (*
                       (f2cl-lib:fref rwork-%data%
                                      ((f2cl-lib:int-sub
                                        (f2cl-lib:int-add iu1cs i) 1))
                                      ((1 *)) rwork-%offset%)
                       (f2cl-lib:fref b11e-%data% ((f2cl-lib:int-add i 1))
                                      ((1 *)) b11e-%offset%)))))
            (setf temp
                    (+
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu2cs i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b21e-%data% (i) ((1 *)) b21e-%offset%))
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu2sn i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b21d-%data% ((f2cl-lib:int-add i 1))
                                     ((1 *)) b21d-%offset%))))
            (setf (f2cl-lib:fref b21d-%data% ((f2cl-lib:int-add i 1)) ((1 *))
                                 b21d-%offset%)
                    (-
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu2cs i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b21d-%data% ((f2cl-lib:int-add i 1))
                                     ((1 *)) b21d-%offset%))
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu2sn i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b21e-%data% (i) ((1 *)) b21e-%offset%))))
            (setf (f2cl-lib:fref b21e-%data% (i) ((1 *)) b21e-%offset%) temp)
            (cond
             ((< i (f2cl-lib:int-add imax (f2cl-lib:int-sub 1)))
              (setf b21bulge
                      (*
                       (f2cl-lib:fref rwork-%data%
                                      ((f2cl-lib:int-sub
                                        (f2cl-lib:int-add iu2sn i) 1))
                                      ((1 *)) rwork-%offset%)
                       (f2cl-lib:fref b21e-%data% ((f2cl-lib:int-add i 1))
                                      ((1 *)) b21e-%offset%)))
              (setf (f2cl-lib:fref b21e-%data% ((f2cl-lib:int-add i 1)) ((1 *))
                                   b21e-%offset%)
                      (*
                       (f2cl-lib:fref rwork-%data%
                                      ((f2cl-lib:int-sub
                                        (f2cl-lib:int-add iu2cs i) 1))
                                      ((1 *)) rwork-%offset%)
                       (f2cl-lib:fref b21e-%data% ((f2cl-lib:int-add i 1))
                                      ((1 *)) b21e-%offset%)))))
            (setf temp
                    (+
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu1cs i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b12d-%data% (i) ((1 *)) b12d-%offset%))
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu1sn i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b12e-%data% (i) ((1 *)) b12e-%offset%))))
            (setf (f2cl-lib:fref b12e-%data% (i) ((1 *)) b12e-%offset%)
                    (-
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu1cs i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b12e-%data% (i) ((1 *)) b12e-%offset%))
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu1sn i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b12d-%data% (i) ((1 *)) b12d-%offset%))))
            (setf (f2cl-lib:fref b12d-%data% (i) ((1 *)) b12d-%offset%) temp)
            (setf b12bulge
                    (*
                     (f2cl-lib:fref rwork-%data%
                                    ((f2cl-lib:int-sub
                                      (f2cl-lib:int-add iu1sn i) 1))
                                    ((1 *)) rwork-%offset%)
                     (f2cl-lib:fref b12d-%data% ((f2cl-lib:int-add i 1))
                                    ((1 *)) b12d-%offset%)))
            (setf (f2cl-lib:fref b12d-%data% ((f2cl-lib:int-add i 1)) ((1 *))
                                 b12d-%offset%)
                    (*
                     (f2cl-lib:fref rwork-%data%
                                    ((f2cl-lib:int-sub
                                      (f2cl-lib:int-add iu1cs i) 1))
                                    ((1 *)) rwork-%offset%)
                     (f2cl-lib:fref b12d-%data% ((f2cl-lib:int-add i 1))
                                    ((1 *)) b12d-%offset%)))
            (setf temp
                    (+
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu2cs i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b22d-%data% (i) ((1 *)) b22d-%offset%))
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu2sn i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b22e-%data% (i) ((1 *)) b22e-%offset%))))
            (setf (f2cl-lib:fref b22e-%data% (i) ((1 *)) b22e-%offset%)
                    (-
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu2cs i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b22e-%data% (i) ((1 *)) b22e-%offset%))
                     (*
                      (f2cl-lib:fref rwork-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add iu2sn i) 1))
                                     ((1 *)) rwork-%offset%)
                      (f2cl-lib:fref b22d-%data% (i) ((1 *)) b22d-%offset%))))
            (setf (f2cl-lib:fref b22d-%data% (i) ((1 *)) b22d-%offset%) temp)
            (setf b22bulge
                    (*
                     (f2cl-lib:fref rwork-%data%
                                    ((f2cl-lib:int-sub
                                      (f2cl-lib:int-add iu2sn i) 1))
                                    ((1 *)) rwork-%offset%)
                     (f2cl-lib:fref b22d-%data% ((f2cl-lib:int-add i 1))
                                    ((1 *)) b22d-%offset%)))
            (setf (f2cl-lib:fref b22d-%data% ((f2cl-lib:int-add i 1)) ((1 *))
                                 b22d-%offset%)
                    (*
                     (f2cl-lib:fref rwork-%data%
                                    ((f2cl-lib:int-sub
                                      (f2cl-lib:int-add iu2cs i) 1))
                                    ((1 *)) rwork-%offset%)
                     (f2cl-lib:fref b22d-%data% ((f2cl-lib:int-add i 1))
                                    ((1 *)) b22d-%offset%)))
           label100011))
        (setf x1
                (+
                 (*
                  (sin
                   (f2cl-lib:fref theta-%data% ((f2cl-lib:int-sub imax 1))
                                  ((1 *)) theta-%offset%))
                  (f2cl-lib:fref b11e-%data% ((f2cl-lib:int-sub imax 1))
                                 ((1 *)) b11e-%offset%))
                 (*
                  (cos
                   (f2cl-lib:fref theta-%data% ((f2cl-lib:int-sub imax 1))
                                  ((1 *)) theta-%offset%))
                  (f2cl-lib:fref b21e-%data% ((f2cl-lib:int-sub imax 1))
                                 ((1 *)) b21e-%offset%))))
        (setf y1
                (+
                 (*
                  (sin
                   (f2cl-lib:fref theta-%data% ((f2cl-lib:int-sub imax 1))
                                  ((1 *)) theta-%offset%))
                  (f2cl-lib:fref b12d-%data% ((f2cl-lib:int-sub imax 1))
                                 ((1 *)) b12d-%offset%))
                 (*
                  (cos
                   (f2cl-lib:fref theta-%data% ((f2cl-lib:int-sub imax 1))
                                  ((1 *)) theta-%offset%))
                  (f2cl-lib:fref b22d-%data% ((f2cl-lib:int-sub imax 1))
                                 ((1 *)) b22d-%offset%))))
        (setf y2
                (+
                 (*
                  (sin
                   (f2cl-lib:fref theta-%data% ((f2cl-lib:int-sub imax 1))
                                  ((1 *)) theta-%offset%))
                  b12bulge)
                 (*
                  (cos
                   (f2cl-lib:fref theta-%data% ((f2cl-lib:int-sub imax 1))
                                  ((1 *)) theta-%offset%))
                  b22bulge)))
        (setf (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub imax 1)) ((1 *))
                             phi-%offset%)
                (f2cl-lib:atan2 (abs x1)
                                (f2cl-lib:fsqrt (+ (expt y1 2) (expt y2 2)))))
        (setf restart12
                (<=
                 (+
                  (expt
                   (f2cl-lib:fref b12d-%data% ((f2cl-lib:int-sub imax 1))
                                  ((1 *)) b12d-%offset%)
                   2)
                  (expt b12bulge 2))
                 (expt thresh 2)))
        (setf restart22
                (<=
                 (+
                  (expt
                   (f2cl-lib:fref b22d-%data% ((f2cl-lib:int-sub imax 1))
                                  ((1 *)) b22d-%offset%)
                   2)
                  (expt b22bulge 2))
                 (expt thresh 2)))
        (cond
         ((and (not restart12) (not restart22))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (dlartgp y2 y1
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iv2tsn imax)
                                                 1 1))
                              ((1 *)) rwork-%offset%)
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iv2tcs imax)
                                                 1 1))
                              ((1 *)) rwork-%offset%)
               r)
            (declare (ignore var-0 var-1))
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv2tsn imax) 1 1))
                                 ((1 *)) rwork-%offset%)
                    var-2)
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv2tcs imax) 1 1))
                                 ((1 *)) rwork-%offset%)
                    var-3)
            (setf r var-4)))
         ((and (not restart12) restart22)
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (dlartgp b12bulge
               (f2cl-lib:fref b12d-%data% ((f2cl-lib:int-sub imax 1)) ((1 *))
                              b12d-%offset%)
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iv2tsn imax)
                                                 1 1))
                              ((1 *)) rwork-%offset%)
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iv2tcs imax)
                                                 1 1))
                              ((1 *)) rwork-%offset%)
               r)
            (declare (ignore var-0 var-1))
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv2tsn imax) 1 1))
                                 ((1 *)) rwork-%offset%)
                    var-2)
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv2tcs imax) 1 1))
                                 ((1 *)) rwork-%offset%)
                    var-3)
            (setf r var-4)))
         ((and restart12 (not restart22))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (dlartgp b22bulge
               (f2cl-lib:fref b22d-%data% ((f2cl-lib:int-sub imax 1)) ((1 *))
                              b22d-%offset%)
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iv2tsn imax)
                                                 1 1))
                              ((1 *)) rwork-%offset%)
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iv2tcs imax)
                                                 1 1))
                              ((1 *)) rwork-%offset%)
               r)
            (declare (ignore var-0 var-1))
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv2tsn imax) 1 1))
                                 ((1 *)) rwork-%offset%)
                    var-2)
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv2tcs imax) 1 1))
                                 ((1 *)) rwork-%offset%)
                    var-3)
            (setf r var-4)))
         ((< nu mu)
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (dlartgs
               (f2cl-lib:fref b12e-%data% ((f2cl-lib:int-sub imax 1)) ((1 *))
                              b12e-%offset%)
               (f2cl-lib:fref b12d-%data% (imax) ((1 *)) b12d-%offset%) nu
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iv2tcs imax)
                                                 1 1))
                              ((1 *)) rwork-%offset%)
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iv2tsn imax)
                                                 1 1))
                              ((1 *)) rwork-%offset%))
            (declare (ignore var-0 var-1 var-2))
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv2tcs imax) 1 1))
                                 ((1 *)) rwork-%offset%)
                    var-3)
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv2tsn imax) 1 1))
                                 ((1 *)) rwork-%offset%)
                    var-4)))
         (t
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (dlartgs
               (f2cl-lib:fref b22e-%data% ((f2cl-lib:int-sub imax 1)) ((1 *))
                              b22e-%offset%)
               (f2cl-lib:fref b22d-%data% (imax) ((1 *)) b22d-%offset%) mu
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iv2tcs imax)
                                                 1 1))
                              ((1 *)) rwork-%offset%)
               (f2cl-lib:fref rwork-%data%
                              ((f2cl-lib:int-sub (f2cl-lib:int-add iv2tsn imax)
                                                 1 1))
                              ((1 *)) rwork-%offset%))
            (declare (ignore var-0 var-1 var-2))
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv2tcs imax) 1 1))
                                 ((1 *)) rwork-%offset%)
                    var-3)
            (setf (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv2tsn imax) 1 1))
                                 ((1 *)) rwork-%offset%)
                    var-4))))
        (setf temp
                (+
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv2tcs imax) 1 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b12e-%data% ((f2cl-lib:int-sub imax 1))
                                 ((1 *)) b12e-%offset%))
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv2tsn imax) 1 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b12d-%data% (imax) ((1 *)) b12d-%offset%))))
        (setf (f2cl-lib:fref b12d-%data% (imax) ((1 *)) b12d-%offset%)
                (-
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv2tcs imax) 1 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b12d-%data% (imax) ((1 *)) b12d-%offset%))
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv2tsn imax) 1 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b12e-%data% ((f2cl-lib:int-sub imax 1))
                                 ((1 *)) b12e-%offset%))))
        (setf (f2cl-lib:fref b12e-%data% ((f2cl-lib:int-sub imax 1)) ((1 *))
                             b12e-%offset%)
                temp)
        (setf temp
                (+
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv2tcs imax) 1 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b22e-%data% ((f2cl-lib:int-sub imax 1))
                                 ((1 *)) b22e-%offset%))
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv2tsn imax) 1 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b22d-%data% (imax) ((1 *)) b22d-%offset%))))
        (setf (f2cl-lib:fref b22d-%data% (imax) ((1 *)) b22d-%offset%)
                (-
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv2tcs imax) 1 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b22d-%data% (imax) ((1 *)) b22d-%offset%))
                 (*
                  (f2cl-lib:fref rwork-%data%
                                 ((f2cl-lib:int-sub
                                   (f2cl-lib:int-add iv2tsn imax) 1 1))
                                 ((1 *)) rwork-%offset%)
                  (f2cl-lib:fref b22e-%data% ((f2cl-lib:int-sub imax 1))
                                 ((1 *)) b22e-%offset%))))
        (setf (f2cl-lib:fref b22e-%data% ((f2cl-lib:int-sub imax 1)) ((1 *))
                             b22e-%offset%)
                temp)
        (cond
         (wantu1
          (cond
           (colmajor
            (zlasr "R" "V" "F" p
             (f2cl-lib:int-add (f2cl-lib:int-sub imax imin) 1)
             (f2cl-lib:array-slice rwork-%data% double-float
                                   ((+ iu1cs imin (f2cl-lib:int-sub 1)))
                                   ((1 *)) rwork-%offset%)
             (f2cl-lib:array-slice rwork-%data% double-float
                                   ((+ iu1sn imin (f2cl-lib:int-sub 1)))
                                   ((1 *)) rwork-%offset%)
             (f2cl-lib:array-slice u1-%data% f2cl-lib:complex16 (1 imin)
                                   ((1 ldu1) (1 *)) u1-%offset%)
             ldu1))
           (t
            (zlasr "L" "V" "F"
             (f2cl-lib:int-add (f2cl-lib:int-sub imax imin) 1) p
             (f2cl-lib:array-slice rwork-%data% double-float
                                   ((+ iu1cs imin (f2cl-lib:int-sub 1)))
                                   ((1 *)) rwork-%offset%)
             (f2cl-lib:array-slice rwork-%data% double-float
                                   ((+ iu1sn imin (f2cl-lib:int-sub 1)))
                                   ((1 *)) rwork-%offset%)
             (f2cl-lib:array-slice u1-%data% f2cl-lib:complex16 (imin 1)
                                   ((1 ldu1) (1 *)) u1-%offset%)
             ldu1)))))
        (cond
         (wantu2
          (cond
           (colmajor
            (zlasr "R" "V" "F" (f2cl-lib:int-sub m p)
             (f2cl-lib:int-add (f2cl-lib:int-sub imax imin) 1)
             (f2cl-lib:array-slice rwork-%data% double-float
                                   ((+ iu2cs imin (f2cl-lib:int-sub 1)))
                                   ((1 *)) rwork-%offset%)
             (f2cl-lib:array-slice rwork-%data% double-float
                                   ((+ iu2sn imin (f2cl-lib:int-sub 1)))
                                   ((1 *)) rwork-%offset%)
             (f2cl-lib:array-slice u2-%data% f2cl-lib:complex16 (1 imin)
                                   ((1 ldu2) (1 *)) u2-%offset%)
             ldu2))
           (t
            (zlasr "L" "V" "F"
             (f2cl-lib:int-add (f2cl-lib:int-sub imax imin) 1)
             (f2cl-lib:int-sub m p)
             (f2cl-lib:array-slice rwork-%data% double-float
                                   ((+ iu2cs imin (f2cl-lib:int-sub 1)))
                                   ((1 *)) rwork-%offset%)
             (f2cl-lib:array-slice rwork-%data% double-float
                                   ((+ iu2sn imin (f2cl-lib:int-sub 1)))
                                   ((1 *)) rwork-%offset%)
             (f2cl-lib:array-slice u2-%data% f2cl-lib:complex16 (imin 1)
                                   ((1 ldu2) (1 *)) u2-%offset%)
             ldu2)))))
        (cond
         (wantv1t
          (cond
           (colmajor
            (zlasr "L" "V" "F"
             (f2cl-lib:int-add (f2cl-lib:int-sub imax imin) 1) q
             (f2cl-lib:array-slice rwork-%data% double-float
                                   ((+ iv1tcs imin (f2cl-lib:int-sub 1)))
                                   ((1 *)) rwork-%offset%)
             (f2cl-lib:array-slice rwork-%data% double-float
                                   ((+ iv1tsn imin (f2cl-lib:int-sub 1)))
                                   ((1 *)) rwork-%offset%)
             (f2cl-lib:array-slice v1t-%data% f2cl-lib:complex16 (imin 1)
                                   ((1 ldv1t) (1 *)) v1t-%offset%)
             ldv1t))
           (t
            (zlasr "R" "V" "F" q
             (f2cl-lib:int-add (f2cl-lib:int-sub imax imin) 1)
             (f2cl-lib:array-slice rwork-%data% double-float
                                   ((+ iv1tcs imin (f2cl-lib:int-sub 1)))
                                   ((1 *)) rwork-%offset%)
             (f2cl-lib:array-slice rwork-%data% double-float
                                   ((+ iv1tsn imin (f2cl-lib:int-sub 1)))
                                   ((1 *)) rwork-%offset%)
             (f2cl-lib:array-slice v1t-%data% f2cl-lib:complex16 (1 imin)
                                   ((1 ldv1t) (1 *)) v1t-%offset%)
             ldv1t)))))
        (cond
         (wantv2t
          (cond
           (colmajor
            (zlasr "L" "V" "F"
             (f2cl-lib:int-add (f2cl-lib:int-sub imax imin) 1)
             (f2cl-lib:int-sub m q)
             (f2cl-lib:array-slice rwork-%data% double-float
                                   ((+ iv2tcs imin (f2cl-lib:int-sub 1)))
                                   ((1 *)) rwork-%offset%)
             (f2cl-lib:array-slice rwork-%data% double-float
                                   ((+ iv2tsn imin (f2cl-lib:int-sub 1)))
                                   ((1 *)) rwork-%offset%)
             (f2cl-lib:array-slice v2t-%data% f2cl-lib:complex16 (imin 1)
                                   ((1 ldv2t) (1 *)) v2t-%offset%)
             ldv2t))
           (t
            (zlasr "R" "V" "F" (f2cl-lib:int-sub m q)
             (f2cl-lib:int-add (f2cl-lib:int-sub imax imin) 1)
             (f2cl-lib:array-slice rwork-%data% double-float
                                   ((+ iv2tcs imin (f2cl-lib:int-sub 1)))
                                   ((1 *)) rwork-%offset%)
             (f2cl-lib:array-slice rwork-%data% double-float
                                   ((+ iv2tsn imin (f2cl-lib:int-sub 1)))
                                   ((1 *)) rwork-%offset%)
             (f2cl-lib:array-slice v2t-%data% f2cl-lib:complex16 (1 imin)
                                   ((1 ldv2t) (1 *)) v2t-%offset%)
             ldv2t)))))
        (cond
         ((>
           (+
            (f2cl-lib:fref b11e ((f2cl-lib:int-add imax (f2cl-lib:int-sub 1)))
                           ((1 *)))
            (f2cl-lib:fref b21e ((f2cl-lib:int-add imax (f2cl-lib:int-sub 1)))
                           ((1 *))))
           0)
          (setf (f2cl-lib:fref b11d-%data% (imax) ((1 *)) b11d-%offset%)
                  (- (f2cl-lib:fref b11d-%data% (imax) ((1 *)) b11d-%offset%)))
          (setf (f2cl-lib:fref b21d-%data% (imax) ((1 *)) b21d-%offset%)
                  (- (f2cl-lib:fref b21d-%data% (imax) ((1 *)) b21d-%offset%)))
          (cond
           (wantv1t
            (cond
             (colmajor
              (zscal q negonecomplex
               (f2cl-lib:array-slice v1t-%data% f2cl-lib:complex16 (imax 1)
                                     ((1 ldv1t) (1 *)) v1t-%offset%)
               ldv1t))
             (t
              (zscal q negonecomplex
               (f2cl-lib:array-slice v1t-%data% f2cl-lib:complex16 (1 imax)
                                     ((1 ldv1t) (1 *)) v1t-%offset%)
               1)))))))
        (setf x1
                (+
                 (*
                  (cos
                   (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub imax 1))
                                  ((1 *)) phi-%offset%))
                  (f2cl-lib:fref b11d-%data% (imax) ((1 *)) b11d-%offset%))
                 (*
                  (sin
                   (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub imax 1))
                                  ((1 *)) phi-%offset%))
                  (f2cl-lib:fref b12e-%data% ((f2cl-lib:int-sub imax 1))
                                 ((1 *)) b12e-%offset%))))
        (setf y1
                (+
                 (*
                  (cos
                   (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub imax 1))
                                  ((1 *)) phi-%offset%))
                  (f2cl-lib:fref b21d-%data% (imax) ((1 *)) b21d-%offset%))
                 (*
                  (sin
                   (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub imax 1))
                                  ((1 *)) phi-%offset%))
                  (f2cl-lib:fref b22e-%data% ((f2cl-lib:int-sub imax 1))
                                 ((1 *)) b22e-%offset%))))
        (setf (f2cl-lib:fref theta-%data% (imax) ((1 *)) theta-%offset%)
                (f2cl-lib:atan2 (abs y1) (abs x1)))
        (cond
         ((<
           (+ (f2cl-lib:fref b11d (imax) ((1 *)))
              (f2cl-lib:fref b12e
                             ((f2cl-lib:int-add imax (f2cl-lib:int-sub 1)))
                             ((1 *))))
           0)
          (setf (f2cl-lib:fref b12d-%data% (imax) ((1 *)) b12d-%offset%)
                  (- (f2cl-lib:fref b12d-%data% (imax) ((1 *)) b12d-%offset%)))
          (cond
           (wantu1
            (cond
             (colmajor
              (zscal p negonecomplex
               (f2cl-lib:array-slice u1-%data% f2cl-lib:complex16 (1 imax)
                                     ((1 ldu1) (1 *)) u1-%offset%)
               1))
             (t
              (zscal p negonecomplex
               (f2cl-lib:array-slice u1-%data% f2cl-lib:complex16 (imax 1)
                                     ((1 ldu1) (1 *)) u1-%offset%)
               ldu1)))))))
        (cond
         ((>
           (+ (f2cl-lib:fref b21d (imax) ((1 *)))
              (f2cl-lib:fref b22e
                             ((f2cl-lib:int-add imax (f2cl-lib:int-sub 1)))
                             ((1 *))))
           0)
          (setf (f2cl-lib:fref b22d-%data% (imax) ((1 *)) b22d-%offset%)
                  (- (f2cl-lib:fref b22d-%data% (imax) ((1 *)) b22d-%offset%)))
          (cond
           (wantu2
            (cond
             (colmajor
              (zscal (f2cl-lib:int-sub m p) negonecomplex
               (f2cl-lib:array-slice u2-%data% f2cl-lib:complex16 (1 imax)
                                     ((1 ldu2) (1 *)) u2-%offset%)
               1))
             (t
              (zscal (f2cl-lib:int-sub m p) negonecomplex
               (f2cl-lib:array-slice u2-%data% f2cl-lib:complex16 (imax 1)
                                     ((1 ldu2) (1 *)) u2-%offset%)
               ldu2)))))))
        (cond
         ((<
           (+ (f2cl-lib:fref b12d (imax) ((1 *)))
              (f2cl-lib:fref b22d (imax) ((1 *))))
           0)
          (cond
           (wantv2t
            (cond
             (colmajor
              (zscal (f2cl-lib:int-sub m q) negonecomplex
               (f2cl-lib:array-slice v2t-%data% f2cl-lib:complex16 (imax 1)
                                     ((1 ldv2t) (1 *)) v2t-%offset%)
               ldv2t))
             (t
              (zscal (f2cl-lib:int-sub m q) negonecomplex
               (f2cl-lib:array-slice v2t-%data% f2cl-lib:complex16 (1 imax)
                                     ((1 ldv2t) (1 *)) v2t-%offset%)
               1)))))))
        (f2cl-lib:fdo (i imin (f2cl-lib:int-add i 1))
                      ((> i imax) nil)
          (tagbody
            (cond
             ((< (f2cl-lib:fref theta (i) ((1 *))) thresh)
              (setf (f2cl-lib:fref theta-%data% (i) ((1 *)) theta-%offset%)
                      zero))
             ((> (f2cl-lib:fref theta (i) ((1 *))) (+ piover2 (- thresh)))
              (setf (f2cl-lib:fref theta-%data% (i) ((1 *)) theta-%offset%)
                      piover2)))
           label100012))
        (f2cl-lib:fdo (i imin (f2cl-lib:int-add i 1))
                      ((> i (f2cl-lib:int-add imax (f2cl-lib:int-sub 1))) nil)
          (tagbody
            (cond
             ((< (f2cl-lib:fref phi (i) ((1 *))) thresh)
              (setf (f2cl-lib:fref phi-%data% (i) ((1 *)) phi-%offset%) zero))
             ((> (f2cl-lib:fref phi (i) ((1 *))) (+ piover2 (- thresh)))
              (setf (f2cl-lib:fref phi-%data% (i) ((1 *)) phi-%offset%)
                      piover2)))
           label100013))
       label100014
        (if (not
             (and (> imax 1)
                  (=
                   (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub imax 1))
                                  ((1 *)) phi-%offset%)
                   zero)))
            (go label100015))
        (setf imax (f2cl-lib:int-sub imax 1))
        (go label100014)
       label100015
        (if (> imin (f2cl-lib:int-sub imax 1))
            (setf imin (f2cl-lib:int-sub imax 1)))
       label100016
        (if (not
             (and (> imin 1)
                  (/=
                   (f2cl-lib:fref phi-%data% ((f2cl-lib:int-sub imin 1))
                                  ((1 *)) phi-%offset%)
                   zero)))
            (go label100017))
        (setf imin (f2cl-lib:int-sub imin 1))
        (go label100016)
       label100017
        (go label100006)
       label100007
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i q) nil)
          (tagbody
            (setf mini i)
            (setf thetamin
                    (f2cl-lib:fref theta-%data% (i) ((1 *)) theta-%offset%))
            (f2cl-lib:fdo (j (f2cl-lib:int-add i 1) (f2cl-lib:int-add j 1))
                          ((> j q) nil)
              (tagbody
                (cond
                 ((< (f2cl-lib:fref theta (j) ((1 *))) thetamin) (setf mini j)
                  (setf thetamin
                          (f2cl-lib:fref theta-%data% (j) ((1 *))
                                         theta-%offset%))))
               label100019))
            (cond
             ((/= mini i)
              (setf (f2cl-lib:fref theta-%data% (mini) ((1 *)) theta-%offset%)
                      (f2cl-lib:fref theta-%data% (i) ((1 *)) theta-%offset%))
              (setf (f2cl-lib:fref theta-%data% (i) ((1 *)) theta-%offset%)
                      thetamin)
              (cond
               (colmajor
                (if wantu1
                    (zswap p
                     (f2cl-lib:array-slice u1-%data% f2cl-lib:complex16 (1 i)
                                           ((1 ldu1) (1 *)) u1-%offset%)
                     1
                     (f2cl-lib:array-slice u1-%data% f2cl-lib:complex16
                                           (1 mini) ((1 ldu1) (1 *))
                                           u1-%offset%)
                     1))
                (if wantu2
                    (zswap (f2cl-lib:int-sub m p)
                     (f2cl-lib:array-slice u2-%data% f2cl-lib:complex16 (1 i)
                                           ((1 ldu2) (1 *)) u2-%offset%)
                     1
                     (f2cl-lib:array-slice u2-%data% f2cl-lib:complex16
                                           (1 mini) ((1 ldu2) (1 *))
                                           u2-%offset%)
                     1))
                (if wantv1t
                    (zswap q
                     (f2cl-lib:array-slice v1t-%data% f2cl-lib:complex16 (i 1)
                                           ((1 ldv1t) (1 *)) v1t-%offset%)
                     ldv1t
                     (f2cl-lib:array-slice v1t-%data% f2cl-lib:complex16
                                           (mini 1) ((1 ldv1t) (1 *))
                                           v1t-%offset%)
                     ldv1t))
                (if wantv2t
                    (zswap (f2cl-lib:int-sub m q)
                     (f2cl-lib:array-slice v2t-%data% f2cl-lib:complex16 (i 1)
                                           ((1 ldv2t) (1 *)) v2t-%offset%)
                     ldv2t
                     (f2cl-lib:array-slice v2t-%data% f2cl-lib:complex16
                                           (mini 1) ((1 ldv2t) (1 *))
                                           v2t-%offset%)
                     ldv2t)))
               (t
                (if wantu1
                    (zswap p
                     (f2cl-lib:array-slice u1-%data% f2cl-lib:complex16 (i 1)
                                           ((1 ldu1) (1 *)) u1-%offset%)
                     ldu1
                     (f2cl-lib:array-slice u1-%data% f2cl-lib:complex16
                                           (mini 1) ((1 ldu1) (1 *))
                                           u1-%offset%)
                     ldu1))
                (if wantu2
                    (zswap (f2cl-lib:int-sub m p)
                     (f2cl-lib:array-slice u2-%data% f2cl-lib:complex16 (i 1)
                                           ((1 ldu2) (1 *)) u2-%offset%)
                     ldu2
                     (f2cl-lib:array-slice u2-%data% f2cl-lib:complex16
                                           (mini 1) ((1 ldu2) (1 *))
                                           u2-%offset%)
                     ldu2))
                (if wantv1t
                    (zswap q
                     (f2cl-lib:array-slice v1t-%data% f2cl-lib:complex16 (1 i)
                                           ((1 ldv1t) (1 *)) v1t-%offset%)
                     1
                     (f2cl-lib:array-slice v1t-%data% f2cl-lib:complex16
                                           (1 mini) ((1 ldv1t) (1 *))
                                           v1t-%offset%)
                     1))
                (if wantv2t
                    (zswap (f2cl-lib:int-sub m q)
                     (f2cl-lib:array-slice v2t-%data% f2cl-lib:complex16 (1 i)
                                           ((1 ldv2t) (1 *)) v2t-%offset%)
                     1
                     (f2cl-lib:array-slice v2t-%data% f2cl-lib:complex16
                                           (1 mini) ((1 ldv2t) (1 *))
                                           v2t-%offset%)
                     1))))))
           label100018))
        (go end_label)
       end_label
        (return
         (values nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
                 nil nil nil nil nil nil nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zbbcsd
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((string 1) (string 1) (string 1)
                                              (string 1) (string 1)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (array double-float (*))
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
                                              (array double-float (*))
                                              (array double-float (*))
                                              (array double-float (*))
                                              (array double-float (*))
                                              (array double-float (*))
                                              (array double-float (*))
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4)
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil nil nil nil
                                              nil nil nil nil nil nil nil nil
                                              nil nil nil nil nil nil nil nil
                                              nil nil nil nil
                                              fortran-to-lisp::info)
                                            :calls
                                            '(fortran-to-lisp::zswap
                                              fortran-to-lisp::zscal
                                              fortran-to-lisp::zlasr
                                              fortran-to-lisp::xerbla
                                              fortran-to-lisp::lsame
                                              fortran-to-lisp::dlartgp
                                              fortran-to-lisp::dlartgs
                                              fortran-to-lisp::dlas2
                                              fortran-to-lisp::dlamch))))

