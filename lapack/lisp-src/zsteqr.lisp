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


(let* ((zero 0.0d0)
       (one 1.0d0)
       (two 2.0d0)
       (three 3.0d0)
       (czero (f2cl-lib:cmplx 0.0d0 0.0d0))
       (cone (f2cl-lib:cmplx 1.0d0 0.0d0))
       (maxit 30))
  (declare (type (double-float 0.0d0 0.0d0) zero)
           (type (double-float 1.0d0 1.0d0) one)
           (type (double-float 2.0d0 2.0d0) two)
           (type (double-float 3.0d0 3.0d0) three)
           (type (f2cl-lib:complex16) czero)
           (type (f2cl-lib:complex16) cone)
           (type (f2cl-lib:integer4 30 30) maxit)
           (ignorable zero one two three czero cone maxit))
  (defun zsteqr (compz n d e z ldz work info)
    (declare (type (array f2cl-lib:complex16 (*)) z)
             (type (array double-float (*)) work e d)
             (type (f2cl-lib:integer4) info ldz n)
             (type (string 1) compz))
    (f2cl-lib:with-multi-array-data
        ((compz character compz-%data% compz-%offset%)
         (d double-float d-%data% d-%offset%)
         (e double-float e-%data% e-%offset%)
         (work double-float work-%data% work-%offset%)
         (z f2cl-lib:complex16 z-%data% z-%offset%))
      (prog ((anorm 0.0d0) (b 0.0d0) (c 0.0d0) (eps 0.0d0) (eps2 0.0d0)
             (f 0.0d0) (g 0.0d0) (p 0.0d0) (r 0.0d0) (rt1 0.0d0) (rt2 0.0d0)
             (s 0.0d0) (safmax 0.0d0) (safmin 0.0d0) (ssfmax 0.0d0)
             (ssfmin 0.0d0) (tst 0.0d0) (i 0) (icompz 0) (ii 0) (iscale 0)
             (j 0) (jtot 0) (k 0) (l 0) (l1 0) (lend 0) (lendm1 0) (lendp1 0)
             (lendsv 0) (lm1 0) (lsv 0) (m 0) (mm 0) (mm1 0) (nm1 0)
             (nmaxit 0))
        (declare
         (type (double-float) anorm b c eps eps2 f g p r rt1 rt2 s safmax
          safmin ssfmax ssfmin tst)
         (type (f2cl-lib:integer4) i icompz ii iscale j jtot k l l1 lend lendm1
          lendp1 lendsv lm1 lsv m mm mm1 nm1 nmaxit))
        (setf info 0)
        (cond ((lsame compz "N") (setf icompz 0))
              ((lsame compz "V") (setf icompz 1))
              ((lsame compz "I") (setf icompz 2)) (t (setf icompz -1)))
        (cond ((< icompz 0) (setf info -1)) ((< n 0) (setf info -2))
              ((or (< ldz 1)
                   (and (> icompz 0)
                        (< ldz
                           (max (the f2cl-lib:integer4 1)
                                (the f2cl-lib:integer4 n)))))
               (setf info -6)))
        (cond
         ((/= info 0) (xerbla "ZSTEQR" (f2cl-lib:int-sub info))
          (go end_label)))
        (if (= n 0)
            (go end_label))
        (cond
         ((= n 1)
          (if (= icompz 2)
              (setf (f2cl-lib:fref z-%data% (1 1) ((1 ldz) (1 *)) z-%offset%)
                      cone))
          (go end_label)))
        (setf eps (dlamch "E"))
        (setf eps2 (expt eps 2))
        (setf safmin (dlamch "S"))
        (setf safmax (/ one safmin))
        (setf ssfmax (/ (f2cl-lib:fsqrt safmax) three))
        (setf ssfmin (/ (f2cl-lib:fsqrt safmin) eps2))
        (if (= icompz 2)
            (zlaset "F" n n czero cone z ldz))
        (setf nmaxit (f2cl-lib:int-mul n maxit))
        (setf jtot 0)
        (setf l1 1)
        (setf nm1 (f2cl-lib:int-sub n 1))
       label10
        (if (> l1 n)
            (go label160))
        (if (> l1 1)
            (setf (f2cl-lib:fref e-%data% ((f2cl-lib:int-sub l1 1)) ((1 *))
                                 e-%offset%)
                    zero))
        (cond
         ((<= l1 nm1)
          (f2cl-lib:fdo (m l1 (f2cl-lib:int-add m 1))
                        ((> m nm1) nil)
            (tagbody
              (setf tst (abs (f2cl-lib:fref e-%data% (m) ((1 *)) e-%offset%)))
              (if (= tst zero)
                  (go label30))
              (cond
               ((<= tst
                    (* (f2cl-lib:fsqrt (abs (f2cl-lib:fref d (m) ((1 *)))))
                       (f2cl-lib:fsqrt
                        (abs
                         (f2cl-lib:fref d ((f2cl-lib:int-add m 1)) ((1 *)))))
                       eps))
                (setf (f2cl-lib:fref e-%data% (m) ((1 *)) e-%offset%) zero)
                (go label30)))
             label20))))
        (setf m n)
       label30
        (setf l l1)
        (setf lsv l)
        (setf lend m)
        (setf lendsv lend)
        (setf l1 (f2cl-lib:int-add m 1))
        (if (= lend l)
            (go label10))
        (setf anorm
                (dlanst "I" (f2cl-lib:int-add (f2cl-lib:int-sub lend l) 1)
                 (f2cl-lib:array-slice d-%data% double-float (l) ((1 *))
                                       d-%offset%)
                 (f2cl-lib:array-slice e-%data% double-float (l) ((1 *))
                                       e-%offset%)))
        (setf iscale 0)
        (if (= anorm zero)
            (go label10))
        (cond
         ((> anorm ssfmax) (setf iscale 1)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
              (dlascl "G" 0 0 anorm ssfmax
               (f2cl-lib:int-add (f2cl-lib:int-sub lend l) 1) 1
               (f2cl-lib:array-slice d-%data% double-float (l) ((1 *))
                                     d-%offset%)
               n info)
            (declare
             (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8))
            (setf info var-9))
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
              (dlascl "G" 0 0 anorm ssfmax (f2cl-lib:int-sub lend l) 1
               (f2cl-lib:array-slice e-%data% double-float (l) ((1 *))
                                     e-%offset%)
               n info)
            (declare
             (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8))
            (setf info var-9)))
         ((< anorm ssfmin) (setf iscale 2)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
              (dlascl "G" 0 0 anorm ssfmin
               (f2cl-lib:int-add (f2cl-lib:int-sub lend l) 1) 1
               (f2cl-lib:array-slice d-%data% double-float (l) ((1 *))
                                     d-%offset%)
               n info)
            (declare
             (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8))
            (setf info var-9))
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
              (dlascl "G" 0 0 anorm ssfmin (f2cl-lib:int-sub lend l) 1
               (f2cl-lib:array-slice e-%data% double-float (l) ((1 *))
                                     e-%offset%)
               n info)
            (declare
             (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8))
            (setf info var-9))))
        (cond
         ((< (abs (f2cl-lib:fref d (lend) ((1 *))))
             (abs (f2cl-lib:fref d (l) ((1 *)))))
          (setf lend lsv) (setf l lendsv)))
        (cond
         ((> lend l)
          (tagbody
           label40
            (cond
             ((/= l lend) (setf lendm1 (f2cl-lib:int-sub lend 1))
              (f2cl-lib:fdo (m l (f2cl-lib:int-add m 1))
                            ((> m lendm1) nil)
                (tagbody
                  (setf tst
                          (expt
                           (abs
                            (f2cl-lib:fref e-%data% (m) ((1 *)) e-%offset%))
                           2))
                  (if (<= tst
                          (+
                           (* eps2
                              (abs
                               (f2cl-lib:fref d-%data% (m) ((1 *)) d-%offset%))
                              (abs
                               (f2cl-lib:fref d-%data% ((f2cl-lib:int-add m 1))
                                              ((1 *)) d-%offset%)))
                           safmin))
                      (go label60))
                 label50))))
            (setf m lend)
           label60
            (if (< m lend)
                (setf (f2cl-lib:fref e-%data% (m) ((1 *)) e-%offset%) zero))
            (setf p (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%))
            (if (= m l)
                (go label80))
            (cond
             ((= m (f2cl-lib:int-add l 1))
              (cond
               ((> icompz 0)
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                    (dlaev2 (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%)
                     (f2cl-lib:fref e-%data% (l) ((1 *)) e-%offset%)
                     (f2cl-lib:fref d-%data% ((f2cl-lib:int-add l 1)) ((1 *))
                                    d-%offset%)
                     rt1 rt2 c s)
                  (declare (ignore var-0 var-1 var-2))
                  (setf rt1 var-3)
                  (setf rt2 var-4)
                  (setf c var-5)
                  (setf s var-6))
                (setf (f2cl-lib:fref work-%data% (l) ((1 *)) work-%offset%) c)
                (setf (f2cl-lib:fref work-%data%
                                     ((f2cl-lib:int-add (f2cl-lib:int-sub n 1)
                                                        l))
                                     ((1 *)) work-%offset%)
                        s)
                (zlasr "R" "V" "B" n 2
                 (f2cl-lib:array-slice work-%data% double-float (l) ((1 *))
                                       work-%offset%)
                 (f2cl-lib:array-slice work-%data% double-float
                                       ((+ n (f2cl-lib:int-sub 1) l)) ((1 *))
                                       work-%offset%)
                 (f2cl-lib:array-slice z-%data% f2cl-lib:complex16 (1 l)
                                       ((1 ldz) (1 *)) z-%offset%)
                 ldz))
               (t
                (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                    (dlae2 (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%)
                     (f2cl-lib:fref e-%data% (l) ((1 *)) e-%offset%)
                     (f2cl-lib:fref d-%data% ((f2cl-lib:int-add l 1)) ((1 *))
                                    d-%offset%)
                     rt1 rt2)
                  (declare (ignore var-0 var-1 var-2))
                  (setf rt1 var-3)
                  (setf rt2 var-4))))
              (setf (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%) rt1)
              (setf (f2cl-lib:fref d-%data% ((f2cl-lib:int-add l 1)) ((1 *))
                                   d-%offset%)
                      rt2)
              (setf (f2cl-lib:fref e-%data% (l) ((1 *)) e-%offset%) zero)
              (setf l (f2cl-lib:int-add l 2))
              (if (<= l lend)
                  (go label40))
              (go label140)))
            (if (= jtot nmaxit)
                (go label140))
            (setf jtot (f2cl-lib:int-add jtot 1))
            (setf g
                    (/
                     (-
                      (f2cl-lib:fref d-%data% ((f2cl-lib:int-add l 1)) ((1 *))
                                     d-%offset%)
                      p)
                     (* two (f2cl-lib:fref e-%data% (l) ((1 *)) e-%offset%))))
            (setf r (dlapy2 g one))
            (setf g
                    (+ (- (f2cl-lib:fref d-%data% (m) ((1 *)) d-%offset%) p)
                       (/ (f2cl-lib:fref e-%data% (l) ((1 *)) e-%offset%)
                          (+ g (f2cl-lib:sign r g)))))
            (setf s one)
            (setf c one)
            (setf p zero)
            (setf mm1 (f2cl-lib:int-sub m 1))
            (f2cl-lib:fdo (i mm1 (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                          ((> i l) nil)
              (tagbody
                (setf f (* s (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%)))
                (setf b (* c (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%)))
                (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                    (dlartg g f c s r)
                  (declare (ignore var-0 var-1))
                  (setf c var-2)
                  (setf s var-3)
                  (setf r var-4))
                (if (/= i (f2cl-lib:int-sub m 1))
                    (setf (f2cl-lib:fref e-%data% ((f2cl-lib:int-add i 1))
                                         ((1 *)) e-%offset%)
                            r))
                (setf g
                        (-
                         (f2cl-lib:fref d-%data% ((f2cl-lib:int-add i 1))
                                        ((1 *)) d-%offset%)
                         p))
                (setf r
                        (+
                         (*
                          (- (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%) g)
                          s)
                         (* two c b)))
                (setf p (* s r))
                (setf (f2cl-lib:fref d-%data% ((f2cl-lib:int-add i 1)) ((1 *))
                                     d-%offset%)
                        (+ g p))
                (setf g (- (* c r) b))
                (cond
                 ((> icompz 0)
                  (setf (f2cl-lib:fref work-%data% (i) ((1 *)) work-%offset%)
                          c)
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add
                                         (f2cl-lib:int-sub n 1) i))
                                       ((1 *)) work-%offset%)
                          (- s))))
               label70))
            (cond
             ((> icompz 0)
              (setf mm (f2cl-lib:int-add (f2cl-lib:int-sub m l) 1))
              (zlasr "R" "V" "B" n mm
               (f2cl-lib:array-slice work-%data% double-float (l) ((1 *))
                                     work-%offset%)
               (f2cl-lib:array-slice work-%data% double-float
                                     ((+ n (f2cl-lib:int-sub 1) l)) ((1 *))
                                     work-%offset%)
               (f2cl-lib:array-slice z-%data% f2cl-lib:complex16 (1 l)
                                     ((1 ldz) (1 *)) z-%offset%)
               ldz)))
            (setf (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%)
                    (- (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%) p))
            (setf (f2cl-lib:fref e-%data% (l) ((1 *)) e-%offset%) g)
            (go label40)
           label80
            (setf (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%) p)
            (setf l (f2cl-lib:int-add l 1))
            (if (<= l lend)
                (go label40))
            (go label140)))
         (t
          (tagbody
           label90
            (cond
             ((/= l lend) (setf lendp1 (f2cl-lib:int-add lend 1))
              (f2cl-lib:fdo (m l (f2cl-lib:int-add m (f2cl-lib:int-sub 1)))
                            ((> m lendp1) nil)
                (tagbody
                  (setf tst
                          (expt
                           (abs
                            (f2cl-lib:fref e-%data% ((f2cl-lib:int-sub m 1))
                                           ((1 *)) e-%offset%))
                           2))
                  (if (<= tst
                          (+
                           (* eps2
                              (abs
                               (f2cl-lib:fref d-%data% (m) ((1 *)) d-%offset%))
                              (abs
                               (f2cl-lib:fref d-%data% ((f2cl-lib:int-sub m 1))
                                              ((1 *)) d-%offset%)))
                           safmin))
                      (go label110))
                 label100))))
            (setf m lend)
           label110
            (if (> m lend)
                (setf (f2cl-lib:fref e-%data% ((f2cl-lib:int-sub m 1)) ((1 *))
                                     e-%offset%)
                        zero))
            (setf p (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%))
            (if (= m l)
                (go label130))
            (cond
             ((= m (f2cl-lib:int-add l (f2cl-lib:int-sub 1)))
              (cond
               ((> icompz 0)
                (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                    (dlaev2
                     (f2cl-lib:fref d-%data% ((f2cl-lib:int-sub l 1)) ((1 *))
                                    d-%offset%)
                     (f2cl-lib:fref e-%data% ((f2cl-lib:int-sub l 1)) ((1 *))
                                    e-%offset%)
                     (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%) rt1 rt2 c
                     s)
                  (declare (ignore var-0 var-1 var-2))
                  (setf rt1 var-3)
                  (setf rt2 var-4)
                  (setf c var-5)
                  (setf s var-6))
                (setf (f2cl-lib:fref work-%data% (m) ((1 *)) work-%offset%) c)
                (setf (f2cl-lib:fref work-%data%
                                     ((f2cl-lib:int-add (f2cl-lib:int-sub n 1)
                                                        m))
                                     ((1 *)) work-%offset%)
                        s)
                (zlasr "R" "V" "F" n 2
                 (f2cl-lib:array-slice work-%data% double-float (m) ((1 *))
                                       work-%offset%)
                 (f2cl-lib:array-slice work-%data% double-float
                                       ((+ n (f2cl-lib:int-sub 1) m)) ((1 *))
                                       work-%offset%)
                 (f2cl-lib:array-slice z-%data% f2cl-lib:complex16
                                       (1 (f2cl-lib:int-sub l 1))
                                       ((1 ldz) (1 *)) z-%offset%)
                 ldz))
               (t
                (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                    (dlae2
                     (f2cl-lib:fref d-%data% ((f2cl-lib:int-sub l 1)) ((1 *))
                                    d-%offset%)
                     (f2cl-lib:fref e-%data% ((f2cl-lib:int-sub l 1)) ((1 *))
                                    e-%offset%)
                     (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%) rt1 rt2)
                  (declare (ignore var-0 var-1 var-2))
                  (setf rt1 var-3)
                  (setf rt2 var-4))))
              (setf (f2cl-lib:fref d-%data% ((f2cl-lib:int-sub l 1)) ((1 *))
                                   d-%offset%)
                      rt1)
              (setf (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%) rt2)
              (setf (f2cl-lib:fref e-%data% ((f2cl-lib:int-sub l 1)) ((1 *))
                                   e-%offset%)
                      zero)
              (setf l (f2cl-lib:int-sub l 2))
              (if (>= l lend)
                  (go label90))
              (go label140)))
            (if (= jtot nmaxit)
                (go label140))
            (setf jtot (f2cl-lib:int-add jtot 1))
            (setf g
                    (/
                     (-
                      (f2cl-lib:fref d-%data% ((f2cl-lib:int-sub l 1)) ((1 *))
                                     d-%offset%)
                      p)
                     (* two
                        (f2cl-lib:fref e-%data% ((f2cl-lib:int-sub l 1))
                                       ((1 *)) e-%offset%))))
            (setf r (dlapy2 g one))
            (setf g
                    (+ (- (f2cl-lib:fref d-%data% (m) ((1 *)) d-%offset%) p)
                       (/
                        (f2cl-lib:fref e-%data% ((f2cl-lib:int-sub l 1))
                                       ((1 *)) e-%offset%)
                        (+ g (f2cl-lib:sign r g)))))
            (setf s one)
            (setf c one)
            (setf p zero)
            (setf lm1 (f2cl-lib:int-sub l 1))
            (f2cl-lib:fdo (i m (f2cl-lib:int-add i 1))
                          ((> i lm1) nil)
              (tagbody
                (setf f (* s (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%)))
                (setf b (* c (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%)))
                (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                    (dlartg g f c s r)
                  (declare (ignore var-0 var-1))
                  (setf c var-2)
                  (setf s var-3)
                  (setf r var-4))
                (if (/= i m)
                    (setf (f2cl-lib:fref e-%data% ((f2cl-lib:int-sub i 1))
                                         ((1 *)) e-%offset%)
                            r))
                (setf g (- (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%) p))
                (setf r
                        (+
                         (*
                          (-
                           (f2cl-lib:fref d-%data% ((f2cl-lib:int-add i 1))
                                          ((1 *)) d-%offset%)
                           g)
                          s)
                         (* two c b)))
                (setf p (* s r))
                (setf (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%) (+ g p))
                (setf g (- (* c r) b))
                (cond
                 ((> icompz 0)
                  (setf (f2cl-lib:fref work-%data% (i) ((1 *)) work-%offset%)
                          c)
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add
                                         (f2cl-lib:int-sub n 1) i))
                                       ((1 *)) work-%offset%)
                          s)))
               label120))
            (cond
             ((> icompz 0)
              (setf mm (f2cl-lib:int-add (f2cl-lib:int-sub l m) 1))
              (zlasr "R" "V" "F" n mm
               (f2cl-lib:array-slice work-%data% double-float (m) ((1 *))
                                     work-%offset%)
               (f2cl-lib:array-slice work-%data% double-float
                                     ((+ n (f2cl-lib:int-sub 1) m)) ((1 *))
                                     work-%offset%)
               (f2cl-lib:array-slice z-%data% f2cl-lib:complex16 (1 m)
                                     ((1 ldz) (1 *)) z-%offset%)
               ldz)))
            (setf (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%)
                    (- (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%) p))
            (setf (f2cl-lib:fref e-%data% (lm1) ((1 *)) e-%offset%) g)
            (go label90)
           label130
            (setf (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%) p)
            (setf l (f2cl-lib:int-sub l 1))
            (if (>= l lend)
                (go label90))
            (go label140))))
       label140
        (cond
         ((= iscale 1)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
              (dlascl "G" 0 0 ssfmax anorm
               (f2cl-lib:int-add (f2cl-lib:int-sub lendsv lsv) 1) 1
               (f2cl-lib:array-slice d-%data% double-float (lsv) ((1 *))
                                     d-%offset%)
               n info)
            (declare
             (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8))
            (setf info var-9))
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
              (dlascl "G" 0 0 ssfmax anorm (f2cl-lib:int-sub lendsv lsv) 1
               (f2cl-lib:array-slice e-%data% double-float (lsv) ((1 *))
                                     e-%offset%)
               n info)
            (declare
             (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8))
            (setf info var-9)))
         ((= iscale 2)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
              (dlascl "G" 0 0 ssfmin anorm
               (f2cl-lib:int-add (f2cl-lib:int-sub lendsv lsv) 1) 1
               (f2cl-lib:array-slice d-%data% double-float (lsv) ((1 *))
                                     d-%offset%)
               n info)
            (declare
             (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8))
            (setf info var-9))
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
              (dlascl "G" 0 0 ssfmin anorm (f2cl-lib:int-sub lendsv lsv) 1
               (f2cl-lib:array-slice e-%data% double-float (lsv) ((1 *))
                                     e-%offset%)
               n info)
            (declare
             (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8))
            (setf info var-9))))
        (cond
         ((= jtot nmaxit)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i (f2cl-lib:int-add n (f2cl-lib:int-sub 1))) nil)
            (tagbody
              (if (/= (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%) zero)
                  (setf info (f2cl-lib:int-add info 1)))
             label150))
          (go end_label)))
        (go label10)
       label160
        (cond
         ((= icompz 0)
          (multiple-value-bind (var-0 var-1 var-2 var-3)
              (dlasrt "I" n d info)
            (declare (ignore var-0 var-1 var-2))
            (setf info var-3)))
         (t
          (f2cl-lib:fdo (ii 2 (f2cl-lib:int-add ii 1))
                        ((> ii n) nil)
            (tagbody
              (setf i (f2cl-lib:int-sub ii 1))
              (setf k i)
              (setf p (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))
              (f2cl-lib:fdo (j ii (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (cond
                   ((< (f2cl-lib:fref d (j) ((1 *))) p) (setf k j)
                    (setf p (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%))))
                 label170))
              (cond
               ((/= k i)
                (setf (f2cl-lib:fref d-%data% (k) ((1 *)) d-%offset%)
                        (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))
                (setf (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%) p)
                (zswap n
                 (f2cl-lib:array-slice z-%data% f2cl-lib:complex16 (1 i)
                                       ((1 ldz) (1 *)) z-%offset%)
                 1
                 (f2cl-lib:array-slice z-%data% f2cl-lib:complex16 (1 k)
                                       ((1 ldz) (1 *)) z-%offset%)
                 1)))
             label180))))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zsteqr
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((string 1)
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (array double-float (*))
                                              (array fortran-to-lisp::complex16
                                               (*))
                                              (fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil nil nil nil nil
                                              fortran-to-lisp::info)
                                            :calls
                                            '(fortran-to-lisp::zswap
                                              fortran-to-lisp::dlasrt
                                              fortran-to-lisp::dlartg
                                              fortran-to-lisp::dlapy2
                                              fortran-to-lisp::dlae2
                                              fortran-to-lisp::zlasr
                                              fortran-to-lisp::dlaev2
                                              fortran-to-lisp::dlascl
                                              fortran-to-lisp::dlanst
                                              fortran-to-lisp::zlaset
                                              fortran-to-lisp::dlamch
                                              fortran-to-lisp::xerbla
                                              fortran-to-lisp::lsame))))

