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


(let* ((zero 0.0d0) (one 1.0d0) (two 2.0d0) (three 3.0d0) (maxit 30))
  (declare (type (double-float 0.0d0 0.0d0) zero)
           (type (double-float 1.0d0 1.0d0) one)
           (type (double-float 2.0d0 2.0d0) two)
           (type (double-float 3.0d0 3.0d0) three)
           (type (f2cl-lib:integer4 30 30) maxit)
           (ignorable zero one two three maxit))
  (defun dsterf (n d e info)
    (declare (type (array double-float (*)) e d)
             (type (f2cl-lib:integer4) info n))
    (f2cl-lib:with-multi-array-data
        ((d double-float d-%data% d-%offset%)
         (e double-float e-%data% e-%offset%))
      (prog ((alpha 0.0d0) (anorm 0.0d0) (bb 0.0d0) (c 0.0d0) (eps 0.0d0)
             (eps2 0.0d0) (gamma 0.0d0) (oldc 0.0d0) (oldgam 0.0d0) (p 0.0d0)
             (r 0.0d0) (rt1 0.0d0) (rt2 0.0d0) (rte 0.0d0) (s 0.0d0)
             (safmax 0.0d0) (safmin 0.0d0) (sigma 0.0d0) (ssfmax 0.0d0)
             (ssfmin 0.0d0) (i 0) (iscale 0) (jtot 0) (l 0) (l1 0) (lend 0)
             (lendsv 0) (lsv 0) (m 0) (nmaxit 0))
        (declare
         (type (double-float) alpha anorm bb c eps eps2 gamma oldc oldgam p r
          rt1 rt2 rte s safmax safmin sigma ssfmax ssfmin)
         (type (f2cl-lib:integer4) i iscale jtot l l1 lend lendsv lsv m
          nmaxit))
        (setf info 0)
        (cond
         ((< n 0) (setf info -1) (xerbla "DSTERF" (f2cl-lib:int-sub info))
          (go end_label)))
        (if (<= n 1)
            (go end_label))
        (setf eps (dlamch "E"))
        (setf eps2 (expt eps 2))
        (setf safmin (dlamch "S"))
        (setf safmax (/ one safmin))
        (setf ssfmax (/ (f2cl-lib:fsqrt safmax) three))
        (setf ssfmin (/ (f2cl-lib:fsqrt safmin) eps2))
        (setf nmaxit (f2cl-lib:int-mul n maxit))
        (setf sigma zero)
        (setf jtot 0)
        (setf l1 1)
       label10
        (if (> l1 n)
            (go label170))
        (if (> l1 1)
            (setf (f2cl-lib:fref e-%data% ((f2cl-lib:int-sub l1 1)) ((1 *))
                                 e-%offset%)
                    zero))
        (f2cl-lib:fdo (m l1 (f2cl-lib:int-add m 1))
                      ((> m (f2cl-lib:int-add n (f2cl-lib:int-sub 1))) nil)
          (tagbody
            (cond
             ((<= (abs (f2cl-lib:fref e (m) ((1 *))))
                  (* (f2cl-lib:fsqrt (abs (f2cl-lib:fref d (m) ((1 *)))))
                     (f2cl-lib:fsqrt
                      (abs (f2cl-lib:fref d ((f2cl-lib:int-add m 1)) ((1 *)))))
                     eps))
              (setf (f2cl-lib:fref e-%data% (m) ((1 *)) e-%offset%) zero)
              (go label30)))
           label20))
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
        (f2cl-lib:fdo (i l (f2cl-lib:int-add i 1))
                      ((> i (f2cl-lib:int-add lend (f2cl-lib:int-sub 1))) nil)
          (tagbody
            (setf (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%)
                    (expt (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%) 2))
           label40))
        (cond
         ((< (abs (f2cl-lib:fref d (lend) ((1 *))))
             (abs (f2cl-lib:fref d (l) ((1 *)))))
          (setf lend lsv) (setf l lendsv)))
        (cond
         ((>= lend l)
          (tagbody
           label50
            (cond
             ((/= l lend)
              (f2cl-lib:fdo (m l (f2cl-lib:int-add m 1))
                            ((> m (f2cl-lib:int-add lend (f2cl-lib:int-sub 1)))
                             nil)
                (tagbody
                  (if (<= (abs (f2cl-lib:fref e-%data% (m) ((1 *)) e-%offset%))
                          (* eps2
                             (abs
                              (*
                               (f2cl-lib:fref d-%data% (m) ((1 *)) d-%offset%)
                               (f2cl-lib:fref d-%data% ((f2cl-lib:int-add m 1))
                                              ((1 *)) d-%offset%)))))
                      (go label70))
                 label60))))
            (setf m lend)
           label70
            (if (< m lend)
                (setf (f2cl-lib:fref e-%data% (m) ((1 *)) e-%offset%) zero))
            (setf p (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%))
            (if (= m l)
                (go label90))
            (cond
             ((= m (f2cl-lib:int-add l 1))
              (setf rte
                      (f2cl-lib:fsqrt
                       (f2cl-lib:fref e-%data% (l) ((1 *)) e-%offset%)))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlae2 (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%) rte
                   (f2cl-lib:fref d-%data% ((f2cl-lib:int-add l 1)) ((1 *))
                                  d-%offset%)
                   rt1 rt2)
                (declare (ignore var-0 var-1 var-2))
                (setf rt1 var-3)
                (setf rt2 var-4))
              (setf (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%) rt1)
              (setf (f2cl-lib:fref d-%data% ((f2cl-lib:int-add l 1)) ((1 *))
                                   d-%offset%)
                      rt2)
              (setf (f2cl-lib:fref e-%data% (l) ((1 *)) e-%offset%) zero)
              (setf l (f2cl-lib:int-add l 2))
              (if (<= l lend)
                  (go label50))
              (go label150)))
            (if (= jtot nmaxit)
                (go label150))
            (setf jtot (f2cl-lib:int-add jtot 1))
            (setf rte
                    (f2cl-lib:fsqrt
                     (f2cl-lib:fref e-%data% (l) ((1 *)) e-%offset%)))
            (setf sigma
                    (/
                     (-
                      (f2cl-lib:fref d-%data% ((f2cl-lib:int-add l 1)) ((1 *))
                                     d-%offset%)
                      p)
                     (* two rte)))
            (setf r (dlapy2 sigma one))
            (setf sigma (- p (/ rte (+ sigma (f2cl-lib:sign r sigma)))))
            (setf c one)
            (setf s zero)
            (setf gamma
                    (- (f2cl-lib:fref d-%data% (m) ((1 *)) d-%offset%) sigma))
            (setf p (* gamma gamma))
            (f2cl-lib:fdo (i (f2cl-lib:int-add m (f2cl-lib:int-sub 1))
                           (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                          ((> i l) nil)
              (tagbody
                (setf bb (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%))
                (setf r (+ p bb))
                (if (/= i (f2cl-lib:int-sub m 1))
                    (setf (f2cl-lib:fref e-%data% ((f2cl-lib:int-add i 1))
                                         ((1 *)) e-%offset%)
                            (* s r)))
                (setf oldc c)
                (setf c (/ p r))
                (setf s (/ bb r))
                (setf oldgam gamma)
                (setf alpha (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))
                (setf gamma (- (* c (- alpha sigma)) (* s oldgam)))
                (setf (f2cl-lib:fref d-%data% ((f2cl-lib:int-add i 1)) ((1 *))
                                     d-%offset%)
                        (+ oldgam (- alpha gamma)))
                (cond ((/= c zero) (setf p (/ (* gamma gamma) c)))
                      (t (setf p (* oldc bb))))
               label80))
            (setf (f2cl-lib:fref e-%data% (l) ((1 *)) e-%offset%) (* s p))
            (setf (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%)
                    (+ sigma gamma))
            (go label50)
           label90
            (setf (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%) p)
            (setf l (f2cl-lib:int-add l 1))
            (if (<= l lend)
                (go label50))
            (go label150)))
         (t
          (tagbody
           label100
            (f2cl-lib:fdo (m l (f2cl-lib:int-add m (f2cl-lib:int-sub 1)))
                          ((> m (f2cl-lib:int-add lend 1)) nil)
              (tagbody
                (if (<=
                     (abs
                      (f2cl-lib:fref e-%data% ((f2cl-lib:int-sub m 1)) ((1 *))
                                     e-%offset%))
                     (* eps2
                        (abs
                         (* (f2cl-lib:fref d-%data% (m) ((1 *)) d-%offset%)
                            (f2cl-lib:fref d-%data% ((f2cl-lib:int-sub m 1))
                                           ((1 *)) d-%offset%)))))
                    (go label120))
               label110))
            (setf m lend)
           label120
            (if (> m lend)
                (setf (f2cl-lib:fref e-%data% ((f2cl-lib:int-sub m 1)) ((1 *))
                                     e-%offset%)
                        zero))
            (setf p (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%))
            (if (= m l)
                (go label140))
            (cond
             ((= m (f2cl-lib:int-add l (f2cl-lib:int-sub 1)))
              (setf rte
                      (f2cl-lib:fsqrt
                       (f2cl-lib:fref e-%data% ((f2cl-lib:int-sub l 1)) ((1 *))
                                      e-%offset%)))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlae2 (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%) rte
                   (f2cl-lib:fref d-%data% ((f2cl-lib:int-sub l 1)) ((1 *))
                                  d-%offset%)
                   rt1 rt2)
                (declare (ignore var-0 var-1 var-2))
                (setf rt1 var-3)
                (setf rt2 var-4))
              (setf (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%) rt1)
              (setf (f2cl-lib:fref d-%data% ((f2cl-lib:int-sub l 1)) ((1 *))
                                   d-%offset%)
                      rt2)
              (setf (f2cl-lib:fref e-%data% ((f2cl-lib:int-sub l 1)) ((1 *))
                                   e-%offset%)
                      zero)
              (setf l (f2cl-lib:int-sub l 2))
              (if (>= l lend)
                  (go label100))
              (go label150)))
            (if (= jtot nmaxit)
                (go label150))
            (setf jtot (f2cl-lib:int-add jtot 1))
            (setf rte
                    (f2cl-lib:fsqrt
                     (f2cl-lib:fref e-%data% ((f2cl-lib:int-sub l 1)) ((1 *))
                                    e-%offset%)))
            (setf sigma
                    (/
                     (-
                      (f2cl-lib:fref d-%data% ((f2cl-lib:int-sub l 1)) ((1 *))
                                     d-%offset%)
                      p)
                     (* two rte)))
            (setf r (dlapy2 sigma one))
            (setf sigma (- p (/ rte (+ sigma (f2cl-lib:sign r sigma)))))
            (setf c one)
            (setf s zero)
            (setf gamma
                    (- (f2cl-lib:fref d-%data% (m) ((1 *)) d-%offset%) sigma))
            (setf p (* gamma gamma))
            (f2cl-lib:fdo (i m (f2cl-lib:int-add i 1))
                          ((> i (f2cl-lib:int-add l (f2cl-lib:int-sub 1))) nil)
              (tagbody
                (setf bb (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%))
                (setf r (+ p bb))
                (if (/= i m)
                    (setf (f2cl-lib:fref e-%data% ((f2cl-lib:int-sub i 1))
                                         ((1 *)) e-%offset%)
                            (* s r)))
                (setf oldc c)
                (setf c (/ p r))
                (setf s (/ bb r))
                (setf oldgam gamma)
                (setf alpha
                        (f2cl-lib:fref d-%data% ((f2cl-lib:int-add i 1))
                                       ((1 *)) d-%offset%))
                (setf gamma (- (* c (- alpha sigma)) (* s oldgam)))
                (setf (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                        (+ oldgam (- alpha gamma)))
                (cond ((/= c zero) (setf p (/ (* gamma gamma) c)))
                      (t (setf p (* oldc bb))))
               label130))
            (setf (f2cl-lib:fref e-%data% ((f2cl-lib:int-sub l 1)) ((1 *))
                                 e-%offset%)
                    (* s p))
            (setf (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%)
                    (+ sigma gamma))
            (go label100)
           label140
            (setf (f2cl-lib:fref d-%data% (l) ((1 *)) d-%offset%) p)
            (setf l (f2cl-lib:int-sub l 1))
            (if (>= l lend)
                (go label100))
            (go label150))))
       label150
        (if (= iscale 1)
            (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
                (dlascl "G" 0 0 ssfmax anorm
                 (f2cl-lib:int-add (f2cl-lib:int-sub lendsv lsv) 1) 1
                 (f2cl-lib:array-slice d-%data% double-float (lsv) ((1 *))
                                       d-%offset%)
                 n info)
              (declare
               (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8))
              (setf info var-9)))
        (if (= iscale 2)
            (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
                (dlascl "G" 0 0 ssfmin anorm
                 (f2cl-lib:int-add (f2cl-lib:int-sub lendsv lsv) 1) 1
                 (f2cl-lib:array-slice d-%data% double-float (lsv) ((1 *))
                                       d-%offset%)
                 n info)
              (declare
               (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8))
              (setf info var-9)))
        (if (< jtot nmaxit)
            (go label10))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i (f2cl-lib:int-add n (f2cl-lib:int-sub 1))) nil)
          (tagbody
            (if (/= (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%) zero)
                (setf info (f2cl-lib:int-add info 1)))
           label160))
        (go label180)
       label170
        (multiple-value-bind (var-0 var-1 var-2 var-3)
            (dlasrt "I" n d info)
          (declare (ignore var-0 var-1 var-2))
          (setf info var-3))
       label180
        (go end_label)
       end_label
        (return (values nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dsterf
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((fortran-to-lisp::integer4)
                                              (array double-float (*))
                                              (array double-float (*))
                                              (fortran-to-lisp::integer4))
                                            :return-values
                                            '(nil nil nil
                                              fortran-to-lisp::info)
                                            :calls
                                            '(fortran-to-lisp::dlasrt
                                              fortran-to-lisp::dlapy2
                                              fortran-to-lisp::dlae2
                                              fortran-to-lisp::dlascl
                                              fortran-to-lisp::dlanst
                                              fortran-to-lisp::dlamch
                                              fortran-to-lisp::xerbla))))

