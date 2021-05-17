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


(let* ((negone (- 1.0d0)) (one 1.0d0) (zero 0.0d0))
  (declare (type (double-float) negone)
           (type (double-float 1.0d0 1.0d0) one)
           (type (double-float 0.0d0 0.0d0) zero)
           (ignorable negone one zero))
  (defun dlartgs (x y sigma cs sn)
    (declare (type (double-float) sn cs sigma y x))
    (prog ((r 0.0d0) (s 0.0d0) (thresh 0.0d0) (w 0.0d0) (z 0.0d0))
      (declare (type (double-float) r s thresh w z))
      (setf thresh (dlamch "E"))
      (cond
       ((or (and (= sigma zero) (< (abs x) thresh))
            (and (= (abs x) sigma) (= y zero)))
        (setf z zero) (setf w zero))
       ((= sigma zero)
        (cond ((>= x zero) (setf z x) (setf w y))
              (t (setf z (- x)) (setf w (- y)))))
       ((< (abs x) thresh) (setf z (* (- sigma) sigma)) (setf w zero))
       (t (cond ((>= x zero) (setf s one)) (t (setf s negone)))
        (setf z (* s (- (abs x) sigma) (+ s (/ sigma x)))) (setf w (* s y))))
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
          (dlartgp w z sn cs r)
        (declare (ignore var-0 var-1))
        (setf sn var-2)
        (setf cs var-3)
        (setf r var-4))
      (go end_label)
     end_label
      (return (values nil nil nil cs sn)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlartgs
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types
                                            '((double-float) (double-float)
                                              (double-float) (double-float)
                                              (double-float))
                                            :return-values
                                            '(nil nil nil fortran-to-lisp::cs
                                              fortran-to-lisp::sn)
                                            :calls
                                            '(fortran-to-lisp::dlartgp
                                              fortran-to-lisp::dlamch))))

