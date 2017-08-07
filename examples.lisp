(defpackage #:magicl-examples
  (:use :common-lisp :fnv :fnv-utils :magicl)
  #+package-local-nicknames
  (:local-nicknames (:blas :magicl.blas-cffi)
                    (:lapack :magicl.lapack-cffi))
  (:export :dot-example :eigenvalue-example :qr-example :svd-example :csd-example :det-example :inv-example))

(in-package #:magicl-examples)

;; This is a demonstration of the features of the CL blapack
;; interface.  We are working with BLAS/LAPACK (henceforth blapack) at
;; the FORTRAN level, so there are no such things as matrices.  All we
;; have is vectors.  This should show you how the interface works, and
;; should make clear why we'd like another layer on top.

(defun dot (u v)
  (assert (= (length u) (length v)) (u v))
  (let ((n (length u)))
    (sb-int:with-float-traps-masked (:divide-by-zero :underflow :overflow :inexact :invalid)
      (let ((cx (fnv:make-fnv-complex-double n))
            (cy (fnv:make-fnv-complex-double n)))
        (dotimes (i n)
          (setf (fnv:fnv-complex-double-ref cx i) (aref u i)
                (fnv:fnv-complex-double-ref cy i) (aref v i)))
        (format t "x: ~A~%y: ~A~%" cx cy)
        (blas:%zdotc
         n
         cx
         1
         cy
         1)))))

(defun dot-example ()
  (let ((a (fnv:make-fnv-complex-float 4 :initial-value (complex 1.0e0)))
        (b (fnv:make-fnv-complex-float 4 :initial-value (complex 2.0e0))))
    (format t "a^t = ~A~%b^t = ~A~%a^t b = ~A~%~%"
            a b (blas:%cdotu 4 a 1 b 1))))

(defun eigenvalue-example ()
  ;; Set the traps
  (sb-int:with-float-traps-masked (:divide-by-zero :invalid)
    
    ;; An eigenvalue example.  Note that we have no matrix abstraction a
    ;; this point.  We pretend 4-vectors are 2-by-2 matrices.

    ;; BLAS/LAPACK expects column major order, we are creating the
    ;; (matlab notation) matrix M = [1 2; 2 3].
    (let ((M (make-fnv-double 4)))
      (setf (fnv-double-ref M 0) 1.0d0
            (fnv-double-ref M 1) 2.0d0
            (fnv-double-ref M 2) 2.0d0
            (fnv-double-ref M 3) 3.0d0)

      (let ((V (make-fnv-double 4))
            (D (make-fnv-double 2))
            (lwork 4096)
            (liwork 4096)
            (info 0)
            (eigs-found 0))

        (lapack:%dsyevr "V" "A" "U" 2 (copy-fnv-double M) 2 0.0d0 0.0d0
                        0 0 -1.0d0  eigs-found D V 2 (make-fnv-int32 4)
                        (make-fnv-double lwork) lwork
                        (make-fnv-int32 liwork) liwork
                        info)
        (format t "M = ~A~%V=~A~%D=~A~%~%" M V D)

        ;; Construct a "matlab-style D" --- is there a better way?
        (let ((Df (make-fnv-double 4 :initial-value 0.0d0)))
          (setf (fnv-double-ref Df 0) (fnv-double-ref D 0)
                (fnv-double-ref Df 3) (fnv-double-ref D 1))
          ;; Reconstruct M as V*Df*V';
          (let ((Mri (make-fnv-double 4))
                (Mr (make-fnv-double 4)))
            (blas:%dgemm "N" "N" 2 2 2 1.0d0 V 2 Df 2 0.0d0 Mri 2)
            (blas:%dgemm "N" "T" 2 2 2 1.0d0 Mri 2 V 2 0.0d0 Mr 2)
            (format t "Reconstructed M = ~A~%" Mr)))))))

(defun qr-example ()
  (let ((a (make-complex-matrix 3 2 #C (1 2) #C (-4 3) #C (-3 -3) #C (9 2) 4 #C (0 -2.9d0))))
    (qr-printing a)))

(defun svd-example ()
  (let ((a (make-complex-matrix 3 2 #C (1 2) #C (-4 3) #C (-3 -3) #C (9 2) 4 #C (0 -2.9d0))))
    (svd-printing a)))

(defun qr-printing (a)
  (multiple-value-bind (q r)
      (qr a)
    (let ((a-reconst (multiply-complex-matrices q r)))
      (format t "A~%")
      (print-matrix a)
      (format t "Q~%")
      (print-matrix q)
      (format t "R~%")
      (print-matrix r)
      (format t "Reconstructed A~%")
      (print-matrix a-reconst))))

(defun svd-printing (a)
  (multiple-value-bind (u sigma vt) 
      (svd a)
    (let* ((rows (matrix-rows a))
           (cols (matrix-cols a))
           (complex-sigma (fnv:make-fnv-complex-double (* rows cols))))
        (dotimes (j cols)
          (dotimes (i rows)
            (setf (fnv:fnv-complex-double-ref complex-sigma (+ (* rows j) i)) (ref sigma i j))))
        (let ((a-reconst (multiply-complex-matrices 
                          (multiply-complex-matrices u 
                                                     (make-matrix :rows rows
                                                                  :cols cols
                                                                  :data complex-sigma)) vt)))
          (format t "A~%")
          (print-matrix a)
          (format t "U~%")
          (print-matrix u)
          (format t "SIGMA~%")
          (print-matrix sigma)
          (format t "VT~%")
          (print-matrix vt)
          (format t "Reconstructed A~%")
          (print-matrix a-reconst)))))

(defun csd-example ()
  (let ((x (make-complex-matrix 2 2 -0.894288 #C(-0.372336 -0.248224) #C(0.372336 -0.248224) -0.894288)))
    (csd-printing x 1 1)))

(defun csd-printing (x p q)
  (multiple-value-bind (u sigma vt)
      (csd x p q)
    (let ((x-reconst (multiply-complex-matrices u (multiply-complex-matrices sigma vt))))
      (format t "X~%")
      (print-matrix x)
      (format t "U~%")
      (print-matrix u)
      (format t "SIGMA~%")
      (print-matrix sigma)
      (format t "VT~%")
      (print-matrix vt)
      (format t "Reconstructed A~%")
      (print-matrix x-reconst))))

(defun det-example ()
  (let* ((x (make-complex-matrix 3 3 2 3 5 #C(1.5 -2) -3 1.5 #C(0 2) 0 #C(0 -3)))
         (d (det x)))
    (format t "X~%")
    (print-matrix x)
    (format t "det(X) = ~D~%" d)
    (values nil)))

(defun inv-example ()
  (let* ((x (make-complex-matrix 3 3 2 3 5 #C(1.5 -2) -3 1.5 #C(0 2) 0 #C(0 -3)))
         (inv-x (inv x))
         (id (multiply-complex-matrices x inv-x)))
    (format t "X~%")
    (print-matrix x)
    (format t "X^-1~%")
    (print-matrix inv-x)
    (format t "X*X^-1~%")
    (print-matrix id)
    (values nil)))
