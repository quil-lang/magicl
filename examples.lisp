(defpackage #:magicl-examples
  (:use #:common-lisp #:magicl)
  #+package-local-nicknames
  (:local-nicknames (:blas :magicl.blas-cffi)
                    (:lapack :magicl.lapack-cffi)
                    (:expokit :magicl.expokit-cffi))
  (:export #:dot-example
           #:eigenvalue-example
           #:qr-example
           #:svd-example
           #:csd-example
           #:det-example
           #:inv-example
           #:eig-example
           #:all-examples))

(in-package #:magicl-examples)

;; This is a demonstration of the features of the CL blapack
;; interface.  We are working with BLAS/LAPACK (henceforth blapack) at
;; the FORTRAN level, so there are no such things as matrices.  All we
;; have is vectors.  This should show you how the interface works, and
;; should make clear why we'd like another layer on top.

(defun dot (u v)
  (assert (= (length u) (length v)) (u v))
  (let ((n (length u)))
    (with-blapack
      (let ((cx (magicl::copy-matrix-storage u))
            (cy (magicl::copy-matrix-storage v)))
        (format t "x: ~A~%y: ~A~%" cx cy)
        (blas:%zdotc
         n
         cx
         1
         cy
         1)))))

(defun dot-example ()
  (let ((a (magicl::make-lisp-complex-float 4))
        (b (magicl::make-lisp-complex-float 4)))
    (dotimes (i 4)
      (setf (aref a i) #C(1.0f0 0.0f0)
            (aref b i) #C(2.0f0 0.0f0)))
    (format t "a^t = ~A~%b^t = ~A~%a^t b = ~A~%~%"
            a b (blas:%cdotu 4 a 1 b 1))))

(defun eigenvalue-example ()
  ;; Set the traps
  (with-blapack

    ;; An eigenvalue example.  Note that we have no matrix abstraction a
    ;; this point.  We pretend 4-vectors are 2-by-2 matrices.

    ;; BLAS/LAPACK expects column major order, we are creating the
    ;; (matlab notation) matrix M = [1 2; 2 3].
    (let ((M (magicl::make-lisp-double 4)))
      (setf (aref M 0) 1.0d0
            (aref M 1) 2.0d0
            (aref M 2) 2.0d0
            (aref M 3) 3.0d0)

      (let ((V (magicl::make-lisp-double 4))
            (D (magicl::make-lisp-double 2))
            (lwork 4096)
            (liwork 4096)
            (info 0)
            (eigs-found 0))

        (lapack:%dsyevr "V" "A" "U" 2 (magicl::copy-matrix-storage M) 2 0.0d0 0.0d0
                        0 0 -1.0d0  eigs-found D V 2 (magicl::make-lisp-int32 4)
                        (magicl::make-lisp-double lwork) lwork
                        (magicl::make-lisp-int32 liwork) liwork
                        info)
        (format t "M = ~A~%V=~A~%D=~A~%~%" M V D)

        ;; Construct a "matlab-style D" --- is there a better way?
        (let ((Df (magicl::make-lisp-double 4)))
          (setf (aref Df 0) (aref D 0)
                (aref Df 3) (aref D 1))
          ;; Reconstruct M as V*Df*V';
          (let ((Mri (magicl::make-lisp-double 4))
                (Mr (magicl::make-lisp-double 4)))
            (blas:%dgemm "N" "N" 2 2 2 1.0d0 V 2 Df 2 0.0d0 Mri 2)
            (blas:%dgemm "N" "T" 2 2 2 1.0d0 Mri 2 V 2 0.0d0 Mr 2)
            (format t "Reconstructed M = ~A~%" Mr)))))))

(defun qr-example ()
  (let ((a (make-complex-matrix 3 2 #C(1 2) #C(-4 3) #C(-3 -3) #C(9 2) 4 #C(0 -2.9d0))))
    (qr-printing a)))

(defun ql-example ()
  (let ((a (make-complex-matrix 3 2 #C(1 2) #C(-4 3) #C(-3 -3) #C(9 2) 4 #C(0 -2.9d0))))
    (ql-printing a)))

(defun rq-example ()
  (let ((a (make-complex-matrix 2 3 #C(1 2) #C(-4 3) #C(-3 -3) #C(9 2) 4 #C(0 -2.9d0))))
    (rq-printing a)))

(defun lq-example ()
  (let ((a (make-complex-matrix 2 3 #C(1 2) #C(-4 3) #C(-3 -3) #C(9 2) 4 #C(0 -2.9d0))))
    (lq-printing a)))

(defun svd-example ()
  (let ((a (make-complex-matrix 3 2 #C(1 2) #C(-4 3) #C(-3 -3) #C(9 2) 4 #C(0 -2.9d0))))
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

(defun ql-printing (a)
  (multiple-value-bind (q l)
      (ql a)
    (let ((a-reconst (multiply-complex-matrices q l)))
      (format t "A~%")
      (print-matrix a)
      (format t "Q~%")
      (print-matrix q)
      (format t "L~%")
      (print-matrix l)
      (format t "Reconstructed A~%")
      (print-matrix a-reconst))))

(defun rq-printing (a)
  (multiple-value-bind (q r)
      (rq a)
    (let ((a-reconst (multiply-complex-matrices r q)))
      (format t "A~%")
      (print-matrix a)
      (format t "R~%")
      (print-matrix r)
      (format t "Q~%")
      (print-matrix q)
      (format t "Reconstructed A~%")
      (print-matrix a-reconst))))

(defun lq-printing (a)
  (multiple-value-bind (q l)
      (lq a)
    (let ((a-reconst (multiply-complex-matrices l q)))
      (format t "A~%")
      (print-matrix a)
      (format t "L~%")
      (print-matrix l)
      (format t "Q~%")
      (print-matrix q)
      (format t "Reconstructed A~%")
      (print-matrix a-reconst))))

(defun svd-printing (a)
  (multiple-value-bind (u sigma vt) (svd a)
    (let* ((rows (matrix-rows a))
           (cols (matrix-cols a))
           (complex-sigma (magicl::make-lisp-complex-double (* rows cols))))
        (dotimes (j cols)
          (dotimes (i rows)
            ;; TODO: COLUMN-MAJOR-INDEX
            (setf (aref complex-sigma (+ (* rows j) i))
                  (coerce (ref sigma i j) '(complex double-float)))))
        (let ((a-reconst (multiply-complex-matrices
                          (multiply-complex-matrices u
                                                     (make-matrix :rows rows
                                                                  :cols cols
                                                                  :data complex-sigma))
                          vt)))
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
    (format t "det(X) = ~D~%" d)))

(defun inv-example ()
  (let* ((x (make-complex-matrix 3 3 2 3 5 #C(1.5 -2) -3 1.5 #C(0 2) 0 #C(0 -3)))
         (inv-x (inv x))
         (id (multiply-complex-matrices x inv-x)))
    (format t "X~%")
    (print-matrix x)
    (format t "X^-1~%")
    (print-matrix inv-x)
    (format t "X*X^-1~%")
    (print-matrix id)))

(defun expm-example ()
  (let* ((x  (make-complex-matrix 4 4 0 0 0 0 0 0 1.5 0 0 -1.5 0 0.5 0 0 -0.5 0))
         (expx (expm x))
         (d (det expx)))
    (format t "X~%")
    (print-matrix x)
    (format t "e^X~%")
    (print-matrix expx)
    (format t "det(X) = ~D~%" d)))

(defun eig-printing (m)
  (multiple-value-bind (vals vects)
      (eig m)
    (let* ((rows (matrix-rows m))
           (val-diag (apply #'diag rows rows vals)))
      (format t "M~%")
      (print-matrix m)
      (format t "Eigenvalues LAMBDA~%")
      (print-matrix val-diag)
      (format t "Eigenvectors V~%")
      (print-matrix vects)
      (format t "M*V~%")
      (print-matrix (multiply-complex-matrices m vects))
      (format t "V*LAMBDA~%")
      (print-matrix (multiply-complex-matrices vects val-diag)))))

(defun eig-example ()
  (let ((m (make-complex-matrix 3 3 -2 -1 1 -2 1 1 -9 -3 4)))
    (eig-printing m)))

(defun all-examples ()
  "Run all of the examples."
  (flet ((call-example (fn-name)
           (format t ";;; Example: ~A~%" fn-name)
           (handler-case (funcall fn-name)
             (error (c)
               (cerror "Continue"
                       "Error in example ~A:~%~A"
                       fn-name
                       c)
               (format t "!!! Error: ~A~%" c)))
           (format t ";;; End of example ~A~2%" fn-name)
           (finish-output)))
    (mapc #'call-example
          '(dot-example
            eigenvalue-example
            qr-example
            ql-example
            rq-example
            lq-example
            svd-example
            csd-example
            det-example
            inv-example
            expm-example
            eig-example))
    (finish-output)
    ;; Return T for test runs.
    t))
