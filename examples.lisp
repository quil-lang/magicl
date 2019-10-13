(defpackage #:magicl-examples
  (:use #:common-lisp #:magicl-transcendental)
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

(defun print-matrix (m)
  (princ m)
  (terpri)
  (terpri))

(defun dot-example ()
  (let ((a (magicl:const #C(1d0 0d0) '(4)))
        (b (magicl:const #C(2d0 0d0) '(4))))
    (format t "a^t = ~A~%b^t = ~A~%a^t b = ~A~%~%"
            a b (magicl:dot a b))))

(defun eigenvalue-example ()
  (let ((a (magicl:from-list '(1 2 3 4) '(2 2) :type 'double-float)))
    (multiple-value-bind (val vect)
        (magicl:eig a)
      (format t "EVALUES = ~A~%EVECRTORS=~A~%~%" val vect))))

(defun qr-example ()
  (let ((a (magicl:from-list '(#C(1 2) #C(-4 3) #C(-3 -3) #C(9 2) 4 #C(0 -2.9d0)) '(3 2) :type '(complex double-float))))
    (qr-printing a)))

(defun ql-example ()
  (let ((a (magicl:from-list '(#C(1 2) #C(-4 3) #C(-3 -3) #C(9 2) 4 #C(0 -2.9d0)) '(3 2) :type '(complex double-float))))
    (ql-printing a)))

(defun rq-example ()
  (let ((a (magicl:from-list '(#C(1 2) #C(-4 3) #C(-3 -3) #C(9 2) 4 #C(0 -2.9d0)) '(2 3) :type '(complex double-float))))
    (rq-printing a)))

(defun lq-example ()
  (let ((a (magicl:from-list '(#C(1 2) #C(-4 3) #C(-3 -3) #C(9 2) 4 #C(0 -2.9d0)) '(2 3) :type '(complex double-float))))
    (lq-printing a)))

(defun svd-example ()
  (let ((a (magicl:from-list '(#C(1 2) #C(-4 3) #C(-3 -3) #C(9 2) 4 #C(0 -2.9d0)) '(3 2) :type '(complex double-float))))
    (svd-printing a)))

(defun qr-printing (a)
  (multiple-value-bind (q r)
      (magicl:qr a)
    (format t "A~%~a~%" a)
    (format t "Q~%~a~%" q)
    (format t "R~%~a~%" r)
    (let ((a-reconst (magicl:@ q r)))
      (format t "Reconstructed A~%~a~%" a-reconst)
      (assert (magicl:= a a-reconst)))))

(defun ql-printing (a)
  (multiple-value-bind (q l)
      (magicl:ql a)
    (format t "A~%~a~%" a)
    (format t "Q~%~a~%" q)
    (format t "L~%~a~%" l)
    (let ((a-reconst (magicl:@ q l)))
      (format t "Reconstructed A~%~a~%" a-reconst)
      (assert (magicl:= a a-reconst)))))

(defun rq-printing (a)
  (multiple-value-bind (q r)
      (magicl:rq a)
    (format t "A~%~a~%" a)
    (format t "Q~%~a~%" q)
    (format t "R~%~a~%" r)
    (let ((a-reconst (magicl:@ r q)))
      (format t "Reconstructed A~%~a~%" a-reconst)
      (assert (magicl:= a a-reconst)))))

(defun lq-printing (a)
  (multiple-value-bind (q l)
      (magicl:lq a)
    (format t "A~%~a~%" a)
    (format t "Q~%~a~%" q)
    (format t "L~%~a~%" l)
    (let ((a-reconst (magicl:@ l q)))
      (format t "Reconstructed A~%~a~%" a-reconst)
      (assert (magicl:= a a-reconst)))))

(defun svd-printing (a)
  (multiple-value-bind (u sigma vt) (svd a)
    (let* ((rows (matrix-rows a))
           (cols (matrix-cols a))
           (complex-sigma (magicl::make-Z-storage (* rows cols))))
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
  (let ((x (from-list (list -0.894288 #C(0.372336 -0.248224) #C(-0.372336 -0.248224) -0.894288)
                      '(2 2)
                      :type '(complex double-float))))
    (csd-printing x 1 1)))

(defun csd-printing (x p q)
  (multiple-value-bind (u sigma vt)
      (csd x p q)
    (let ((x-reconst (@ u (@ sigma vt))))
      (format t "X~%~a:~a~%"  (order x) x)
      (format t "U~%~a~%" u)
      (format t "SIGMA~%~a~%" sigma)
      (format t "VT~%~a~%" vt)
      (format t "Reconstructed A~%~a~%" x-reconst))))

(defun det-example ()
  (let* ((x (make-complex-matrix 3 3 (list 2 3 5 #C(1.5 -2) -3 1.5 #C(0 2) 0 #C(0 -3))))
         (d (det x)))
    (format t "X~%")
    (print-matrix x)
    (format t "det(X) = ~D~%" d)))

(defun inv-example ()
  (let* ((x (make-complex-matrix 3 3 (list 2 3 5 #C(1.5 -2) -3 1.5 #C(0 2) 0 #C(0 -3))))
         (inv-x (inv x))
         (id (multiply-complex-matrices x inv-x)))
    (format t "X~%")
    (print-matrix x)
    (format t "X^-1~%")
    (print-matrix inv-x)
    (format t "X*X^-1~%")
    (print-matrix id)))

(defun expm-example ()
  (let* ((x  (make-complex-matrix 4 4 (list 0 0 0 0 0 0 1.5 0 0 -1.5 0 0.5 0 0 -0.5 0)))
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
           (val-diag (funcall #'diag rows rows vals)))
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
  (let ((m (make-complex-matrix 3 3 (list -2 -1 1 -2 1 1 -9 -3 4))))
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
