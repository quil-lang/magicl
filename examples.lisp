(defpackage #:magicl-examples
  (:use #:common-lisp)
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
    (format t "a = ~A~%b = ~A~%<a, b> = ~A~%~%"
            a b (magicl:dot a b))))

(defun eigenvalue-example ()
  (let ((a (magicl:from-list '(1 2 3 4) '(2 2) :type 'double-float)))
    (multiple-value-bind (val vect)
        (magicl:eig a)
      (format t "Eigenvalues = ~A~%Eigenvectors=~A~%~%" val vect))))

(defun qr-example ()
  (let ((a (magicl:from-list '(#C(1 2) #C(-4 3) #C(-3 -3) #C(9 2)) '(2 2) :type '(complex double-float))))
    (qr-printing a)))

(defun ql-example ()
  (let ((a (magicl:from-list '(#C(1 2) #C(-4 3) #C(-3 -3) #C(9 2)) '(2 2) :type '(complex double-float))))
    (ql-printing a)))

(defun rq-example ()
  (let ((a (magicl:from-list '(#C(1 2) #C(-4 3) #C(-3 -3) #C(9 2)) '(2 2) :type '(complex double-float))))
    (rq-printing a)))

(defun lq-example ()
  (let ((a (magicl:from-list '(#C(1 2) #C(-4 3) #C(-3 -3) #C(9 2)) '(2 2) :type '(complex double-float))))
    (lq-printing a)))

(defun svd-example ()
  (let ((a (magicl:from-list '(#C(1 2) #C(-4 3) #C(-3 -3) #C(9 2)) '(2 2) :type '(complex double-float))))
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
  (multiple-value-bind (r q)
      (magicl:rq a)
    (format t "A~%~a~%" a)
    (format t "Q~%~a~%" q)
    (format t "R~%~a~%" r)
    (let ((a-reconst (magicl:@ r q)))
      (format t "Reconstructed A~%~a~%" a-reconst)
      (assert (magicl:= a a-reconst)))))

(defun lq-printing (a)
  (multiple-value-bind (l q)
      (magicl:lq a)
    (format t "A~%~a~%" a)
    (format t "Q~%~a~%" q)
    (format t "L~%~a~%" l)
    (let ((a-reconst (magicl:@ l q)))
      (format t "Reconstructed A~%~a~%" a-reconst)
      (assert (magicl:= a a-reconst)))))

(defun svd-printing (a)
  (multiple-value-bind (u sigma vt) (magicl:svd a)
    (let ((complex-sigma (magicl::coerce-type sigma '(complex double-float))))
      (let ((a-reconst (magicl:@ u
                                 complex-sigma
                                 vt)))
        (format t "A~%~a~%" a)
        (format t "U~%~a~%" u)
        (format t "SIGMA~%~a~%" sigma)
        (format t "VT~%~a~%" vt)
        (format t "Reconstructed A~%~a~%" a-reconst)))))

(defun csd-example ()
  (let ((x (magicl:from-list '(-0.894288 #C(0.372336 -0.248224) #C(-0.372336 -0.248224) -0.894288)
                             '(2 2)
                             :type '(complex double-float))))
    (csd-printing x 1 1)))

(defun csd-printing (x p q)
  (multiple-value-bind (u sigma vt)
      (magicl:csd x p q)
    (let ((x-reconst (magicl:@ u sigma vt)))
      (format t "X~%~a:~a~%"  (magicl::layout x) x)
      (format t "U~%~a~%" u)
      (format t "SIGMA~%~a~%" sigma)
      (format t "VT~%~a~%" vt)
      (format t "Reconstructed A~%~a~%" x-reconst))))

(defun det-example ()
  (let* ((x (magicl:from-list (list 2 3 5 #C(1.5 -2) -3 1.5 #C(0 2) 0 #C(0 -3)) '(3 3) :type '(complex double-float)))
         (d (magicl:det x)))
    (format t "X~%")
    (print-matrix x)
    (format t "det(X) = ~D~%" d)))

(defun inv-example ()
  (let* ((x (magicl:from-list (list 2 3 5 #C(1.5 -2) -3 1.5 #C(0 2) 0 #C(0 -3)) '(3 3) :type '(complex double-float)))
         (inv-x (magicl:inv x))
         (id (magicl:@ x inv-x)))
    (format t "X~%~a~%" x)
    (format t "X^-1~%~a~%" inv-x)
    (format t "X*X^-1~%~a~%" id)))

(defun expih-example ()
  (let* ((h  (magicl:from-list (list 0 #C(0d0 -1d0) #C(0d0 1d0) 0) '(2 2) :type '(complex double-float)))
         (expih (magicl:expih h))
         (d (magicl:det expih)))
    (format t "H~%~a~%" h)
    (format t "e^iH~%~a~%" expih)
    (format t "det(e^iH) = ~D~%" d)))

(defun ensure-complex (x)
  (etypecase x
    (complex x)
    (real (complex x))))

(defun eig-printing (m)
  (let ((m-complex (magicl:.complex m (magicl:zeros (magicl:shape m)
                                                    :type (magicl:element-type m)))))
    (multiple-value-bind (vals vects)
        (magicl:eig m)
      (let ((val-diag (magicl:from-diag (mapcar #'ensure-complex vals))))
        (format t "M~%~a~%" m)
        (format t "Eigenvalues LAMBDA~%~a~%" val-diag)
        (format t "Eigenvectors V~%~a~%" vects)
        (format t "M*V~%~a~%" (magicl:@ m-complex vects))
        (format t "V*LAMBDA~%~a~%" (magicl:@ vects val-diag))
        (format t "M*V = V*LAMBDA (within 10^-10)? ~:[no~;yes~]~%"
                (magicl:= (magicl:@ m-complex vects)
                          (magicl:@ vects val-diag)
                          1d-10))))))

(defun eig-example ()
  (let ((m (magicl:from-list (list -2 -1 1 -2 1 1 -9 -3 4) '(3 3) :type 'double-float)))
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
            expih-example
            eig-example))
    (finish-output)
    ;; Return T for test runs.
    t))
