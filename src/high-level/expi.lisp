;;; expi.lisp
;;;
;;; Author: Erik Davis

(in-package #:magicl)


(define-extensible-function (expi expi-lisp) (matrix)
  (:documentation "Compute the exponential exp(i*M) for a symmetric or Hermitian matrix MATRIX.")
  (:method ((matrix matrix/complex-double-float))
    ;; this is a much simpler problem than what expokit solves, since
    ;; we only need to compute 1D exponentials under the hood. In particular,
    ;; if M = V diag(t1,...,tn) V^* then
    ;; exp(iM) = V diag(exp(i*t1),...,exp(i*tn)) V^*.
    (multiple-value-bind (evals V)
        (hermitian-eig matrix)
      (@ V
         (from-diag (mapcar (lambda (theta) (exp (* #C(0d0 1d0) theta)))
                            evals))
         (conjugate-transpose V))))

  (:method ((matrix matrix))
    ;; Just for convenience, we should be able to exponentiate real symmetric matrices.
    (expi (coerce-type matrix '(complex double-float)))))
