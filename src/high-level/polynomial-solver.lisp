;;;; src/polynomial-solver.lisp
;;;;
;;;; Author: Juan M. Bello-Rivas

(in-package #:magicl)

;;; This file implements a simple univariate polynomial solver that relies on
;;; the eigendecomposition of the companion matrix of the given polynomial to
;;; obtain all of its roots.

;;; A univariate polynomial is represented as a one-dimensional array of
;;; complex coefficients.

(deftype polynomial-coefficients () '(simple-array (complex double-float) (*)))

(defstruct (polynomial (:constructor %make-polynomial))
  (coefficients nil :type polynomial-coefficients))

(defun make-polynomial (&rest args)
  "Return a POLYNOMIAL structure whose (complex) coefficients are the elements of ARGS."
  (assert (and args (first args)))
  (%make-polynomial :coefficients (coerce (loop :for arg :in args
                                                :collect (coerce arg '(complex double-float)))
                                          'polynomial-coefficients)))

(defun polynomial-solve (polynomial)
  "Return all the roots of POLYNOMIAL.

It is assumed that POLYNOMIAL is well-formed in the sense that its leading coefficient is non-zero."
  (declare (type polynomial polynomial)
           (values list))

  (labels ((polynomial-normalize (polynomial)
             "Return a polynomial with the same roots as POLYNOMIAL but with leading coefficient equal to one."
             (let* ((coefficients (copy-seq (polynomial-coefficients polynomial)))
                    (degree (length coefficients))
                    (leading-coefficient (aref coefficients (1- degree))))
               (dotimes (i degree)
                 (setf (aref coefficients i) (/ (aref coefficients i) leading-coefficient)))
               (%make-polynomial :coefficients coefficients)))

           (make-companion-matrix (polynomial)
             "Return the companion matrix of POLYNOMIAL."
             (loop :with polynomial := (polynomial-normalize polynomial)
                   :with coefficients := (polynomial-coefficients polynomial)
                   :with n := (1- (length coefficients))
                   :with matrix := (make-zero-matrix n n)
                   :for i :below n
                   :for coefficient :of-type (complex double-float) := (aref coefficients i)
                   :when (plusp i) :do (setf (ref matrix i (1- i)) 1.0d0)
                   :do (setf (ref matrix i (1- n)) (- coefficient))
                   :finally (return matrix))))

    (nth-value 0 (eig (make-companion-matrix polynomial)))))

(defun polynomial-eval (polynomial value)
  "Return the result of evaluating POLYNOMIAL at VALUE using Horner's rule."
  (declare (type polynomial polynomial)
           (type (complex double-float) value))

  (let ((coefficients (polynomial-coefficients polynomial)))
    (do* ((i 0 (1+ i))
          (c (aref coefficients 0) (aref coefficients i))
          (x (complex 1.0d0) (* x value))
          (y (* x c) (+ y (* x c))))
         ((= i (1- (length coefficients))) y)

      (declare (type fixnum i)
               (type (complex double-float) x y)))))

(defun polynomial-diff (polynomial)
  "Return the polynomial corresponding to the derivative of POLYNOMIAL."
  (declare (type polynomial polynomial)
           (values polynomial))

  (loop :with coefficients := (polynomial-coefficients polynomial)
        :with n := (length coefficients)
        :with new-coefficients := (subseq coefficients 1 n)
        :for i :below (1- n)
        :for c := (aref new-coefficients i)
        :do (setf (aref new-coefficients i) (* (1+ i) c))
        :finally (return (%make-polynomial :coefficients new-coefficients))))

(defconstant +default-tolerance+ (* 1e2 +double-float-epsilon+)
  "Default tolerance for Newton's method.")

(defconstant +default-max-iterations+ 100
  "Default maximum number of iterations in Newton's method.")

(defun polynomial-newton-iteration (polynomial initial-value
                                    &key (tolerance +default-tolerance+)
                                      (max-iterations +default-max-iterations+))
  "Return the result of running Newton's method on POLYNOMIAL starting with INITIAL-VALUE. The arguments TOLERANCE and MAX-ITERATIONS determine the convergence criterion."
  (declare (type polynomial polynomial)
           (type (complex double-float) initial-value)
           (type double-float tolerance)
           (type fixnum max-iterations)
           (values (complex double-float)))

  (loop :with diff :of-type polynomial := (polynomial-diff polynomial)
        :with x :of-type (complex double-float) := initial-value
        :for i :of-type fixnum :below (min 1 max-iterations)
        :for p :of-type (complex double-float) := (polynomial-eval polynomial x)
        :for dp :of-type (complex double-float) := (polynomial-eval diff x)
        :until (< (abs p) tolerance)
        :do (setf x (- x (/ p dp)))
        :finally (return x)))
