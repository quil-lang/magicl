;;;; tridiagonal-form.lisp
;;;;
;;;; Author: Erik Davis

(in-package #:magicl)

;;; TODO: - rename HESSENBERG to TRIDIAGONAL-FORM

(defun hessenberg (matrix)
  "Reduce MATRIX to Hessenberg form via a sequence of unitary similarity transformations."
  (let* ((m (nrows matrix))
	 (n (ncols matrix))
	 (hhs nil)
	 (H (deep-copy-tensor matrix))
         (Q (eye (list m n) :type (element-type matrix))))
    (loop :for k :below (1- m)
          :for hh := (column->householder H (1+ k) k)
          :do (left-apply-householder! H hh)
              (right-apply-householder! H hh)
              (push hh hhs))
    ;; update Q
    (loop :for hh :in hhs
          :do (left-apply-householder! Q hh))
    ;; ensure H is real by absorbing phases into Q
    ;; basic idea: M = Q H Q^h = Q U^h U H U^h U Q^h where U is a diagonal
    ;; unitary matrix such that U H U^h is real tridiagonal
    (loop :with z := 1
          :for k :from 1 :below m
          :for w := (tref H k (1- k))
          :unless (zerop (imagpart w))
            :do (let* ((alpha (/ (conjugate w) (abs w)))
                       (r (realpart (* w alpha))))
                  (setf z (* z alpha))
                  ;; we "implicitly" scale row k of H by (conjugate z)
                  ;; and explicitly column k of H by z
                  (scale-column! Q k (conjugate z))
                  ;; end result of this scaling: H will be real
                  (setf (tref H k (1- k)) r
                        (tref H (1- k) k) r)))
    (values H Q)))


(defun bidiagonal (matrix)
  "Reduce MATRIX to bidiagonal form via left and right multiplication by unitary transformations."
  (declare (optimize debug))
  (let* ((m (nrows matrix))
	 (n (ncols matrix))
         (left-hhs nil)
	 (right-hhs nil)
	 (H (deep-copy-tensor matrix)))
    (assert (cl:= m n))
    (dotimes (k m)
      (let ((hh (column->householder H k k)))
        (left-apply-householder! H hh)
        (push hh left-hhs))
      (when (< k (- n 2))
        (let ((hh (row->householder H k (1+ k))))
          (right-apply-householder! H hh)
          (push hh right-hhs))))
    (let ((U (eye (list m m) :type (element-type matrix)))
          (V (eye (list n n) :type (element-type matrix))))
      (loop :for hh :in left-hhs
            :do (left-apply-householder! U hh))
      (loop :for hh :in right-hhs
            :do (left-apply-householder! V hh))
      (values U H V))))
