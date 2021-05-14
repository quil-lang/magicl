;;; tests/polynomial-solver-tests.lisp
;;;
;;;; Author: Juan M. Bello-Rivas
;;;          Erik Davis

(in-package #:magicl-tests)

(deftest test-polynomial-solver ()
  "Test univariate polynomial solver."
  ;; Test random polynomials with favorable coefficients.
  (flet ((make-random-polynomial (degree)
           (let ((c (magicl::storage (magicl:rand (list 1 degree)
                                                  :type '(complex double-float)))))
             (setf (aref c (1- degree)) (complex 1.0d0))
             (magicl::%make-polynomial :coefficients c))))
    (dotimes (i 10)
      (let* ((polynomial (make-random-polynomial 5))
             (roots (magicl::polynomial-solve polynomial)))
        (is (= (length roots) (1- (length (magicl:polynomial-coefficients polynomial)))))
        (dolist (root roots)
          (let ((refined-root (magicl:polynomial-newton-iteration polynomial root)))
            (is (< (abs (- root refined-root)) 1.0d-9))
            (is (< (abs (magicl:polynomial-eval polynomial refined-root))
                   (* 1.0d2 double-float-epsilon))))))))

  ;; Test polynomial with multiple roots.
  (let* ((polynomial (magicl:make-polynomial -4 8 -3 -2 1))
         (roots (magicl:polynomial-solve polynomial))
         (reference-roots '(#c(-2.0d0 0.0d0) #c(1.0d0 0.0d0) #c(1.0d0 0.0d0) #c(2.0d0 0.0d0)))
         (relative-error-tolerances (list (* 1.0d2 double-float-epsilon)
                                          (* 1.0d2 single-float-epsilon) ; Accuracy drops at double root.
                                          (* 1.0d2 single-float-epsilon)
                                          (* 1.0d2 double-float-epsilon))))
    (loop :for root :in roots
          :for reference-root :in reference-roots
          :for relative-error-tolerance :in relative-error-tolerances :do
            (is (< (abs (/ (- root reference-root) reference-root)) relative-error-tolerance)))))
