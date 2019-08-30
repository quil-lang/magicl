;;;; random.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:magicl)

;;; Some routines to create random matrices.

(defun random-matrix (rows cols)
  "Create a Z matrix of size ROWS x COLS with uniformly random complex entries in [0,1) + [0,1)i."
  (tabulate rows cols
            (lambda (i j)
              (declare (ignore i j))
              (complex (random 1.0d0) (random 1.0d0)))))

(defun random-gaussian-matrix (rows cols)
  "Create a Z matrix of size ROWS x COLS with normally distributed random complex entries."
  (tabulate rows cols
            (lambda (i j)
              (declare (ignore i j))
              (complex (alexandria:gaussian-random)
                       (alexandria:gaussian-random)))))

(defun random-unitary (n)
  "Generate a uniformly random element of U(n)."
  (declare (optimize (speed 0) (debug 3)))
  (multiple-value-bind (q r) (qr (random-gaussian-matrix n n))
    (let ((d (matrix-diagonal r)))
      (map-into d (lambda (di) (/ di (sqrt (* di (conjugate di))))) d)
      (multiply-complex-matrices q (funcall #'diag n n d)))))
