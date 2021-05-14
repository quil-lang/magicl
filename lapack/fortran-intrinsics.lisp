(in-package #:magicl.lisp-lapack)

(defun epsilon (float-val)
  "Return the smallest number E of the same float type as FLOAT-VAL such that 1 + E > 1."
  (etypecase float-val
    (single-float single-float-epsilon)
    (double-float double-float-epsilon)))

(defun tiny (float-val)
  (etypecase float-val
    (single-float least-positive-normalized-single-float)
    (double-float least-positive-normalized-double-float)))

(defun huge (float-val)
  (etypecase float-val
    (single-float most-positive-single-float)
    (double-float most-positive-double-float)))

(setf (symbol-function 'radix) #'float-radix)
(setf (symbol-function 'digits) #'float-digits)

;;; TODO: fix me
(defun minexponent (float-val)
  (etypecase float-val
    (single-float -125)
    (double-float -1021)))

(defun maxexponent (float-val)
  (etypecase float-val
    (single-float 128)
    (double-float 1024)))
