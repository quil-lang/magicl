(in-package #:magicl.high-level)

(defstruct matrix
  rows
  cols
  data)

(defun make-complex-vector (&rest entries)
  (let* ((n (length entries))
         (v (fnv:make-fnv-complex-double n)))
    (dotimes (i n)
      (let ((e (nth i entries)))
        (assert (numberp e)
                ()
                "~S is not a number" e)
        (setf (fnv:fnv-complex-double-ref v i) (complex (coerce e 'double-float)))))
    (values v)))

; (defun make-complex-matrix (m n &rest entries))

; (defun ref (m i j))

; (defun qr (m))

; (defun svd (m))

