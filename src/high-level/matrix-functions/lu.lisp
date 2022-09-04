;;;; lu.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:magicl)

(defun doolittle (matrix)
  "Perform an LU decomposition of a square matrix MATRIX. Return (VALUES L U)."
  (assert (square-matrix-p matrix))
  (let ((n (nrows matrix))
        ;; Since this routine is pedagogical and isn't very efficient
        ;; to begin with, we just create two separate matricies to
        ;; store L and U, even though we could do with just one.
        (lower (zeros (shape matrix) :type (element-type matrix)))
        (upper (zeros (shape matrix) :type (element-type matrix)))
        (one   (coerce 1 (element-type matrix))))
    (loop :for i :below n :do
      ;; Upper triangle
      (loop :for j :from i :below n :do
        (setf (tref upper i j) (tref matrix i j))
        (loop :for k :below i :do
          (decf (tref upper i j) (* (tref lower i k) (tref upper k j)))))
      ;; Lower triangle
      (loop :for j :from (1+ i) :below n :do
        (setf (tref lower j i) (tref matrix j i))
        (loop :for k :below i :do
          (decf (tref lower j i) (* (tref lower j k) (tref upper k i))))
        (setf (tref lower j i) (/ (tref lower j i) (tref upper i i))))
      ;; Set the lower triangle's diagonal.
      (setf (tref lower i i) one))
    (values lower upper)))

(defun merge-lu (lower upper)
  (let* ((n (nrows lower))
         (r (zeros (list n n) :type (element-type lower))))
    (dotimes (i n r)
      ;; diagonal comes from UPPER. LOWER diag is always 1.
      (setf (tref r i i) (tref upper i i))
      (dotimes (j i)
        (setf (tref r i j) (tref lower i j)
              (tref r j i) (tref upper j i))))))

(defun exchange-rows! (matrix p q)
  (destructuring-bind (rows cols) (shape matrix)
    (assert (<= 0 p (1- rows)))
    (assert (<= 0 q (1- rows)))
    (when (/= p q)
      (dotimes (c cols)
        (rotatef (tref matrix p c)
                 (tref matrix q c)))
      nil)))

(defun pivot (matrix)
  (destructuring-bind (rows cols) (shape matrix)
    (assert (cl:= rows cols) () "Matrix must be square")
    (let ((matrix (deep-copy-tensor matrix))
          (ipiv   (map! #'1+ (arange rows :type '(signed-byte 32)))))
      (flet ((exch! (a b)
               (setf (tref ipiv a) (1+ b))
               (exchange-rows! matrix a b)))
        (loop :for col :below cols
              :for starting-row := col
              :for r := starting-row
              :do (loop :with max := (abs (tref matrix starting-row col))
                        :for row :from starting-row :below rows
                        :for next := (abs (tref matrix row col))
                        :when (> next max)
                          :do (setf max next
                                    r row))
              :unless (cl:= starting-row r)
                :do (exch! starting-row r)
              :finally (return (values matrix ipiv)))))))

;;; We could specialize the above for each type, but it's pedagogical
;;; anyway...
(defmethod lu-lisp ((matrix matrix))
  (multiple-value-bind (pivoted-matrix ipiv) (pivot matrix)
    (values (multiple-value-call #'merge-lu (doolittle pivoted-matrix))
            ipiv)))
