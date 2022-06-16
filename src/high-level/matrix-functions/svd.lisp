;;;; svd.lisp
;;;;
;;;; Author: Erik Davis

(in-package #:magicl)

(defun svdstep! (B)
  (flet ((abs2 (x)
           (* (conjugate x) x)))
    (let* ((m (nrows B))
           (n (ncols B))
           (Bij (if (> m 2) (tref B (- m 3) (- n 2)) 0))
           (Bjj (tref B (- m 2) (- n 2)))
           (Bjk (tref B (- m 2) (1- n)))
           (Bkk (tref B (1- n) (1- n)))
           ;; we want to shift by the amount associated with the tridiagonal B^H B
           ;; the following can be worked out by hand or found in 8.6 of Golub & van Loan
           (shift (wilkinson-shift (+ (abs2 Bij) (abs2 Bjj))
                                   (* Bjj Bjk)
                                   (+ (abs2 Bjk) (abs2 Bkk))))
           (y (- (tref B 0 0) shift))
           (z (tref B 0 1))
           (gs nil))
      (dotimes (k (1- n))
        (multiple-value-bind (c s) (givens-entries y z)
          (let ((g (make-givens-rotation :i k :j (1+ k) :c c :s s)))
            (right-apply-givens! B g)
            (push g gs)
            (setf y (tref B k k)
                  z (tref B (1+ k) k)))
          (multiple-value-bind (c s) (givens-entries y z)
            (let ((g (make-givens-rotation :i k :j (1+ k) :c c :s s)))
              (left-apply-givens! B g)
              (push g gs)
              (when (< k (- n 2))
                (setf y (tref B k (1+ k))
                      z (tref B k (+ k 2))))))))
      (nreverse gs))))

;;; TODO: consistent use of M and N


(defun unitary-extension (columns)
  (let ((m (nrows columns))
        (n (ncols columns)))
    (assert (<= n m))
    (let ((u (zeros (list m m) :type (element-type columns))))
      (slice-to columns (list 0 0) (list m n)
                u (list 0 0))
      (multiple-value-bind (q r) (qr u)
        (declare (ignore r))
        q))))

(defun svd-lisp (matrix &key reduced)
  ;; short and fat => find svd of tranpose
  (when (> (ncols matrix) (nrows matrix))
    (multiple-value-bind (U D Vh) (svd-lisp (transpose matrix) :reduced reduced)
      (return-from svd-lisp
        (values (transpose! Vh :fast t)
                (transpose! D :fast t)
                (transpose! U :fast t)))))
  (multiple-value-bind (Q R) (%qr-lisp matrix)
    (let ((svals nil))
      (multiple-value-bind (U B V) (bidiagonal R)
        ;; TODO: realpart
        (assert (cl:= (nrows b) (ncols b)))
        (loop :with i := (1- (nrows B))
              :until (zerop i)
              :do (loop :for (gv gu) :on (svdstep! B) :by #'cddr
                        :do (right-apply-givens! U gu)
                            (right-apply-givens! V gv))
              :when (<= (abs (tref B (1- i) i))
                        (* 2 *double-comparison-threshold* ; TODO
                           (+ (abs (tref B (1- i) (1- i))) (abs (tref B i i)))))
                :do (push (tref B i i) svals)
                    (setf B (slice B (list 0 0) (list i i)))
                    (decf i)
              :finally (push (tref B 0 0) svals))
        (setf U (@ Q U))
        ;; permute singular values
        ;; we want positive values, from greatest to smallest
        (let ((U-sorted (zeros (shape U) :type (element-type U)))
              (Vt-sorted (zeros (shape V) :type (element-type V))))
          (let ((svals-sorted
                  (loop :with tagged := (loop :for i :from 0 :for v :in svals :collect (cons i v))
                        :for (old-idx . val) :in (sort tagged #'> :key (lambda (x) (abs (cdr x))))
                        :for new-idx :from 0
                        :do (dotimes (i (nrows U))
                              (setf (tref U-sorted i new-idx)                                    
                                    (if (minusp val)
                                        (- (tref U i old-idx))
                                        (tref U i old-idx))))
                            (dotimes (j (ncols V))
                              (setf (tref Vt-sorted j new-idx)
                                    (tref V old-idx j)))
                        :collect (abs val))))
            (if reduced
                (values U-sorted
                        (from-diag svals-sorted :type (element-type matrix))
                        Vt-sorted)
                (let ((d (zeros (list (nrows matrix) (ncols matrix)) :type (element-type matrix))))
                  (loop :with etype := (element-type matrix)
                        :for i :below (min (nrows matrix) (ncols matrix))
                        :do (setf (tref d i i) (coerce (pop svals-sorted) etype)))
                  (values (unitary-extension U-sorted)
                          d
                          Vt-sorted)))))))))
