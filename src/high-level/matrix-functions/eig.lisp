;;;; eig.lisp
;;;;
;;;; Author: Erik Davis

(in-package #:magicl)

(defun wilkinson-shift (ajj ajk akk)
  "Get the eigenvalue of [ajj ajk; ajk akk] closest to akk."
  ;; The following is from Golub & Van Loan sec 8.3.5
  (let* ((ajk2 (* ajk (conjugate ajk)))
         (delta (/ (- ajj akk) 2))
         (d2 (* delta delta))
         (sgnd (if (minusp delta) -1d0 1d0)) ; we do NOT want SIGNUM here
         )
    (- akk
       (/ ajk2
          (+ delta
             (* sgnd (sqrt (+ d2 ajk2))))))))

(defun qrstep! (A)
  (let* ((n (ncols A))
         (shift (wilkinson-shift (tref A (- n 2) (- n 2))
                                 (tref A (- n 2) (1- n))
                                 (tref A (1- n) (1- n))))
         (x (- (tref A 0 0) shift))
         (z (tref A 1 0)))
    (loop :for k :below (1- n)
          :for (c s) := (multiple-value-list (givens-entries x z))
          :for g := (make-givens-rotation :i k :j (1+ k) :c c :s s)
          :do (left-apply-givens! A g)
              (right-apply-givens! A g)
          :when (< k (- n 2))
            :do (setf x (tref A (1+ k) k)
                      z (tref A (+ 2 k) k))
          :collect g)))


(defun matrix-realpart (matrix)
  (typecase matrix
    (matrix/complex-single-float
     (let ((result (empty (shape matrix) :type 'single-float)))
       (map-to #'realpart matrix result)
       result))
    (matrix/complex-double-float
     (let ((result (empty (shape matrix) :type 'double-float)))
       (map-to #'realpart matrix result)
       result))
    (otherwise matrix)))


(defmethod hermitian-eig-lisp (matrix)
  (assert (hermitian-matrix-p matrix))
  (let ((m (ncols matrix))        
        (eigs nil)
        (matrix (deep-copy-tensor matrix)))
    (multiple-value-bind (H Q) (hessenberg matrix)
      (setf H (matrix-realpart H))
      (loop :with i := (1- m)
            :until (zerop i)
            :do (dolist (g (qrstep! H))
                  (right-apply-givens! Q g))
            :when (<= (abs (tref H (1- i) i))
                      (* *double-comparison-threshold*
                         (+ (abs (tref H i i)) (abs (tref H (1- i) (1- i))))))
              :do (push (tref H i i) eigs)
                  (setf H (slice H (list 0 0) (list i i)))
                  (decf i)
            :finally (push (tref H 0 0) eigs))
      (let* ((Q-sorted (zeros (shape Q) :type (element-type Q)))
             (eigs-sorted
               (loop :with tagged := (loop :for i :from 0 :for v :in eigs :collect (cons i v))
                     :for (old-idx . val) :in (sort tagged #'< :key #'cdr)
                     :for new-idx :from 0
                     :do (dotimes (i (nrows Q))
                           (setf (tref Q-sorted i new-idx)
                                 (tref Q i old-idx)))
                     :collect val)))
        (values eigs-sorted
                Q-sorted)))))
