;;;; qr.lisp
;;;;
;;;; Author: Erik Davis

(in-package #:magicl)

;;;  TODO - get Q matrix from above
;;;       - apply givens rotation to Q

(defun scale-column! (matrix idx scalar)
  (loop :for i :below (nrows matrix)
        :do (setf (tref matrix i idx)
                  (* scalar (tref matrix i idx)))
        :finally (return matrix)))

(defun scale-row! (matrix idx scalar)
  (loop :for j :below (ncols matrix)
        :do (setf (tref matrix idx j)
                  (* scalar (tref matrix idx j)))
        :finally (return matrix)))

;;; QR

(defun %qr-lisp (matrix &key (reduced t))
  (let* ((m (nrows matrix))
	 (n (ncols matrix))
         (p (min m n))
	 (hhs nil)
         (Q (eye (list m (if reduced p m)) :type (element-type matrix)))
	 (R (deep-copy-tensor matrix)))
    ;; compute R and vs
    (loop :for k :below p
          :for hh := (column->householder R k k)
          :do (left-apply-householder! R hh)
              (push hh hhs))
    ;; compute Q
    (loop :for hh :in hhs
          :do (left-apply-householder! Q hh))
    (when reduced
      ;; reduced factorization; trim R
      (setf R (slice R (list 0 0) (list p (ncols R)))))
    ;; TODO: fix this with the choice of householder coeffs
    ;; force positive values on the diagonal
    (loop :for i :below (min (nrows R) (ncols R))
          :for val := (tref R i i)
          :unless (and (zerop (imagpart val))
                       (not (minusp (realpart val))))
            :do (scale-column! Q i (/ val (abs val)))
                (scale-row! R i (/ (conjugate val) (abs val))))
    (values Q R)))

(defmethod qr-lisp ((matrix matrix))
  (%qr-lisp matrix :reduced (< (ncols matrix) (nrows matrix))))
