(defpackage #:steven
  (:use #:common-lisp #:magicl))

(in-package #:steven)

(defun determinant (m)
  ; make sure that matrix is square
  (let ((cols (length m))
        (rows (length (car m))))
    (assert (= cols rows)))
  (let ((n (length m)))
    (if (= n 1)
      ; if it's a scalar return that
      (car (car m))
      (labels (
             ; return a list without the nth element
             (without (i list) (append (subseq list 0 i) (nthcdr (+ i 1) list)))
             ; [1, 2, 3]
             ; [4, 5, 6]
             ; [7, 8, 9]
             ; offset-matrix of 1st element (4) is:
             ; [2, 3]
             ; [8, 9]
             (offset-matrix (i vecs) (mapcar #'(lambda (vec) (without i vec)) vecs))
             ; ith term of the determinant calculation
             ; nth element of the first * determinant of offset matrix
             (term (i vecs) (* (nth i (car vecs)) (determinant (offset-matrix i (cdr vecs)))))
             )
        (loop :for i :from 0 :below n
              ; +/- sign varies depending on the term
              :sum (* (expt -1 i) (term i m)))))))

(defun dot (u v)
  (assert (= (length u) (length v)) (u v))
  (reduce #'+ (mapcar #'* u v)))

(defun orthogonal (u v)
  (= (dot u v) 0))

(defun test-orthogonal ()
  (let ((u '(#C(1.0f0 0.0f0) #C(0.0f0 0.0f0)))
        (v '(#C(0.0f0 0.0f0) #C(1.0f0 0.0f0))))
    (orthogonal u v)))

(defun is-basis (vecs)
  (not (= (determinant vecs) 0)))

; ROBERT Why does this complain with a style warning??
(defun is-orthogonal-basis (vecs)
  ; all distinct combinations of 2 vectors have an inner product of 0
  (let ((n (length vecs)))
    (loop :for i :from 1 :until n
          :always (loop :for j :from 0 :until i
                        :always (orthogonal (nth i vecs) (nth j vecs))))))

(defun test-is-orthogonal-basis ()
  (let ((u '(#C(2.0f0 0.0f0) #C(0.0f0 0.0f0) #C(0.0f0 0.0f0)))
        (v '(#C(0.0f0 0.0f0) #C(2.0f0 0.0f0) #C(0.0f0 0.0f0)))
        (w '(#C(0.0f0 0.0f0) #C(0.0f0 0.0f0) #C(2.0f0 0.0f0))))
    (is-orthogonal-basis '(u v w))))

(defun is-orthonormal-basis (vecs)
  ; check if it's an orthogonal basis
  (and (is-orthogonal-basis vecs)
      ; and that all elements have a norm of 1
      (loop :for vec :in vecs
            :always (= (dot vec vec) 1))))

(defun test-is-orthonormal-basis ()
  (let ((u '(#C(1.0f0 0.0f0) #C(0.0f0 0.0f0) #C(0.0f0 0.0f0)))
        (v '(#C(0.0f0 0.0f0) #C(1.0f0 0.0f0) #C(0.0f0 0.0f0)))
        (w '(#C(0.0f0 0.0f0) #C(0.0f0 0.0f0) #C(1.0f0 0.0f0))))
    (is-orthonormal-basis 3 (list u v w))))
