(defpackage #:steven
  (:use #:common-lisp #:magicl))

(in-package #:steven)

(defun dot (u v)
  (assert (= (length u) (length v)) (u v))
  (apply '+ (mapcar #'* u v)))

(defun orthogonal (u v)
  (= (dot u v) 0))

(defun test-orthogonal ()
  (let ((u '(#C(1.0f0 0.0f0) #C(0.0f0 0.0f0)))
        (v '(#C(0.0f0 0.0f0) #C(1.0f0 0.0f0))))
    (orthogonal u v)))

(defun is-basis (n vecs)
  ; all distinct combinations of 2 vectors have an inner product of 0
  (loop :for i :from 1 :to (- n 1)
        :always (loop :for j :from 0 :to (- i 1)
                       :always (orthogonal (nth i vecs) (nth j vecs)))))

(defun test-is-basis ()
  (let ((u '(#C(2.0f0 0.0f0) #C(0.0f0 0.0f0) #C(0.0f0 0.0f0)))
        (v '(#C(0.0f0 0.0f0) #C(2.0f0 0.0f0) #C(0.0f0 0.0f0)))
        (w '(#C(0.0f0 0.0f0) #C(0.0f0 0.0f0) #C(2.0f0 0.0f0))))
    (is-basis 3 (list u v w))))

(defun is-orthonormal-basis (n vecs)
  ; check if it's a basis
  (and (is-basis n vecs)
       ; and that all elements have a norm of 1
       (loop :for vec :in vecs
             :always (= (dot vec vec) 1))))

(defun test-is-orthonormal-basis ()
  (let ((u '(#C(1.0f0 0.0f0) #C(0.0f0 0.0f0) #C(0.0f0 0.0f0)))
        (v '(#C(0.0f0 0.0f0) #C(1.0f0 0.0f0) #C(0.0f0 0.0f0)))
        (w '(#C(0.0f0 0.0f0) #C(0.0f0 0.0f0) #C(1.0f0 0.0f0))))
    (is-orthonormal-basis 3 (list u v w))))
