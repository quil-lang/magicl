;;;; tests/vector-tests.lisp
;;;;
;;;;

(in-package #:magicl-tests)

;; MAGICL:= only specializes on tensor types, but we need an analog to numpy.isclose for scalars
(defgeneric == (val1 val2 &optional epsilon)
  (:method ((val1 double-float) (val2 double-float) &optional (epsilon magicl::*double-comparison-threshold*))
    (unless (<= (abs (- val1 val2))
                epsilon)
      (return-from == nil))
    t)
  (:method ((val1 single-float) (val2 single-float) &optional (epsilon magicl::*float-comparison-threshold*))
    (unless (<= (abs (- val1 val2))
                epsilon)
      (return-from == nil))
    t)
  (:method ((val1 complex) (val2 complex) &optional (epsilon (list magicl::*float-comparison-threshold*
                                                                   magicl::*double-comparison-threshold*)))
    (let ((epsilon (if (typep val1 '(complex single-float))
                     (first epsilon)
                     (second epsilon))))
      (unless (and (<= (abs (- (realpart val1) (realpart val2)))
                       epsilon)
                   (<= (abs (- (imagpart val1) (imagpart val2)))
                       epsilon))
        (return-from == nil))
      t)))

(deftest test-dot-product ()
  (labels ((dot (x y)
             (if (typep (magicl:tref x 0) 'complex)
               (loop :for i :below (magicl:size x)
                     :sum (* (magicl:tref x i) (conjugate (magicl:tref y i))))
               (loop :for i :below (magicl:size x)
                     :sum (* (magicl:tref x i) (magicl:tref y i))))))
    (dolist (magicl::*default-tensor-type* +magicl-float-types+)
      (loop :for i :below 1000
            :do (let* ((n (1+ (random 10)))
                       (a (magicl:rand (list n n)))
                       (x (magicl:rand (list n)))
                       (y (magicl:rand (list n))))
                  (is (==
                       (dot x y)
                       (magicl:dot (magicl:transpose x) y)))
                  (is (==
                       (dot y x)
                       (magicl:mult (magicl:transpose y) x)))

                  (is (==
                       (dot (magicl:dagger x) y)
                       (magicl:dot (magicl:dagger x) y)))
                  (is (==
                       (dot (magicl:dagger y) x)
                       (magicl:mult (magicl:dagger y) x)))

                  (is (==
                       (dot (magicl:mult (magicl:transpose y) a) x)
                       (magicl:@ (magicl:transpose y) a x))))))))

(deftest test-outer-product ()
  (dolist (magicl::*default-tensor-type* +magicl-float-types+)
    (loop :for i :below 1000
          :do (let* ((m (1+ (random 10)))
                     (n (1+ (random 10)))
                     (x (magicl:rand (list m)))
                     (y (magicl:rand (list n))))
                (is (magicl:=
                     (magicl:outer x y)
                     (magicl:mult x (magicl:transpose y))))
                (is (magicl:=
                     (magicl:outer y x)
                     (magicl:mult y (magicl:transpose x))))

                (is (magicl:=
                     (magicl:outer x (magicl:dagger y))
                     (magicl:mult x (magicl:dagger y))))
                (is (magicl:=
                     (magicl:outer y (magicl:dagger x))
                     (magicl:mult y (magicl:dagger x))))))))

(deftest test-dagger ()
  (dolist (magicl::*default-tensor-type* +magicl-float-types+)
    (loop :for i :below 1000
          :do (let* ((n (1+ (random 10)))
                     (x (magicl:rand (list n)))
                     (y (magicl:rand (list n)))
                     (xd (magicl:dagger x)))
                (loop :for j :below n
                      :do (is (== (magicl:tref xd j)
                                  (conjugate (magicl:tref x j))))
                         (setf (magicl:tref xd j)
                               (magicl:tref y j))
                         (is (== (magicl:tref xd j)
                                 (magicl:tref y j))))))))
