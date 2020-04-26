;;;; tests/vector-tests.lisp
;;;;
;;;;

(in-package #:magicl-tests)

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
                  (is (magicl:=
                       (dot x y)
                       (magicl:dot (magicl:transpose x) y)))
                  (is (magicl:=
                       (dot y x)
                       (magicl:mult (magicl:transpose y) x)))

                  (is (magicl:=
                       (dot (magicl:dagger x) y)
                       (magicl:dot (magicl:dagger x) y)))
                  (is (magicl:=
                       (dot (magicl:dagger y) x)
                       (magicl:mult (magicl:dagger y) x)))

                  (is (magicl:=
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
                      :do (is (magicl:= (magicl:tref xd j)
                                  (conjugate (magicl:tref x j))))
                         (setf (magicl:tref xd j)
                               (magicl:tref y j))
                         (is (magicl:= (magicl:tref xd j)
                                 (magicl:tref y j))))))))
