;;;; tests/vector-tests.lisp
;;;;
;;;;

(in-package #:magicl-tests)

(deftest test-dot-product ()
  (labels ((dot (x y)
             (loop :for i :below (magicl:size x)
                   :sum (* (magicl:tref x i) (magicl:tref y i)))))
    (dolist (magicl::*default-tensor-type* +magicl-float-types+)
      (loop :for i :below 1000
            :do (let* ((n (1+ (random 10)))
                       (a (magicl:rand (list n n)))
                       (x (magicl:rand (list n)))
                       (y (magicl:rand (list n))))

                  ;; DOT conjugates one complex input
                  (is (magicl:=
                       (dot x (magicl:dagger y))
                       (magicl:dot x y)))

                  ;; Simple transpose does not change DOT's behavior
                  (is (magicl:=
                       (dot x (magicl:dagger y))
                       (magicl:dot (magicl:transpose x) y)))
                  (is (magicl:=
                       (dot x (magicl:dagger y))
                       (magicl:dot x (magicl:transpose y))))
                  
                  ;; MULT does not silently conjugate
                  (is (magicl:=
                       (dot y x)
                       (magicl:mult (magicl:transpose y) x)))

                  ;; MULT conjugates on request
                  (is (magicl:=
                       (dot (magicl:dagger y) x)
                       (magicl:mult (magicl:dagger y) x)))

                  ;; Check @
                  (is (magicl:=
                       (dot (magicl:mult (magicl:dagger y) a) x)
                       (magicl:@ (magicl:dagger y) a x)))
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

(defun scale-vector (a vec)
  (loop :with n := (magicl::size vec)
        :with out := (magicl:empty (list n))
        :for i :below n
        :do (setf (magicl:tref out i)
                  (* a (magicl:tref vec i)))
        :finally (return out)))

(deftest test-vector-scaling ()
  "Check scaling of random vectors by random amounts"
  (dolist (magicl::*default-tensor-type* +magicl-float-types+)
    (loop :for i :below 1000 :do
          (let* ((n (1+ (random 5)))
                 (a (coerce
                     (if (subtypep magicl::*default-tensor-type* 'complex)
                         (complex (random 1d0) (random 1d0))
                         (random 1d0))
                     magicl::*default-tensor-type*))
                 (b (coerce
                     (if (subtypep magicl::*default-tensor-type* 'complex)
                         (complex (random 1d0) (random 1d0))
                         (random 1d0))
                     magicl::*default-tensor-type*))
                 (vec (magicl:rand (list n))))
             (is (magicl:= (magicl:mult a vec)
                           (scale-vector a vec)))
             (is (magicl:= (magicl:mult vec a)
                           (scale-vector a vec)))

             (is (magicl:= (magicl:mult vec a :alpha b)
                           (scale-vector (* a b) vec)))
             (is (magicl:= (magicl:mult a vec :alpha b)
                           (scale-vector (* a b) vec)))))))
