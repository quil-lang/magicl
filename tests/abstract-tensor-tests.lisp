;;;; abstract-tensor-tests.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl-tests)

(defmacro swapping-arguments-is ((predicate arg1 arg2))
  "Try both argument orders for a commutative predicate."
  (let ((arg1sym (gensym))
        (arg2sym (gensym)))
    `(let ((,arg1sym ,arg1)
           (,arg2sym ,arg2))
       (is (,predicate ,arg1sym ,arg2sym))
       (is (,predicate ,arg2sym ,arg1sym)))))

(defmacro swapping-arguments-not ((predicate arg1 arg2))
  "Try both argument orders for a negated commutative predicate."
  (let ((arg1sym (gensym))
        (arg2sym (gensym)))
    `(let ((,arg1sym ,arg1)
           (,arg2sym ,arg2))
       (is (not (,predicate ,arg1sym ,arg2sym)))
       (is (not (,predicate ,arg2sym ,arg1sym))))))

(deftest test-scalar-equality ()
  "Test the various scalar equality predicates."
  (let ((exactvalues '((-1 0 1) ; integers
                       (-3/2 0 3/2) ; ratios
                       (-1.0s0 0.0s0 1.0s0) ; single-floats
                       (-1.0d0 0.0d0 1.0d0) ; double-floats
                       (#c(-1.0s0 1.0s0) #c(-1.0s0 0.0s0) #c(1.0s0 -1.0s0) #c(1.0s0 1.0s0)) ; complex-singles
                       (#c(-1.0d0 1.0d0) #c(-1.0d0 0.0d0) #c(1.0d0 -1.0d0) #c(1.0d0 1.0d0)) ; complex-doubles
                       ))
        (inexactvalues '(-1.0s0 0.0s0 1.0s0 ; single-floats
                                -1.0d0 0.0d0 1.0d0 ; double-floats
                                #c(-1.0s0 1.0s0) #c(-1.0s0 0.0s0) #c(1.0s0 -1.0s0) #c(1.0s0 1.0s0) ; complex-singles
                                #c(-1.0d0 1.0d0) #c(-1.0d0 0.0d0) #c(1.0d0 -1.0d0) #c(1.0d0 1.0d0) ; complex-doubles
                       ))
        (small-single-delta (/ magicl::*float-comparison-threshold* 2))
        (small-double-delta (/ magicl::*double-comparison-threshold* 2))
        (big-single-delta (* magicl::*float-comparison-threshold* 2))
        (big-double-delta (* magicl::*double-comparison-threshold* 2)))
    
    (flet ((test-exact (group1 group2)
             "Verify that magicl:= matches common-lisp:= where appropriate"
             (dolist (x1 group1)
               (dolist (x2 group2)
                 (if (common-lisp:= x1 x2)
                     (swapping-arguments-is (magicl:= x1 x2))
                     (swapping-arguments-not (magicl:= x1 x2))))))
           
           (test-inexact (x)
             "Verify that magicl:= works as expected on inexact values close to epsilon"
             (let ((smalldelta (etypecase x ; a delta small enough that = should still be true
                                 (single-float small-single-delta)
                                 (double-float small-double-delta)
                                 ((complex single-float) small-single-delta)
                                 ((complex double-float) small-double-delta)))
                   (bigdelta (etypecase x ; a delta big enough that = should become false
                               (single-float big-single-delta)
                               (double-float big-double-delta)
                               ((complex single-float) big-single-delta)
                               ((complex double-float) big-double-delta))))
               
               (swapping-arguments-is (magicl:= x (+ x smalldelta)))
               (swapping-arguments-is (magicl:= x (- x smalldelta)))
               (swapping-arguments-not (magicl:= x (+ x bigdelta)))
               (swapping-arguments-not (magicl:= x (- x bigdelta)))
               ; offset the imaginary parts. Bonus: This also causes real/complex comparisons when x is real.
               (swapping-arguments-is (magicl:= x (+ x (complex 0.0 smalldelta))))
               (swapping-arguments-is (magicl:= x (- x (complex 0.0 smalldelta))))
               (swapping-arguments-not (magicl:= x (+ x (complex 0.0 bigdelta))))
               (swapping-arguments-not (magicl:= x (- x (complex 0.0 bigdelta)))))))
      
      (dolist (group1 exactvalues)
        (dolist (group2 exactvalues)
          (test-exact group1 group2)))
      
      (dolist (x inexactvalues)
        (test-inexact x)))))

(deftest test-tensor-equality ()
  "Test that tensor equality is sane for tensors of dimension 1 to 8"
  (loop :for dimensions :on '(8 7 6 5 4 3 2 1) :do
    (loop :for type :in +magicl-types+ :do
      (is (magicl:= (magicl:const 1 dimensions :type type)
             (magicl:const 1 dimensions :type type)))
      (is (not (magicl:= (magicl:const 1 dimensions :type type)
                  (magicl:const 2 dimensions :type type)))))))

(deftest test-tensor-shape ()
  "Test that the shape is returned correctly for tensors of dimension 1 to 8"
  (loop :for dimensions :on '(8 7 6 5 4 3 2 1) :do
    (is (equal dimensions (magicl:shape (magicl:empty dimensions))))))

(deftest test-tensor-order ()
  "Test that the order is returned correctly for tensors of dimension 1 to 8"
  (loop :for dimensions :on '(8 7 6 5 4 3 2 1)
        :for i :from 8 :downto 1 :do
    (is (equal i (magicl:order (magicl:empty dimensions))))))

(deftest test-tensor-tref ()
  (let ((tensor (magicl:from-list '(1  2  3  4  5
                                    6  7  8  9  10
                                    11 12 13 14 15)
                                  '(3 5)
                                  :type 'double-float)))
    (loop :for i :below 15
          :do (= i (apply #'magicl:tref tensor (magicl::from-row-major-index i '(3 5)))))))
