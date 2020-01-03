;;;; abstract-tensor-tests.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl-tests)

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
