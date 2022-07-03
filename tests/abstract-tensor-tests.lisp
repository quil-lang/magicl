;;;; abstract-tensor-tests.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl-tests)

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

(deftest test-tensor-number-ops ()
  "Test that basic operations on a tensor and number give the expected results"
  (let* ((input '(-1.0 -0.5 0.5 1.0))
         (tensor (magicl:from-list input
                                   '(2 2)
                                   :type 'single-float)))

    ;; .+
    (is (magicl:= (magicl:.+ tensor 3.14)
                  (magicl:from-list (loop for i in input collect (+ i 3.14))
                                    (magicl:shape tensor)
                                    :type 'single-float)))

    ;; .-
    (is (magicl:= (magicl:.- 3.14 tensor)
                  (magicl:from-list (loop for i in input collect (- 3.14 i))
                                    (magicl:shape tensor)
                                    :type 'single-float)))

    ;; .*
    (is (magicl:= (magicl:.* 3.14 tensor)
                  (magicl:from-list (loop for i in input collect (* 3.14 i))
                                    (magicl:shape tensor)
                                    :type 'single-float)))

    ;; ./
    (is (magicl:= (magicl:./ 3.14 tensor)
                  (magicl:from-list (loop for i in input collect (/ 3.14 i))
                                    (magicl:shape tensor)
                                    :type 'single-float)))

    ;; .max
    (is (magicl:= (magicl:.max tensor 0.0)
                  (magicl:from-list (loop for i in input collect (max 0.0 i))
                                    (magicl:shape tensor)
                                    :type 'single-float)))
    ;; .min
    (is (magicl:= (magicl:.min 0.0 tensor)
                  (magicl:from-list (loop for i in input collect (min i 0.0))
                                    (magicl:shape tensor)
                                    :type 'single-float)))))

(deftest test-unary-ops ()
  "Test that basic unary operations on a tensor give the expected results"
  (let* ((input '(-1.1 -0.4 0.5 1.0))
         (tensor (magicl:from-list input
                                   '(2 2)
                                   :type 'single-float)))

    ;; .exp
    (is (magicl:= (magicl:.exp tensor)
                  (magicl:from-list (loop for i in input collect (exp i))
                                    (magicl:shape tensor)
                                    :type 'single-float)))

    ;; .log - Recall natural log of negative numbers are undefined!
    (let* ((input '(0.1 0.5 1.1 2.0))
           (tensor (magicl:from-list input
                                     '(2 2)
                                     :type 'single-float)))
      (is (magicl:= (magicl:.log tensor)
                    (magicl:from-list (loop for i in input collect (log i))
                                      (magicl:shape tensor)
                                      :type 'single-float))))))
