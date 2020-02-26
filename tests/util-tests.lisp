;;;; util-tests.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl-tests)

(deftest test-row-major-index ()
  (let ((rows 3)
        (cols 5))
    (loop :for i :below (* rows cols)
          :do (is (cl:= i (magicl::row-major-index
                           (magicl::from-row-major-index i (list rows cols))
                           (list rows cols)))))
    (let ((examples '((0 (0 0))
                      (1 (0 1))
                      (4 (0 4))
                      (5 (1 0))
                      (7 (1 2)))))
      (loop :for ex :in examples
            :do (is (cl:= (first ex)
                          (magicl::row-major-index
                           (second ex)
                           (list rows cols))))
                (is (equal (second ex)
                           (magicl::from-row-major-index
                            (first ex)
                            (list rows cols))))))))

(deftest test-column-major-index ()
  (let ((rows 3)
        (cols 5))
    (loop :for i :below (* rows cols)
          :do (is (cl:= i (magicl::column-major-index
                           (magicl::from-column-major-index i (list rows cols))
                           (list rows cols)))))
    (let ((examples '((0 (0 0))
                      (1 (1 0))
                      (4 (1 1))
                      (5 (2 1))
                      (7 (1 2)))))
      (loop :for ex :in examples
            :do (is (cl:= (first ex)
                          (magicl::column-major-index
                           (second ex)
                           (list rows cols))))
                (is (equal (second ex)
                           (magicl::from-column-major-index
                            (first ex)
                            (list rows cols))))))))

(deftest test-map-indexes ()
  (let* ((rows 3)
         (cols 5)
         (arr (make-array (list rows cols) :element-type 'double-float :initial-element 0d0)))
    (magicl::map-indexes (list rows cols)
                         (lambda (&rest pos)
                           (incf (apply #'aref arr pos))))
    (loop :for r :below rows
          :do (loop :for c :below cols
                    :do (is (cl:= 1 (aref arr r c)))))))
