;;;; specialization-tests.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl-tests)

(defmacro is-subtype (type &rest rest)
  `(progn ,@(loop :for m :in rest
                  :collect `(is (subtypep (type-of ,m) ',type)))))

(defmacro is-not-subtype (type &rest rest)
  `(progn ,@(loop :for m :in rest
                  :collect `(is (not (subtypep (type-of ,m) ',type))))))

(deftest test-tensor-shape-specialization ()
  (let ((vector (empty '(1)))
        (matrix (empty '(1 2)))
        (tensor (empty '(1 2 3))))
    (is-subtype magicl::vector vector)
    (is-subtype magicl::matrix matrix)
    (is-subtype magicl::tensor tensor)
    (is-not-subtype magicl::vector matrix tensor)
    (is-not-subtype magicl::matrix vector tensor)
    (is-not-subtype magicl::tensor vector matrix)))

(deftest test-tensor-type-specialization ()
  (loop :for type :in +magicl-types+
        :for class :in +magicl-matrix-classes+
        :do (is (equalp
                 class
                 (class-name (class-of (empty '(2 2) :type type)))))))
