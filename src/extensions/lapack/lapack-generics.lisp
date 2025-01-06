;;;; lapack-generics.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl-lapack)

;;; We use the convention "FOO-EXTENSION" to refer to this extension's
;;; generic function implementation of MAGICL:FOO.

(magicl:extend-function (magicl:mult mult-extension :lapack) (a b &key target alpha beta transa transb))

(magicl:extend-function (magicl:eig lapack-eig :lapack) (matrix))

(magicl:extend-function (magicl:hermitian-eig lapack-hermitian-eig :lapack) (matrix))

(magicl:extend-function (magicl:lu lapack-lu :lapack) (matrix))

(magicl:extend-function (magicl:lu-solve lapack-lu-solve :lapack) (lu ipiv b))

(magicl:extend-function (magicl:inv lapack-inv :lapack) (matrix))

(magicl:extend-function (magicl:csd-blocks csd-blocks-extension :lapack) (matrix p q))

(magicl:extend-function (magicl:svd lapack-svd :lapack) (matrix &key reduced))

(magicl:extend-function (magicl:schur schur-extension :lapack) (matrix))

(magicl:extend-function (magicl:qz qz-extension :lapack) (matrix1 matrix2))

(magicl:extend-function (magicl:ql ql-extension :lapack) (matrix))

(magicl:extend-function (magicl:qr qr-extension :lapack) (matrix))

(magicl:extend-function (magicl:rq rq-extension :lapack) (matrix))

(magicl:extend-function (magicl:lq lq-extension :lapack) (matrix))

(defgeneric lapack-ql (matrix)
  (:documentation "Find the LAPACK intermediate representation of ql of a matrix"))

(defgeneric lapack-qr (matrix)
  (:documentation "Find the LAPACK intermediate representation of qr of a matrix"))

(defgeneric lapack-rq (matrix)
  (:documentation "Find the LAPACK intermediate representation of rq of a matrix"))

(defgeneric lapack-lq (matrix)
  (:documentation "Find the LAPACK intermediate representation of lq of a matrix"))

(defgeneric lapack-ql-q (matrix tau)
  (:documentation "Finds the unitary matrix Q from QL factorization of the matrix M, given the reflectors and intermediate representation provided by lapack-ql"))

(defgeneric lapack-qr-q (matrix tau)
  (:documentation "Finds the unitary matrix Q from QR factorization of the matrix M, given the reflectors and intermediate representation provided by lapack-qr"))

(defgeneric lapack-rq-q (matrix tau)
  (:documentation "Finds the unitary matrix Q from RQ factorization of the matrix M, given the reflectors and intermediate representation provided by lapack-rq"))

(defgeneric lapack-lq-q (matrix tau)
  (:documentation "Finds the unitary matrix Q from LQ factorization of the matrix M, given the reflectors and intermediate representation provided by lapack-lq"))
