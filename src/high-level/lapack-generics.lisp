;;;; lapack-generics.lisp
;;;;
;;;; Author: Cole Scott

(in-package #:magicl)

(defgeneric mult (a b &key target alpha beta transa transb)
  (:documentation "Multiply a by b, storing in target or creating a new tensor if target is not specified.
Target cannot be the same as a or b."))

(defgeneric lapack-eig (matrix))

(defgeneric lapack-hermitian-eig (matrix))

(defgeneric lapack-lu (matrix))

(defgeneric lapack-inv (matrix))

(defgeneric lapack-csd (matrix p q))

(defgeneric lapack-svd (matrix &key reduced))

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
