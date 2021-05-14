(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :f2cl :silent t))

(defpackage #:magicl.build-blas
  (:use #:cl)
  (:export #:build #:print-system-definition #:print-package-definition))

(in-package #:magicl.build-blas)

;;; TODO: relative to this system?
(defparameter *fortran-source-dir*
  (asdf:system-relative-pathname '#:magicl "blas/fortran-src/"))

(defparameter *lisp-source-dir*
  (asdf:system-relative-pathname '#:magicl "blas/lisp-src/"))


(defun build ()
  (uiop:delete-directory-tree *lisp-source-dir* :validate t :if-does-not-exist :ignore)
  (ensure-directories-exist *lisp-source-dir*)
  (loop :for input-file :in (uiop:directory-files *fortran-source-dir* "*.f")
        :for output-name := (concatenate 'string (pathname-name input-file) ".lisp")
        :for output-file := (merge-pathnames output-name *lisp-source-dir*)
        :do (f2cl:f2cl input-file :output-file output-file :package :magicl.lisp-blas)))

(defun print-system-definition ()
  (let ((*print-case* :downcase))
    (print
     `(asdf:defsystem #:magicl/lisp-blas
        :description "Lisp BLAS routines in MAGICL"
        :depends-on (#:f2cl)
        :serial t
        :pathname "blas/"
        :components
        ((:file "package")
         ,@(loop :for input-file :in (uiop:directory-files *fortran-source-dir* "*.f")
                 :collect (list :file (concatenate 'string "lisp-src/" (pathname-name input-file)))))))
    nil))


(defun print-package-definition ()
  (let ((*print-case* :downcase))
    (print
     `(defpackage #:magicl.lisp-blas
        (:use :common-lisp)
        (:export
         ,@(loop :for input-file :in (uiop:directory-files *fortran-source-dir* "*.f")
                 :collect (make-symbol (string-upcase (pathname-name input-file)))))))
    nil))
