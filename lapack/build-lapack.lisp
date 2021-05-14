(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :f2cl :silent t))

(defpackage #:magicl.build-lapack
  (:use #:cl)
  (:export #:build #:print-system-definition #:print-package-definition))

(in-package #:magicl.build-lapack)

;;; TODO: relative to this system?
(defparameter *fortran-source-dir*
  (asdf:system-relative-pathname '#:magicl "lapack/fortran-src/"))

(defparameter *lisp-source-dir*
  (asdf:system-relative-pathname '#:magicl "lapack/lisp-src/"))

(defun build ()
  (uiop:delete-directory-tree *lisp-source-dir* :validate t :if-does-not-exist :ignore)
  (ensure-directories-exist *lisp-source-dir*)
  (loop :for input-file :in (uiop:directory-files *fortran-source-dir* "*.f")
        :for output-name := (concatenate 'string (pathname-name input-file) ".lisp")
        :for output-file := (merge-pathnames output-name *lisp-source-dir*)
        :do (f2cl:f2cl input-file :output-file output-file
                                  :package :magicl.lisp-lapack
                                  :common-as-array t ; per f2cl test suite
                                  :relaxed-array-decls nil ; per f2cl test suite
                                  )))

(defun print-system-definition ()
  (let ((*print-case* :downcase))
    (print
     `(asdf:defsystem #:magicl/lisp-lapack
        :description "Lisp LAPACK routines in MAGICL"
        :depends-on (#:f2cl #:magicl/lisp-blas)
        :serial t
        :pathname "lapack/"
        :components
        ((:file "package")
         (:file "fortran-intrinsics")
         ,@(loop :for input-file :in (uiop:directory-files *fortran-source-dir* "*.f")
                 :collect (list :file (concatenate 'string "lisp-src/" (pathname-name input-file)))))))
    nil))


(defun print-package-definition ()
  (let ((*print-case* :downcase))
    (print
     `(defpackage #:magicl.lisp-lapack
        (:use #:cl #:magicl.lisp-blas)
        (:export
         ,@(loop :for input-file :in (uiop:directory-files *fortran-source-dir* "*.f")
                 :collect (make-symbol (string-upcase (pathname-name input-file)))))))
    nil))
