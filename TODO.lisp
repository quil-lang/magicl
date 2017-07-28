
;;; -*- mode: lisp -*-

;;; Time-stamp: <2008-10-07 08:02:03 tony>
;;; Creation:   <2008-09-08 08:06:30 tony>
;;; File:       TODO.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c) 2007-2008, AJ Rossini <blindglobe@gmail.com>.  BSD.
;;; Purpose:    demonstrations of how one might use CLS.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This file contains the current challenges to solve, including a
;;; description of the setup and the work to solve....
 
;;; SET UP

(in-package :cl-user)

(asdf:oos 'asdf:load-op 'lift)
(asdf:oos 'asdf:load-op 'org.middleangle.cl-blapack)



(in-package :lisp-matrix-user)
