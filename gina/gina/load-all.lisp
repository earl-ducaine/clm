;;; -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-
(in-package :user)

(setq *sccs-id* "@(#)load-all.lisp	1.2 11/10/92")

;; Make Garbage collecting rarer within CMU-CL
#+cmu (setf *bytes-consed-between-gcs* 10000000)


;; system-specific definitions

#+cmu
(defun cd (&optional (d "home:")) (setf (default-directory)  d) (pwd))

#+excl
(defun cd (&optional (d "~")) (chdir d))

#+cmu
(defun pwd () (default-directory))

#+excl
(defun pwd () (excl:current-directory))

;; load all files for the GINA-System

(cd "../clm/lisp")
(load "load")
(cd "../../gina")
(load "load")
(cd "../gina-demos")
(load "load")
(cd "../ib")
(load "load")

(cd "../documents")
(gina:make-finder)


