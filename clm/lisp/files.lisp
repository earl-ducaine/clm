;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:xtk; Base:10 -*-

(in-package :xtk)

(setq *sccsid* "@(#)files.lisp	1.9 9/11/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Test whether a given pathname is a directory.
;;;;; Uses the system dependent function 'is-directory
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-genera
(defun directoryp (path &aux result)
  (setq result (is-directory path))
  (when (minusp result)
	(error "directory-p: Can't stat ~s~%" path))
  (plusp result))

