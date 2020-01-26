;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:(user (lisp)); Base:10 -*-

(in-package 'user)
(use-package 'xtk)

(defvar *sccsid* "@(#)sample-app.lisp	1.3 11/27/91")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Prototype of a simple CLM application
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(run-motif-application 'sample-app)

(defun sample-app (&aux app-shell app-rc hw)
  (setf app-shell (create-application-shell)
	app-rc (create-row-column "row" app-shell)
	hw   (create-label "label" app-rc :label-string "Hello, world."))
  (realize-widget app-shell))
