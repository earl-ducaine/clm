;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:(user (lisp)); Base:10 -*-

(in-package 'user)
(use-package 'xtk)

(defvar *sccsid* "@(#)raise.lisp	1.3 11/27/91")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; raises the shell widgets when the button is clicked.
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(run-motif-application 'raise-test)

(defun raise-test (&aux app-shell app-rc hw)
  (setf app-shell (create-application-shell)
	app-rc (create-widget :row-column app-shell)
	button (create-widget :push-button app-rc :label-string "Raise shell"))
  (add-callback button :activate #'(lambda (w cl-d &rest ca-d)
					   (raise-widget cl-d)) app-shell)
  (realize-widget app-shell))
