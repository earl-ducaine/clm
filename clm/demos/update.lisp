;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:(user (lisp)); Base:10 -*-

(in-package 'user)
(use-package 'xtk)

(defvar *sccsid* "@(#)update.lisp	1.3 11/27/91")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; demo for the non-documented macro 'with-immediate-update-enabled
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(run-motif-application 'update-test)

(defun update-test (&aux app-shell app-rc hw b)
  (setf app-shell (create-application-shell)
	app-rc (create-widget :row-column app-shell)
	hw   (create-widget :push-button app-rc :label-string "immediate")
	b (create-widget :push-button app-rc :label-string "delayed"))
  (add-callback hw :activate 'in-update hw)
  (add-callback b :activate 'd-update b)
  (realize-widget app-shell))

(defun d-update (widget client-data &rest call-data)
   (loop for i from 1 to 20 do
     (set-values client-data :label-string "aaaaaaaaaaaaaaaaaaaa")
     (set-values client-data :label-string ""))
   (set-values client-data :label-string "aaaaaaaaaaaaaaaaaaaa"))

(defun in-update (widget client-data &rest call-data)
  (with-immediate-update-enabled
   (loop for i from 1 to 20 do
     (set-values client-data :label-string "aaaaaaaaaaaaaaaaaaaa")
     (set-values client-data :label-string ""))
   (set-values client-data :label-string "aaaaaaaaaaaaaaaaaaaa")))




