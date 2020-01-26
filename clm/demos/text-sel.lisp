;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:(user (lisp)); Base:10 -*-

(in-package 'user)
(use-package 'xtk)

(defvar *sccsid* "@(#)text-sel.lisp	1.3 11/27/91")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Dumps core because of bugs in the XmText widget ....
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(run-motif-application 'text-sel)

(defun text-sel (&aux app-shell app-rc)
  (setf app-shell (create-application-shell)
	app-rc (create-widget :row-column app-shell)
	text-1 (create-widget :text app-rc :edit-mode :multi-line-edit 
			      :columns 40 :rows 5)
	copy-b (create-widget :push-button app-rc
			      :label-string "Copy selection")
	text-2 (create-widget :text app-rc :edit-mode :multi-line-edit 
			      :columns 40 :rows 5))
  (add-callback copy-b :activate 
		#'(lambda (widget client-data &rest call-data)
			  (set-values text-2 
				      :value (get-text-selection text-1)))
		nil)
  (realize-widget app-shell))
