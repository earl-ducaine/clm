;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:(gina-view (xtk)); Base:10 -*-

(in-package 'gina-view)

(use-package 'xtk)

(defvar *sccsid* "@(#)gina-view.lisp	1.3 11/27/91")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Main program
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(run-motif-application 'gina-view::gina-view)

(defun gina-view (&aux connection app-shell app-rc rc draw frame view)
  (setf app-shell (create-application-shell :allow-shell-resize :t)
	app-rc (create-widget :row-column app-shell)
	frame (create-widget :frame app-rc)
	draw (create-widget :drawing-area frame 
			    :resize-policy :none
			    :width 200 :height 200)
	frame (create-widget :frame app-rc)
	view (create-widget :gina-view frame 
			    :resize-policy :none
			    :width 200 :height 200))
  (add-callback draw :input #'input nil)
  (add-callback draw :resize #'resize nil)
  (add-callback draw :expose #'expose nil)
  (add-callback view :input #'input nil)
  (add-callback view :resize #'resize nil)
  (add-callback view :expose #'expose nil)
  (realize-widget app-shell))

(defun input (widget client-data &rest call-data)
  (format t "input~%"))
(defun resize (widget client-data &rest call-data)
  (format t "resize~%"))
(defun expose (widget client-data &rest call-data)
  (format t "expose~%"))
