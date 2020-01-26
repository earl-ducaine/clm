;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:(user (lisp xtk)); Base:10 -*-

(in-package 'user)
(use-package 'xtk)

'(xtk::run-motif-application 'two-popups)

(defun two-popups ()
 (let* ((shell (create-application-shell))
	(top-1 (create-popup-shell "top1" :toplevel-shell shell))
	(button-1 (create-widget :push-button top-1 :label-string "Popup-1"))
	(top-2 (create-popup-shell "top2" :toplevel-shell shell))
	(button-2 (create-widget :push-button top-2 :label-string "Popup-2")))
   (add-wm-protocol-callback top-1 :wm-delete-window
			     #'xtk::quit-application shell)
   (add-wm-protocol-callback top-2 :wm-delete-window
			     #'xtk::quit-application shell)
   (add-callback button-1 :activate #'xtk::quit-application shell)
   (add-callback button-2 :activate #'xtk::quit-application shell)
   (popup top-1)
   (popup top-2)))

'(xtk::run-motif-application 'popup-menu)

(defun popup-menu ()
 (let* ((shell (create-application-shell))
	(label (create-widget :label shell :label-string "Popup"))
	;; Use left button for selections
	(menu (create-popup-menu label "Menu" :which-button 1))
	(button-1 (create-push-button menu "Red"))
	(button-2 (create-push-button menu "Yellow"))
	(button-3 (create-push-button menu "Green"))
	(button-4 (create-push-button menu "Blue")))
   (add-event-handler label :button-press-mask #'do-popup menu)
   (realize-widget shell)))

(defun do-popup (widget client-data &rest call-data)
  (declare (ignore widget))
  (set-values client-data :x (fourth call-data) :y (fifth call-data))
  (manage-widgets client-data))
