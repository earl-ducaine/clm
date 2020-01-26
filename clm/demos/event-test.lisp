;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:(user (xtk)); Base:10 -*-

(in-package 'user)
(use-package 'xtk)

(defvar *sccsid* "@(#)event-test.lisp	1.3 11/27/91")


(defun event-handler (widget client-data &rest event)
  (format t "event: widget=~a, client_data=~a, event=~a~%"
	  widget client-data event))

(defun expose-event-handler (widget client-data &rest event)
  (format t "expose-event: widget=~a, client_data=~a, event=~a~%"
	  widget client-data event))

(defun motion-handler (widget client-data &rest event)
  (format t "motion-notify-event: widget=~a, client_data=~a, event=~a~%"
	  widget client-data event))

(defun event-test (&aux shell label)
  (setf shell (create-application-shell)
	label (create-widget :push-button shell :label-string "Hallo"))
  ;(add-event-handler label :button-2-motion-mask 'event-handler nil)
  ;(add-event-handler label :button-3-motion-mask 'event-handler nil)
  (add-event-handler label :exposure-mask 'expose-event-handler nil)
  (add-event-handler label :pointer-motion-mask 'motion-handler nil)
  ;(remove-event-handler label :button-2-motion-mask 'event-handler nil)
  (realize-widget shell))

'(run-motif-application 'event-test)
