;;; -*- mode: lisp -*-

(in-package 'user)

(use-package 'xtk)

(defvar *sccsid* "@(#)pusht.lisp	1.3 11/27/91")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Main program
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(run-motif-application 'pusht)

(defun pusht (&aux push-pop app-shell app-rc sb)
  (setf app-shell (create-application-shell)
	app-rc (create-widget :row-column app-shell)
	push-pop (create-widget :toggle-button app-rc 
				:label-string "Discard Translations"
				:set t)
	sb (create-widget :scroll-bar app-rc 
			  :orientation :horizontal
			  :maximum 100 :width 300))
  (push-translations sb)
  (add-callback sb :value-changed #'pusht-value-changed nil)
  (add-callback sb :decrement #'pusht-decrement nil)
  (add-callback sb :drag #'pusht-drag nil)
  (add-callback sb :increment #'pusht-increment nil)
  (add-callback sb :page-decrement #'pusht-page-decrement nil)
  (add-callback sb :page-increment #'pusht-page-increment nil)
  (add-callback sb :to-bottom #'pusht-to-bottom nil)
  (add-callback sb :to-top #'pusht-to-top nil)
  (add-callback push-pop :value-changed #'pusht-push-and-pop sb)
  (realize-widget app-shell))

(defun pusht-push-and-pop (widget client-data &rest call-data)
  (declare (ignore widget))
  (if (first call-data)
      (push-translations client-data)
      (pop-translations client-data)))

(defun pusht-decrement (widget client-data &rest call-data)
  (declare (ignore widget client-data))
  (format t "decremented to ~a~%" (first call-data)))

(defun pusht-drag (widget client-data &rest call-data)
  (declare (ignore widget client-data))
  (format t "draged to ~a~%" (first call-data)))

(defun pusht-increment (widget client-data &rest call-data)
  (declare (ignore widget client-data))
  (format t "incremented to ~a~%" (first call-data)))

(defun pusht-page-decrement (widget client-data &rest call-data)
  (declare (ignore widget client-data))
  (format t "page-decremented to ~a~%" (first call-data)))

(defun pusht-page-increment (widget client-data &rest call-data)
  (declare (ignore widget client-data))
  (format t "page-incremented to ~a~%" (first call-data)))

(defun pusht-to-bottom (widget client-data &rest call-data)
  (declare (ignore widget client-data))
  (format t "moved to bottom ~a ~a~%" (first call-data) (second call-data)))

(defun pusht-to-top (widget client-data &rest call-data)
  (declare (ignore widget client-data))
  (format t "moved to top ~a ~a~%" (first call-data) (second call-data)))

(defun pusht-value-changed (widget client-data &rest call-data)
  (declare (ignore widget client-data))
  (format t "value changed to ~a~%" (first call-data)))
