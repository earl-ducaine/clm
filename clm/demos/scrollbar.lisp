;;; -*- Mode:lisp; Syntax:Common-Lisp; Package: (user (lisp)); Base:10 -*-

(in-package 'user)

(use-package 'xtk)

(defvar *sccsid* "@(#)scrollbar.lisp	1.3 11/27/91")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Main program
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(run-motif-application 'scrollbar)

(defun scrollbar (&aux app-shell app-rc sb)
  (setf app-shell (create-application-shell)
	app-rc (create-widget :row-column app-shell)
	sb (create-widget :scroll-bar app-rc 
			  :orientation :horizontal
			  :maximum 100 :width 300))
  (add-callback sb :value-changed #'value-changed nil)
  (add-callback sb :decrement #'decrement nil)
  (add-callback sb :drag #'drag nil)
  (add-callback sb :increment #'increment nil)
  (add-callback sb :page-decrement #'page-decrement nil)
  (add-callback sb :page-increment #'page-increment nil)
  (add-callback sb :to-bottom #'to-bottom nil)
  (add-callback sb :to-top #'to-top nil)
  (realize-widget app-shell))

(defun decrement (widget client-data &rest call-data)
  (declare (ignore widget client-data))
  (format t "decremented to ~a~%" (first call-data)))

(defun drag (widget client-data &rest call-data)
  (declare (ignore widget client-data))
  (format t "draged to ~a~%" (first call-data)))

(defun increment (widget client-data &rest call-data)
  (declare (ignore widget client-data))
  (format t "incremented to ~a~%" (first call-data)))

(defun page-decrement (widget client-data &rest call-data)
  (declare (ignore widget client-data))
  (format t "page-decremented to ~a~%" (first call-data)))

(defun page-increment (widget client-data &rest call-data)
  (declare (ignore widget client-data))
  (format t "page-incremented to ~a~%" (first call-data)))

(defun to-bottom (widget client-data &rest call-data)
  (declare (ignore widget client-data))
  (format t "moved to bottom ~a ~a~%" (first call-data) (second call-data)))

(defun to-top (widget client-data &rest call-data)
  (declare (ignore widget client-data))
  (format t "moved to top ~a ~a~%" (first call-data) (second call-data)))

(defun value-changed (widget client-data &rest call-data)
  (declare (ignore widget client-data))
  (format t "value changed to ~a~%" (first call-data)))
