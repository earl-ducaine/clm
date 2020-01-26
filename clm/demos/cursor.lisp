;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:(user (lisp)); Base:10 -*-

(in-package 'user)
(use-package 'xtk)

(defvar *sccsid* "@(#)cursor.lisp	1.3 11/27/91")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Main program
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(run-motif-application 'qp :server-host "aquire")

(defun qp (&aux app-shell app-rc hw)
  (setf app-shell (create-application-shell)
	app-rc (create-widget :row-column app-shell)
	hw   (create-widget :push-button app-rc :label-string "Query pointer"))
  (add-callback hw :activate #'query hw)
  (realize-widget app-shell))

(defun query (widget client-data &rest call-data)
  (setf pp (query-pointer client-data))
  (format t "Absolute pointer coordinates: (~a,~a)~%" (first pp) (second pp))
  (format t "Relative pointer coordinates: (~a,~a)~%" (third pp) (fourth pp)))
