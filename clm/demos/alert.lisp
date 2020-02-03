;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:(alert (lisp)); Base:10 -*-

(defpackage :alert
  (:use cl))

(in-package :alert)
(use-package :xtk)

(defvar *sccsid* "@(#)alert.lisp	1.3 11/27/91")



;;; Main program


'(run-motif-application 'alert::alert-test)

(defun alert-test (&aux app-shell app-rc hw)
  (setf app-shell (create-application-shell)
	app-rc (create-widget :row-column app-shell)
	hw   (create-widget :push-button app-rc :label-string "Alert"))
  (add-callback hw :activate #'alert hw)
  (realize-widget app-shell))

(defun alert (widget client-data &rest call-data)
  (declare (ignore widget call-data))
  (let* ((d-shell (create-popup-shell "Test" :dialog-shell client-data))
	(form (create-unmanaged-widget :form d-shell
				       :auto-unmanage nil))
	(butt (create-push-button form "alert")))
    (add-callback butt :activate #'unmanage-popup form)
    (alert-dialog form))
  (let* ((d-shell (create-popup-shell "Test" :dialog-shell client-data))
	(form (create-unmanaged-widget :form d-shell
				       :auto-unmanage nil))
	(butt (create-push-button form "alert")))
    (add-callback butt :activate #'unmanage-popup form)
    (alert-dialog form))
  (let* ((d-shell (create-popup-shell "Test" :dialog-shell client-data))
	(form (create-unmanaged-widget :form d-shell
				       :auto-unmanage nil))
	(butt (create-push-button form "alert")))
    (add-callback butt :activate #'unmanage-popup form)
    (alert-dialog form))
  (let* ((d-shell (create-popup-shell "Test" :dialog-shell client-data))
	(form (create-unmanaged-widget :form d-shell
				       :auto-unmanage nil))
	(butt (create-push-button form "alert")))
    (add-callback butt :activate #'unmanage-popup form)
    (alert-dialog form))
  (let* ((d-shell (create-popup-shell "Test" :dialog-shell client-data))
	(form (create-unmanaged-widget :form d-shell
				       :auto-unmanage nil))
	(butt (create-push-button form "alert")))
    (add-callback butt :activate #'unmanage-popup form)
    (alert-dialog form)))
