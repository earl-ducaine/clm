;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:(user (lisp)); Base:10 -*-

(in-package 'user)
(use-package 'xtk)

(defvar *sccsid* "@(#)app.lisp	1.3 11/27/91")


'(run-motif-application 'app)

(defun app (&aux popup-shell popdown-b dialog-shell form  
		    d-popdown row-column 
		    popup-b d-popup toggle scale text  push-button 
		    arrow-button app-shell label d-form)
  (setf app-shell (create-application-shell))
  (setf popup-shell (create-popup-shell "Test" :dialog-shell app-shell))
  (setf form (create-unmanaged-widget :form popup-shell))
  (setf popdown-b (create-widget :push-button form :label-string "Down"))
  (add-callback popdown-b :activate #'unmanage-popup form)
  
  (setf dialog-shell (create-popup-shell "Dialog" :dialog-shell app-shell))
  (setf d-form (create-unmanaged-widget :form dialog-shell))
  (setf d-popdown (create-widget :push-button d-form))
  (add-callback d-popdown :activate #'unmanage-popup d-form)
  
  (setf row-column (create-widget :row-column app-shell 
				  :orientation :vertical))
  
  (setf popup-b (create-widget :push-button row-column 
			       :label-string "Alert ..."))
  (add-callback popup-b :activate #'alert-it form)
  
  (setf d-popup (create-widget :push-button row-column :label-string "Pop up"))
  (add-callback d-popup :activate #'manage-popup d-form)
  (setf label (create-widget :label row-column :label-string "Hello world"))
  (setf toggle (create-widget :toggle-button row-column :label-string "Click"
			      :set t))
  (setf scale (create-widget :scale row-column :orientation :horizontal))
  (add-callback scale :drag #'scale-new-value nil)
  (add-callback scale :value-changed #'scale-new-value nil)
  (setf text (create-widget :text row-column))
  (setf push-button (create-widget :push-button row-column))
  (add-callback push-button :activate #'test-callback row-column)
  (setf arrow-button (create-widget :arrow-button row-column))
  (add-callback arrow-button :arm #'on-off-callback t)
  (add-callback arrow-button :disarm #'on-off-callback nil)
  (realize-widget app-shell))

(defun test-callback (widget client-data &rest call-data)
  (declare (ignore widget client-data call-data))
  (format t "Click! Click !! ~%"))

(defun on-off-callback (widget client-data &rest call-data)
  (declare (ignore widget call-data))
  (format t (if client-data "On..  " "Off !~%")))

(defun scale-new-value (widget client-data value)
  (declare (ignore widget client-data))
  (format t "~d~%" value))

(defun alert-it (widget client-data &rest call-data)
  (declare (ignore widget call-data))
  (alert-dialog client-data))
