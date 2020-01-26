;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:xtk; Base:10 -*-

(in-package :xtk)

(setq *sccsid* "@(#)demo.lisp	1.1 1/29/92")

(defun clm-demo ()
  (run-motif-application 'clm-demo-app 
			 :application-name "clm-demo"
			 :application-class "Clm-demo"
			 :process-name "CLM demo"))

(defun clm-demo-app ()
 (let* ((shell (create-application-shell :title "CLM demo program"))
	(main-window (create-main-window shell "mainw"))
        (mb (create-menu-bar main-window "mb"))
	(file-pane (create-pulldown-menu mb "filepane"))
        (file-cascade (create-cascade-button mb "mbfile"
					     :label-string "File"
					     :mnemonic "F"
					     :sub-menu-id file-pane))
        (test-pane (create-pulldown-menu mb "testpane"))
        (test-cascade (create-cascade-button mb "mbtest"
					     :label-string "Test"
					     :mnemonic "T"
					     :sub-menu-id test-pane))
        (close-button (create-push-button file-pane "close-button"
					  :label-string "Exit"
					  :accelerator "Alt<Key>E"
					  :accelerator-text "Alt+E"))
	(new-button (create-push-button test-pane "new"
				    :label-string "Add Item"))
	(enable-button (create-push-button test-pane "enable"
					   :label-string "Start Timer"))
	(disable-button (create-push-button test-pane "disable"
					    :label-string "Stop Timer"))
        (info-button (create-push-button test-pane "info-button"
					 :label-string "Info Dialog"))
        (form (create-form main-window "form"
			   :horizontal-spacing 4
			   :vertical-spacing 4))
        (time-label1 (create-label form "label1"
				   :top-attachment :attach-form
				   :left-attachment :attach-form
				   :label-string "Current Time is"))
        (time-label2 (create-label form "label2"
				   :top-attachment :attach-form
				   :left-attachment :attach-widget
				   :left-widget time-label1
				   :label-string "00:00:00"))
        (status-text (create-text form "text"
				  :columns 30
				  :bottom-attachment :attach-form
				  :left-attachment :attach-form
				  :right-attachment :attach-form))
        (list1 (create-scrolled-list form "list1"
				     :automatic-selection t
				     :top-attachment :attach-widget
				     :top-widget time-label1
				     :left-attachment :attach-form
				     :bottom-attachment :attach-widget
				     :bottom-widget status-text))
        (scrolled-window (create-scrolled-window form "scroller"
				     :scrolling-policy :automatic
				     :top-attachment :attach-widget
				     :top-widget time-label1
				     :left-attachment :attach-widget
				     :left-widget list1
				     :right-attachment :attach-form
				     :bottom-attachment :attach-widget
				     :bottom-widget status-text))
        (view (create-drawing-area scrolled-window "da" 
				   :width 500 :height 500
				   :resize-policy :resize-none))
	(d-form (create-information-dialog form "info-dialog"
					   :message-string
					   "Click any button to continue !"
					   :dialog-title "Information"))
	(entry-form (create-prompt-dialog form "entry-dialog"
					  :selection-label-string
					  "Enter the new item"
					  :dialog-title "New Item"))
	(timer (create-timer 1000 #'update-time-cb time-label2)))
   (declare (ignore file-cascade test-cascade))
   (set-items list1 "First item" "Second item" "Third item")
   (add-callback list1 :browse-selection #'selection-cb status-text)
   (add-callback close-button :activate #'xtk:quit-application shell)
   (add-callback info-button :activate #'xtk:manage-popup d-form)
   (set-insensitive enable-button)
   (add-callback enable-button :activate #'start-timer-cb
		 (list timer enable-button disable-button))
   (add-callback disable-button :activate #'stop-timer-cb
		 (list timer enable-button disable-button))
   (add-callback new-button :activate #'xtk:manage-popup entry-form)
   (add-callback entry-form :ok #'add-entry-cb list1)
   (add-event-handler view :button-press-mask #'view-event-handler status-text)
   (realize-widget shell)))

(defun update-time-cb (timer client-data)
  "one time interval is over => refresh the clock"
  (declare (ignore timer))
  (multiple-value-bind (sec min hrs) 
      (decode-universal-time (get-universal-time))
    ;; Display new time in the label widget
    (set-values client-data :label-string
	      (format nil "~2,'0d:~2,'0d:~2,'0d" hrs min sec))))

(defun stop-timer-cb (widget timer-and-buttons &rest call-data)
  (declare (ignore widget call-data))
  (stop-timer (first timer-and-buttons))
  (set-sensitive (second timer-and-buttons))
  (set-insensitive (third timer-and-buttons)))

(defun start-timer-cb (widget timer-and-buttons &rest call-data)
  (declare (ignore widget call-data))
  (start-timer (first timer-and-buttons))
  (set-insensitive (second timer-and-buttons))
  (set-sensitive (third timer-and-buttons)))

(defun add-entry-cb (widget list-widget &rest call-data)
  (declare (ignore call-data))
  (let ((new-value (first (get-values widget :text-string))))
    (list-add-item list-widget new-value 0)))

(defun view-event-handler (widget text-widget type &rest event)
  (declare (ignore widget))
  (set-values text-widget :value
	      (format nil "Event ~d ~{~s ~}" type event)))

(defun selection-cb (widget text-widget &rest call-data)
  (declare (ignore widget))
  (set-values text-widget :value
	      (format nil "Select ~a (~s)" (first call-data) 
		      (second call-data))))
