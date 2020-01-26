;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:(user (lisp xtk)); Base:10 -*-

(in-package 'user)
(use-package 'xtk)

(defvar *sccsid* "@(#)clm-test.lisp	1.4 11/27/91")

'(xtk::run-motif-application 'clm-test :execute-main-loop nil
  :application-name "Actions" :use-clx t)

(defun xtk::hello (widget args event)
  (format t "New hello~%")
  (format t "widget ~a args ~a event ~a~%" widget args event))

(defstruct button-list
  (blist nil)
  (expose 0)
  (expose-events 0)
  (lock (xtk::make-process-lock)))

(defstruct process-info
  (running t)
  (button nil))

(defun  create-inspector (widget-id id &rest ignore)
  (let* ((result nil)
	 (shell (create-dialog-shell id "foo"))
	 (form (create-selection-box shell "foo Value"
				     :auto-unmanage nil
				     :dialog-type :dialog-prompt
				     :list-visible-item-count 0)))

    (add-callback form :ok #'(lambda (id ignore  string)
			       (declare (ignore ignore))
			       (format t "string = ~s~%" string)
			       (unmanage-popup-child id)
			       (setq result string)) nil)
    (alert-dialog form)
    (destroy-widget form)
    (format t "Exit result is ~S~%" result)))

(defun clm-test ()
 (let* ((shell (create-application-shell :geometry "600x600"))
	(main-window (create-main-window shell "mainw"
					 :scrolling-policy :automatic))
	(d-shell (create-popup-shell "Test" :dialog-shell shell))
	(d-form (create-information-dialog d-shell "InformationDialog"
					   :message-string
					   "Click any button to continue !"))
        (mb (create-menu-bar main-window "mb"))
	(file-pane (create-pulldown-menu mb "filepane"))
        (file-cascade (create-cascade-button mb "mbfile"
					     :label-string "Buttons"
					     :sub-menu-id file-pane))
	(*expose-counter* (create-cascade-button mb "expose"
						 :label-string "Expose: 0"))
	(*expose-e-counter* (create-cascade-button mb "expose"
						   :label-string
						   "Expose Events: 0"))
	(clock-1 (create-cascade-button mb "mbfile"
					:label-string "00:00:00"))
	(clock-2 (create-cascade-button mb "mbfile"
					:label-string "00:00:00"))
	(clock-3 (create-cascade-button mb "mbfile"
					:label-string "00:00:00"))
        (new-button (create-push-button file-pane "new"
					:label-string "New Moving Button"))
	(center (create-push-button file-pane "center"
				    :label-string "Center All"))
	(enable-button (create-push-button file-pane "enable"
					   :label-string "Start Timers"))
	(disable-button (create-push-button file-pane "disable"
					    :label-string "Stop Timers"))
	(start-timer-b (create-push-button file-pane "start"
					  :label-string "Start First Timer"))
	(destroy-timer-b (create-push-button file-pane "destroy"
					  :label-string "Destroy First Timer"))
	(alert-button (create-push-button file-pane "alert"
					  :label-string "Alert dialog"))
	(draw (create-drawing-area main-window "draw" :width 600 :height 600
				   :resize-policy :resize-none))
	(timer-1 (create-timer 2000 #'update-time clock-1 :active-in-handler t
			       :enabled nil))
	(timer-2 (create-timer 100 #'update-time clock-2 :active-in-handler nil))
	(timer-3 (create-timer 1000 #'update-time clock-3 :active-in-handler t
			       :active-in-recursive-main-loops t))
	(inspector-button (create-push-button file-pane "inspect"
					      :label-string "Inspect"))
	(unmanaged-button (create-push-button file-pane "unmanaged"
					      :managed nil))
	(managed-button (create-push-button file-pane "managed"
					    :managed t))
	(button-list (make-button-list))
	(client-message-button (create-push-button file-pane "scm"
						 :label-string
						 "Send client message"))
	(*clx-window* nil)
	)
   (declare (special *expose-counter* *expose-e-counter* *clx-window*)
	    (ignore file-cascade unmanaged-button managed-button))
   (add-callback inspector-button :activate #'create-inspector draw)
   (add-callback new-button :activate #'fork-process (list draw button-list))
   (add-callback center :activate #'center-all button-list)
   (add-callback draw :expose #'count-expose (list button-list *expose-counter*))
   (add-callback draw :input #'show-input nil)
   (add-event-handler draw  :no-event-mask #'report-client-message nil
		      :non-maskable t)
   (add-event-handler draw :exposure-mask #'count-expose-events
		      (list button-list *expose-e-counter*))
   (add-callback client-message-button :activate #'send-client-message draw)
   (add-callback enable-button :activate #'enable-timers
		 (list disable-button timer-2 timer-3))
   (add-event-handler enable-button :button-release-mask #'notify-user
		      (list button-list *expose-e-counter*) :non-maskable t)
   (add-callback disable-button :activate #'disable-timers
		 (list enable-button timer-2 timer-3))
   (add-callback start-timer-b :activate #'start-first-timer
		 (list timer-1 destroy-timer-b))
   (add-callback destroy-timer-b :activate #'destroy-first-timer
		 (list timer-1 clock-1))
   (add-callback alert-button :activate #'alert-a-dialog d-form)
   (set-insensitive enable-button)
   (set-insensitive destroy-timer-b)
   ;;(alert-dialog d-form)
   (realize-widget shell)
   ;;(alert-dialog d-form)
   (app-main-loop)))

(defun start-first-timer (widget client-data &rest call-data)
  (start-timer (first client-data))
  (set-insensitive widget)
  (set-sensitive (second client-data)))

(defun destroy-first-timer (widget client-data &rest call-data)
  (destroy-timer (first client-data))
  (destroy-widget (second client-data))
  (set-insensitive widget))

(defun notify-user (widget client-data &rest call-data)
  (format t "BUTTON-RELEASE~%"))

(defun report-client-message (widget client-data &rest call-data)
  (format t "Client message for widget ~a is: <~a> <~a>~%"
	  widget client-data call-data))

'(xtk::run-motif-application #'clm-test :execute-main-loop nil
  :application-name "Actions" :use-clx t)

(defun send-client-message (widget client-data &rest call-data)
  (declare (ignore call-data) (special *clx-window*))
  (unless *clx-window*
    (setf *clx-window* (make-clx-window client-data)))
;;  (xlib:send-event *clx-window* :client-message
;;		   nil ;; send to owner
;;		   :window *clx-window*
;;		   :event-window *clx-window*
;;		   :format 32
;;		   :type :gina-message
;;		   :data (list 0 0 0 0 0)
;;		   )
  )

(defun alert-a-dialog (widget client-data &rest call-data)
  (alert-dialog client-data))

(defun enable-timers (widget client-data &rest call-data)
  (set-sensitive (first client-data))
  (set-insensitive widget)
  (start-timer (second client-data))
  (start-timer (third client-data)))

(defun disable-timers (widget client-data &rest call-data)
  (declare (special *motif-connection*))
  (set-sensitive (first client-data))
  (set-insensitive widget)
  (stop-timer (second client-data))
  (stop-timer (third client-data)))

(defun update-time (timer clock &aux now)
  "one time interval is over => refresh the clock"
  (setq now (multiple-value-list (decode-universal-time (get-universal-time))))
  ;; Display new time in the label widget
  (set-values clock :label-string
	      (format nil "~2,'0d:~2,'0d:~2,'0d"
		      (third now) (second now) (first now))))

(defun count-expose (widget client-data &rest call-data)
  (declare (ignore widget))
  (format t "~a~%" call-data)
  (incf (button-list-expose (first client-data)))
  (set-values (second client-data) :label-string
	      (format nil "Expose: ~d"
		      (button-list-expose (first client-data)))))

(defun show-input (widget client-data &rest call-data)
  (declare (ignore widget client-data))
  (format t "~a~%" call-data))

(defun count-expose-events (widget client-data &rest call-data)
  (declare (ignore widget))
  (incf (button-list-expose-events (first client-data)) (second call-data))
  (set-values (second client-data) :label-string
	      (format nil "Expose Events: ~d"
		      (button-list-expose-events (first client-data)))))

(defun fork-process (widget client-data &rest call-data)
  (declare (ignore widget call-data))
  (run-secondary-process "Moving Button" #'moving-button
			 (first client-data) (second client-data)))

(defun moving-button (draw button-list)
  (let* ((button (create-push-button draw "push" :label-string "Stop me"
				     :x (random 500) :y (random 500)))
	 (struct (make-process-info)))
    (xtk::with-process-lock ((button-list-lock button-list))
			    (push struct (button-list-blist button-list)))
    (setf (process-info-button struct) button)
    (add-callback button :activate #'stop-button (list struct button-list))
    (loop while (and (process-info-running struct)
		     (not (connection-closed))) do
      (set-values button :x (random 500) :y (random 500))
      (sleep 5))
    (destroy-widget button)))

(defun stop-button (widget client-data &rest call-data)
  (declare (ignore widget call-data))
  (setf (process-info-running (first client-data)) nil)
  (xtk::with-process-lock ((button-list-lock (second client-data)))
			  (delete (first client-data)
				  (button-list-blist (second client-data)))))

(defun center-all (widget client-data &rest call-data)
  (declare (ignore widget call-data))
  (loop for button in (button-list-blist client-data) do
    (set-values (process-info-button button)
		:x (+ 250 (- 25 (random 50)))
		:y (+ 250 (- 25 (random 50))))
    (update-display (process-info-button button)))
  (sleep 2))

(defun stop-all (widget client-data &rest call-data)
  (declare (ignore widget call-data))
  (loop for button in (button-list-blist client-data) do
    (setf (process-info-running button) nil))
  (sleep 5))
