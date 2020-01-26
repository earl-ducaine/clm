;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:(xprocesses (lisp)); Base:10 -*-

(in-package 'xprocesses)
(use-package 'xtk)

(defvar *sccsid* "@(#)xprocesses.lisp	1.3 11/27/91")

'(xprocesses)
'(xprocesses 700)

(defun xprocesses (&optional (update-interval 5000))
  "Run the xprocesses application"
  (run-motif-application 'init-processes
                         :init-arguments (list update-interval)
			 :application-name "xprocesses"
			 :application-class "Xprocesses"
                         :process-name "Process Monitor"))

(defun init-processes (update-interval &aux shell row list-w scale timer)
  "Create the widget hierarchy for the xprocesses application"
  ;; Create the application's main window
  (setf shell (create-application-shell :allow-shell-resize t))
  ;; Create a row-colum widget which contains the list widget and a scale
  (setf row (create-row-column shell "Row"))
  ;; Create a list widget for displaying the processes
  (setf list-w (create-list row "ProcessList" :visible-item-count 5))
  (setf scale (create-scale row "Scale" :orientation :horizontal
			    :show-value t :value update-interval
			    :minimum 500 :maximum 5000))
  (setf timer (create-timer update-interval 'display-processes list-w))
  (add-callback scale :value-changed 'set-new-interval timer)
  ;; Initialize contents of the list widget
  (display-processes timer list-w)
  ;; Realize widget hierarchy
  (realize-widget shell))

(defun set-new-interval (scale timer call-data)
  ;; Scale's call-data is the new slider position
  (declare (ignore scale))
  (change-timer timer call-data))

(defun display-processes (timer list-w &aux ap)
  "Display all processes in the list widget"
  (declare (ignore timer))
  (setq ap (all-processes))
  (set-values list-w :visible-item-count (length ap))
  (apply 'set-items (cons list-w ap)))

(defun all-processes ()
  "Return a list of strings describing the processes"
  #+excl
  (loop for process in mp::*all-processes*
    collect (format nil "~a (~a) Quantum: ~a, Priority: ~a"
		    (mp::process-name process)
		    (mp::process-whostate process)
		    (mp::process-quantum process)
		    (mp::process-priority process)))
  #+lucid
  (loop for process in *all-processes* do
    collect (format nil "~s (~s) State: ~a"
		    (process-name process)
		    (process-whostate process)
		    (process-state process)))
  #-(or lucid excl)
  '("Not implemented")
  )


