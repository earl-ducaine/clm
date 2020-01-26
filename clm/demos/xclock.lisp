;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:(xclock (lisp)); Base:10 -*-

(in-package 'xclock)
(use-package 'xtk)

(defvar *sccsid* "@(#)xclock.lisp	1.3 11/27/91")

'(xclock::xclock)

(defun xclock (&optional (update-interval 1000))
  "Run the xclock application"
  (run-motif-application 'init-xclock
			 :init-arguments (list update-interval)
                         :execute-main-loop nil
			 :application-name "clock"
			 :application-class "Clock"
                         :process-name "Motif Clock Application"))

(defun init-xclock (update-interval &aux shell)
  "Create the widget hierarchy for the clock application"
  ;; Create the application's main window
  (setf shell (create-application-shell :allow-shell-resize t))
  (let ((*clock-widget* (create-label shell "ClockLabel"))
	(timer (create-timer update-interval 'tick nil :active-in-handler t)))
    ;; Make *clock-widget* globally available so the
    ;; timer callback can access the label widget
    (declare (special *clock-widget*))
    ;; Initialize clock
    (tick timer nil)
    ;; Realize widget hierarchy
    (realize-widget shell)
    ;; Enter application main loop
    (app-main-loop)))

(defun tick (timer client-data &aux now)
  "one time interval is over => refresh the clock"
  (declare (special *clock-widget*)
	   (ignore timer client-data))
  (setq now (multiple-value-list (decode-universal-time (get-universal-time))))
  ;; Display new time in the label widget
  (xtk::set-values *clock-widget* :label-string
                   (format nil "~2,'0d:~2,'0d:~2,'0d"
                           (third now) (second now) (first now))))




