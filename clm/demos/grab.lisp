;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:(user (lisp)); Base:10 -*-

(in-package 'user)
(use-package 'xtk)

(defvar *sccsid* "@(#)grab.lisp	1.3 11/27/91")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Prototype of a simple CLM application
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Color display
'(run-motif-application 'grab :execute-main-loop nil :display-host "gina")
;; Greyscale
'(run-motif-application 'grab :execute-main-loop nil :display-host "n32")
;; BW display
'(run-motif-application 'grab :execute-main-loop nil)

(defun grab (&aux app-shell app-rc hw listw sw bview)
  (setf app-shell (create-application-shell)
	app-rc (create-widget :row-column app-shell)
	hw   (create-widget :label app-rc
			    :label-string "Press button here to view Cursor")
	sw (create-widget :scrolled-window app-rc)
	listw (create-widget :list sw :selection-policy :single
			     :visible-item-count 10))
  (let ((*current-cursor* (xtk::create-pixmap-cursor
			  "../demos/test-foreground"
			  "../demos/test-background"
			  :foreground "red" :background "blue")))
    (declare (special *current-cursor*))
    (apply 'set-items (cons listw (mapcar #'symbol-name (xtk::all-cursors))))
    (add-callback listw :single-selection #'set-other-cursor listw)
    (add-event-handler hw :button-press-mask #'other-cursor nil)
    (add-event-handler hw :button-motion-mask #'motion-notify nil)
    (realize-widget app-shell)
    (define-cursor *current-cursor* listw)
    (app-main-loop)))

(defun other-cursor (widget client-data &rest event)
  (declare (special *current-cursor*))
  (change-grab-cursor *current-cursor*
		      :add-mask '(:button-1-motion-mask)
		      :remove-mask :button-motion-mask))

(defun motion-notify (widget client-data &rest event)
  (format t "."))

(defun set-other-cursor (widget client-data &rest event)
  (declare (special *current-cursor*))
  (setf *current-cursor*
    (xtk::create-font-cursor
     (intern (first (get-selected-items client-data)) :keyword)))
  (define-cursor *current-cursor* client-data))



