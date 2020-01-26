;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:xtk; Base:10 -*-

(in-package :xtk)

(setq *sccsid* "@(#)display.lisp	1.13 9/21/93")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Motif function: XmIsMotifWMRunning()
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (is-mwm-running)
(defun is-mwm-running (shell)
  (first (execute-request 74 (list shell) :num-results 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Motif function: XmVersion
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (motif-version)
(defun motif-version ()
  (or (toolkit-connection-motif-version *motif-connection*) 1001))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Motif function: XmUpdateDisplay()
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (update-display)
(defun update-display (widget)
  (execute-request 76 (list widget)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Query the position of the pointer
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (query-pointer)
(defun query-pointer (widget)
  (check-type widget integer)
  (execute-request 66 (list widget) :num-results 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Query the size of the default screen
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (get-screen-size)
(defun get-screen-size (display &optional (screen 0))
  (check-type display string)
  (check-type screen integer)
  (execute-request 34 (list display screen) :num-results 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Enable/disable updating of the display after each call to a server 
;;;;; function
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (forced-output-mode)
(defun forced-output-mode (mode)
  (execute-request 73 (if mode '(1) '(0))))

;; EXPORT (with-immediate-update-enabled)
(defmacro with-immediate-update-enabled (&body body)
  `(progn
     (forced-output-mode t)
     ,(cons 'progn body)
     (forced-output-mode nil)))

;; EXPORT (with-immediate-update-disabled)
(defmacro with-immediate-update-disabled (&body body)
  `(progn
     (forced-output-mode nil)
     ,(cons 'progn body)
     (forced-output-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Xt function: XtLastTimestampProcessed()
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (last-timestamp-processed)
(defun last-timestamp-processed ()
  (first (execute-request 145 '() :num-results 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Mouse grabbing with clock cursor
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun enable-without-mouse (w grab-p)
  (execute-request 153 (list w grab-p)))

(defun disable-without-mouse (w ungrab-p)
  (execute-request 154 (list w ungrab-p)))

;;(export 'with-pointer-grab)

(defmacro with-pointer-grab ((widget &optional (mode :sync)) &body body)
  `(unwind-protect
    (progn
      (enable-without-mouse ,widget ,(if (null mode) 0
				       (if (eq mode :sync) 1 2)))
      (progn ,@body))
    (disable-without-mouse ,widget ,(if (null mode) 0
				       (if (eq mode :sync) 1 2)))))
