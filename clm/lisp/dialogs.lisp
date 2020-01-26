;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:xtk; Base:10 -*-

(in-package :xtk)

(setq *sccsid* "@(#)dialogs.lisp	1.9 9/11/92")

;; EXPORT (popup)
(defun popup (widget &optional (grab-kind :grab-none))
  (check-type widget integer)
  (check-type grab-kind symbol)
  (execute-request 19 (list widget grab-kind)))

;; EXPORT (popdown)
(defun popdown (widget)
  (check-type widget integer)
  (execute-request 20 (list widget)))

;; EXPORT (manage-popup-child)
(defun manage-popup-child (widget)
  (check-type widget integer)
  (execute-request 35 (list widget)))

;; EXPORT (unmanage-popup-child)
(defun unmanage-popup-child (widget)
  (check-type widget integer)
  (execute-request 36 (list widget)))

;; EXPORT (alert-dialog)
(defun alert-dialog (dialog &aux terminated)
  (declare (special *motif-connection*))
  (update-display dialog)
  (setf terminated
    (toolkit-connection-dispatcher-terminated *motif-connection*))
  (setf (toolkit-connection-dispatcher-terminated *motif-connection*) nil)
  ;; Clients must set :auto-unmange nil at widget creation time
  ;;(set-values dialog :auto-unmanage nil)
  (add-callback dialog :unmap #'do-terminate nil)
  (manage-popup-child dialog)
  (recursive-main-loop) ;; request CLM server to enter main loop
  (update-display dialog)
  (remove-callback dialog :unmap #'do-terminate nil)
  (setf (toolkit-connection-dispatcher-terminated *motif-connection*) 
	terminated))

;; EXPORT (wait-for-input)
(defun wait-for-input (text &aux terminated)
  (declare (special *motif-connection*))
  (update-display text)
  (setf terminated
    (toolkit-connection-dispatcher-terminated *motif-connection*))
  (setf (toolkit-connection-dispatcher-terminated *motif-connection*) nil)
  (set-values text :edit-mode :single-line-edit)
  (add-callback text :activate #'do-terminate nil)
  (recursive-main-loop) ;; request CLM server to enter main loop
  (update-display text)
  (remove-callback text :activate #'do-terminate nil)
  (setf (toolkit-connection-dispatcher-terminated *motif-connection*) 
	terminated))
