;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:xtk; Base:10 -*-

(in-package :xtk)

(setq *sccsid* "@(#)convenience.lisp	1.5 1/15/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Create a widget by using the Motif convenience functions
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convenience-function (index n-res parent name resource-list)
  (check-type parent integer)
  (check-type name (or string symbol))
  (check-type index integer)
  (execute-request 75 (cons index (cons parent (cons name resource-list)))
		   :num-results n-res))

;; create-application-shell is defined in widget.lisp

;; EXPORT (create-arrow-button)
(defun create-arrow-button (parent name &rest resources)
  (car (convenience-function 1 1 parent name resources)))

;; EXPORT (create-arrow-button-gadget)
(defun create-arrow-button-gadget (parent name &rest resources)
  (car (convenience-function 2 1 parent name resources)))

;; EXPORT (create-bulletin-board) 
(defun create-bulletin-board (parent name &rest resources)
  (car (convenience-function 3 1 parent name resources)))

;; EXPORT (create-bulletin-board-dialog)
(defun create-bulletin-board-dialog (parent name &rest resources)
  (values-list (convenience-function 4 2 parent name resources)))

;; EXPORT (create-cascade-button)
(defun create-cascade-button (parent name &rest resources)
  (car (convenience-function 5 1 parent name resources)))

;; EXPORT (create-cascade-button-gadget)
(defun create-cascade-button-gadget (parent name &rest resources)
  (car (convenience-function 6 1 parent name resources)))

;; EXPORT (create-command) 
(defun create-command (parent name &rest resources)
  (car (convenience-function 7 1 parent name resources)))

;; EXPORT (create-dialog-shell)
(defun create-dialog-shell (parent name &rest resources)
  (car (convenience-function 8 1 parent name resources)))

;; EXPORT (create-drawing-area)
(defun create-drawing-area (parent name &rest resources)
  (car (convenience-function 9 1 parent name resources)))

;; EXPORT (create-drawn-button)
(defun create-drawn-button (parent name &rest resources)
  (car (convenience-function 10 1 parent name resources)))

;; EXPORT (create-error-dialog)
(defun create-error-dialog (parent name &rest resources)
  (values-list (convenience-function 11 2 parent name resources)))

;; EXPORT (create-file-selection-box) 
(defun create-file-selection-box (parent name &rest resources)
  (car (convenience-function 12 1 parent name resources)))

;; EXPORT (create-file-selection-dialog)
(defun create-file-selection-dialog (parent name &rest resources)
  (values-list (convenience-function 13 2 parent name resources)))

;; EXPORT (create-form) 
(defun create-form (parent name &rest resources)
  (car (convenience-function 14 1 parent name resources)))

;; EXPORT (create-form-dialog) 
(defun create-form-dialog (parent name &rest resources)
  (values-list (convenience-function 15 2 parent name resources)))

;; EXPORT (create-frame) 
(defun create-frame (parent name &rest resources)
  (car (convenience-function 16 1 parent name resources)))

;; EXPORT (create-information-dialog)
(defun create-information-dialog (parent name &rest resources)
  (values-list (convenience-function 17 2 parent name resources)))

;; EXPORT (create-label) 
(defun create-label (parent name &rest resources)
  (car (convenience-function 18 1 parent name resources)))

;; EXPORT (create-label-gadget) 
(defun create-label-gadget (parent name &rest resources)
  (car (convenience-function 19 1 parent name resources)))

;; EXPORT (create-list) 
(defun create-list (parent name &rest resources)
  (car (convenience-function 20 1 parent name resources)))

;; EXPORT (create-main-window)
(defun create-main-window (parent name &rest resources)
  (car (convenience-function 21 1 parent name resources)))

;; EXPORT (create-menu-bar)
(defun create-menu-bar (parent name &rest resources)
  (car (convenience-function 22 1 parent name resources)))

;; EXPORT (create-menu-shell)
(defun create-menu-shell (parent name &rest resources)
  (car (convenience-function 23 1 parent name resources)))

;; EXPORT (create-message-box) 
(defun create-message-box (parent name &rest resources)
  (car (convenience-function 24 1 parent name resources)))

;; EXPORT (create-message-dialog)
(defun create-message-dialog (parent name &rest resources)
  (values-list (convenience-function 25 2 parent name resources)))

;; EXPORT (create-override-shell)
(defun create-override-shell (parent name &rest resources)
  (car (convenience-function 26 1 parent name resources)))

;; EXPORT (create-option-menu)
(defun create-option-menu (parent name &rest resources)
  (car (convenience-function 27 1 parent name resources)))

;; EXPORT (create-paned-window) 
(defun create-paned-window (parent name &rest resources)
  (car (convenience-function 28 1 parent name resources)))

;; EXPORT (create-popup-menu)
(defun create-popup-menu (parent name &rest resources)
  (values-list (convenience-function 29 2 parent name resources)))

;; EXPORT (create-prompt-dialog)
(defun create-prompt-dialog (parent name &rest resources)
  (values-list (convenience-function 30 2 parent name resources)))

;; EXPORT (create-pulldown-menu)
(defun create-pulldown-menu (parent name &rest resources)
  (values-list (convenience-function 31 2 parent name resources)))

;; EXPORT (create-push-button)
(defun create-push-button (parent name &rest resources)
  (car (convenience-function 32 1 parent name resources)))

;; EXPORT (create-push-button-gadget)
(defun create-push-button-gadget (parent name &rest resources)
  (car (convenience-function 33 1 parent name resources)))

;; EXPORT (create-question-dialog)
(defun create-question-dialog (parent name &rest resources)
  (values-list (convenience-function 34 2 parent name resources)))

;; EXPORT (create-radio-box)
(defun create-radio-box (parent name &rest resources)
  (car (convenience-function 35 1 parent name resources)))

;; EXPORT (create-row-column) 
(defun create-row-column (parent name &rest resources)
  (car (convenience-function 36 1 parent name resources)))

;; EXPORT (create-scale) 
(defun create-scale (parent name &rest resources)
  (car (convenience-function 37 1 parent name resources)))

;; EXPORT (create-scroll-bar) 
(defun create-scroll-bar (parent name &rest resources)
  (car (convenience-function 38 1 parent name resources)))

;; EXPORT (create-scrolled-list)
(defun create-scrolled-list (parent name &rest resources)
  (car (convenience-function 39 1 parent name resources)))

;; EXPORT (create-scrolled-text)
(defun create-scrolled-text (parent name &rest resources)
  (car (convenience-function 40 1 parent name resources)))

;; EXPORT (create-scrolled-window) 
(defun create-scrolled-window (parent name &rest resources)
  (car (convenience-function 41 1 parent name resources)))

;; EXPORT (create-selection-box) 
(defun create-selection-box (parent name &rest resources)
  (car (convenience-function 42 1 parent name resources)))

;; EXPORT (create-selection-dialog)
(defun create-selection-dialog (parent name &rest resources)
  (values-list (convenience-function 43 2 parent name resources)))

;; EXPORT (create-separator) 
(defun create-separator (parent name &rest resources)
  (car (convenience-function 44 1 parent name resources)))

;; EXPORT (create-separator-gadget) 
(defun create-separator-gadget (parent name &rest resources)
  (car (convenience-function 45 1 parent name resources)))

;; EXPORT (create-text) 
(defun create-text (parent name &rest resources)
  (car (convenience-function 46 1 parent name resources)))

;; EXPORT (create-text-field) 
(defun create-text-field (parent name &rest resources)
  (car (convenience-function 47 1 parent name resources)))

;; EXPORT (create-toggle-button)
(defun create-toggle-button (parent name &rest resources)
  (car (convenience-function 48 1 parent name resources)))

;; EXPORT (create-toggle-button-gadget)
(defun create-toggle-button-gadget (parent name &rest resources)
  (car (convenience-function 49 1 parent name resources)))

;; EXPORT (create-toplevel-shell)
(defun create-toplevel-shell (parent name &rest resources)
  (car (convenience-function 50 1 parent name resources)))

;; EXPORT (create-transient-shell)
(defun create-transient-shell (parent name &rest resources)
  (car (convenience-function 51 1 parent name resources)))

;; EXPORT (create-warning-dialog)
(defun create-warning-dialog (parent name &rest resources)
  (values-list (convenience-function 52 2 parent name resources)))

;; EXPORT (create-work-area)
(defun create-work-area (parent name &rest resources)
  (values-list (convenience-function 53 2 parent name resources)))

;; EXPORT (create-working-dialog)
(defun create-working-dialog (parent name &rest resources)
  (values-list (convenience-function 54 2 parent name resources)))

;; EXPORT (create-graph)
(defun create-graph (parent name &rest resources)
  (car (convenience-function 55 1 parent name resources)))
