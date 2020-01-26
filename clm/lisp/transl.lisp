;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:xtk; Base:10 -*-

(in-package :xtk)

(setq *sccsid* "@(#)transl.lisp	1.8 1/15/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; TranslationMgr functions
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (augment-translations)
(defun augment-translations (widget translation-table)
  (check-type widget integer)
  (check-type translation-table string)
  (execute-request 25 (list widget translation-table)))

;; EXPORT (override-translations)
(defun override-translations (widget translation-table)
  (check-type widget integer)
  (check-type translation-table string)
  (execute-request 26 (list widget translation-table)))

;; EXPORT (push-translations)
(defun push-translations (&rest widgets)
  (execute-request 67 widgets))

;; EXPORT (pop-translations)
(defun pop-translations (&rest widgets)
  (execute-request 68 widgets))

;; EXPORT (add-tab-group)
(defun add-tab-group (widget)
  (check-type widget integer)
  (execute-request 41 (list widget)))

;; EXPORT (remove-tab-group)
(defun remove-tab-group (widget)
  (check-type widget integer)
  (execute-request 42 (list widget)))

;; EXPORT (process-traversal)
(defun process-traversal (widget direction)
  (check-type widget integer)
  (check-type direction symbol)
  (execute-request 102 (list widget direction) :num-results 1))
