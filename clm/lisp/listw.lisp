;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:xtk; Base:10 -*-

(in-package :xtk)

(setq *sccsid* "@(#)listw.lisp	1.8 1/15/92")

;;; Add items to a widget (List, SelectionBox or Command)

;; EXPORT (set-items)
(defun set-items (widget &rest items)
  (check-type widget integer)
  (execute-request 45 (cons widget items)))

;; EXPORT (get-items)
(defun get-items (widget)
  (check-type widget integer)
  (execute-request 61 (list widget 1) :num-results :arbitrary))

;; EXPORT (get-selected-items)
(defun get-selected-items (widget)
  (execute-request 61 (list widget 0) :num-results :arbitrary))

;;; Convenience functions for list widgets

;; EXPORT (list-add-item)
(defun list-add-item (widget item position)
  (check-type widget integer)
  (check-type position integer)
  (check-type item string)
  (execute-request 46 (list widget item position)))

;; EXPORT (list-add-item-unselected)
(defun list-add-item-unselected (widget item position)
  (check-type widget integer)
  (check-type position integer)
  (check-type item string)
  (execute-request 47 (list widget item position)))

;; EXPORT (list-delete-item)
(defun list-delete-item (widget item)
  (check-type widget integer)
  (check-type item string)
  (execute-request 48 (list widget item)))

;; EXPORT (list-delete-pos)
(defun list-delete-pos (widget position)
  (check-type widget integer)
  (check-type position integer)
  (execute-request 49 (list widget position)))

;; EXPORT (list-deselect-all-items)
(defun list-deselect-all-items (widget)
  (check-type widget integer)
  (execute-request 51 (list widget)))

;; EXPORT (list-deselect-item)
(defun list-deselect-item (widget item)
  (check-type widget integer)
  (check-type item string)
  (execute-request 50 (list widget item)))

;; EXPORT (list-deselect-pos)
(defun list-deselect-pos (widget position)
  (check-type widget integer)
  (check-type position integer)
  (execute-request 59 (list widget position)))

;; EXPORT (list-select-item)
(defun list-select-item (widget item &optional (notify nil))
  (check-type widget integer)
  (check-type item string)
  (execute-request 52 (list widget item (if notify 1 0))))

;; EXPORT (list-select-pos)
(defun list-select-pos (widget position &optional (notify nil))
  (check-type widget integer)
  (check-type position integer)
  (execute-request 58 (list widget position (if notify 1 0))))

;; EXPORT (list-set-bottom-item)
(defun list-set-bottom-item (widget item)
  (check-type widget integer)
  (check-type item string)
  (execute-request 56 (list widget item)))

;; EXPORT (list-set-bottom-pos)
(defun list-set-bottom-pos (widget position)
  (check-type widget integer)
  (check-type position integer)
  (execute-request 57 (list widget position)))

;; EXPORT (list-set-horiz-pos)
(defun list-set-horiz-pos (widget position)
  (check-type widget integer)
  (check-type position integer)
  (execute-request 53 (list widget position)))

;; EXPORT (list-set-top-item)
(defun list-set-top-item (widget item)
  (check-type widget integer)
  (check-type item string)
  (execute-request 54 (list widget item)))

;; EXPORT (list-set-top-pos)
(defun list-set-top-pos (widget position)
  (check-type widget integer)
  (check-type position integer)
  (execute-request 55 (list widget position)))
