;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:xtk; Base:10 -*-

(in-package :xtk)

(setq *sccsid* "@(#)graph.lisp	1.5 1/15/92")

;; EXPORT (add-graph-relations)
(defun add-graph-relations (relation ancestor decedent &rest pairs)
  (check-type ancestor integer)
  (check-type decedent integer)
  (check-type relation symbol)
  (execute-request 78 (append (list relation ancestor decedent) pairs)))

;; EXPORT (remove-graph-relations)
(defun remove-graph-relations (relation ancestor decedent &rest pairs)
  (check-type ancestor integer)
  (check-type decedent integer)
  (check-type relation symbol)
  (execute-request 79 (append (list relation ancestor decedent) pairs)))

;; EXPORT (do-layout)
(defun do-layout (graph)
  (check-type graph integer)
  (execute-request 80 (list graph)))

