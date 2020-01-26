;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:xtk; Base:10 -*-

(in-package :xtk)

(setq *sccsid* "@(#)text.lisp	1.9 1/15/92")

;; EXPORT (text-insert)
(defun text-insert (widget position text 
			       &key (move-cursor t) (scroll-window nil))
  (check-type widget integer)
  (check-type position integer)
  (check-type text string)
  (execute-request 62 (list widget position (if move-cursor 1 0) 
			    (if scroll-window 1 0) text)))

;; for compatibility with documented behaviour of 2.0
;; EXPORT (insert-text)
(defun insert-text (widget position text 
			       &key (move-cursor t) (scroll-window nil))
  (text-insert widget position text :move-cursor move-cursor
	       :scroll-window scroll-window))

;; EXPORT (text-append)
(defun text-append (widget text &key (move-cursor t) (scroll-window nil))
  (check-type widget integer)
  (check-type text string)
  (execute-request 62 (list widget -1 (if move-cursor 1 0) 
			    (if scroll-window 1 0) text)))

;; for compatibility with documented behaviour of 2.0
;; EXPORT (append-text)
(defun append-text (widget text &key (move-cursor t) (scroll-window nil))
  (text-append widget text :move-cursor move-cursor 
	       :scroll-window scroll-window))

;; EXPORT (text-get-selection)
(defun text-get-selection (widget)
  (check-type widget integer)
  (first (execute-request 118 (list widget) :num-results 1)))

;;; new functions for 2.1

;; EXPORT (text-clear-selection)
(defun text-clear-selection (widget)
  (check-type widget integer)
  (execute-request 110 (list widget)))

;; EXPORT (text-cut)
(defun text-cut (widget)
  (check-type widget integer)
  (first (execute-request 112 (list widget) :num-results 1)))

;; EXPORT (text-copy)
(defun text-copy (widget)
  (check-type widget integer)
  (first (execute-request 111 (list widget) :num-results 1)))

;; EXPORT (text-get-baseline)
(defun text-get-baseline (widget)
  (check-type widget integer)
  (first (execute-request 113 (list widget) :num-results 1)))

;; EXPORT (text-get-selection-position)
;; returns (left right is-ok)
(defun text-get-selection-position (widget)
  (check-type widget integer)
  (values-list (execute-request 119 (list widget) :num-results 3)))

;; EXPORT (text-paste)
(defun text-paste (widget)
  (check-type widget integer)
  (first (execute-request 122 (list widget) :num-results 1)))

;; EXPORT (text-pos-to-xy)
;; returns (x y is-visible)
(defun text-pos-to-xy (widget position-in-text)
  (check-type widget integer)
  (check-type position-in-text integer)
  (values-list (execute-request 123 (list widget position-in-text)
				:num-results 3)))

;; EXPORT (text-remove)
(defun text-remove (widget)
  (check-type widget integer)
  (first (execute-request 124 (list widget) :num-results 1)))

;; EXPORT (text-scroll)
(defun text-scroll (widget lines)
  (check-type widget integer)
  (check-type lines integer)
  (execute-request 137 (list widget lines)))

;; EXPORT (text-set-add-mode)
(defun text-set-add-mode (widget mode)
  (check-type widget integer)
  (execute-request 126 (list widget (if mode 1 0))))

;; EXPORT (text-show-position)
(defun text-show-position (widget position-in-text)
  (check-type widget integer)
  (check-type position-in-text integer)
  (execute-request 133 (list widget position-in-text)))

;; EXPORT (text-xy-to-pos)
(defun text-xy-to-pos (widget x y)
  (check-type widget integer)
  (check-type x integer)
  (check-type y integer)
  (first (execute-request 134 (list widget x y) :num-results 1)))

;;;   The rest of this file originally contributed by Peter Cousseau

;; ;; EXPORT (call-action)
;(defun call-action (text-widget fun-name arg-string num-arg)
;  (check-type text-widget integer)
;  (check-type fun-name string)
;  (check-type arg-string string)
;  (check-type num-arg integer)
;  (execute-request 156 (list text-widget fun-name arg-string num-arg))
;  )

;; EXPORT (text-get-last-position)
(defun text-get-last-position (text-widget)
  (check-type text-widget integer)
  (first (execute-request 116 (list text-widget) :num-results 1)))

;; EXPORT (text-set-selection)
(defun text-set-selection (text-widget start-pos end-pos)
  (check-type text-widget integer)
  (check-type start-pos integer)
  (check-type end-pos integer)
  (execute-request 131 (list text-widget start-pos end-pos)))

;; EXPORT (text-replace)
(defun text-replace (widget start-pos end-pos new-value)
  (check-type widget integer)
  (check-type start-pos integer)
  (check-type end-pos integer)
  (check-type new-value string)
  (execute-request 125 (list widget start-pos end-pos new-value)))

;; EXPORT (text-get-insertion-position)
(defun text-get-insertion-position (text-widget)
  (check-type text-widget integer)
  (first (execute-request 115 (list text-widget) :num-results 1)))  

;; EXPORT (text-set-highlight)
(defun text-set-highlight (text-widget start-pos end-pos mode)
  (check-type text-widget integer)
  (check-type start-pos integer)
  (check-type end-pos integer)
  (cond ((equal mode :normal) (setq mode 0))
	((equal mode :select) (setq mode 1))
	((equal mode :secondary-select) (setq mode 2))
	(t (error 
          "~%highlight-mode must be :normal, :select, or :secondary-select~%")))
  (execute-request 128 (list text-widget start-pos end-pos mode)))	 

;; EXPORT (text-get-substring)
(defun text-get-substring (widget length &optional (start-position -1))
  (check-type widget integer)
  (check-type start-position integer)
  (check-type length integer)
  (first (execute-request 151 (list widget start-position length)
			  :num-results 1)))  

;  0 is the position of the top of the text widget.
;  set :end-pos to :the-end to search backwards from the end of the file.
;  ; C note   A :end-pos of -1 means search from the current cursor
;  position.  A :end-pos of -2 means search from the end of the text widget.

;; EXPORT (text-search)
(defun text-search (widget the-string
			   &key  (start-pos -1) (end-pos -1)
			   (case-sensitive nil) (backward nil) )
  (check-type widget integer)
  (check-type the-string string)
  (check-type start-pos integer)
  (and (equal end-pos :the-end)
       (cond (backward (setq end-pos -2))
	     (t (error 
                  "Only set :end-pos to :the-end when searching backwards.")))
       )
  (check-type end-pos integer)
  (first (execute-request 152
			  (list widget the-string
				start-pos end-pos
				(if case-sensitive 1 0)
				(if backward 1 0))
			  :num-results 1)))

