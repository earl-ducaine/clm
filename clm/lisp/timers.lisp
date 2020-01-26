;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:xtk; Base:10 -*-

(in-package :xtk)

(setq *sccsid* "@(#)timers.lisp	1.7 9/11/92")

;; EXPORT (create-timer)
(defun create-timer (interval function client-data
		    &key (once nil)
			 (enabled t)
			 (active-in-handler nil)
			 (active-in-recursive-main-loops nil)
		    &aux timer-id)
  (declare (special *motif-connection*))
  (check-type interval integer)
  (unless (callablep function)
    (clm-cerror "" "function expected: ~a~%" function))
  ;;; Compute a hash value for client-data
  ;;; Add timer to table
  (setq timer-id
    (first (execute-request 81 (list interval
				     (if once 1 0)
				     (if active-in-handler 1 0)
				     (if active-in-recursive-main-loops 1 0)
				     (if enabled 1 0))
			    :num-results 1)))
  (setf (gethash timer-id
		 (toolkit-connection-timer-table *motif-connection*))
    (list function client-data))
  timer-id)

;; EXPORT (destroy-timer)
(defun destroy-timer (timer-id)
  (declare (special *motif-connection*))
  (check-type timer-id integer)
  ;; check for a valid timer-id here
  (unless (gethash timer-id
		   (toolkit-connection-timer-table *motif-connection*))
    (clm-error "Illegal timer ID <~d>~%" timer-id))
  (execute-request 82 (list timer-id))
  (remhash timer-id (toolkit-connection-timer-table *motif-connection*)))

;; EXPORT (change-timer)
(defun change-timer (timer-id new-interval)
  (declare (special *motif-connection*))
  (check-type timer-id integer)
  (check-type new-interval integer)
  (unless (gethash timer-id
		   (toolkit-connection-timer-table *motif-connection*))
    (clm-error "Illegal timer ID <~d>~%" timer-id))
  (execute-request 83 (list timer-id new-interval)))

;; EXPORT (start-timer)
(defun start-timer (timer-id &key (interval -1))
  (declare (special *motif-connection*))
  (check-type timer-id integer)
  (check-type interval integer)
  (unless (gethash timer-id
		   (toolkit-connection-timer-table *motif-connection*))
    (clm-error "Illegal timer ID <~d>~%" timer-id))
  (execute-request 84 (list timer-id interval)))

;; EXPORT (stop-timer)
(defun stop-timer (timer-id)
  (declare (special *motif-connection*))
  (check-type timer-id integer)
  (unless (gethash timer-id
		   (toolkit-connection-timer-table *motif-connection*))
    (clm-error "Illegal timer ID <~d>~%" timer-id))
  (execute-request 85 (list timer-id)))




