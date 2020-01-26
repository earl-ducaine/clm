;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:xtk; Base:10 -*-

(in-package :xtk)

(setq *sccsid* "@(#)events.lisp	1.10 9/11/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Event Manager functions
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf (get :no-event-mask :bit-mask) 0)
(setf (get :key-press-mask :bit-mask) (ash 1 0))
(setf (get :key-release-mask :bit-mask) (ash 1 1))
(setf (get :button-press-mask :bit-mask) (ash 1 2))
(setf (get :button-release-mask :bit-mask) (ash 1 3))
(setf (get :enter-window-mask :bit-mask) (ash 1 4))
(setf (get :leave-window-mask :bit-mask) (ash 1 5))
(setf (get :pointer-motion-mask :bit-mask) (ash 1 6))
(setf (get :pointer-motion-hint-mask :bit-mask) (ash 1 7))
(setf (get :button-1-motion-mask :bit-mask) (ash 1 8))
(setf (get :button-2-motion-mask :bit-mask) (ash 1 9))
(setf (get :button-3-motion-mask :bit-mask) (ash 1 10))
(setf (get :button-4-motion-mask :bit-mask) (ash 1 11))
(setf (get :button-5-motion-mask :bit-mask) (ash 1 12))
(setf (get :button-motion-mask :bit-mask) (ash 1 13))
(setf (get :keymap-state-mask :bit-mask) (ash 1 14))
(setf (get :exposure-mask :bit-mask) (ash 1 15))
(setf (get :visibility-change-mask :bit-mask) (ash 1 16))
(setf (get :structure-notify-mask :bit-mask) (ash 1 17))
(setf (get :resize-redirect-mask :bit-mask) (ash 1 18))
(setf (get :substructure-notify-mask :bit-mask) (ash 1 19))
(setf (get :substructure-redirect-mask :bit-mask) (ash 1 20))
(setf (get :focus-change-mask :bit-mask) (ash 1 21))
(setf (get :property-change-mask :bit-mask) (ash 1 22))
(setf (get :colormap-change-mask :bit-mask) (ash 1 23))
(setf (get :owner-grab-button-mask :bit-mask) (ash 1 24))

(defun get-bit-mask (mask)
  (or (get mask :bit-mask)
      (clm-cerror "Use :no-event-mask instead." "Invalid event mask: ~a~%" mask)
      (get-bit-mask :no-event-mask)))

(defun get-event-mask (pattern)
  (when (and pattern (symbolp pattern))
    (setq pattern (list pattern)))
  (apply #'logior (mapcar #'get-bit-mask pattern)))

(defun add-event-function (widget event function client-data 
				  &aux actual-handlers)
   (declare (special *motif-connection*))
   (setf actual-handlers
     (gethash (list widget event)
	      (toolkit-connection-event-table *motif-connection*)))
   (setf (gethash (list widget event)
		  (toolkit-connection-event-table *motif-connection*))
     (append actual-handlers (list (list function client-data))))
   actual-handlers)


;; EXPORT (add-event-handler)
(defun add-event-handler (widget event-mask function client-data
			  &key (non-maskable nil) 
			  &aux event-pattern-list)
  (declare (special *motif-connection*))
  (check-type widget integer)
  (check-type event-mask (or list atom))
  (unless (callablep function) 
    (clm-cerror "" "function expected: ~a~%" function))
  (setf event-pattern-list
    (if (atom event-mask)
	(list (get-bit-mask event-mask))
      (mapcar #'get-bit-mask event-mask)))
  (dolist (single-mask event-pattern-list)
    (unless (add-event-function widget single-mask function client-data)
      (execute-request 32 (list widget single-mask (if non-maskable 1 0))))))
  
(defun remove-event-function (widget single-event function client-data)
   (declare (special *motif-connection*))
   (setf (gethash (list widget single-event)
		  (toolkit-connection-event-table *motif-connection*))
     (set-difference
      (gethash (list widget single-event)
	       (toolkit-connection-event-table *motif-connection*))
      (list (list function client-data)) :test #'equal)))


;; EXPORT (remove-event-handler)
(defun remove-event-handler (widget event-mask function client-data
			     &key (non-maskable nil)
			     &aux event-pattern-list)
  (declare (special *motif-connection*))
  (check-type widget integer)
  (check-type event-mask (or list atom))
  (unless (callablep function) 
    (clm-cerror "" "function expected: ~a~%" function))
  (setf event-pattern-list
    (if (atom event-mask)
	(list (get-bit-mask event-mask))
      (mapcar #'get-bit-mask event-mask)))
  (dolist (single-mask event-pattern-list)
    (unless (remove-event-function widget single-mask function client-data)
      (execute-request 33 (list widget single-mask (if non-maskable 1 0))))))

;;; Return the Event-Handler entries for a given pair (widget event-name)

(defun lookup-event-functions (widget event-name)
  (declare (special *motif-connection*))
  (gethash (list widget event-name) 
           (toolkit-connection-event-table *motif-connection*)))
