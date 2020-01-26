;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:xtk; Base:10 -*-

(in-package :xtk)

(setq *sccsid* "@(#)callbacks.lisp	1.10 9/11/92")

;;; Return the callback entries for a given pair (widget callback-name)

(defun lookup-callback-functions (widget callback-name)
  (declare (special *motif-connection*))
  (gethash (list widget callback-name) 
	   (toolkit-connection-callback-table *motif-connection*)))

;;; type checking for callback functions
;;; added because of Cltl2 instead of functionp (Berlage 26.03.91)

(defun callablep (obj)
  (or (typep obj '(or function symbol))
      (and (listp obj) (eql (car obj) 'lambda))))


;;; Add the specified callback entry. Return nil if this was the first
;;; callback entry for the pair (widget callback-name)
;;; Otherwise return non-nil.

(defun add-callback-function (widget callback-name function client-data
				     &aux actual-callbacks)
  (declare (special *motif-connection*))
  (setq actual-callbacks 
    (gethash (list widget callback-name) 
	     (toolkit-connection-callback-table *motif-connection*)))
  (setf (gethash (list widget callback-name) 
		 (toolkit-connection-callback-table *motif-connection*))
    (append actual-callbacks (list (list function client-data))))
   actual-callbacks)

;;; Delete the specified callback entry. Return nil if there are no remaining
;;; callback entries for the pair (widget callback-name)
;;; Otherwise return non-nil

(defun remove-callback-function (widget callback-name function client-data)
  (declare (special *motif-connection*))
  (setf (gethash (list widget callback-name) 
		 (toolkit-connection-callback-table *motif-connection*))
    (set-difference (gethash 
		     (list widget callback-name) 
		     (toolkit-connection-callback-table *motif-connection*))
		    (list (list function client-data)) :test #'equal)))

;;; Delete all callback entries for a given pair (widget callback-name)

(defun remove-all-callback-functions (widget callback-name
					     &aux actual-callbacks)
  (declare (special *motif-connection*))
  (setf actual-callbacks
    (gethash (list widget callback-name)
	     (toolkit-connection-callback-table *motif-connection*)))
  (setf (gethash (list widget callback-name) 
		 (toolkit-connection-callback-table *motif-connection*)) nil)
  actual-callbacks)

;;; Add a callback to a widgets callback list

;; EXPORT (add-callback)
(defun add-callback (widget callback-name function client-data)
  (check-type widget integer)
  (check-type callback-name symbol)
  (unless (callablep function) 
    (clm-cerror "" "function expected: ~a~%" function))
  ;;; client-data is checked in convert-arguments
  (unless (add-callback-function widget callback-name function client-data)
    (execute-request 9 (list widget callback-name))))

;; EXPORT (remove-callback)
(defun remove-callback (widget callback-name function client-data)
  (check-type widget integer)
  (check-type callback-name symbol)
  (unless (callablep function) 
    (clm-cerror "" "function expected: ~a~%" function))
  ;;; client-data is checked in convert-arguments
  (unless (remove-callback-function widget callback-name function client-data)
    (execute-request 10 (list widget callback-name))))

;; EXPORT (remove-all-callbacks)
(defun remove-all-callbacks (widget callback-name)
  (check-type widget integer)
  (check-type callback-name symbol)
  (when (remove-all-callback-functions widget callback-name)
    (execute-request 10 (list widget callback-name))))

;; EXPORT (has-callbacks)
(defun has-callbacks (widget callback-name)
  (check-type widget integer)
  (check-type callback-name symbol)
  (first (execute-request 12 (list widget callback-name) :num-results 1)))

;; EXPORT (add-protocol-callback)
(defun add-protocol-callback (widget property protocol function client-data)
  (check-type widget integer)
  (check-type property symbol)
  (check-type protocol symbol)
  (unless (callablep function) 
	  (clm-cerror "" "function expected: ~a~%" function))
  (unless (add-callback-function widget `(,property ,protocol) 
				 function client-data)
    (execute-request 63 (list widget property protocol))))

;; EXPORT (add-wm-protocol-callback)
(defun add-wm-protocol-callback (widget protocol function client-data) 
  (add-protocol-callback widget :wm-protocols protocol function client-data))

;; EXPORT (remove-protocol-callback)
(defun remove-protocol-callback (widget property protocol function client-data) 
  (check-type widget integer)
  (check-type property symbol)
  (check-type protocol symbol)
  (unless (callablep function) 
	  (clm-cerror "" "function expected: ~a~%" function))
  (unless (remove-callback-function widget `(,property ,protocol)
				    function client-data)
    (execute-request 64 (list widget property protocol))))

;; EXPORT (remove-wm-protocol-callback)
(defun remove-wm-protocol-callback (widget protocol function client-data) 
  (remove-protocol-callback widget :wm-protocols protocol
			    function client-data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;;  Center a popup shell (widget) above another widget (client-data)
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (center-at)
(defun center-at (widget client-data &rest call-data 
			 &aux absxy button-sizes shell-sizes)
  (declare (ignore call-data))
  (setf absxy (xt-translate-coordinates client-data 0 0)
        button-sizes (get-values client-data :width :height)
        shell-sizes (get-values widget :width :height))
  (move-widget widget 
	       (- (+ (/ (first button-sizes) 2) (first absxy)) 
		  (/ (first shell-sizes) 2))
	       (- (+ (/ (second button-sizes) 2) (second absxy))
		  (/ (second shell-sizes) 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Destroy an application and terminate the callback dispatcher
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (quit-application)
(defun quit-application (widget client-data &rest call-data)
  (declare (ignore widget call-data) (special *motif-connection*))
  (destroy-application client-data)
  (terminate-dispatcher))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; popup a shell with an exclusive pointer grab
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (popup-exclusive)
(defun popup-exclusive (widget client-data &rest call-data)
  (declare (ignore widget call-data))
  (popup client-data :grab-exclusive))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; popup a shell with a nonexclusive pointer grab
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (popup-nonexclusive)
(defun popup-nonexclusive (widget client-data &rest call-data)
  (declare (ignore widget call-data))
  (popup client-data :grab-nonexclusive))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; popup a shell with no pointer grab
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (popup-none)
(defun popup-none (widget client-data &rest call-data)
  (declare (ignore widget call-data))
  (popup client-data :grab-none))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; popup a shell by managing it's child
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (manage-popup)
(defun manage-popup (widget client-data &rest call-data)
  (declare (ignore widget call-data))
  (manage-popup-child client-data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; popdown a shell by unmanaging it's child
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (unmanage-popup)
(defun unmanage-popup (widget client-data &rest call-data)
  (declare (ignore widget call-data))
  (unmanage-popup-child client-data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; popdown a shell
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (popdown-shell)
(defun popdown-shell (widget client-data &rest call-data)
  (declare (ignore widget call-data))
  (popdown client-data))
