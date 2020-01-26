;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:xtk; Base:10 -*-

(in-package :xtk)

(setq *sccsid* "@(#)widgets.lisp	1.14 9/8/93")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Motif function: XtAppCreateShell()
;;;;; Forces :delete-response to nil and adds a protocol callback for the
;;;;; "Quit" function from Mwm's system menu if Mwm is running.
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (create-application-shell)
(defun create-application-shell (&rest resources 
				 &key (name "app-shell") &allow-other-keys
				 &aux wid)
  (remf resources :name)
  (setq wid (car (convenience-function 0 1 0 name
				       (append resources (list :delete-response
							       :do-nothing)))))
  (when (is-mwm-running wid)
    (add-wm-protocol-callback wid :wm-delete-window #'quit-application wid))
  wid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Motif function: none
;;;;; Destroys the application's main shell widget
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (destroy-application)
(defun destroy-application (shell)
  (check-type shell integer)
  (execute-request 14 (list shell)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Create a widget
;;;;; Keyword parameters:  managed --> Boolean
;;;;;                      name    --> string or symbol
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (create-widget)
(defun create-widget (class parent &rest resources)
  (create-named-widget class parent (string-downcase (symbol-name class))
		       t resources))

;; EXPORT (create-unmanaged-widget)
(defun create-unmanaged-widget (class parent &rest resources)
  (create-named-widget class parent (string-downcase (symbol-name class))
		       nil resources))

;; EXPORT (create-named-widget)
(defun create-named-widget (class parent name managed resources &aux wid)
  (check-type class symbol)
  (check-type parent integer)
  (check-type managed symbol)
  (check-type name (or string symbol))
  (setf wid (gen-widget-id))
  (execute-request 2 (nconc (list name class parent wid (if managed 1 0))
			    resources))
  wid)

;; EXPORT (create-popup-shell)
(defun create-popup-shell (widget-name class parent &rest resources &aux wid)
  (check-type widget-name string)
  (check-type class symbol)
  (check-type parent integer)
  ;;; Resources are checked in the toolkit server
  (setf wid (gen-widget-id))
  (execute-request 18 (append (list widget-name class parent wid) resources))
  wid)

;;; Destroy a widget instance. 

;; EXPORT (destroy-widget)
(defun destroy-widget (widget)
  (check-type widget integer)
  (execute-request 4 (list widget)))

;;; Realize a widget tree

;; EXPORT (realize-widget)
(defun realize-widget (widget)
  (check-type widget integer)
  (execute-request 8 (list widget)))

;;; Get the ID of the parent widget 

;; EXPORT (get-parent)
(defun get-parent (widget)
  (check-type widget integer)
  (first (execute-request 44 (list widget) :num-results 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Get the ID of the window associated with a widget
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-window-id (widget)
  (check-type widget integer)
  (first (execute-request 43 (list widget) :num-results 1)))

;; EXPORT (make-clx-window)
#+clm-needs-clx
(defun make-clx-window (widget &aux window-id x-window)
  (declare (special *x-display*))
  (check-type widget integer)
  (setq window-id (get-window-id widget))
  (setq x-window (xlib::lookup-window *x-display* window-id))
  (unless x-window
    (setq x-window (xlib::make-window :id window-id :display *x-display*))
    (xlib::save-id *x-display* window-id x-window))
  x-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Get the ID of the window associated with a widget
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clm-needs-clx
(defun make-x-window (window-id &optional (display nil) &aux x-window)
  (declare (special *x-display*))
  (unless display 
	  (setq display *x-display*))
  (setq x-window (xlib::make-window :id window-id :display display))
  (xlib::save-id display window-id x-window)
  x-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Raise a window (Only works if window isn't iconified)
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (raise-widget)
(defun raise-widget (widget)
  (check-type widget integer)
  (execute-request 72 (list widget)))

(defun toolkit-do-set (op-code state widgets)
  (cond ((member state '(t on true 1))
	 (execute-request op-code (cons 1 widgets)))
	((member state '(nil off false 0))
	 (execute-request op-code (cons 0 widgets)))
	(t (error "invalid state: ~a~%" state))))

;; EXPORT (manage-widgets)
(defun manage-widgets (&rest widgets)
  (toolkit-do-set 13 t widgets))

;; EXPORT (unmanage-widgets)
(defun unmanage-widgets (&rest widgets)
  (toolkit-do-set 13 nil widgets))

;; EXPORT (set-sensitive)
(defun set-sensitive (&rest widgets)
  (toolkit-do-set 17 t widgets))

;; EXPORT (set-insensitive)
(defun set-insensitive (&rest widgets)
  (toolkit-do-set 17 nil widgets))

;; EXPORT (map-widgets)
(defun map-widgets (&rest widgets)
  (toolkit-do-set 15 t widgets))

;; EXPORT (unmap-widgets)
(defun unmap-widgets (&rest widgets)
  (toolkit-do-set 15 nil widgets))

;; EXPORT (xt-translate-coordinates)
(defun xt-translate-coordinates (widget x y)
  (check-type widget integer)
  (execute-request 21 (list widget (round x) (round y)) :num-results 2))

(defun move-widget (widget x y)
  (declare (fixnum x y))
  (check-type widget integer)
  (execute-request 22 (list widget (round x) (round y))))

(defun resize-widget (widget width height border-width)
  (check-type widget integer)
  (execute-request 23 (list widget (round width) (round height) 
			    (round border-width))))

(defun configure-widget (widget x y width height border-width)
  (check-type widget integer)
  (execute-request 24 (list widget (round x) (round y) (round width) 
			    (round height) (round border-width))))

;; EXPORT (set-values)
(defun set-values (widget &rest args)
  (check-type widget integer)
  (execute-request 5 (cons widget args)))

;; EXPORT (get-values)
(defun get-values (widget &rest resource-names)
  (check-type widget integer)
  (execute-request 6 (cons widget resource-names)
		   :num-results (length resource-names)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; get all available widget classes excluding shell classes
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (get-simple-classes)
(defun get-simple-classes ()
  '(:label :toggle-button :push-button :cascade-button :arrow-button
	   drawn-button :scroll-bar :seperator :text :scale))

;; EXPORT (get-composite-classes)
(defun get-composite-classes ()
  '(:row-column :form :bulletin-board :frame :list :main-window 
		:paned-window :scrolled-window))

;; EXPORT (get-widget-classes)
(defun get-widget-classes ()
  (append (get-simple-classes) (get-composite-classes)))

;; EXPORT (get-shell-classes)
(defun get-shell-classes ()
  '(:application-shell :transient-shell :toplevel-shell
    :override-shell :dialog-shell))

;; EXPORT (get-popup-classes)
(defun get-popup-classes ()
  '(:override-shell :dialog-shell))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; access implicitly created children
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (selection-box-get-child)
(defun selection-box-get-child (widget child-type)
  (check-type widget integer)
  (check-type child-type symbol)
  (first (execute-request 108 (list widget child-type) :num-results 1)))

;; EXPORT (message-box-get-child)
(defun message-box-get-child (widget child-type)
  (check-type widget integer)
  (check-type child-type symbol)
  (first (execute-request 99 (list widget child-type) :num-results 1)))

;; for undocumented backward compatibility
(setf (symbol-function 'get-message-box-child) #'message-box-get-child)

;; EXPORT (file-selection-box-get-child)
(defun file-selection-box-get-child (widget child-type)
  (check-type widget integer)
  (check-type child-type symbol)
  (first (execute-request 93 (list widget child-type) :num-results 1)))

;; EXPORT (command-get-child)
(defun command-get-child (widget child-type)
  (check-type widget integer)
  (check-type child-type symbol)
  (first (execute-request 90 (list widget child-type) :num-results 1)))

;; EXPORT (option-label-gadget)
(defun option-label-gadget (widget)
  (check-type widget integer)
  (first (execute-request 101 (list widget) :num-results 1)))

;; EXPORT (option-button-gadget)
(defun option-button-gadget (widget)
  (check-type widget integer)
  (first (execute-request 100 (list widget) :num-results 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; set child roles
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (scrolled-window-set-areas)
(defun scrolled-window-set-areas (scrolled-window-widget
				  &key (horizontal-scroll-bar-widget nil)
				  (vertical-scroll-bar-widget nil)
				  (work-region-widget nil))
  (execute-request 107 (list scrolled-window-widget
			     (or horizontal-scroll-bar-widget 0)
			     (or vertical-scroll-bar-widget 0)
			     (or work-region-widget 0))))

;; EXPORT (main-window-set-areas)
(defun main-window-set-areas (main-window-widget
			      &key (menu-widget nil)
			      (command-widget nil)
			      (horizontal-scroll-bar-widget nil)
			      (vertical-scroll-bar-widget nil)
			      (work-region-widget nil))
  (execute-request 150 (list main-window-widget
			     (or menu-widget 0)
			     (or command-widget 0)
			     (or horizontal-scroll-bar-widget 0)
			     (or vertical-scroll-bar-widget 0)
			     (or work-region-widget 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Misc queries
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (is-realized)
(defun is-realized (widget)
  (check-type widget integer)
  (first (execute-request 143 (list widget) :num-results 1)))

;; EXPORT (is-managed)
(defun is-managed (widget)
  (check-type widget integer)
  (first (execute-request 16 (list widget) :num-results 1)))

;; EXPORT (get-multi-click-time)
(defun get-multi-click-time (widget)
  (check-type widget integer)
  (first (execute-request 144 (list widget) :num-results 1)))

;; EXPORT (is-valid-widget-id)
(defun is-valid-widget-id (widget)
  (check-type widget integer)
  (first (execute-request 146 (list widget) :num-results 1)))

;; EXPORT (widget-full-name)
(defun widget-full-name (widget)
  (check-type widget integer)
  (first (execute-request 147 (list widget) :num-results 1)))

;; EXPORT (widget-full-class)
(defun widget-full-class (widget)
  (check-type widget integer)
  (first (execute-request 148 (list widget) :num-results 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; cascade button specific highlight function
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (cascade-button-highlight)
(defun cascade-button-highlight (widget highlight)
  (check-type widget integer)
  (execute-request 87 (list widget (if highlight 1 0))))

