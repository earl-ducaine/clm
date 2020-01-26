;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:(user (lisp)); Base:10 -*-

(in-package 'user)
(use-package 'xtk)

(defvar *sccsid* "@(#)color.lisp	1.3 11/27/91")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; This one should be run on a color display ..........
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(run-motif-application 'color-test :execute-main-loop nil)

(defun color-test (&aux app-shell app-rc label radio-box arm-c
			red-scale green-scale blue-scale i-scale
			lbl sw list-w back-c border-c bs-c fore-c high-c 
			select-c ts-c )
  (setf app-shell (create-application-shell)
	app-rc (create-widget :row-column app-shell)
	label (create-widget :push-button app-rc 
			     :shadow-thickness 10
			     :width 200 :height 50)
	lbl (create-widget :label app-rc :label-string "Red:")
	red-scale (create-widget :scale app-rc 
				 :orientation :horizontal
				 :show-value t
				 :value 255
				 :maximum 255)
	lbl (create-widget :label app-rc :label-string "Green:")
	green-scale (create-widget :scale app-rc 
				   :orientation :horizontal
				   :show-value t
				   :value 0
				   :maximum 255)
	lbl (create-widget :label app-rc :label-string "Blue:")
	blue-scale (create-widget :scale app-rc 
				  :orientation :horizontal
				  :show-value t
				  :value 0
				  :maximum 255)
	lbl (create-widget :label app-rc :label-string "Intensity:")
	i-scale (create-widget :scale app-rc 
			       :orientation :horizontal
			       :show-value t
			       :value 255
			       :maximum 255)
	sw (create-widget :scrolled-window app-rc)
	list-w (create-widget :list sw
			      :selection-policy :single
			      :visible-item-count 20)
	radio-box (create-widget :row-column 
				 (create-widget :frame app-rc)
				 :radio-behavior t)
	arm-c (create-widget :toggle-button radio-box 
			     :label-string ":arm-color")
	back-c (create-widget :toggle-button radio-box 
			      :label-string ":background"
			      :set t)
	border-c (create-widget :toggle-button radio-box 
				:label-string ":border-color")
	bs-c (create-widget :toggle-button radio-box 
			    :label-string ":bottom-shadow-color")
	fore-c (create-widget :toggle-button radio-box 
			      :label-string ":foreground")
	high-c (create-widget :toggle-button radio-box 
			      :label-string ":highlight-color")
	select-c (create-widget :toggle-button radio-box 
				:label-string ":select-color")
	ts-c (create-widget :toggle-button radio-box 
			    :label-string ":top-shadow-color"))
  (let ((*current-widget* label)
	(*current-pixel-resource* :background)
	(*current-color* (fourth (first (get-x-colors))))
	(*current-red* 255)
	(*current-green* 0)
	(*current-blue* 0)
	(*intensity-scale* i-scale))
       (declare (special *current-widget* *current-pixel-resource*
			 *current-color* *current-red* *current-green*
			 *current-blue* *intensity-scale*))
       (apply 'set-items (cons list-w (mapcar #'fourth (get-x-colors))))
       ;(list-select-item list-w (fourth (first (get-x-colors))))
       
       (add-callback list-w :single-selection #'set-color 
		     (list red-scale green-scale blue-scale))
       (add-callback arm-c :value-changed #'set-pixel-resource :arm-color)
       (add-callback back-c :value-changed #'set-pixel-resource :background)
       (add-callback border-c :value-changed #'set-pixel-resource :border-color)
       (add-callback bs-c :value-changed #'set-pixel-resource 
		     :bottom-shadow-color)
       (add-callback fore-c :value-changed #'set-pixel-resource :foreground)
       (add-callback high-c :value-changed #'set-pixel-resource 
		     :highlight-color)
       (add-callback select-c :value-changed #'set-pixel-resource :select-color)
       (add-callback ts-c :value-changed #'set-pixel-resource :top-shadow-color)
       (add-callback red-scale :value-changed #'set-red nil)
       (add-callback green-scale :value-changed #'set-green nil)
       (add-callback blue-scale :value-changed #'set-blue nil)
       (add-callback *intensity-scale* :value-changed #'adjust-intensity 
		     (list red-scale green-scale blue-scale))
       (realize-widget app-shell)
       (app-main-loop)))

(defun adjust-intensity (widget client-data &rest call-data &aux old-max factor)
  (declare (ignore widget)
           (special *current-widget* *current-pixel-resource*
		    *current-color* *current-red* *current-green*
		    *current-blue* *intensity-scale*))
  (setf old-max (max *current-red* *current-green* *current-blue*))
  (if (zerop old-max)
      (progn
       (setf *current-red* (first call-data))
       (setf *current-green* (first call-data))
       (setf *current-blue* (first call-data)))
      (progn
       (setf factor (/ (first call-data) old-max))
       (setf *current-red* (min 255 (round (* factor *current-red*))))
       (setf *current-green* (min 255 (round (* factor *current-green*))))
       (setf *current-blue* (min 255 (round (* factor *current-blue*))))))

  (set-values (first client-data) :value *current-red*)
  (set-values (second client-data) :value *current-green*)
  (set-values (third client-data) :value *current-blue*)
  (make-rgb)
  (set-rgb))

(defun set-rgb ()
  (declare (special *current-widget* *current-pixel-resource*
		    *current-color* *current-red* *current-green*
		    *current-blue* *intensity-scale*))
  (set-values *intensity-scale* :value 
	      (max *current-red* *current-green* *current-blue*))
  (set-values *current-widget* *current-pixel-resource* *current-color*))

(defun make-rgb ()
  (declare (special *current-widget* *current-pixel-resource*
		    *current-color* *current-red* *current-green*
		    *current-blue* *intensity-scale*))
  (setf *current-color* (format nil "#~4,'0x~4,'0x~4,'0x"
				(* 256 *current-red*)
				(* 256 *current-green*)
				(* 256 *current-blue*))))

(defun set-red (widget client-data &rest call-data)
  (declare (special *current-red*) (ignore client-data call-data))
  (setf *current-red* (first (get-values widget :value)))
  (make-rgb)
  (set-rgb))

(defun set-green (widget client-data &rest call-data)
  (declare (special *current-green*) (ignore client-data call-data))
  (setf *current-green* (first (get-values widget :value)))
  (make-rgb)
  (set-rgb))

(defun set-blue (widget client-data &rest call-data)
  (declare (special *current-blue*) (ignore client-data call-data))
  (setf *current-blue* (first (get-values widget :value)))
  (make-rgb)
  (set-rgb))

(defun set-pixel-resource (widget client-data &rest call-data)
  (declare (special *current-pixel-resource*) (ignore widget call-data))
  (setf *current-pixel-resource* client-data))

(defun set-color (widget client-data &rest call-data)
  (declare (ignore widget)
	   (special *current-widget* *current-pixel-resource*
		    *current-color* *current-red* *current-green*
		    *current-blue* *intensity-scale*))
  (setf *current-color* (first call-data))
  (loop for color in (get-x-colors) do
	(when (equal *current-color* (fourth color))
	      (setf *current-red* (first color))
	      (setf *current-green* (second color))
	      (setf *current-blue* (third color))
	      (set-values (first client-data) :value *current-red*)
	      (set-values (second client-data) :value *current-green*)
	      (set-values (third client-data) :value *current-blue*)))
  (set-rgb))
