;;;-*-Mode:LISP;Syntax: Common-Lisp;Package:grec;Base:10-*-
;;;
;;; Copyright 1992 GMD (German National Research Center for Computer Science)
;;;
;;; Permission to use, copy, modify, distribute, and sell this software and its
;;; documentation for any purpose is hereby granted without fee, provided that
;;; the above copyright notice appear in all copies and that both that
;;; copyright notice and this permission notice appear in supporting
;;; documentation, and that the name of GMD not be used in advertising or
;;; publicity pertaining to distribution of the software without specific,
;;; written prior permission.  GMD makes no representations about the
;;; suitability of this software for any purpose.  It is provided "as is"
;;; without express or implied warranty.
;;;
;;; GMD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL GMD
;;; BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
;;; OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
;;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;;
;;; Authors: Project GINA (spenke@gmd.de)
;;;          P.O. Box 1316
;;;          D-5205 Sankt Augustin 1
;;;

(in-package :GINA)
(defginapackage :grec) 
(in-package :grec)
(setq *sccs-id* "@(#)graphic-recorder.lisp	1.4 11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This demo requires that the file "interaction-rec.lisp" is loaded in GINA.
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class graphic-recorder-application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass graphic-recorder-application (application)
  (;; overrides
   (name              :initform "Graphic Recorder"         :allocation :class)
   (document-type     :initform 'graphic-recorder-document :allocation :class)
   (signature         :initform "grec"                     :allocation :class)
   (file-type         :initform "grec"                     :allocation :class))
  (:documentation "a simple graphic editor application"))

(defun make-graphic-recorder-application ()
  "start the graphic-recorder-application"
  (make-application :class 'graphic-recorder-application))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class graphic-recorder-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass graphic-recorder-document (document)
  (;; overrides
   (shell-width  :initform 400)
   (shell-height :initform 500)
   (selection-undoable :allocation :class :initform t)
   ;; new slots
   (objects          :accessor objects          :initform nil)
   ;(taps             :accessor taps             :initform nil)
   ;; animation slots
   (animator        :initform (make-history-animator))
   ;; dialogs
   (colour-dialog    :accessor colour-dialog)
   (undo-dialog      :accessor undo-dialog)
  )
  (:documentation "application dependent document type"))

(defmethod create-windows ((doc graphic-recorder-document)
			   &aux scroller form radio-group paned-window
			        second-view)
  "create the windows belonging to this document"
  (with-slots (main-shell main-view) doc
    (setq main-shell (make-document-shell doc))
    ;; create a form containing the different parts
    (setq form      (make-form main-shell))
    (setq paned-window (make-paned-window form))
    (setq scroller  (make-scroller paned-window))    
    (setq main-view (make-graphic-recorder-view scroller doc))
    (setq second-view
      (make-graphic-recorder-view (make-scroller paned-window :managed nil) 
				  doc 1.0))
    (setq radio-group
	  (make-radio-button-group form
			   '(("arrow"     :select-mode)
                             ("rectangle" :rect-mode)
			     ("circle"    :circle-mode)
			     ("line"      :line-mode)
			     ("triangle"  :poly-mode)
                             ("glyph"     :text-mode))
			   :button-label-type :pixmap
			   :button-resources '(:shadow-thickness 3)))
    (setf (value-changed-callback radio-group)
	  (make-value-changing-callback 'mode-switch-command doc nil))
    ;(make-value-changed-tap doc radio-group '(mode-switch-command) nil)
    (make-tap radio-group '(mode-switch-command) #'update-mode
	      :static-params (list main-view))

    (setf (colour-dialog doc) (make-colour-dialog doc))
    (setf (undo-dialog doc) (make-undo-box doc))

    ;; layout definition:
    (define-form-constraint radio-group
			    :top-attachment :form
			    :left-attachment :form)
    (define-form-constraint paned-window ;;scroller
			    :left-attachment :widget :left-widget radio-group
			    :top-attachment :form 
			    :right-attachment :form
			    :bottom-attachment :form)
    
    ;; add some menu commands
    (remove-menu-entry (main-menu main-shell)
                       "Edit" "Replay History")
    (add-menu-command (main-menu main-shell) "Edit" "Selective Undo..."
       (make-callback #'pop-up (undo-dialog doc)))
    (setf (callback (find-menu-entry (main-menu main-shell) "Edit" "Undo"))
          (make-callback #'gina::new-undo doc))
    (setf (callback (find-menu-entry (main-menu main-shell) "Edit" "Redo"))
          (make-callback #'gina::new-redo doc))

    (add-menu-command (main-menu main-shell)"Draw" "Set Colour..."
		      (make-activation-callback 'pop-up-command doc 
						(colour-dialog doc)))
    (add-menu-command (main-menu main-shell) "Draw" "Clear"
		      (make-activation-callback 'clear-command doc))
    (add-menu-command (main-menu main-shell) "Draw" "Clear All"
		      (make-activation-callback 'clear-all-command doc))
    (add-menu-command (main-menu main-shell) "Draw" "Move To Front"
		      (make-activation-callback 'move-to-front-command doc))
    (add-menu-command (main-menu main-shell) "Draw" "Move To Back"
		      (make-activation-callback 'move-to-back-command doc))
    (make-tap (find-menu-entry (main-menu main-shell) "Draw" "Move To Front")
	      '(object-mover object-selector)  ;; no longer 'direct-selector
	      #'check-selection
	      :static-params (list (main-view doc)))
    (make-tap (find-menu-entry (main-menu main-shell) "Draw" "Move To Back")
	      '(object-mover object-selector)  ;; no longer 'direct-selector
	      #'check-selection
	      :static-params (list (main-view doc)))
    (let ((menu-entry (make-radio-group-entry "Zoom"
					      '(("0.25" 0.25) ("0.5" 0.5) 
						("1" 1.0) ("2" 2.0) ("4" 4.0))
			(make-value-changing-callback 'zoom-command doc nil
						      second-view)
			:initial-value 1.0)))
      (insert-menu-entry (main-menu main-shell) "Draw" "Zoom" menu-entry)
      ;; must be after insert to get parent shell from widget!
      ;(make-value-changed-tap doc menu-entry '(zoom-command) nil)
      (make-tap menu-entry '(zoom-command) #'update-zoom-factor
		:static-params (list second-view)))
    ))

(defun update-mode (radio-box cmd view)
  (declare (ignore cmd))
  (when-changed (radio-box)
    ((value (drawing-mode view)))
    (update-slots)))

(defun check-selection (menu-entry cmd view)
  (declare (ignore cmd))
  (when-changed (menu-entry)
    ((sensitive (loop for obj in (view-objects view)
		      when (selected obj)
		        do (return t))))
    (update-slots)))

(defun update-zoom-factor (menu-entry cmd second-view)
  (declare (ignore cmd))
  (when-changed (menu-entry)
    ((value (zoom-factor second-view)))
    (update-slots)))

(defmethod update-undo-redo-menu-entries ((doc graphic-recorder-document))
  "do nothing because a tap is used"
  (gina::update-new-undo-redo-menu-entries doc))

(defmethod write-to-stream ((doc graphic-recorder-document) stream)
  "write the document to the specified stream"
  ;; write number of objects
  (format stream "~d~%" (length (objects doc)))
  
  ;; write all objects
  (loop for object in (objects doc)
	do (write-to-stream object stream)))

(defmethod read-from-stream ((doc graphic-recorder-document) stream
			     &aux nr-of-objects) 
  "read the document from the specified stream"
  
  ;; read number of objects
  (setq nr-of-objects (read stream))

  ;; read positions of objects
  (setf (objects doc)
        (loop repeat nr-of-objects
	    collect (read-from-stream 'graphical-object stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class graphical-object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass graphical-object ()
  ((inst-count     :accessor inst-count    :initform 1
		   :allocation :class)
   (icount         :accessor icount)
   (object-type    :accessor object-type   :initarg :object-type)
   (x-pos          :accessor x-pos         :initarg :x-pos)
   (y-pos          :accessor y-pos         :initarg :y-pos)
   (width          :accessor width         :initarg :width)
   (height         :accessor height        :initarg :height)
   (ensure-square  :accessor ensure-square :initarg :ensure-square)
   (colour         :accessor colour        :initarg :colour)))

(defun make-graphical-object (type width height x y
                              &key (ensure-square nil) (colour :none))
  (let ((new-object (make-object 'graphical-object nil 
				 :object-type type
				 :width width :height height :x-pos x :y-pos y
				 :ensure-square ensure-square :colour colour)))
    (setf (icount new-object) (inst-count new-object))
    (incf (inst-count new-object))
    new-object))

(defmethod print-object ((obj graphical-object) stream)
  (format stream "#<~a ~d>" (object-type obj) (icount obj)))

(defmethod write-to-stream ((obj graphical-object) stream)
  (format stream "~s ~d ~d ~d ~d ~s ~s~%" (object-type obj)
	  (width obj) (height obj) (x-pos obj) (y-pos obj)
          (ensure-square obj) (colour obj)))

(defmethod read-from-stream ((type (eql 'graphical-object)) stream)
  (make-graphical-object (read stream)                 ;; type
			 (read stream) (read stream)   ;; width height
			 (read stream) (read stream)   ;; x-pos y-pos
                         :ensure-square (read stream)
                         :colour (read stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class clear-all-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clear-all-command (activate-command)
  (;; overrides
   (name :initform "Clear All" :allocation :class)
   ;; instance-parameters
   (objects :accessor objects :initarg :objects
	    :documentation "objects being cleared"))
  (:documentation "a command to delete all objects"))

(defmethod initialize-parameters ((cmd clear-all-command) &rest inits)
  (declare (ignore inits))
  (setf (objects cmd) (objects (document cmd))))

(defmethod executable ((cmd clear-all-command))
  (objects cmd))

(defmethod doit ((cmd clear-all-command))
  "delete all objects from the view"
  (loop for object in (objects cmd)
	do (setf (objects (document cmd))
	         (remove object (objects (document cmd))))))

(defmethod undoit ((cmd clear-all-command))
  "reinstall all objects in the view"
  (loop for object in (objects cmd)
	do (pushnew object (objects (document cmd)))))

(defmethod selective-undo-possible ((cmd clear-all-command))
  "possible if one of the deleted objects is not undeleted"
  (loop for obj in (objects cmd)
        unless (member obj (objects (document cmd)))
          do (return t)))

(defmethod object-description ((cmd clear-all-command))
  (format nil "~s~{ ~s~}" (first (objects cmd)) (rest (objects cmd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class clear-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clear-command (clear-all-command)
  ((name    :initform "Clear"    :allocation :class)))

(defmethod initialize-parameters ((cmd clear-command) &rest inits)
  (declare (ignore inits))
  (setf (objects cmd)
        (loop for view-obj in (view-objects (main-view (document cmd)))
	      when (selected view-obj)
	      collect (view-for view-obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class mode-switch-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass mode-switch-command (value-changed-command)
  ((name     :initform "Change Mode"   :allocation :class)
   (causes-change :initform nil        :allocation :class)))

(defmethod get-state ((cmd mode-switch-command))
  (drawing-mode (main-view (document cmd))))

(defmethod propagate-value ((cmd mode-switch-command) value)
  (set-mode (main-view (document cmd)) value nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class scroll-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass scroll-command (value-changed-command)
  ((name      :initform "Scroll"    :allocation :class)
   (causes-change :initform nil     :allocation :class)
   (vertical  :accessor vertical    :initarg :vertical)))

(defmethod initialize-parameters ((cmd scroll-command) &rest inits)
  (setf (vertical cmd) (first inits)))

(defmethod get-state ((cmd scroll-command))
  (let ((current
           (if (vertical cmd)
             (slot-value (work-area (parent (widget (interactor cmd)))) 
			 'y-pos)
             (slot-value (work-area (parent (widget (interactor cmd)))) 
			 'x-pos))))
    ;; defer update-slots because have already been moved
    (update-slots (work-area (parent (widget (interactor cmd)))))
    (- current)))

;(defmethod update-widget ((cmd scroll-command) widget value)
;  (scroll-to (parent widget)
;	     (if (vertical cmd)
;		 (value (horizontal-scrollbar (parent widget)))
;	         value)
;             (if (vertical cmd)
;		 value
;	         (value (vertical-scrollbar (parent widget)))))
;  (update-slots (work-area (parent (widget cmd)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class graphic-recorder-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass graphic-recorder-view (view)
  ((drawing-mode  :accessor drawing-mode  :initform :select-mode)
   (zoom-factor   :accessor zoom-factor   :initarg :zoom-factor)
   ;; temporarily
   (gina::old-view-objects :accessor gina::old-view-objects :initform nil))
  (:documentation "a view with special reaction to clicks"))

(defun make-graphic-recorder-view (parent doc &optional (zoom-factor 1))
  (let ((view (make-view parent
			 ;:width (round (* 1000 zoom-factor))
			 ;:height (round (* 1000 zoom-factor))
			 :document doc
			 :initargs (list :zoom-factor zoom-factor)
			 :class 'graphic-recorder-view)))
    ;; establish tap for view-objects
    (make-tap view 
	      '(rectangle-drawer circle-drawer clear-all-command
		clear-command move-to-front-command move-to-back-command)
	      #'update-view-objects)
    (make-tap view '(zoom-command) #'update-size)

    ;; make scrolling undoable
    (update-slots view)
    (when (scroller view)
      (setf (value-changed-callback (vertical-scrollbar (scroller view)))
	    (make-value-changing-callback 'scroll-command doc nil t))
      (setf (value-changed-callback (horizontal-scrollbar (scroller view)))
	    (make-value-changing-callback 'scroll-command doc nil nil))
      (make-tap (scroller view) '(scroll-command) #'update-scroller
		:not-initial t))
    view))

(defmethod update-scroller (scroller (cmd scroll-command))
  (scroll-to scroller
	     (if (vertical cmd)
		 (value (horizontal-scrollbar scroller))
	         (current-value cmd))
             (if (vertical cmd)
		 (current-value cmd)
	         (value (vertical-scrollbar scroller))))
  (update-slots (work-area scroller)))

(defmethod update-view-objects ((view graphic-recorder-view) cmd)
  (declare (ignore cmd))
  (compare-view-objects-with-list view (objects (document view))
				  #'view-for #'create-view-object))
    
(defun create-view-object (obj)
  (case (object-type obj)
     ((:rectangle) (make-rectangle obj))
     ((:circle)    (make-circle obj))))

(defmethod update-size ((view graphic-recorder-view) cmd)
  (declare (ignore cmd))
  (setf (width view) (round (* (zoom-factor view) 1000)))
  (setf (height view) (round (* (zoom-factor view) 1000))))

(defmethod button-press ((view graphic-recorder-view) code repetition x y)
  "react to button-press event in the window"
  (declare (ignore repetition))
  (case (drawing-mode view)
    (:select-mode  (make-object-selector  view x y code))
    (:rect-mode    (make-rectangle-drawer view x y code))
    (:circle-mode  (make-circle-drawer    view x y code))))

(defmethod set-mode ((view graphic-recorder-view) new-mode old-mode)
  "depending on the palette, button-press is propagated to the objects or not"
  (declare (ignore old-mode))
  (with-slots (drawing-mode propagate-button-press) view
    (setq drawing-mode new-mode)
    (setq propagate-button-press (eql drawing-mode :select-mode))))

;(defmethod scroll-for-undo-redo ((view graphic-recorder-view) x y)
;  (declare (ignore x y)))

;(defmethod show-for-redo ((view graphic-recorder-view) x y)
;  (when (and (scroller view) (not (in-animation (document view))))
;    (show-point (scroller view) x y)))

;; because of bug in X11R5 server
'(defmethod determine-window-id :after ((view graphic-recorder-view))
  (setf (xlib:gcontext-line-width (gcontext view)) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class zoom-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass zoom-command (value-changed-command)
  ((name   :initform "Zoom"  :allocation :class)
   (causes-change   :initform nil   :allocation :class)))

(defmethod initialize-parameters ((cmd zoom-command) &rest inits)
  (setf (view cmd) (first inits)))

(defmethod get-state ((cmd zoom-command))
  (zoom-factor (view cmd)))

(defmethod propagate-value ((cmd zoom-command) new-value)
  (let ((was-invisible (= (zoom-factor (view cmd)) 1))
	(now-visible (not (= new-value 1))))
    (setf (zoom-factor (view cmd)) new-value)
    (when (and was-invisible now-visible)
      (manage (parent (view cmd))))
    (unless (or was-invisible now-visible)
      (unmanage (parent (view cmd))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class display-object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile eval load)
  (import 'gina::initargs))

(defclass display-object (direct-manipulation-object)
  (;; overrides
   (facilities :initform :resizable :allocation :class)
   (outlines   :initform t          :allocation :class)
   ;; backward reference
   (view-for   :accessor view-for :initarg :view-for)
   ;; new slots
   (pixel      :accessor pixel    :initform nil))
  (:documentation "a graphical object in the view"))

(defun make-display-object (view-for &key (initargs nil)
					  (class 'display-object))
  (let ((new-obj (make-direct-manipulation-object 
		           (width view-for) (height view-for) 
			   :class class
			   :initargs (more-initargs :view-for view-for))))
    (setf (x-pos new-obj) (x-pos view-for))
    (setf (y-pos new-obj) (y-pos view-for))
    new-obj))

(defmethod representation-in-history ((obj display-object))
  (format nil "~s" (view-for obj)))

(defun check-colour (obj cmd)
  (declare (ignore cmd))
  (let ((new-value (colour (view-for obj))))
    (when-changed (obj)
      ((pixel (and (stringp new-value)
		   (xlib:alloc-color (first (xlib:installed-colormaps 
					     (x-window (enclosing-shell obj))))
				     new-value))))
      (update-slots)
      (force-redraw obj))))

(defmethod check-reconfigure ((obj display-object) cmd)
  (declare (ignore cmd))
  (let ((factor (zoom-factor (parent-view obj))))
    (when-changed (obj)
      ((x-pos (round (* factor (x-pos (view-for obj)))))
       (y-pos (round (* factor (y-pos (view-for obj)))))
       (width (round (* factor (width (view-for obj)))))
       (height (round (* factor (height (view-for obj))))))
      (reconfigure obj x-pos y-pos width height))))

(defmethod install-taps ((obj display-object))
  "install and evaluate taps before object is drawn"
  (make-tap obj '(colour-command) #'check-colour)
  (make-tap obj '(object-mover object-resizer zoom-command)
            #'check-reconfigure))

(defmethod reconfigured-by-user ((obj display-object) was-moved was-resized)
  (let ((factor (zoom-factor (parent-view obj))))
    (when was-moved
      (setf (x-pos (view-for obj)) (round (/ (x-pos obj) factor)))
      (setf (y-pos (view-for obj)) (round (/ (y-pos obj) factor))))
    (when was-resized
      (setf (width (view-for obj)) (round (/ (width obj) factor)))
      (setf (height (view-for obj)) (round (/ (height obj) factor))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class rectangle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rectangle (display-object)
  ()
  (:documentation "a rectangular object in the view"))

(defun make-rectangle (view-for)
  (make-display-object view-for
		       :class 'rectangle))

(defmethod draw ((rect rectangle) count x y width height)
  "draw rect into the view"
  (declare (ignore x y width height))
  (when (zerop count) ;; Ignore all but the last exposure event
    (when (pixel rect)
        (xlib:with-gcontext ((gcontext (parent-view rect)) 
			     :foreground (pixel rect))
          (draw-rectangle rect 0 0 (width rect) (height rect) t)))
    (draw-rectangle rect 0 0 (width rect) (height rect))))

(defmethod constrain-size ((rect rectangle) width height)
  "make sure that width and height remain equal"
  (if (not (ensure-square (view-for rect)))
      ;; just return width and height unmodified
      (values width height)
    ;; else: compute minimum
    (values (min width height) (min width height))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class circle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass circle (display-object)
  ()
  (:documentation "a circle in the view"))

(defun make-circle (view-for)
  (make-display-object view-for
		       :class 'circle))

(defmethod draw ((cc circle) count x y width height)
  "draw circle into the view"
  (declare (ignore x y width height))
  (when (zerop count) ;; Ignore all but the last exposure event
    (when (pixel cc)
        (xlib:with-gcontext ((gcontext (parent-view cc)) 
			     :foreground (pixel cc))
          (draw-arc cc 0 0 (width cc) (height cc) 0 (* 2 pi) t)))
    (draw-arc cc 0 0 (width cc) (height cc) 0 (* 2 pi))))

(defmethod draw-outline ((cc circle) new-x new-y width height &key clear)
  "draw circular outline"
  (declare (ignore clear))
  (with-slots (parent-view) cc
    (xlib:with-gcontext ((gcontext parent-view) :line-style :dash)
      (draw-arc parent-view new-x new-y width height 0 (* 2 pi)))))

(defmethod constrain-size ((cc circle) width height)
  "make sure that width and height remain equal"
  (if (not (ensure-square (view-for cc)))
      ;; just return width and height unmodified
      (values width height)
    ;; else: compute minimum
    (values (min width height) (min width height))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class graphical-object-drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass graphical-object-drawer (mouse-down-command)
  (;; overrides
   (name :initform "Draw Object"    :allocation :class)
   (hysteresis :initform 5          :allocation :class)
   ;; new class parameter
   (object-type  :accessor object-type  :initform :rectangle
		 :allocation :class)
   ;; instance-variables
   (shift-pressed :accessor  shift-pressed :initarg :shift-pressed)
   (new-object :accessor new-object :initform nil))
  (:documentation "a mouse-down-command to draw a rectangle"))

(defun make-graphical-object-drawer (view x y code
				     &key (class 'graphical-object-drawer))
  (make-mouse-down-command (document view) view x y
			   :cursor :crosshair
			   :class class
			   :initargs (list :shift-pressed (eq code :extend))))

(defmethod constrain-mouse ((cmd graphical-object-drawer) x y &aux edge-length)
  "make sure width and height of outline is > 0; ensure square"
  (with-slots (start-x start-y shift-pressed) cmd
    (when shift-pressed
      ;; we draw a square
      (multiple-value-bind (x-pos y-pos width height)
	  (mouse-rectangle cmd x y)
	(declare (ignore x-pos y-pos))
        (setq edge-length (min width height))
        (setq x (if (>= x start-x) 
		    (+ start-x edge-length)
		    (- start-x edge-length)))
        (setq y (if (>= y start-y)
		    (+ start-y edge-length)
		    (- start-y edge-length))))))
  (values x y))

(defmethod draw-feedback ((cmd graphical-object-drawer) x y &key clear)
  "draw rectangular feedback"
  (declare (ignore clear))
  (with-slots (view) cmd
    (xlib:with-gcontext ((gcontext view) :line-style :dash)
      (multiple-value-bind (x-pos y-pos width height)
	  (mouse-rectangle cmd x y)
        (draw-object-outline cmd view x-pos y-pos width height)))))

(defmethod doit ((cmd graphical-object-drawer))
  "install the specified rectangle"
  (with-slots (view new-object shift-pressed) cmd
    (multiple-value-bind (x-pos y-pos width height)
	(mouse-rectangle cmd (last-x cmd) (last-y cmd))
      (unless new-object
        (setq new-object
              (make-graphical-object (object-type cmd) width height x-pos y-pos
                                     :ensure-square shift-pressed))))
    (push new-object (objects (document cmd)))))

(defmethod undoit ((cmd graphical-object-drawer))
  "deinstall the specified rectangle again"
  (with-slots (new-object) cmd
    (setf (objects (document cmd))
          (remove new-object (objects (document cmd))))))

(defmethod draw-object-outline ((cmd graphical-object-drawer)
				view x-pos y-pos width height)
  (draw-rectangle view x-pos y-pos width height))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class rectangle-drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rectangle-drawer (graphical-object-drawer)
  (;; overrides
   (name :initform "Draw Rectangle" :allocation :class)
   (object-type     :initform :rectangle  :allocation :class))
  (:documentation "a mouse-down-command to draw a rectangle"))

(defun make-rectangle-drawer (view x y code)
  (make-graphical-object-drawer view x y code :class 'rectangle-drawer))

(defmethod draw-object-outline ((cmd rectangle-drawer)
				view x-pos y-pos width height)
  (draw-rectangle view x-pos y-pos width height))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class circle-drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass circle-drawer (graphical-object-drawer)
  (;; overrides
   (name :initform "Draw Circle"    :allocation :class)
   (object-type     :initform :circle  :allocation :class))
  (:documentation "a mouse-down-command to draw a circle"))

(defun make-circle-drawer (view x y code)
  (make-graphical-object-drawer view x y code :class 'circle-drawer))

(defmethod draw-object-outline ((cmd circle-drawer)
				view x-pos y-pos width height)
  (draw-arc view x-pos y-pos width height 0 (* 2 pi)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class move-to-front-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass move-to-front-command (activate-command)
  ((name     :initform "Move To Front"    :allocation :class)
   (objects   :accessor objects  :initarg :objects)
   (old-sequence  :accessor old-sequence  :initform nil)))

(defmethod initialize-parameters ((cmd move-to-front-command) &rest inits)
  (declare (ignore inits))
  (setf (objects cmd)
        (loop for obj in (view-objects (main-view (document cmd)))
			       when (selected obj)
			       collect obj)))

(defmethod executable ((cmd move-to-front-command))
  (objects cmd))

(defmethod doit ((cmd move-to-front-command))
  (unless (old-sequence cmd)
    (setf (old-sequence cmd) (copy-list (objects (document cmd)))))
  (loop for obj in (reverse (objects cmd))
        do (setf (objects (document cmd))
                 (cons (view-for obj)
		       (delete (view-for obj) (objects (document cmd)))))))

(defmethod undoit ((cmd move-to-front-command))
  (setf (objects (document cmd)) (old-sequence cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class move-to-back-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass move-to-back-command (move-to-front-command)
  ((name     :initform "Move To Back"    :allocation :class)))

(defmethod doit ((cmd move-to-back-command))
  (unless (old-sequence cmd)
    (setf (old-sequence cmd) (copy-list (objects (document cmd)))))
  (loop for obj in (objects cmd)
        do (setf (objects (document cmd))
                 (append (delete (view-for obj) (objects (document cmd)))
			 (list (view-for obj))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; colour dialog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass colour-dialog (modeless-dialog-box)
  ((ok-button    :accessor ok-button)
   (colour-entry :accessor colour-entry)))

(defun make-colour-dialog (document &aux box)
  (setq box (make-modeless-dialog-box "Colour"
			  :document document
			  :class 'colour-dialog
			  :motif-resources '(:vertical-spacing 10
					     :horizontal-spacing 10)))
  (setf (ok-button box)
        (make-push-button box "Dismiss"))
  (setf (colour-entry box)
        (make-radio-button-group box
			  '(("none" :none) ("30" "gray30") ("40" "gray40")
                            ("50" "gray50") ("60" "gray60") ("70" "gray70")
                            ("80" "gray80") ("90" "gray90") ("100" "gray100"))
			  :label-string "Gray Level"
			  :orientation :horizontal
			  :num-columns 3
			  :button-resources '(:indicator-on nil
					      :shadow-thickness 3)))
  (setf (value-changed-callback (colour-entry box))
        (make-value-changing-callback 'colour-command document nil))
				      ;((#'value-to-colour))
  (make-tap (colour-entry box) 
	    '(object-mover object-selector colour-command clear-command
	      clear-all-command)
	#'adapt-scale-to-selection
        :static-params (list (main-view document)))
  (setf (activate-callback (ok-button box))
        (make-activation-callback 'pop-down-command document box))
  (define-form-constraint (colour-entry box)
	:top-attachment :form :left-attachment :form :right-attachment :form)
  (define-form-constraint (ok-button box)
	:top-attachment :widget :top-widget (colour-entry box)
	:left-attachment :form)
  box)

(defun list-to-single-value (list-of-values 
			     &optional (value-when-different nil))
  (if (listp list-of-values)
      (let ((compare (first list-of-values)))
        (loop for another in (rest list-of-values)
	      unless (equal another compare)
              do (return-from list-to-single-value value-when-different))
	compare)
      list-of-values))

(defun adapt-scale-to-selection (widget cmd view)
  (declare (ignore cmd))
  (let ((selected-colours (loop for obj in (view-objects view)
			      when (selected obj)
			      collect (colour (view-for obj)))))
    (when-changed (widget)
        ((value (list-to-single-value selected-colours)))
      (update-slots)) 
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class colour-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass colour-command (value-changed-command)
  ((name   :initform "Change Colour"   :allocation :class)
   (objects :accessor objects :initarg :objects)))

(defmethod initialize-parameters ((cmd colour-command) &rest inits)
  (declare (ignore inits))
  (setf (objects cmd)
        (loop for obj in (view-objects (main-view (document cmd)))
	      when (selected obj)
	      collect (view-for obj))))

(defmethod propagate-value ((cmd colour-command) value)
  (if (and value (listp value))
      (loop for obj in (objects cmd)
            for colour in value
            do (setf (colour obj) colour))
      (loop for obj in (objects cmd)
	    do (setf (colour obj) value))))

(defmethod selective-undo-possible ((cmd colour-command))
  (loop for obj in (objects cmd)
        when (member obj (objects (document cmd)))
          do (return t)))

(defmethod selective-redo-possible ((cmd colour-command))
  (selective-undo-possible cmd))

(defun colours (objects)
  (let* ((values (loop for obj in objects
	               collect (colour obj)))
	 (different (loop for val in values
		          unless (equal val (first values))
		            do (return t))))
    (if different
	values
        (first values))))

(defmethod get-state ((cmd colour-command))
  (setf (objects cmd)
        (loop for obj in (objects cmd)
	      when (member obj (objects (document cmd)))
	      collect obj))
  (colours (objects cmd)))

(defmethod value-description ((cmd colour-command) value)
  (if (and value (listp value))
      (concatenate 'simple-string (first value) "+...")
      (if value
	  (format nil "~a" value)
	  "[]")))

(defmethod object-description ((cmd colour-command))
  (format nil "~s~{ ~s~}" (first (objects cmd)) (rest (objects cmd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; main program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(register-application "grec" 'graphic-recorder-application "grec")
'(make-graphic-recorder-application) 


