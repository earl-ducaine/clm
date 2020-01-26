;;;-*-Mode:LISP;Syntax: Common-Lisp;Package:gredit;Base:10-*-
;;;
;;; Copyright 1990 GMD (German National Research Center for Computer Science)
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
(defginapackage :gredit) 
(in-package :gredit)
(setq *sccs-id* "@(#)graphic-editor.lisp	1.10  11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class graphic-editor-application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass graphic-editor-application (application)
  (;; overrides
   (name                 :initform "Graphic Editor"         :allocation :class)
   (document-type        :initform 'graphic-editor-document :allocation :class)
   (signature            :initform "gredit"                 :allocation :class)
   (file-type            :initform "gredit"                 :allocation :class))
  (:documentation "a simple graphic editor application"))

(defun make-graphic-editor-application ()
  "start the graphic-editor-application"
  (make-application :class 'graphic-editor-application))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class graphic-editor-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass graphic-editor-document (document)
  (;; overrides
   (shell-width  :initform 400)
   (shell-height :initform 500))
  (:documentation "application dependent document type"))

(defmethod create-windows ((doc graphic-editor-document)
			   &aux scroller form radio-group)
  "create the windows belonging to this document"
  (with-slots (main-shell main-view) doc
    (setq main-shell (make-document-shell doc))
    ;; create a form containing the different parts
    (setq form      (make-form main-shell))
    (setq scroller  (make-scroller form))    
    (setq main-view (make-graphic-editor-view scroller doc))
    (setq radio-group
	  (make-radio-button-group form
			   '(("arrow"     :select-mode)
                             ("rectangle" :rect-mode)
			     ("circle"    :circle-mode)
			     ("line"      :line-mode)
			     ("triangle"  :poly-mode)
                             ("glyph"     :text-mode))
                           :value-changed-callback
                              (make-callback #'set-mode main-view)
			   :button-label-type :pixmap
			   :button-resources '(:shadow-thickness 3)))
    
    ;; layout definition:
    (define-form-constraint radio-group
			    :top-attachment :form
			    :left-attachment :form)
    (define-form-constraint scroller
			    :left-attachment :widget :left-widget radio-group
			    :top-attachment :form 
			    :right-attachment :form
			    :bottom-attachment :form)
    
    ;; add some menu commands
    (add-menu-command (main-menu main-shell)
		      "Draw" "Clear All"
		      (make-callback #'make-clear-all-command doc))
    ))


(defmethod write-to-stream ((doc graphic-editor-document) stream)
  "write the document to the specified stream"
  ;; write number of objects
  (format stream "~d~%" (length (view-objects (main-view doc))))
  
  ;; write all objects
  (loop for object in (view-objects (main-view doc))
	do (write-to-stream object stream)))

(defmethod read-from-stream ((doc graphic-editor-document) stream
			     &aux nr-of-objects) 
  "read the document from the specified stream"
  ;; deinstall existing objects
  (loop for object in (view-objects (main-view doc))
	do (deinstall object))
  
  ;; read number of objects
  (setq nr-of-objects (read stream))

  ;; read positions of objects
  (loop repeat nr-of-objects
	as class = (read stream)
	do (install (read-from-stream class stream)
		    (main-view doc) (read stream) (read stream)) ;; x-pos y-pos
	   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class graphic-editor-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass graphic-editor-view (view)
  ((drawing-mode :accessor drawing-mode :initform :select-mode))
  (:documentation "a view with special reaction to clicks"))

(defun make-graphic-editor-view (parent doc)
  (make-view parent
	     :width 1000 :height 1000 :document doc
	     :class 'graphic-editor-view))

(defmethod button-press ((view graphic-editor-view) code repetition x y)
  "react to button-press event in the window"
  (declare (ignore repetition))
  (case (drawing-mode view)
    (:select-mode  (make-object-selector  view x y code))
    (:rect-mode    (make-rectangle-drawer view x y code))
    (:circle-mode  (make-circle-drawer    view x y code))))

(defmethod set-mode ((view graphic-editor-view) new-mode old-mode)
  "depending on the palette, button-press is propagated to the objects or not"
  (declare (ignore old-mode))
  (with-slots (drawing-mode propagate-button-press) view
    (setq drawing-mode new-mode)
    (setq propagate-button-press (eql drawing-mode :select-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class rectangle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rectangle (direct-manipulation-object)
  (;;overrides
   (facilities :initform :resizable :allocation :class
	       :documentation "rectangles can be moved, selected and resized")
   (outlines   :initform t          :allocation :class
	       :documentation
	       "just move an outline of the object, not the object itself")
   (ensure-square :accessor ensure-square :initarg :ensure-square))
  (:documentation "a rectangular object in the view"))

(defun make-rectangle (width height &key (ensure-square nil))
  (make-direct-manipulation-object 
   width height :class 'rectangle
   :initargs (list :ensure-square ensure-square)))

(defmethod draw ((rect rectangle) count x y width height)
  "draw rect into the view"
  (declare (ignore x y width height))
  (when (zerop count) ;; Ignore all but the last exposure event
    (draw-rectangle rect 0 0 (width rect) (height rect))))


(defmethod constrain-size ((rect rectangle) width height)
  "make sure that width and height remain equal"
  (if (not (ensure-square rect))
      ;; just return width and height unmodified
      (values width height)
    ;; else: compute minimum
    (values (min width height) (min width height))))


(defmethod write-to-stream ((rect rectangle) stream)
  "write textual representation of a rectangle to stream"
  (with-slots (x-pos y-pos width height) rect
    (format stream ":rectangle ~d ~d ~d ~d~%" width height x-pos y-pos)))

(defmethod read-from-stream ((class (eql :rectangle)) stream)
  "create a rectangle from its textual representation"
  (make-rectangle (read stream) (read stream)))  ;; width height
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class circle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass circle (direct-manipulation-object)
  (;;overrides
   (facilities :initform :resizable  :allocation :class)
   (outlines   :initform t           :allocation :class)
   (ensure-true-circle :accessor ensure-true-circle 
		       :initarg :ensure-true-circle))
  (:documentation "a circle in the view"))

(defun make-circle (width height &key (ensure-true-circle nil))
  (make-direct-manipulation-object 
   width height :class 'circle
   :initargs (list :ensure-true-circle ensure-true-circle)))

(defmethod draw ((cc circle) count x y width height)
  "draw circle into the view"
  (declare (ignore x y width height))
  (when (zerop count) ;; Ignore all but the last exposure event
    (draw-arc cc 0 0 (width cc) (height cc) 0 (* 2 pi))))

(defmethod draw-outline ((cc circle) new-x new-y width height &key clear)
  "draw circular outline"
  (declare (ignore clear))
  (with-slots (parent-view) cc
    (xlib:with-gcontext ((gcontext parent-view) :line-style :dash)
      (draw-arc parent-view new-x new-y width height 0 (* 2 pi)))))

(defmethod constrain-size ((cc circle) width height)
  "make sure that width and height remain equal"
  (if (not (ensure-true-circle cc))
      ;; just return width and height unmodified
      (values width height)
    ;; else: compute minimum
    (values (min width height) (min width height))))

(defmethod write-to-stream ((obj circle) stream)
  "write textual representation of object to stream"
  (with-slots (x-pos y-pos width height) obj
    (format stream ":circle ~d ~d ~d ~d~%" 
                   width height x-pos y-pos)))

(defmethod read-from-stream ((class (eql :circle)) stream)
  "create a circle from its textual representation"
  (make-circle (read stream) (read stream)))  ;; width height
       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class rectangle-drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rectangle-drawer (mouse-down-command)
  (;; overrides
   (name :initform "Draw Rectangle" :allocation :class)
   (hysteresis :initform 5          :allocation :class)
   ;; instance-variables
   (shift-pressed :accessor  shift-pressed :initarg :shift-pressed)
   (new-rectangle :accessor new-rectangle :initform nil))
  (:documentation "a mouse-down-command to draw a rectangle"))

(defun make-rectangle-drawer (view x y code)
  (make-mouse-down-command (document view) view x y
			   :cursor :crosshair
			   :class 'rectangle-drawer
			   :initargs (list :shift-pressed (eq code :extend))))

(defmethod constrain-mouse ((cmd rectangle-drawer) x y &aux edge-length)
  "make sure width and height of outline is > 0; ensure square"
  (with-slots (start-x start-y shift-pressed) cmd
    (setq x (max x (1+ start-x)))
    (setq y (max y (1+ start-y)))
    (when shift-pressed
      ;; we draw a square
      (setq edge-length (min (- x start-x) (- y start-y)))
      (setq x (+ start-x edge-length))
      (setq y (+ start-y edge-length))))
  (values x y))

(defmethod draw-feedback ((cmd rectangle-drawer) x y &key clear)
  "draw rectangular feedback"
  (declare (ignore clear))
  (with-slots (start-x start-y view) cmd
    (xlib:with-gcontext ((gcontext view) :line-style :dash)
      (draw-rectangle view start-x start-y (- x start-x)  (- y start-y)))))

(defmethod doit ((cmd rectangle-drawer))
  "install the specified rectangle"
  (with-slots (start-x start-y last-x last-y view 
		       new-rectangle shift-pressed) cmd
    (when (not new-rectangle)
      (setq new-rectangle (make-rectangle (- last-x start-x) (- last-y start-y)
					  :ensure-square shift-pressed)))
    (install new-rectangle view start-x start-y)
    ))

(defmethod undoit ((cmd rectangle-drawer))
  "deinstall the specified rectangle again"
  (with-slots (new-rectangle) cmd
    (deinstall new-rectangle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class circle-drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass circle-drawer (mouse-down-command)
  (;; overrides
   (name :initform "Draw Circle"    :allocation :class)
   (hysteresis :initform 5          :allocation :class)
   ;; instance-variables
   (shift-pressed :accessor  shift-pressed :initarg :shift-pressed)
   (new-circle :accessor new-circle :initform nil))
  (:documentation "a mouse-down-command to draw a circle"))

(defun make-circle-drawer (view x y code)
  (make-mouse-down-command (document view) view x y
			   :cursor :crosshair
			   :class 'circle-drawer
			   :initargs (list :shift-pressed (eq code :extend))))

(defmethod constrain-mouse ((cmd circle-drawer) x y &aux diameter)
  "make sure width and height of outline is > 0"
  (with-slots (start-x start-y shift-pressed) cmd
    (setq x (max x (1+ start-x)))
    (setq y (max y (1+ start-y)))
    (when shift-pressed
      ;; we draw a true circle
      (setq diameter (min (- x start-x) (- y start-y)))
      (setq x (+ start-x diameter))
      (setq y (+ start-y diameter))))
  (values x y))

(defmethod draw-feedback ((cmd circle-drawer) x y &key clear)
  "draw circular feedback"
  (declare (ignore clear))
  (with-slots (start-x start-y view) cmd
    (xlib:with-gcontext ((gcontext view) :line-style :dash)
      (draw-arc view start-x start-y (- x start-x) (- y start-y) 0 (* 2 pi)))))

(defmethod doit ((cmd circle-drawer))
  "install the specified circle"
  (with-slots (start-x start-y last-x last-y view new-circle shift-pressed) cmd
    (when (not new-circle)
      (setq new-circle (make-circle (- last-x start-x) (- last-y start-y)
				    :ensure-true-circle shift-pressed)))
    (install new-circle view start-x start-y)))

(defmethod undoit ((cmd circle-drawer))
  "deinstall the specified circle again"
  (with-slots (new-circle) cmd
    (deinstall new-circle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class clear-all-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clear-all-command (command)
  (;; overrides
   (name :initform "Clear All Command" :allocation :class)
   ;; instance-parameters
   (objects :accessor objects :initarg :objects
	    :documentation "objects being cleared"))
  (:documentation "a command to delete all objects"))

(defun make-clear-all-command (document)
  "create a new command object with appropriate parameters"
  (make-command document
		:class 'clear-all-command
		:initargs `(:objects ,(view-objects (main-view document)))))

(defmethod doit ((cmd clear-all-command))
  "delete all objects from the view"
  (loop for object in (objects cmd)
	do (deinstall object)))

(defmethod undoit ((cmd clear-all-command))
  "reinstall all objects in the view"
  (loop for object in (objects cmd)
	do (install object (main-view (document cmd))
		    (x-pos object) (y-pos object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; main program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(register-application "gredit" 'graphic-editor-application "gredit")
'(make-graphic-editor-application) 
