;;;-*-Mode:LISP;Syntax: Common-Lisp;Package:dia;Base:10-*-

(in-package :GINA)
(defginapackage :dia) 
(in-package :dia)
(setq *sccs-id* "@(#)diagram-editor.lisp	1.3  9/22/92")
 
;; This is a simple diagram-editor implemented using the GINA application 
;; framework. 
;; First, as in each GINA application, there are subclasses of the GINA classes
;; APPLICATION, DOCUMENT and VIEW. In this way the standard functionality
;; of a typical interactive application is inherited: there can be multiple
;; documents each with a menu bar and the standard menu entries like
;; NEW, OPEN, SAVE, SAVE AS, REVERT, CLOSE, QUIT, PRINT, UNDO,  REDO etc.
;; Dialogs for opening and saving documents and browsing the directory
;; tree are also available.
;; The VIEW subclass is a drawing area with a special reaction to mouse input.
;; Next come the classes BOX and ARROW which are subclasses of the GINA class
;; DIRECT-MANIPULATION-OBJECT, i.e. they can be installed in a view and 
;; already be moved, resized, and selected. They override the method
;; DRAW where they display themself according to their internal state.
;; BOX-DRAWER is a sublass of MOUSE-DOWN-COMMAND. It inherits the general
;; behaviour of a dragging operation, i.e. a feedback is shown as long as
;; the mouse-buton is held down and moved arround, autoscrolling when
;; the mouse leaves the view etc. Also the undo/redo capabilities of
;; the superclass COMMAND are inherited. The BOX-DRAWER defines a special
;; feedback (a rubberband rectangle), guarantees a minimum size of the new box
;; and creates/destroys a box object in its DOIT and UNDOIT methods.
;; The two COMMAND subclasses DELETE-COMMAND and CHANGE-STYLE-COMMAND are
;; instantiated by the DELETE menu entry resp. by switching the palette.
;; They store sufficient information to redo the operation later when the
;; user calls undo.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class diagram-editor-application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass diagram-editor-application (application)
  (;; override some inherited slots of superclass with new inits
   (name            :initform "Diagram Editor"         :allocation :class)
   (document-type   :initform 'diagram-editor-document :allocation :class)
   (signature       :initform "dia"                    :allocation :class)
   (file-type       :initform "dia"                    :allocation :class)))

(defun make-diagram-editor-application ()
  "start the diagram-editor-application"
  (make-application :class 'diagram-editor-application))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class diagram-editor-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass diagram-editor-document (document)
  (;; override some inherited slots of superclass with new inits
   (shell-width  :initform 400)
   (shell-height :initform 500)
   ;; additional instance slots of subclass
   (palette :accessor palette)))

;; This method is called by GINA whenever a new document is created because
;; the user calls NEW or OPEN and at application startup.
;; It creates the Motif widgets which make up the main window.
;; For more complicated windows this part of the code can be generated
;; by the GINA Interface Builder.

(defmethod create-windows ((doc diagram-editor-document)
			   &aux scroller form)
  "create the windows belonging to this document"
  (with-slots (main-shell main-view palette) doc
    (setq main-shell (make-document-shell doc))
    ;; create a form containing the different parts
    (setq form      (make-form main-shell))
    (setq scroller  (make-scroller form))    
    (setq main-view (make-diagram-editor-view scroller doc))
    (setq palette
	  (make-radio-button-group 
	      form '(("line1"        (:solid 1))
		     ("dashed-line"  (:dash  2))
		     ("line2"        (:solid 2))
		     ("line4"        (:solid 4)))
	      :value-changed-callback
	      (make-callback 'make-change-style-command doc)
	      :button-label-type :pixmap
	      :button-resources '(:shadow-thickness 5)))
    
    ;; layout definition for form widget:
    (define-form-constraint palette 
       :top-attachment :form :left-attachment :form)
    (define-form-constraint scroller 
       :left-attachment :widget :left-widget palette
       :top-attachment :form :right-attachment :form :bottom-attachment :form)
    
    ;; add a menu entry to the menu bar:
    (add-menu-command (main-menu main-shell) "Draw" "Delete"
		      (make-callback 'make-delete-command doc)
		      :accelerator "d")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class diagram-editor-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass diagram-editor-view (view)
  ((box-counter :accessor box-counter :initform 1
     :documentation "number used for next box created")
   (last-box    :accessor last-box :initform nil
     :documentation "box from which to draw arrow when next box is created")))

(defun make-diagram-editor-view (parent doc)
  (make-view parent :width 1000 :height 1000 :document doc
	            :class 'diagram-editor-view))

(defmethod button-press ((view diagram-editor-view) code repetition x y)
  "a box is drawn when the mouse-button is pressed in the view"
  (declare (ignore repetition))
  (make-box-drawer view x y code))

;; line-width and -style used depend on the current value of the palette

(defmethod default-line-width ((view diagram-editor-view))
  (second (value (palette (document view)))))

(defmethod default-line-style ((view diagram-editor-view))
  (first (value (palette (document view)))))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *box-min-width*)  (setq *box-min-width*  65)
(defvar *box-min-height*) (setq *box-min-height* 30)

(defclass box (direct-manipulation-object)
  (;; override some inherited slots of superclass with new inits
   (facilities :initform :resizable :allocation :class
	  :documentation "roundtangles can be moved, selected and resized")
   (outlines   :initform nil        :allocation :class
	  :documentation "do not move only an outline of the object")
   ;; additional instance slots of subclass
   (my-number  :accessor my-number  :initarg :my-number)
   (line-width :accessor line-width :initarg :line-width)
   (line-style :accessor line-style :initarg :line-style) ;; :solid or :dash
   (in-arrow   :accessor in-arrow   :initform nil)
   (out-arrow  :accessor out-arrow  :initform nil)))

(defun make-box (width height line-style line-width new-number)
  (make-direct-manipulation-object width height :class 'box
				   :initargs (list :line-style line-style
						   :line-width line-width
						   :my-number new-number)))

(defmethod draw ((rect box) count x y width height 
		      &aux offset (circle-radius 15))
  "draw box with rounded corners into the view"
   #-allegro-v4.0 (declare (ignore count x y width height))
   (xlib:with-gcontext ((gcontext (parent-view rect))
			:line-width (line-width rect)
			:line-style (line-style rect))
	(with-slots (width height my-number) rect
	  (setq offset (truncate (/ (line-width rect) 2)))
	  (draw-line rect circle-radius offset (- width circle-radius) offset)
	  (draw-line rect circle-radius (- height offset)
		     (- width circle-radius) (- height offset))
	  (draw-line rect offset circle-radius offset (- height circle-radius))
	  (draw-line rect (- width offset) circle-radius
		     (- width offset) (- height circle-radius))
	     
	  (draw-arc rect offset offset
		    (* 2 circle-radius) (* 2 circle-radius)
		    (/ pi 2) (/ pi 2))
	  (draw-arc rect (+ offset (- width (* 2 circle-radius))) offset
		    (- (* 2 circle-radius) (line-width rect))
		    (- (* 2 circle-radius) (line-width rect))
		    (/ pi 2) (- (/ pi 2)))
	  (draw-arc rect offset (- height (* 2 circle-radius))
		    (- (* 2 circle-radius) (line-width rect))
		    (+ offset (- (* 2 circle-radius) (line-width rect)))
		    (- (/ pi 2)) (- (/ pi 2)))
	  (draw-arc rect (- width (* 2 circle-radius) offset)
		    (- height (* 2 circle-radius) offset)
		    (* 2 circle-radius) (* 2 circle-radius)
		    (- (/ pi 2)) (/ pi 2))

	  (draw-glyphs rect (round (- (/ width 2) 25)) 20
		       (format nil "Box ~a" my-number)))))

;; this method is called by GINA whenever the user tries to resize a box
;; the default method just returns width and height unmodified
(defmethod constrain-size ((rect box) width height)
  "make sure that size cannot go below minimum"
  (values (max *box-min-width* width) (max height *box-min-height*)))

;; this method is called by GINA whenever the user moves or resizes a box
;; the default method is just empty
;; we adapt the two arrows whenever a box is moved or resized
(defmethod reconfigured-by-user ((rect box) was-moved was-resized)
  (declare (ignore was-moved was-resized))
  (when (in-arrow rect)  (recompute-geometry (in-arrow  rect)))
  (when (out-arrow rect) (recompute-geometry (out-arrow rect))))

;; whenever a box is selected we show its line style in the palette
(defmethod (setf selected) :after (new-value (rect box))
  (when new-value
    (setf (value (palette (document (parent-view rect))))
	  (list (line-style rect) (line-width rect)))))

;; two new utility methods
(defmethod center-x ((rect box))
  (+ (x-pos rect) (round (/ (width rect) 2))))

(defmethod center-y ((rect box))
  (+ (y-pos rect) (round (/ (height rect) 2))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class arrow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass arrow (direct-manipulation-object)
  (;; override some inherited slots of superclass with new inits
   (facilities :initform :selectable :allocation :class
	 :documentation "arrows can only be selceted, not moved or resized")
   ;; additional instance slots of subclass
   (arrowhead1-x :accessor arrowhead1-x)
   (arrowhead1-y :accessor arrowhead1-y)
   (arrowhead2-x :accessor arrowhead2-x)
   (arrowhead2-y :accessor arrowhead2-y)
   (line-width :accessor line-width  :initarg :line-width)
   (line-style :accessor line-style  :initarg :line-style)
   (start-box  :accessor start-box   :initarg :start-box)
   (end-box    :accessor end-box     :initarg :end-box)))

(defun make-arrow (start-box end-box line-style line-width)
  (make-direct-manipulation-object 10 10 :class 'arrow
	  :initargs (list :line-style line-style :line-width line-width
			  :start-box start-box :end-box end-box)))

;; whenever a box is selected we show its line style in the palette  
(defmethod (setf selected) :after (new-value (aw arrow))
  (when new-value
    (setf (value (palette (document (parent-view aw))))
	  (list (line-style aw) (line-width aw)))))

;; whenever an arrow is installed in the view 
;; we update the pointers in the two boxes it is attached to
(defmethod install :after ((aw arrow) (view diagram-editor-view)
			   &optional x y &key (redraw t))
  "install pointer within attached boxes"
  (declare (ignore x y redraw))
  (setf (out-arrow (start-box aw)) aw)
  (setf (in-arrow  (end-box   aw)) aw))

;; when an arrow disappears the pointers are cleared again
(defmethod deinstall :after ((aw arrow) &key (redraw t))
  "remove pointer from attached boxes"
  (declare (ignore redraw))
  (setf (out-arrow (start-box aw)) nil)
  (setf (in-arrow  (end-box   aw)) nil))

(defmethod recompute-geometry ((aw arrow) &key (redraw t)
			       &aux x y width height tmp len (margin 10)
			            (angle (/ pi 4)) (head-length 15))
  "recompute x,y,width and height when boxes have moved"
  (with-slots (start-box end-box
	       arrowhead1-x arrowhead1-y arrowhead2-x arrowhead2-y) aw
     (setq x      (min (center-x start-box) (center-x end-box)))
     (setq y      (min (center-y start-box) (center-y end-box)))
     (setq width  (abs (- (center-x start-box) (center-x end-box))))
     (setq height (abs (- (center-y start-box) (center-y end-box))))
     (if (= 0 width)
	 (setq tmp (/ pi 2))
	 (setq tmp (atan (/ (- (center-y end-box) (center-y start-box))
			    (- (center-x end-box) (center-x start-box))))))
     (cond ((< (center-x start-box) (center-x end-box))
	    (setq len (- head-length)))
           ((> (center-x start-box) (center-x end-box))
	    (setq len head-length))
	   ((< (center-y start-box) (center-y end-box))
	    (setq len (- head-length)))
	   (t (setq len head-length)))
     (setq arrowhead1-x
       (round (+ (center-x end-box) (* len (cos (+ tmp angle))))))
     (setq arrowhead1-y
       (round (+ (center-y end-box) (* len (sin (+ tmp angle))))))
     (setq arrowhead2-x
       (round (+ (center-x end-box) (* len (cos (- tmp angle))))))
     (setq arrowhead2-y
       (round (+ (center-y end-box) (* len (sin (- tmp angle))))))
     
     (reconfigure aw (- x margin) (- y margin)
		  (+ width (* 2 margin)) (+ height (* 2 margin))
		  :redraw redraw)))

(defmethod draw ((aw arrow) count x y width height)
  "draw arrow into the view"
  #-allegro-v4.0 (declare (ignore count x y width height))
  (with-slots (start-box end-box arrowhead1-x arrowhead1-y
			 arrowhead2-x arrowhead2-y) aw
    (xlib:with-gcontext ((gcontext (parent-view aw))
			 :line-width (line-width aw)
			 :line-style (line-style aw))
      (draw-line (parent-view aw) (center-x start-box) (center-y start-box)
		 (center-x end-box) (center-y end-box))
      (draw-line (parent-view aw) arrowhead1-x arrowhead1-y
		 arrowhead2-x arrowhead2-y)
      (draw-line (parent-view aw) arrowhead1-x arrowhead1-y
		 (center-x end-box) (center-y end-box))
      (draw-line (parent-view aw) (center-x end-box) (center-y end-box)
		 arrowhead2-x arrowhead2-y))))

;; this method is called by GINA to determine if a mouse-click hist an arrow
;; the default just checks for the enclosing rectangle
;; we simply check if we are near the center of the line
;; a more precise version would check whether the point lies near the line
(defmethod point-inside ((aw arrow) x y &aux center-x center-y)
  "determine whether line object is hit"
  ;; simply test whether near the center (unprecise)
  (setq center-x (+ (x-pos aw) (* (width aw) 0.5)))
  (setq center-y (+ (y-pos aw) (* (height aw) 0.5)))
  (and (> x (- center-x 30)) (< x (+ center-x 30))
       (> y (- center-y 30)) (< y (+ center-y 30))))

;; this method is called by GINA to show that an object is selected
;; the default version draw 4 handles at the corners of the enclosing rectangle
;; we draw 3 handles at the start, middle and end of the arrow
(defmethod draw-selected ((aw arrow) count x y width height)
  "draw 3 handles onto the line"
  (declare (ignore count x y width height))
  (draw-rectangle (parent-view aw) (- (center-x (start-box aw)) 2)
		                   (- (center-y (start-box aw)) 2) 5 5 t)
  (draw-rectangle (parent-view aw) (- (center-x (end-box aw)) 2)
		                   (- (center-y (end-box aw)) 2) 5 5 t)
  (draw-rectangle aw (round (/ (- (width aw) 5) 2))
		     (round (/ (- (height aw) 5) 2)) 5 5 t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class box-drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass box-drawer (mouse-down-command)
  (;; override some inherited slots of superclass with new inits
   (name :initform "Draw Box" :allocation :class)
   (hysteresis :initform 1)
   ;; additional instance-variables of subclass
   (new-box :accessor new-box :initform nil)
   (old-last-box    :accessor old-last-box)
   (new-arrow       :accessor new-arrow       :initform nil))
  (:documentation "a mouse-down-command to draw a box"))

(defun make-box-drawer (view x y code)
  (declare (ignore code))
  (make-mouse-down-command (document view) view x y
			   :cursor :crosshair :class 'box-drawer))

;; this method filters the mouse coordinates coming in from the X server
;; the default method returns them unmodified
(defmethod constrain-mouse ((cmd box-drawer) x y)
  "make sure width and height are not less than minimal size"
  (with-slots (start-x start-y) cmd
     (values (max x (+ start-x *box-min-width*))
             (max y (+ start-y *box-min-height*)))))

;; this method is called by GINA whenever the mouse moves
;; the default is a rubberband line from the mouse-down to the current position
(defmethod draw-feedback ((cmd box-drawer) x y &key clear)
  "draw rectangular dashed rubberband feedback"
  (declare (ignore clear))
  (with-slots (start-x start-y view) cmd
    (xlib:with-gcontext ((gcontext view) :line-style :dash)
      (draw-rectangle view start-x start-y (- x start-x)  (- y start-y)))))

;; this method is called when the command is not submitted because the
;; mouse was not moved after it went down
;; it is normally empty
(defmethod not-submitted ((cmd box-drawer))
  "deselect all objects if hysteresis not exceeded i.e. just a click"
  (loop for object in (view-objects (view cmd))
    when (selected object) do (setf (selected object) nil)))
     
;; this method is called as soon as the mouse button is released
(defmethod doit ((cmd box-drawer))
  "do command for the first time"
  (with-slots (start-x start-y last-x last-y view  new-box new-arrow) cmd
     (setq new-box 
       (make-box (- last-x start-x) (- last-y start-y)
		 (default-line-style view) (default-line-width view)
		 (box-counter view)))
     (setf (old-last-box cmd) (last-box view))
     (when (last-box view)
       ;; there is already another box => create arrow
       (setq new-arrow (make-arrow (last-box view) new-box
				   (default-line-style view)
				   (default-line-width view))))
     ;; do what also happens in a REDO
     (redoit cmd)))

(defmethod redoit ((cmd box-drawer))
  "reinstall existing objects"
  (with-slots (new-box new-arrow view start-x start-y) cmd
     (incf (box-counter view))
     (install new-box view start-x start-y)
     (setf (last-box view) new-box)
     (when new-arrow
       (recompute-geometry new-arrow :redraw nil)
       (install new-arrow view (x-pos new-arrow) (y-pos new-arrow)))))

(defmethod undoit ((cmd box-drawer))
  "deinstall the specified box again"
  (with-slots (new-box new-arrow view) cmd
    (decf (box-counter view))
    (deinstall new-box)
    (setf (last-box view) (old-last-box cmd))
    (when new-arrow (deinstall new-arrow))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class delete-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass delete-command (command)
  (;; override some inherited slots of superclass with new inits
   (name :initform "Delete Objects" :allocation :class)
   ;; additional instance-variables of subclass
   (deleted-objects :accessor deleted-objects :initarg :deleted-objects
		    :documentation "objects being deleted")
   (old-last-box :accessor old-last-box :initarg :old-last-box))
  (:documentation "a command to delete the selected objects"))

(defun make-delete-command (document &aux selected-objects)
  "create a new command object with appropriate parameters"
  (setq selected-objects
    (loop for obj in (view-objects (main-view document))
	  when (selected obj) collect obj))

  ;; in addition to the selected objects, all arrows attached to selected
  ;; boxes have to be deleted
  (setq selected-objects
    (delete-duplicates
     (append selected-objects
             (loop for obj in selected-objects
	           when (and (typep obj 'box) (in-arrow obj)) 
	              collect (in-arrow obj)
	           when (and (typep obj 'box) (out-arrow obj)) 
	              collect (out-arrow obj)))))
  (make-command document
		:class 'delete-command
		:initargs `(:deleted-objects ,selected-objects
			    :old-last-box ,(last-box (main-view document)))))

;; GINA calls this method to ask the command if it really wants to be executed
;; the default answer is T
;; we do not want, however, to have a delete command in the history when there
;; was no object deleted
(defmethod executable ((cmd delete-command))
  (not (null (deleted-objects cmd))))

(defmethod doit ((cmd delete-command))
  "delete all objects from the view"
  (loop for object in (deleted-objects cmd)
	do (deinstall object))
  (when (member (old-last-box cmd) (deleted-objects cmd))
    (setf (last-box (view cmd)) nil)))

(defmethod undoit ((cmd delete-command))
  "reinstall all objects in the view"
  (loop for object in (deleted-objects cmd)
	do (install object (main-view (document cmd))
		    (x-pos object) (y-pos object)))
  (setf (last-box (view cmd)) (old-last-box cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class change-style-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass change-style-command (command)
  (;; override some inherited slots of superclass with new inits
   (name :initform "Change Style" :allocation :class)
   ;; additional instance-variables of subclass
   (affected-objects :accessor affected-objects :initarg :affected-objects)
   (old-styles :accessor old-styles :initarg :old-styles)
   (new-style :accessor new-style :initarg :new-style))
  (:documentation "a command to change line-style and/or line-width"))

(defun make-change-style-command (document new-style old-style
				  &aux selected-objects old-styles)
  "create a new command object with appropriate parameters"
  (declare (ignore old-style))
  (setq selected-objects
    (loop for obj in (view-objects (main-view document))
     when (selected obj) collect obj))
  (setq old-styles
    (loop for obj in selected-objects
     collect (list (line-style obj) (line-width obj))))
  (make-command document
		:class 'change-style-command
		:initargs `(:affected-objects ,selected-objects
			    :old-styles ,old-styles
			    :new-style ,new-style)))

;; GINA calls this method to ask the command if it really wants to be executed
;; the default answer is T
;; we do not want, however, to have a command in the history when there
;; was no object affected
(defmethod executable ((cmd change-style-command))
  (not (null (affected-objects cmd))))

(defmethod doit ((cmd change-style-command))
  "change line-width and line-style for objects"
  (with-slots (new-style) cmd
    (loop for object in (affected-objects cmd)
           do (setf (line-style object) (first new-style))
              (setf (line-width object) (second new-style))
              (force-redraw object))))

(defmethod undoit ((cmd change-style-command))
  "change back line-width and line-style for objects"
     (loop for object in (affected-objects cmd)
           for old-style in (old-styles cmd)
           do (setf (line-style object) (first old-style))
              (setf (line-width object) (second old-style))
              (force-redraw object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; main program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make application startable from the GINA Finder
(register-application "dia" 'diagram-editor-application "dia")

;; this would start the application
'(make-diagram-editor-application) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; extra code for saving and restoring documents:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GINA handles saving and resoring of documents almost automatically
;; we only have to transform the document contents, i.e. the boxes and
;; arrows into a stream of characters and vice versa
;; GINA supplies the corresponding menu entries and dialog boxes, 
;; opens the desired file, performs error checking, prints error messages
;; and eventually calls write-to-stream  or read-from-stream
;; with an already open stream

;; We write out all boxes first and then all arrows. The description of 
;; an arrow contains the numbers of its two boxes. When an arrow is
;; later read, there already exist all boxes so that the box holding
;; a certain number can easily be found.

(defmethod write-to-stream ((doc diagram-editor-document) stream)
  "write the document to the specified stream"
  ;; write number of objects
  (format stream "~d~%" (length (view-objects (main-view doc))))
  
  ;; write all boxes
  (loop for object in (view-objects (main-view doc))
	when (typep object 'box) do (write-to-stream object stream))

  ;; write all arrows
  (loop for object in (view-objects (main-view doc))
    when (typep object 'arrow) do (write-to-stream object stream)))

(defmethod read-from-stream ((doc diagram-editor-document) stream
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
        as object = (read-from-stream class stream)
   when (typep object 'arrow)
     do (find-boxes object (main-view doc))
   do (install object (main-view doc)
	       (read stream) (read stream)) ;; x-pos y-pos
	   ))

(defmethod find-boxes ((aw arrow) view)
  "replace boxnumbers by pointers after reading arrow from file"
  (loop for object in (view-objects view)
    when (and (typep object 'box)
	      (= (my-number object) (end-box aw)))
      do (setf (end-box aw) object) (return))
  (loop for object in (view-objects view)
    when (and (typep object 'box)
	      (= (my-number object) (start-box aw)))
      do (setf (start-box aw) object) (return))
  (recompute-geometry aw :redraw nil))

(defmethod write-to-stream ((rect box) stream)
  "write textual representation of a box to stream"
  (with-slots (x-pos y-pos width height line-style line-width my-number) rect
    (format stream ":box ~d ~d ~s ~d ~d ~d ~d~%"
	    width height line-style line-width my-number x-pos y-pos)))

(defmethod read-from-stream ((class (eql :box)) stream)
  "create a box from its textual representation"
  (make-box (read stream) ;; width 
	    (read stream) ;; height
	    (read stream) ;; line-style
	    (read stream) ;; line-width
	    (read stream) ;; boxnumber
   ))

(defmethod write-to-stream ((aw arrow) stream)
  "write textual representation of arrow to stream"
  (with-slots (x-pos y-pos width height line-style line-width
		     start-box end-box) aw
    (format stream ":arrow ~d ~d ~s ~d ~d ~d~%" 
                    (my-number start-box) (my-number end-box)
		    line-style line-width x-pos y-pos)))

(defmethod read-from-stream ((class (eql :arrow)) stream)
  "create an arrow from its textual representation"
  (make-arrow (read stream)   ;; start-box
	      (read stream)   ;; end-box
	      (read stream)   ;; line-style
	      (read stream))) ;; line-width

;; end of file.

