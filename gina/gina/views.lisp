;;; -*- Mode:LISP;Syntax: Common-Lisp;Package: gina ;Base:10-*-
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

(in-package :gina)

(setq *sccs-id* "@(#)views.lisp	1.29  1/13/94")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass view (widget)
   (description "a drawing area where document contents are shown"
		:application-subclasses :always
		:gina-subclasses "only in the demo applications"
		:instantiate "to make very simple tests")
   (;; instance-parameters
    (document     :accessor document     :initarg :document
		  :documentation "the document shown in the view")
    (scroller     :accessor scroller :initarg :scroller
		  :documentation "the scroller containing the view, if any")
    
    ;; overrides
    ;; these two slots are inherited from class widget, but can be initialized here
    (width       :accessor width  :initarg :width
		 :documentation "width of the view in pixels")
    (height      :accessor height :initarg :height
		 :documentation "height of the view in pixels")
    (backing-store :accessor backing-store :initarg :backing-store
		   :documentation "flag, whether backing-store is desired")
    
    ;; instance-variables
    (resize-callback  :accessor resize-callback  :initform nil)
    (destroy-callback :accessor destroy-callback :initform nil)
    (x-window      :accessor x-window :initform nil
		   :documentation "CLX-structure denoting the underlying window")
    (double-buffering :accessor double-buffering :initarg :double-buffering
                      :initform nil)
    (pixmap-buffer    :accessor pixmap-buffer)
    (background       :accessor background :initform nil)
    (drawable      :accessor drawable)
     
    (name-of-font  :accessor name-of-font :initform "9x15"
		   :documentation "name of default font for this view")
    (font          :accessor font
		   :documentation "default font for this view")
    (gcontext      :accessor gcontext     :initform nil
		   :documentation "CLX graphics context used for drawing primitives")
    (printing      :accessor printing     :initform nil
		   :documentation "flag, whether drawing is currently done for printing")
    (view-objects  :accessor view-objects :initform nil
		   :documentation "list of objects displayed in the view")
    (object-cache  :accessor object-cache :initform nil)
                   ;; reverse list of view objects for drawing
    (old-view-objects :accessor old-view-objects :initform nil)
                   ;; old view objects removed in taps to be reused
    (propagate-button-press
      :accessor propagate-button-press :initform t
      :documentation "flag, whether button-press is propagated to the view-objects")
    (repeated-click :accessor repeated-click :initform 1
		    :documentation "Number of very recent clicks")
    (last-click-time :accessor last-click-time :initform nil
		     :documentation "Internal real-time of last button-press event")
    ;; class variables
    (double-click-interval
      :accessor double-click-interval :allocation :class :initform 250
      :documentation "maximum time interval for double clicks in milliseconds")))

(defcallback view resize-callback  :resize  :hidden t)
(defcallback view destroy-callback :destroy :hidden t)

(defginafun make-view (parent "the parent widget object"
		  &key (width 1000) "the initial width of the view"
		       (height 1000) "the initial height of the view"
		       (document nil) 
		       "the document shown in the view"
		       (backing-store nil)
		       "flag, whether backing-store is desired"
		       (double-buffering nil)
		       "flag, whether all output is buffered in a pixmap"
		       (resize-policy :none) ":any :grow or :none"
		       ;; :none => size of view can be changed by resize at
		       ;; any time by the application  
		       (class 'view) "view or any subclass of it"
		       (initargs nil)
		       "further initializations for view subclass"
		       (managed t)
		       (motif-widget-class :gina-view)
		       (motif-resources nil) "further Motif resource settings"
		  &aux new-view)
  (description "create a new view where X graphics primitives can be used"
	       :constructor-for-class view
	       :called-by-application :sometimes)
  (when (not document) (error "document must be supplied when making views"))
  (setq new-view
	(make-widget parent
		     :class class
		     :initargs
		     (more-initargs :document document
				    :scroller (when (eq (class-of parent)
							(find-class 'scroller))
						parent)
				    :width width
				    :height height
				    :backing-store backing-store
				    :double-buffering double-buffering)
		     :managed managed
		     :motif-widget-class motif-widget-class
		     :motif-resources
		     (more-motif-resources :width width
					   :height height
					   :margin-height 0
					   :margin-width 0
					   :translations ""
					   :resize-policy resize-policy)))

  ;; make resize-callback call a method
  (setf (resize-callback new-view) (make-callback #'resized new-view))
  ;;(setf (destroy-callback new-view) (make-callback #'destroyed new-view))

  ;; tell the toolkit-server to send events
  (add-event-handler new-view :exposure-mask
		     (make-callback #'handle-expose new-view))
  (add-event-handler new-view :button-press-mask
		     (make-callback #'handle-button-press new-view))
  (add-event-handler new-view :button-release-mask
		     (make-callback #'handle-button-release document))
  (add-event-handler new-view :button-motion-mask
		     (make-callback #'handle-button-motion document))
  (add-event-handler new-view :key-press-mask
		     (make-callback #'handle-key-press new-view))
  (add-event-handler new-view :key-release-mask
		     (make-callback #'handle-key-release new-view))

  (setf (views document)
    (append (views document) (list new-view)))

  ;; register the view in its enclosing shell object
  (push new-view (views (enclosing-shell parent)))
  
  new-view)

(defginamethod determine-window-id ((view view))
  (description
    "ask for the window id of the underlying X-window"
    :called-by-gina "as soon as the widget has been realized"
    :override "adding an :after deamon to do initializations when the x-window is known"
    :default-version "opens a font and creates a graphic-context")
  (declare (special *application* *display*))
  (with-slots (widget-id x-window gcontext font name-of-font 
			 double-buffering pixmap-buffer backing-store
			 drawable width height background) view
    
    ;; ask toolkit server for underlying window
    (setq x-window (xtk:make-clx-window widget-id))
    (setq drawable x-window)
    
    ;; tell the X-Server to use backing store for window contents
    (when backing-store
      (setf (xlib:window-backing-store x-window) :when-mapped))

    ;; create a font
    (setq font (xlib:open-font *display* name-of-font))
    
    ;; create a graphics context for the view
    (setq gcontext (xlib:create-gcontext :drawable x-window
					 :line-width 0 ;; faster ???
					 :background (xlib:screen-white-pixel
						       (screen *application*))
					 :foreground (xlib:screen-black-pixel
						       (screen *application*))
                                         :clip-mask :none
					 :exposures :off
					 :font font))
    (when double-buffering
      ;; create pixmap equivalent to x-window
      (setq pixmap-buffer
	(xlib:create-pixmap :width width
			    :height height
			    :depth (xlib:drawable-depth x-window) 
			    :drawable x-window)))

    ;; compute background of drawing-area widget
    (setq background (first (get-motif-resources view :background))))
    )

(defginamethod xor-foreground ((view view) 
			   &optional 
			   (color-to-achieve 
			      (xlib:screen-black-pixel (screen *application*)))
                              "a pixel value")
  (description
    "determine color value usable as gcontext-foregound for xor drawing"
    :called-by-gina "before draw-feedback to set gc foreground"
    :called-by-application "to get feedback in a different color than black"
    :result "a pixel value that xors the view background to color-to-achieve") 
  (declare (special *application*))
  (the xlib:pixel (logxor (background view) color-to-achieve)))

(defginamethod destroyed ((view view) ignore) 
  (description "the view has been destroyed"
	       :override "to do the necessary cleanup"
	       :default-version "frees the gcontext")
  (declare (ignore ignore))
  
  (setf (x-window view) nil)
  (when (double-buffering view)
    (xlib:free-pixmap (pixmap-buffer view)))
  (xlib:free-gcontext (gcontext view)))

(defun handle-expose (view &rest event-data &aux rects mouse-command)
  "call GINA method in response to event"
  ;;(format t "~a~%" event-data)
  ;; get underlying window id at first expose event
  (when (not (x-window view))
    (determine-window-id view)
    (when (double-buffering view)
      ;; let application draw into pixmap
      (force-redraw view)) ;; => one superflous COPY-AREA !
    )

  ;; clear feedback if any because of autoscrolling
  (setq mouse-command (and (document view)
			   (active-mouse-command (document view))))
  (when (and mouse-command (mouse-already-moved mouse-command))
    (draw-feedback mouse-command 
		   (last-x mouse-command) (last-y mouse-command) :clear t)
    
    ;; clear rests of feedback in exposed area 
    (setq rects (cddr event-data))
    (loop repeat (second event-data)
	do
      (xlib:clear-area (x-window view) 
		       :x (first rects) :y (second rects)
		       :width (third rects) :height (fourth rects))
    (setq rects (cddddr rects))))

  ;; call draw-method for each rect in the event-data
  (setq rects (cddr event-data))
  ;; better call draw once for the enclosing rectangle ??
  (loop for count from (1- (second event-data)) downto 0
	do
    (if (double-buffering view)
	;; just copy an area from pixmap-buffer
	(xlib:copy-area (pixmap-buffer view) (gcontext view)
			(first rects) (second rects) 
			(third rects) (fourth rects)
			(x-window view) ;;destination
			(first rects) (second rects))
	;; tell application to draw
	(draw view (if (view-objects view) 0 count)
	      (first rects) (second rects) (third rects) (fourth rects)))
    (setq rects (cddddr rects)))

  ;; redraw feedback if any
  (when (and mouse-command (mouse-already-moved mouse-command))
    (draw-feedback mouse-command 
		   (last-x mouse-command) (last-y mouse-command) :clear nil)))

(defginamethod handle-button-press ((view view)
				    &rest event-data "a list of event details"
				    &aux code modifiers
				    virtual-code timestamp)
  (description "low level event handler for button-press events"
	       :called-by-gina "in response to button-press events"
	       :default-version "calls method button-press with the important parameters"
	       :override "if you are interested in special event details")
  (declare (special *application*))
  ;;(format t "~a~%" event-data)
  (setq modifiers (sixth event-data))
  (setq code (seventh event-data))
  (setq timestamp (eighth event-data))
  (when (or (not (document view))
	    (not (active-mouse-command (document view)))
	    (>= modifiers 256));; otherwise ignore button-press
    (with-slots (repeated-click last-click-time double-click-interval) view
      ;; compute virtual button code
      (setq virtual-code
	    (if (> modifiers 5)
		:other
		(progn
		  (case (+ (* code 10) modifiers)
		    (10 :select)
		    ((11 20) :extend)
		    ((15 30) :menu)
		    (14 :toggle)
		    (t :other)))))
         
      ;;(format t "Code: ~a Modifiers: ~a => ~s~%" code modifiers virtual-code)

      ;; check for double-, triple- ... click
      (if (and last-click-time
	       (< (- timestamp last-click-time) double-click-interval))
	  (incf repeated-click)
	  (setq repeated-click 1))
      (setq last-click-time timestamp)

      ;;(format t "~a " repeated-click)
  
      (button-press view virtual-code repeated-click
		    ;; x and y
		    (second event-data) (third event-data)))))

(defun handle-button-motion (document &rest event-data)
  ;;(format t "~a~%" event-data)
  (when (active-mouse-command document)
    (motion-notify (active-mouse-command document)
		   (third event-data) (fourth event-data))))

(defun handle-button-release (document &rest event-data &aux modifiers)
  ;;(format t "~a~%" event-data)
  (setq modifiers (sixth event-data))
  (when (and (active-mouse-command document)
	     (>= modifiers 256)
	     (<= modifiers 1279)
	     (not (and (> modifiers 767) (< modifiers 1024))))
    ;; last button released
    (button-release (active-mouse-command document)
                    (second event-data) (third event-data))))


(defmethod handle-key-press ((view view) &rest event-data)
  "call GINA method in response to event"
  ;;(format t "Key-press   ~s~%" event-data)
  (key-press view
	     (eighth event-data)  ;; char
	     (seventh event-data) ;; code
	     (sixth event-data)   ;; modifiers
	     (second event-data)  ;;  x
	     (third event-data)   ;; y
	     ))

(defmethod handle-key-release ((view view) &rest event-data)
  "call GINA method in response to event"
  ;;(format t "Key-release ~s~%" event-data)
  (key-release view
	       (eighth event-data)  ;; char
	       (seventh event-data) ;; code
	       (sixth event-data)   ;; modifiers
	       (second event-data)  ;; x
	       (third event-data)   ;; y
	       ))

(defginamethod button-press :around ((view view)
			     code "which mouse button"
			     repetition "1,2,3,.. for single, double, triple click"
			     x "relative mouse position in the view"
			     y "relative mouse position in the view"
			     &aux hit)
  (description "propagate button-press to hit view-object if any")
  (comment "This deamon determines whether button-press is to be called at the view"
	   "or if it is called at an object installed in the view.")
			  
  (with-slots (view-objects propagate-button-press) view
      (setq hit (and propagate-button-press
		     (loop for view-object in view-objects
			   do (when (and (mouse-sensitive view-object)
					 (point-inside view-object x y))
				(button-press view-object code repetition
					      (- x (x-pos view-object))
					      (- y (y-pos view-object)))
                                (when (> repetition 1)
                                  (double-clicked view-object code repetition
                                              (- x (x-pos view-object))
                                              (- y (y-pos view-object))))

				(return t)))))
    (when (not hit)
      ;; call button-press method of class view
      (call-next-method))))

(defginamethod button-press ((view view)
			     code "which mouse button"
			     repetition "1,2,3,.. for single, double, triple click"
			     x "relative mouse position in the view"
			     y "relative mouse position in the view")
  (description "a button was pressed inside the view"
	       :called-by-gina "in response to button-press events"
	       :default-version "selects objects installed in the view"
	       :override "to define the desired reaction to mouse clicks")
  (comment "Code :select denotes the left mouse button."
	   "Code :extend denotes the middle mouse button (or Shift-left)."
	   "Code :menu denotes the right mouse button (or Shift-Control-left)."
	   "Code :toggle denotes ctrl-left"
	   "Code :other denotes any other combination")
  (declare (ignore repetition))
  ;;(format t "~a ~a ~a ~a " code repetition x y)
  (when (member code '(:select :extend :toggle))
    (make-object-selector view x y code)))

(defmethod inspect-clicked ((view view) &rest parm-list &aux hit)
  "propagate inspect-clicks to the view-objects"
  ;; called by gina whenever a view is clicked, if *inspect-click* flag enabled
  ;; parm-list: type x y root-x root-y state button
  (when (= 1028 (nth 5 parm-list)) ;; Control-right button
    (setq hit (loop for view-object in (view-objects view)
		    do (when (point-inside view-object (second parm-list) (third parm-list))
			 (inspect-dialog view-object)
			 (return t))))
    (when (not hit)
      ;; inspect the view itself
      (inspect-dialog view))))

;; ebenso mit around methode an focus propagieren
(defginamethod key-press ((view view)
			    char "the key character as a string"
			    code "integer identifying  key"
			    modifiers "indicator for shift, ..."
			    x "mouse positioin in view when key was pressed"
			    y "mouse positioin in view when key was pressed")
  (description "a key was pressed inside the view"
	       :called-by-gina "in response to key-press events"
	       :default-version :do-nothing
	       :override "to define the desired reaction to key input")
  (declare (ignore modifiers code char x y))
;  (format t "Key-press   (code: ~s char: ~s modifiers: ~a x: ~s y: ~s)~%"
;	  code char modifiers x y)
  nil)

(defginamethod key-release ((view view)
			    char "the key character as a string"
			    code "integer identifying  key"
			    modifiers "indicator for shift, ..."
			    x "mouse positioin in view when key was pressed"
			    y "mouse positioin in view when key was pressed")
  (description "a key was released in the view"
	       :called-by-gina "in response to key-release events"
	       :default-version :do-nothing
	       :override "to define the desired reaction to key input")
  (declare (ignore code char modifiers x y))
  ;;(format t "Key-release (code: ~s char: ~s x: ~s y: ~s)~%" code char x y)
  nil)

(defginamethod force-redraw ((view view))
  (description "clear and redraw the whole view"
	       :called-by-application
	       "to reflect changes of the internal representation in the display")
  ;; ?? activate :before deamon to ask toolkit server for width and height ??
  (with-slots (x-window width height double-buffering 
			drawable pixmap-buffer gcontext) view
    (when x-window
      (when (not double-buffering)
	(xlib:clear-area x-window)
	(draw view 0 0 0 width height))
      
      (when double-buffering 
	(unwind-protect 
	    (progn
	      (setq drawable pixmap-buffer)
	      (clear-area view 0 0 width height)
	      (draw view 0 0 0 width height)
	      (xlib:copy-area pixmap-buffer gcontext
			      0 0 width height
			      x-window 0 0))
	  ;; cleanup: reset drawable 
	  (setq drawable x-window)
	  )))))

(defginamethod invalidate-rectangle ((view view) x y width height)
  (description "clear and redraw part of the view"
	       :called-by-application
	       "to reflect changes of the internal representation in the display") 
  (with-slots (x-window double-buffering pixmap-buffer drawable gcontext) view
    (when x-window
      (when (not double-buffering)
	(xlib:clear-area x-window :x x :y y :width width :height height) 
	(draw view 0 x y width height))

      (when double-buffering
	(unwind-protect 
	    (progn
	      (setq drawable pixmap-buffer)
	      (clear-area view x y width height)
	      (draw view 0 x y width height)
	      (xlib:copy-area pixmap-buffer gcontext
			      x y width height
			      x-window x y))
	  ;; cleanup: reset drawable 
	  (setq drawable x-window)
	  )))))
	  
(defginamethod clear-area ((view view) x y width height)
  (description "turn area into background color"
	       :called-by-application :sometimes)
  (if (double-buffering view)
      ;; clear-area not defined for pixmaps!!
      (xlib:with-gcontext 
	   ((gcontext view) 
	    :foreground (background view))
	   (draw-rectangle view x y width height t))
    (xlib:clear-area (x-window view) :x x :y y :width width :height height)))

(defginamethod draw ((view view)
		     count "number of calls to draw method that will immediately follow"
		     x "upper left corner of rectangle which has to be drawn"
		     y "upper left corner of rectangle which has to be drawn"
		     width "size of rectangle which has to be drawn"
		     height "size of rectangle which has to be drawn")
  (description "draw view contents from the internal representation"
	       :called-by-gina "in response to expose events"
	       :default-version :do-nothing
	       :override "to define the graphical representation of your document")
  (comment "You can simply draw the whole view when count is 0."
	   "Optimized versions just draw the necessary parts.")
  (declare (ignore count x y width height))
  nil)

(defginamethod draw :around ((view view) count x y width height)
  (description "draw all view-objects in the view after drawing directly in the view")
   (declare (ignore count))

   ;;(format t "draw ~a ~a ~a ~a ~a~%" count x y width height)
   (when (view-objects view)
     ;; restrict all drawing to affected area
     (setf (xlib:gcontext-clip-mask (gcontext view)) (list x y width height)))
   
   (call-next-method) ;; .. which is typically overridden in each application

   ;; repeat for the case that main method used clipping
   (when (view-objects view)
     (setf (xlib:gcontext-clip-mask (gcontext view)) (list x y width height)))

   ;; build a reverse list of view objects for correct drawing order
   (unless (object-cache view)
     (setf (object-cache view) (reverse (view-objects view))))
   (multiple-value-bind (candidates refresh-all)
       (get-refresh-candidates view (object-cache view) x y width height) 
     ;; this version draws all relevant objects at each expose event
     (loop for view-object in candidates
      when (or refresh-all 
	       (inside-rectangle view-object x y width height))
        do ;;(unless object-found
                ;;    ;; set a clipping region to prevent wrong stacking order
                ;;    ;; do not use with-gcontext! Does not restore :none state
                ;;    (setf (xlib:gcontext-clip-mask (gcontext view)) 
                ;;          (list x y width height))
                ;;    (setq object-found t))
           (draw view-object 0 0 0 (width view-object)
		 (height view-object))
           (when (selected view-object)
	     (draw-selected view-object 0 0 0 
			    (width view-object) (height view-object)))))
   ;; restore gc
   (when (view-objects view)
     (setf (xlib:gcontext-clip-mask (gcontext view)) :none)))

(defginamethod get-refresh-candidates ((view view) objects x y width height)
    (description 
        "compute list of objects which may lie within invalidated rectangle"
	:override "to optimize redraw of view-objects using application-specific information"
	:default-version "simply returns all objects"
	:called-by-gina "to determine objects to redraw"
	:result "superset of candidate objects and flag whether set is exact, as multiple values")
  (declare (ignore x y width height))
  (values objects nil))
		 
(defginamethod (setf view-objects) :after (new-list (view view))
  (description "clear cached reverse list of view-objects")
  (declare (ignore new-list))
  (setf (object-cache view) nil))

(defginamethod resized ((view view) ignore) 
  (description "the user has resized the view"
	       :override "to react when the view is resized by the user"
	       :default-version "stores the new size and redraws the view")
  (declare (ignore ignore))
  ;; ask toolkit for new size
  (update-slots view)
  (with-slots (double-buffering pixmap-buffer width height x-window) view
    (when double-buffering
      (when (or (not (and (boundp 'pixmap-buffer) pixmap-buffer))
		 (or (< (xlib:drawable-width pixmap-buffer) width)
		     (< (xlib:drawable-height pixmap-buffer) height)))
	(when (and (boundp 'pixmap-buffer) pixmap-buffer)
	  (xlib:free-pixmap pixmap-buffer))
	(when (null (ignore-errors
		     (setq pixmap-buffer
		       (xlib:create-pixmap :width width :height height
				 :depth (xlib:drawable-depth x-window)
				 :drawable x-window))
		     (xlib:display-finish-output *display*)
		     t))
	  (setq pixmap-buffer nil)
	  (setq double-buffering nil)))
      (force-redraw view)
      double-buffering)))

(defginamethod selected-objects ((view view))
  (description "return all selected objects")
  (loop for obj in (view-objects view)
        when (selected obj)
        collect obj))

(defginamethod (setf selected-objects) (new-list (view view))
  (description "set the selection according to new value")
  (loop for obj in (view-objects view)
      for new-state = (member obj new-list)
      do (when (and (selected obj) (not new-state))
	   (setf (selected obj) nil))
	 (when (and new-state (not (selected obj)))
	   (setf (selected obj) t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; experimental bitmap printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; not yet implemented

;;(defun print-gc (gcontext)
;;  "print important attributes of gcontext"
;;  ;; just to show how to find relevant information for Postscript generation
;;  (format t "Gcontext(font=~s,function=~s,line-width=~s, line-style=~s)~%"
;;	  (xlib:font-name (xlib:gcontext-font gcontext))
;;	  (xlib:gcontext-function gcontext)
;;	  (xlib:gcontext-line-width gcontext)
;;	  (xlib:gcontext-line-style gcontext)))

(defvar *page-width*)
(setq *page-width* 500)
(defvar *page-height*)
(setq *page-height* 700)

(defmethod draw-into-pixmap ((view view)
			     &aux view-pixmap page-pixmap pixmap-id old-x-window)
  "create a pixmap and draw into it instead of x-window for printing"
  (with-slots (x-window gcontext width height document) view
    (unwind-protect
	(progn
	  ;; create pixmap equivalent to x-window
	  (setq view-pixmap (xlib:create-pixmap :width width
						:height height
						:depth (xlib:drawable-depth x-window) 
						:drawable x-window))
	  
	  ;; redraw view into the pixmap
	  (setq old-x-window x-window)
	  (setq x-window view-pixmap)
	  (draw view 0 0 0 width height)
	  (xlib:display-force-output *display*)
          (setq x-window old-x-window)

	  ;; create pixmap for one printer page
	  (setq page-pixmap (xlib:create-pixmap :width *page-width*
						:height *page-height*
						:depth (xlib:drawable-depth x-window) 
						:drawable x-window))
	  (setq pixmap-id (xlib:pixmap-id page-pixmap))

	  ;; loop through necessary printer pages
	  (loop for x-origin from 0 to (1- width) by *page-width* do
	    (loop for y-origin from 0 to (1- height) by *page-height* do
	      ;; clear page-pixmap
	      (xlib:with-gcontext (gcontext :foreground 0)
		(xlib:draw-rectangle page-pixmap gcontext 0 0 *page-width* *page-height* t))
	      ;; copy one page from view-pixmap into page-pixmap
	      (xlib:copy-area view-pixmap gcontext x-origin y-origin
			      ;; full page-width or rest
			      (min (- width x-origin) *page-width*)
			      ;; full page-height or rest
			      (min (- height y-origin) *page-height*)
			      page-pixmap 0 0)
	      (xlib:display-force-output *display*)
	      
	      ;; issue UNIX command to print the pixmap
	      (shell-command 
	       (format nil "/home/spenke/printing/print-window-id 0 ~d 4" 
		       pixmap-id))
	      )))
      ;; cleanup
      (xlib:free-pixmap view-pixmap)
      (xlib:free-pixmap page-pixmap))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; graphic primitives for drawing into a view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ginasection "Graphic Primitives for Drawing into a View")

(defginamethod draw-point ((view view) x y)
  (description "call CLX Function to draw into the view"
	       :called-by-gina nil
	       :called-by-application "(typically within the draw method of a view)")
  (comment "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	     (draw-point (main-view (first (document-list *application*)))
			 10 10)))
  
  (when (not (printing view))
      (xlib:draw-point (drawable view) (gcontext view) x y)))

(defginamethod draw-points ((view view)
			    points "a flat list of alternating x and y values"
			    &optional relative-p)
  (description "call CLX Function to draw into the view"
	       :called-by-gina nil
	       :called-by-application "(typically within the draw method of a view)")
  (comment 
   "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	     (draw-points (main-view (first (document-list *application*)))
			 '(10 10 20 20 30 30)))
	   "draws 3 points")
  (when (not (printing view))
    (xlib:draw-points (drawable view) (gcontext view) points relative-p)))

(defginamethod draw-line ((view view) x1 y1 x2 y2 &optional (relative-p nil))
  (description "call CLX Function to draw into the view"
	       :called-by-gina nil
	       :called-by-application "(typically within the draw method of a view)")
  (comment 
   "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	     (draw-line (main-view (first (document-list *application*)))
			 10 10 50 50)))
 
  (when (not (printing view))
    (xlib:draw-line (drawable view) (gcontext view) x1 y1 x2 y2 relative-p)))

(defginamethod draw-lines ((view view)
			   points "a flat list of alternating x and y values"
			   &key relative-p fill-p (shape :complex))
  (description "call CLX Function to draw into the view"
	       :called-by-gina nil
	       :called-by-application "(typically within the draw method of a view)")
  (comment "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	     (draw-lines (main-view (first (document-list *application*)))
			 '(10 10 50 50 40 30 70 60) :fill-p t)))
 
  (when (not (printing view))
    (xlib:draw-lines (drawable view) (gcontext view) points
		     :relative-p relative-p
		     :fill-p fill-p
		     :shape shape)))

(defginamethod draw-segments ((view view) segments)
  (description "call CLX Function to draw into the view"
	       :called-by-gina nil
	       :called-by-application 
	       "(typically within the draw method of a view)")
  (comment 
   "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	     (draw-segments (main-view (first (document-list *application*)))
			 '(10 10 50 50 40 30 70 60))))
 
  (when (not (printing view))
    (xlib:draw-segments (drawable view) (gcontext view) segments)))
  
(defginamethod draw-rectangle ((view view) x y width height &optional (fill-p nil))
  (description "call CLX Function to draw into the view"
	       :called-by-gina nil
	       :called-by-application 
	       "(typically within the draw method of a view)")
  (comment 
   "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	     (draw-rectangle (main-view (first (document-list *application*)))
			 10 10 50 50 t)))
 
  (when (not (printing view))
    (xlib:draw-rectangle (drawable view) (gcontext view) 
			 x y width height fill-p)))

(defginamethod draw-rectangles ((view view) rectangles &optional (fill-p nil))
  (description "call CLX Function to draw into the view"
	       :called-by-gina nil
	       :called-by-application 
	       "(typically within the draw method of a view)")
  (comment 
   "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	     (draw-rectangles (main-view (first (document-list *application*)))
			    '(10 10 50 50 40 30 70 60)))
	   "draws two rectangles")
 
  (when (not (printing view))
    (xlib:draw-rectangles (drawable view) (gcontext view) rectangles fill-p)))

(defginamethod draw-arc ((view view) x y width height angle1 angle2 
				     &optional (fill-p nil))
  (description "call CLX Function to draw into the view"
	       :called-by-gina nil
	       :called-by-application 
	       "(typically within the draw method of a view)")
  (comment 
   "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	     (draw-arc (main-view (first (document-list *application*)))
			 10 10 50 50 0 (* 2 pi) t))
	   "draws a filled circle")

  (when (not (printing view))
    (xlib:draw-arc (drawable view) (gcontext view) 
		   x y width height angle1 angle2 fill-p)))

(defginamethod draw-arcs ((view view) arcs &optional (fill-p nil))
  (description "call CLX Function to draw into the view"
	       :called-by-gina nil
	       :called-by-application 
	       "(typically within the draw method of a view)")
  (comment 
   "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	     (draw-arcs (main-view (first (document-list *application*)))
			 `(10 10 50 50 0 ,(* 2 pi) 70 70 50 50 0 ,pi) t))
	   "draws one and a half filled circles")
  (when (not (printing view))
    (xlib:draw-arcs (drawable view) (gcontext view) arcs fill-p)))

(defginamethod draw-glyph ((view view)  x y elt
		       &key translate width (size :default))
  (description "call CLX Function to draw into the view"
	       :called-by-gina nil
	       :called-by-application 
	       "(typically within the draw method of a view)")
  (comment 
   "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	     (draw-glyph (main-view (first (document-list *application*)))
			 10 20 #\a))
	   "writes out one character")
  (when (not (printing view))
      (xlib:draw-glyph (drawable view) (gcontext view)
		       x y elt
		       :translate translate
		       :width width
		       :size size)))

(defginamethod draw-glyphs ((view view)  x y sequence
			&key (start 0) end translate width (size :default))
  (description "call CLX Function to draw into the view"
	       :called-by-gina nil
	       :called-by-application "(typically within the draw method of a view)")
  (comment "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	     (draw-glyphs (main-view (first (document-list *application*)))
			 10 20 "Hello World"))
	   "writes out a string")
  (when (not (printing view))
    (xlib:draw-glyphs (drawable view) (gcontext view)
		      x y sequence
		      :start start :end end :translate translate 
		      :width width :size size)))

(defginamethod copy-plane ((view view) pixmap depth source-x source-y
				       source-width source-height 
				       dest-x dest-y)
  (description "call CLX Function to draw into the view"
	       :called-by-gina nil
	       :called-by-application 
	       "(typically within the draw method of a view)")
  (comment 
   "This method directly corresponds to the CLX function with the same name.")
  (when (not (printing view))
    (xlib:copy-plane pixmap 
		     (gcontext view)
		     depth
		     source-x source-y
		     source-width source-height
		     (drawable view)
		     dest-x dest-y)))

(defginamethod copy-area ((view view) pixmap source-x source-y
				       source-width source-height 
				       dest-x dest-y)
  (description "call CLX Function to draw into the view"
	       :called-by-gina nil
	       :called-by-application 
	       "(typically within the draw method of a view)")
  (comment 
   "This method directly corresponds to the CLX function with the same name.")
  (when (not (printing view))
    (xlib:copy-area pixmap 
		    (gcontext view)
		    source-x source-y
		    source-width source-height
		    (drawable view)
		    dest-x dest-y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class view-object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass view-object ()
   (description "the superclass of all objects shown in views"
		:application-subclasses :always
		:gina-subclasses "objects which can be moved and resized with the mouse"
		:instantiate "for testing only")
  (;; instance-parameters
   (width           :accessor width           :initarg :width
       :documentation "extension of enclosing rectangle in pixels")
   (height          :accessor height          :initarg :height
       :documentation "extension of enclosing rectangle in pixels")
   (mouse-sensitive :accessor mouse-sensitive :initarg :mouse-sensitive
       :documentation "whether button-press is to be propagated to this object")

   ;; instance-variables
   (parent-view     :accessor parent-view :initform nil
       :documentation "the view containing the object")
   (x-pos           :accessor x-pos :initform nil
       :documentation "the x-coordinate relative to the containing view")
   (y-pos           :accessor y-pos :initform nil
       :documentation "the y-coordinate relative to the containing view")
   (facilities      :accessor facilities  :initform nil  :allocation :class
       :documentation
	"one-of nil, :selectable, :movable, :move-and-selectable, :resizable")
   (selected        :reader   selected    :initform nil
       :documentation "flag whether object is currently selected")
   (outlines        :accessor outlines    :initform t  :allocation :class
       :documentation "flag whether complete object is moved or just an outline")
   (handle-size     :accessor handle-size :initform 5  :allocation :class
       :documentation "size of little rectangles indicating selected state")))

(defginafun make-view-object
	    (width "extension of enclosing rectangle in pixels"
	     height "extension of enclosing rectangle in pixels"
	     &key
	     (mouse-sensitive t)
	     "flag whether button-press is to be propagted to this object"
	     (class 'view-object) "'view-object or any subclass"
	     (initargs nil) "further initializations for view-object subclass")
  (description "create a graphical object within a view"
	       :constructor-for-class view-object
	       :called-by-gina nil
	       :called-by-application :sometimes)
  (comment "View-objects can exist without being installed in any view.")
  (make-object class initargs
	       :width width :height height
	       :mouse-sensitive mouse-sensitive))

(defginamethod install ((view-object view-object) "the object to install"
			(view view) "the view where it is installed"
			&optional
			(x 0) "position in the view where object is installed"
			(y 0) "position in the view where object is installed"
			&key (redraw t)
		       "flag whether effect is immediately shown on the screen")
  (description "install a view-object in a view"
	       :called-by-gina nil
	       :called-by-application :sometimes)
  (example (with-application-stopped
	     (install (make-view-object 100 100)
		      (main-view (first (document-list *application*)))
		      10 20))
	   "installs a view object into the view")
	       
  (with-slots (parent-view x-pos y-pos) view-object
    (setq parent-view view)
    (setq x-pos x y-pos y))

  (with-slots (view-objects) view
    (push view-object view-objects))
  (setf (object-cache view) nil)
  (install-taps view-object)
  (when redraw
    (force-redraw view-object)))

(defginamethod deinstall ((view-object view-object) "the object to deinstall"
			  &key (redraw t)
			  "flag whether effect is immediately shown on the screen")
  (description "remove view-object from the list of view-objects of its parent-view"
	       :called-by-gina nil :called-by-application :sometimes)
  (example (with-application-stopped
	     (deinstall (first (view-objects (main-view
					       (first (document-list *application*))))))))
	       
  (with-slots (parent-view x-pos y-pos width height) view-object
    (with-slots (view-objects) parent-view
      (setq view-objects (delete view-object view-objects))
      (setf (object-cache parent-view) nil)
      (deinstall-taps view-object)
      (when redraw
	(invalidate-rectangle parent-view x-pos y-pos (1+ width) (1+ height)))
      (setq parent-view nil))))

(defginamethod install-taps ((view-object view-object))
  (description "install any taps for a view-object"
	       :called-by-gina "when the object is installed in a view"
	       :override "to define taps for this object"
	       :default-version :do-nothing))

(defginamethod deinstall-taps ((view-object view-object))
  (description "deinstall all taps"
	       :called-by-gina "when the object is deinstalled"
	       :default-version "calls remove-all-taps")
  (remove-all-taps (enclosing-shell view-object) view-object))

(defmethod enclosing-shell ((obj view-object))
  "determine the next higher shell if installed in a view"
  (and (parent-view obj)
       (enclosing-shell (parent-view obj))))

'(defginamacro point-inside-rectangle 
  (x y rect-x rect-y rect-width rect-height)
   (description "check if a given point lies within a certain rectangle")
   `(and (>= ,x ,rect-x) (>= ,y ,rect-y)
	 (<= ,x (+ ,rect-x ,rect-width)) (<= ,y (+ ,rect-y ,rect-height))))
'(point-inside-rectangle 15 15 10 10 20 20)

(defginamethod point-inside ((view-object view-object) x y)
  (description "check if given point in view coordinates is inside the object"
	       :called-by-application :sometimes
	       :override "for non-rectangular objects")
  ;; this default method works for rectangular objects only
  (with-slots (x-pos y-pos width height) view-object
    (point-inside-rectangle x y x-pos y-pos width height)))

(defginamethod inside-rectangle ((view-object view-object)
				 rect-x rect-y rect-width rect-height)
  (description "check if object intersects with the rectangle"
	       :called-by-application :sometimes
	       :override "for non-rectangular objects")
  (with-slots (x-pos y-pos width height) view-object
    (not (or (< (+ rect-y rect-height) y-pos) ;; rect-bottom above object-top
	     (> rect-y (+ y-pos height))      ;; rect-left right of object-right
	     (< (+ rect-x rect-width) x-pos)  ;; rect-right left of object-left
	     (> rect-x (+ x-pos width))       ;; rect-top below object-right
	     ))))

(defginafun rectangles-overlap (x1 y1 width1 height1 x2 y2 width2 height2)
   (description "check if two given rectangles overlap")
   (not (or (< (+ y1 height1) y2) ;; bottom1 above top2
	    (> y1 (+ y2 height2)) ;; top1 below bottom2
	    (< (+ x1 width1) x2)  ;; right1 left-of left2
	    (> x1 (+ x2 width2))  ;; left1 right of right2
	    )))

;; test with two rectangles in graphic-editor:
'(with-application-stopped 
  (let* ((view (main-view (first (document-list *application*))))
	 (r1 (first (view-objects view)))
	 (r2 (second (view-objects view))))
    (print (rectangles-overlap 
	    (x-pos r1) (y-pos r1) (width r1) (height r1)
	    (x-pos r2) (y-pos r2) (width r2) (height r2)))))

(defginamethod move ((view-object view-object)
		     new-x "desired position within the view"
		     new-y "desired position within the view"
		     &key (redraw t)
		     "flag whether effect is immediately shown on the screen"
		     &allow-other-keys
		     &aux old-x old-y)
  (description "change x and y coordinates"
	       :called-by-application :sometimes)
  (with-slots (x-pos y-pos width height parent-view) view-object
    (setq old-x x-pos old-y y-pos)
    (setq x-pos new-x y-pos new-y)
    (when (and redraw parent-view (x-window parent-view))
      (if (rectangles-overlap old-x old-y (1+ width) (1+ height)
			      new-x new-y (1+ width) (1+ height))
	  ;; redraw enclosing rectangle of new and old position
	  (invalidate-rectangle parent-view 
				(min old-x new-x) (min old-y new-y)
				(+ width 1  (abs (- old-x new-x)))
				(+ height 1 (abs (- old-y new-y))))

	;; else: redraw old and new area separately
	(progn
	  ;; redraw area formerly covered by object
	  (invalidate-rectangle parent-view old-x old-y 
				(1+ width) (1+ height))
	  ;; draw object and overlapping parts at new position
	  (invalidate-rectangle parent-view new-x new-y 
				(1+ width) (1+ height)))))
      ))

(defginamethod resize ((view-object view-object)
		       new-width "desired size"
		       new-height "desired size"
		       &key (redraw t)
		       "flag whether effect is immediately shown on the screen"
		       &allow-other-keys
		       &aux old-width old-height)
  (description "change width and height"
	       :called-by-application :sometimes)
  (with-slots (x-pos y-pos width height parent-view) view-object
    (setq old-width width old-height height)
    (setq width new-width height new-height)
    (when (and redraw parent-view (x-window parent-view))
      (invalidate-rectangle parent-view x-pos y-pos
			    (1+ (max new-width old-width)) (1+ (max new-height old-height))))
    ))

(defginamethod reconfigure ((view-object view-object)
		            new-x "desired position within the view"
		            new-y "desired position within the view"
                            new-width "desired size"
                            new-height "desired size"
		       &key (redraw t)
		              "flag whether effect is immediately shown"
		       &allow-other-keys
		       &aux old-x old-y old-width old-height)
  (description "change position and size"
	       :called-by-application :sometimes)
  (with-slots (x-pos y-pos width height parent-view) view-object
     (setq old-x x-pos old-y y-pos old-width width old-height height)
     (setq x-pos new-x y-pos new-y width new-width height new-height)
     (when (and redraw parent-view (x-window parent-view))
       (invalidate-rectangle parent-view
           (min old-x new-x) (min old-y new-y)
           (1+ (- (max (+ old-x old-width) (+ new-x new-width)) 
                  (min old-x new-x)))
           (1+ (- (max (+ old-y old-height) (+ new-y new-height))
                  (min old-y new-y)))))))

(defginamethod invalidate-rectangle ((view-object view-object) x y width height)
  (description "redraw part of the view-object"
	       :called-by-application :sometimes)
	       
  (with-slots (x-pos y-pos parent-view) view-object
    (when (and parent-view (x-window parent-view))
      (invalidate-rectangle parent-view (+ x-pos x) (+ y-pos y) width height))))

(defginamethod force-redraw ((view-object view-object))
  (description "redraw the whole view-object"
	       :called-by-application :sometimes)
  (with-slots (x-pos y-pos width height parent-view) view-object
    (when (and parent-view (x-window parent-view))
      (invalidate-rectangle parent-view x-pos y-pos (1+ width) (1+ height)))))

(defginamethod draw ((view-object view-object) 
		     count "number of calls to draw method that will immediately follow"
		     x "upper left corner of rectangle which has to be drawn"
		     y "upper left corner of rectangle which has to be drawn"
		     width "size of rectangle which has to be drawn"
		     height "size of rectangle which has to be drawn")
  (description "draw view-object contents from the internal representation"
	       :called-by-gina "in response to expose events"
	       :default-version "draws a rectangle"
	       :override "to define the graphical representation of your view-object")
  (comment "You can simply draw the whole view-object when count is 0."
	   "Optimized versions just draw the necessary parts.")
  (declare (ignore x y width height))
  (with-slots (x-pos y-pos width height parent-view) view-object
    (when (zerop count) ;; Ignore all but the last exposure event
      (draw-rectangle parent-view x-pos y-pos width height))))

(defginamethod button-press ((obj view-object)
			     code "which mouse button"
			     repetition "1,2,3,.. for single, double, triple click"
			     x "relative mouse position in the view-object"
			     y "relative mouse position in the view-object"
			     &aux which-handle)
  (description "react to button-press event in the object"
	       :called-by-gina "if a button is pressed inside the object"
	       :default-version "moves, selects or resizes the object"
	       :override "to define the desired reaction to mouse clicks")
  (comment "Code :select denotes the left   mouse button."
	   "Code :extend denotes the middle mouse button (or Shift-left)."
	   "Code :menu denotes the right  mouse button (or Shift-Control-left)."
	   "Code :toggle denotes ctrl-left"
	   "Code :other denotes any other combination")
  
  (declare (special *display*))

  (when (= repetition 1)
    (case (facilities obj)
      ((:selectable)
           (make-single-selector obj x y code))
      ((:movable)             ;; make mover only
           (make-object-mover obj x y))
      ((:move-and-selectable)
           (let ((old-selection (selected-objects (parent-view obj))))
	     (handle-selection obj code)
	     (make-object-mover obj x y :old-selection old-selection)))
      ((:resizable)           ;; make mover or resizer, depending on handle
           (setq which-handle (which-resize-handle obj x y))
           (if (zerop which-handle)
               (let ((old-selection (selected-objects (parent-view obj))))
                 (handle-selection obj code)
	         (make-object-mover obj x y :old-selection old-selection))
	       (create-handle-command obj code x y which-handle)))
      (t
           (xlib:bell *display*)))))

(defginamethod double-clicked ((obj view-object)
			     code "which mouse button"
			     repetition "2,3,.. for double, triple click"
			     x "relative mouse position in the view-object"
			     y "relative mouse position in the view-object")
  (description "a button was double clicked in the view-object"
	       :called-by-gina "in response to multiple button-press events"
	       :default-version :do-nothing
	       :override "to define the desired reaction to double clicks")
  (comment "Code :select denotes the left   mouse button."
	   "Code :extend denotes the middle mouse button (or Shift-left)."
	   "Code :menu denotes the right  mouse button (or Shift-Control-left)."
	   "Code :toggle denotes ctrl-left"
	   "Code :other denotes any other combination" )
  (declare (ignore code  repetition x y)))

; Wird noch nicht propagiert
;(defginamethod key-press ((view-object view-object) code char x y root-x root-y)
;  "react to key-press event in the object"
;  (declare (ignore char code x y root-x root-y))
;  (declare (special *display*))
;  
;  ;; this default method just beeps
;  (xlib:bell *display*))

(defginamethod (setf selected) (new-value (obj view-object))
  (description "set slot selected and force redraw"
	       :called-by-application "to select an object from the program")	       
  (with-slots (selected) obj
    (when (not (eql selected new-value))
      (setf selected new-value)
      (force-redraw obj))))

(defginamethod draw-selected
	       ((obj view-object)
		count "number of calls to draw method that will immediately follow"
		x "upper left corner of rectangle which has to be drawn"
		y "upper left corner of rectangle which has to be drawn"
		width "size of rectangle which has to be drawn"
		height "size of rectangle which has to be drawn"
		&aux short-width short-height)
  (description "draw something to indicate that the object is currently selected"
	       :called-by-gina "in response to expose events if object is selected"
	       :default-version "draws 4 selection handles"
	       :overridden-by-gina "in class movable-icon"
	       :override "indicate selected state in a different way")
  (comment "You can simply draw the whole object as selected when count is 0."
	   "Optimized versions just draw the necessary parts.")

  (declare (ignore x y width height))
  (when (zerop count) ;; Ignore all but the last exposure event
    (with-slots (width height parent-view handle-size) obj
      (setq short-width  (1+ (- width  handle-size)))
      (setq short-height (1+ (- height handle-size))) 
      (draw-rectangle obj short-width short-height handle-size handle-size t)
      (draw-rectangle obj 0 0  handle-size handle-size t)
      (draw-rectangle obj 0 short-height handle-size handle-size t)
      (draw-rectangle obj short-width 0 handle-size handle-size t))))

(defginamethod draw-outline ((obj view-object)
			     new-x "current position"
			     new-y "current position"
			     width "current size"
			     height "current size"
			     &key clear "flag whether outline is to be drawn or cleared")
  (description "draw outline of the object while being moved or resized"
	       :called-by-gina "during move and resize commands"
	       :default-version "draws a dashed rectangle"
	       :override "if a different form of feedback is desired")
  (comment 
   ":function boole-xor is set in gcontext before this method is called.")
  (declare (ignore clear))
  (with-slots (parent-view) obj
    (xlib:with-gcontext ((gcontext parent-view) :line-style :dash)
      (draw-rectangle parent-view new-x new-y width height))))

(defginamethod constrain-position ((obj view-object)
				x "desired object position"
				y "desired object position")
  (description
    "modify x and y position of object during move"
    :called-by-gina "while object is moved"
    :override "to constrain movement to certain locations in the view"
    :default-version "returns x and y unmodified"
    :result "x and y as multiple values")
  (values x y))

(defginamethod constrain-size ((obj view-object)
				width "desired object width"
				height "desired object height")
  (description
    "modify width and height of object during resize"
    :called-by-gina "while object is resized"
    :override "to constrain possible sizes"
    :default-version "returns width and height unmodified"
    :result "width and height as multiple values")
  (values width height))

(defginamethod which-resize-handle ((obj view-object) x y)
  (description "determine which resize handle is at point (x,y) if any"
	       :result "1 to 4 if handle hit (counterclock), else 0"
	       :called-by-gina "when the mouse is clicked into an object"
	       :override
	       "if the handles do not sit at the corners of the enclosing rectangle"
	       :default-version
	       "assumes that the handles sit at the corners of the enclosing rectangle")
  (with-slots (selected width height handle-size) obj
    (if selected
      (if (<= x handle-size)
        (if (<= y handle-size)
          1
          (if (<= (- height y) handle-size)
            2
            0))
        (if (<= (- width x) handle-size)
          (if (<= y handle-size)
            4
            (if (<= (- height y) handle-size)
              3
              0))
          0))
      0)))

(defginamethod reconfigured-by-user ((obj view-object)
                                     was-moved "if obj has been moved"
                                     was-resized "if object has been resized")
  (description "to react to move and resize commands for an object"
               :called-by-gina "during command execution"
               :override "if additional reaction is required"
               :default-version :do-nothing)
  (declare (ignore was-moved was-resized)))

(defginamethod create-handle-command ((obj view-object) code x y
                                      which-handle "the handle hit")
  (description "creates a command to react to dragging a handle"
               :called-by-gina "on button press in a handle"
               :override "if other than rectangle-resize operation is desired"
               :default-version "calls make-object-resizer")

  (declare (ignore code))
  (make-object-resizer obj x y which-handle))

(defginamethod handle-selection ((obj view-object)
                                 code "which mouse button")
  (description "perform selection action depending on button code"
               :called-by-gina "in button-press method if selectable"
               :override "for non-standard selection model"
               :called-by-application "in app-specific button-press method")
  (case code
    ((:select)
       (when (and (parent-view obj) (not (selected obj)))
          (loop for other in (view-objects (parent-view obj))
                when (member (facilities obj) 
                             '(:selectable :move-and-selectable :resizable))
                do (setf (selected other) (eq other obj)))))
    ((:toggle)
       (setf (selected obj) (not (selected obj))))
    ((:extend)
       (setf (selected obj) t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; graphic primitives for view-objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ginasection "Graphic Primitives for Drawing into View-Objects")

(defginamethod draw-point ((view-object view-object) x y)
  (description "call same method of the view, but take position into account"
	       :called-by-gina nil
	       :called-by-application "(typically within the draw method of a view-object)")
  (comment "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	     (draw-point (first (view-objects (main-view
						(first (document-list *application*)))))
			 10 10)))
  
  (with-slots (parent-view x-pos y-pos) view-object
    (draw-point parent-view (+ x-pos x) (+ y-pos y))))

(defginamethod draw-points ((view-object view-object) points &optional relative-p)
  (description "call same method of the view, but take position into account"
	       :called-by-gina nil
	       :called-by-application "(typically within the draw method of a view-object)")
  (comment "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	     (draw-points (first (view-objects (main-view
						(first (document-list *application*)))))
			 '(10 10 20 20 30 30)))
	   "draws 3 points")
  (with-slots (parent-view x-pos y-pos) view-object
    ;; modify points
    (setq points (loop for coordinate in points
		       for pos from 1
		       when (oddp pos)
			 collect (+ x-pos coordinate)
		       else collect (+ y-pos coordinate)))
    (draw-points parent-view points relative-p)))

(defginamethod draw-line ((view-object view-object) x1 y1 x2 y2 &optional (relative-p nil))
  (description "call same method of the view, but take position into account"
	       :called-by-gina nil
	       :called-by-application "(typically within the draw method of a view-object)")
  (comment "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	     (draw-line (first (view-objects (main-view
						(first (document-list *application*)))))
			 10 10 50 50)))
 
  (with-slots (parent-view x-pos y-pos) view-object
    (draw-line parent-view (+ x-pos x1) (+ y-pos y1) (+ x-pos x2) (+ y-pos y2) relative-p)))

(defginamethod draw-lines ((view-object view-object) points
		       &key relative-p fill-p (shape :complex))
  (description "call same method of the view, but take position into account"
	       :called-by-gina nil
	       :called-by-application "(typically within the draw method of a view-object)")
  (comment "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	     (draw-lines (first (view-objects (main-view
						(first (document-list *application*)))))
			 '(10 10 50 50 40 30 70 60) :fill-p t)))
 
  (with-slots (parent-view x-pos y-pos) view-object
    ;; modify points
    (if relative-p
        (setq points (cons (+ x-pos (first points))
                           (cons (+ y-pos (second points))
                                 (cddr points))))
        (setq points (loop for coordinate in points
		           for pos from 1
		           when (oddp pos)
			     collect (+ x-pos coordinate)
		           else collect (+ y-pos coordinate))))
    (draw-lines parent-view points
		:relative-p relative-p
		:fill-p fill-p
		:shape shape)))

(defginamethod draw-segments ((view-object view-object) segments)
  (description "call same method of the view, but take position into account"
	       :called-by-gina nil
	       :called-by-application "(typically within the draw method of a view-object)")
  (comment "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	     (draw-segments (first (view-objects (main-view
						(first (document-list *application*)))))
			 '(10 10 50 50 40 30 70 60))))
 
  (with-slots (parent-view x-pos y-pos) view-object
    ;; modify segments
    (setq segments (loop for coordinate in segments
			 for pos from 1
			 when (oddp pos)
			   collect (+ x-pos coordinate)
			 else collect (+ y-pos coordinate)))
    (draw-segments parent-view segments)))

(defginamethod clear-area ((view-object view-object) x y width height)
  (description 
   "call same method of the view, but take position into account"
   :called-by-gina nil
   :called-by-application 
       "(typically within the draw method of a view-object)")
  (comment 
   "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	    (clear-area (first (view-objects (main-view
			    (first (document-list *application*)))))
			 10 10 50 50)))
 
  (with-slots (parent-view x-pos y-pos) view-object
    (clear-area parent-view (+ x-pos x) (+ y-pos y) width height)))

(defginamethod draw-rectangle ((view-object view-object) x y width height
			       &optional (fill-p nil))
  (description "call same method of the view, but take position into account"
	       :called-by-gina nil
	       :called-by-application "(typically within the draw method of a view-object)")
  (comment "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	     (draw-rectangle (first (view-objects (main-view
						    (first (document-list *application*)))))
			 10 10 50 50 t)))
 
  (with-slots (parent-view x-pos y-pos) view-object
    (draw-rectangle parent-view (+ x-pos x) (+ y-pos y) width height fill-p)))

(defginamethod draw-rectangles ((view-object view-object) rectangles &optional (fill-p nil))
  (description "call same method of the view, but take position into account"
	       :called-by-gina nil
	       :called-by-application "(typically within the draw method of a view-object)")
  (comment "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	     (draw-rectangles (first (view-objects (main-view
						     (first (document-list *application*)))))
			    '(10 10 50 50 40 30 70 60)))
	   "draws two rectangles")
 
  (with-slots (parent-view x-pos y-pos) view-object
    ;; modify rectangles
    (setq rectangles (loop for coordinate in rectangles
			   for pos from 1
			   when (member (mod pos 4) '(3 0)) collect coordinate 
			   else when (oddp pos) 
				  collect (+ x-pos coordinate)
			   else collect (+ y-pos coordinate)))
    (draw-rectangles parent-view rectangles fill-p)))

(defginamethod draw-arc ((view-object view-object) x y width height angle1 angle2
			 &optional (fill-p nil))
  (description "call same method of the view, but take position into account"
	       :called-by-gina nil
	       :called-by-application "(typically within the draw method of a view-object)")
  (comment "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	     (draw-arc (first (view-objects (main-view
					      (first (document-list *application*)))))
			 10 10 50 50 0 (* 2 pi) t))
	   "draws a filled circle")

  (with-slots (parent-view x-pos y-pos) view-object
    (draw-arc parent-view (+ x-pos x) (+ y-pos y) width height angle1 angle2 fill-p)))

(defginamethod draw-arcs ((view-object view-object) arcs &optional (fill-p nil))
  (description "call same method of the view, but take position into account"
	       :called-by-gina nil
	       :called-by-application "(typically within the draw method of a view-object)")
  (comment "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	     (draw-arcs (first (view-objects (main-view
					      (first (document-list *application*)))))
			 `(10 10 50 50 0 ,(* 2 pi) 70 70 50 50 0 ,pi) t))
	   "draws one and a half filled circles")

  (with-slots (parent-view x-pos y-pos) view-object
    ;; modify arcs
    (setq arcs (loop for coordinate in arcs
		     for pos from 1
		     when (member (mod pos 6) '(3 4 5 0)) collect coordinate 
			   else when (oddp pos) 
				  collect (+ x-pos coordinate)
			   else collect (+ y-pos coordinate)))
    (draw-arcs parent-view arcs fill-p)))

(defginamethod draw-glyph ((view-object view-object) x y elt
			   &key translate width (size :default))
  (description "call same method of the view, but take position into account"
	       :called-by-gina nil
	       :called-by-application "(typically within the draw method of a view-object)")
  (comment "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	     (draw-glyph (first (view-objects (main-view
						(first (document-list *application*)))))
			 10 20 #\a))
	   "writes out one character")

  (with-slots (parent-view x-pos y-pos) view-object
    (draw-glyph parent-view (+ x-pos x) (+ y-pos y) elt
		:translate translate
		:width width
		:size size)))

(defginamethod draw-glyphs ((view-object view-object) x y sequence
			&key (start 0) end translate width (size :default))
  (description "call same method of the view, but take position into account"
	       :called-by-gina nil
	       :called-by-application "(typically within the draw method of a view-object)")
  (comment "This method directly corresponds to the CLX function with the same name.")
  (example (with-application-stopped
	     (draw-glyphs (first (view-objects (main-view
						(first (document-list *application*)))))
			 10 20 "Hello World"))
	   "writes out a string")
 
  (with-slots (parent-view x-pos y-pos) view-object
    (draw-glyphs parent-view (+ x-pos x) (+ y-pos y) sequence
		 :start start :end end :translate translate :width width :size size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; class direct-manipulation-object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass direct-manipulation-object (view-object)
   (description "a view-object with direct-manipulation capabilities"
		:application-subclasses :always
		:gina-subclasses "movable icons and in the demo applications"
		:instantiate "for testing only")
   ((facilities  :accessor facilities  :initform :resizable  :allocation :class
	:documentation
	"one-of nil, :selectable, :movable, :move-and-selectable, :resizable")))

(defginafun make-direct-manipulation-object 
	    (width "extension of enclosing rectangle in pixels"
	     height "extension of enclosing rectangle in pixels"
	     &key
	     (mouse-sensitive t)
	     "flag whether button-press is to be propagated to this object"
	     (class 'direct-manipulation-object) "'direct-manipulation-object or any subclass"
	     (initargs nil) "further initializations for subclass")
  (description "create a new direct manipulation object"
	       :constructor-for-class direct-manipulation-object
	       :called-by-gina nil
	       :called-by-application :sometimes)
  (comment "Direct-manipulation objects can exist without beeing installed in a view.")
  (example (with-application-stopped
	     (install (make-direct-manipulation-object 100 100)
		      (main-view (first (document-list *application*)))
		      100 100)))
  (make-view-object width height
		    :mouse-sensitive mouse-sensitive
		    :initargs initargs
		    :class class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Class movable-icon
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass movable-icon (direct-manipulation-object)
  (description "a direct manipulation-object displaying a pixmap"
	       :application-subclasses :sometimes
	       :gina-subclasses "only in the chess demo application"
	       :instantiate t)
  
  (;; instance slots:

   ;; pixmaps should be :allocation instance. 
   ;; :allocation :class for pixmap leads to errors when
   ;; running an application from same lisp world on different displays!

   (icon-pixmap :accessor icon-pixmap :initform nil
      :documentation "Pixmap of icon on X-server side")
   (mask-pixmap :accessor mask-pixmap :initform nil
      :documentation "Pixmap of mask on X-server side")

   (icon-image :accessor icon-image :initarg :icon-image
      :documentation "Image structure of icon as read from file")
   (mask-image :accessor mask-image :initarg :mask-image
      :documentation "Image structure of mask as read from file")

   (shadow-offset :accessor shadow-offset :initarg :shadow-offset
      :documentation "Offset of shadow (0 = no shadow)")
   (left-margin   :accessor left-margin   :initarg :left-margin
      :documentation "Free space on left of icon")
   (top-margin    :accessor top-margin    :initarg :top-margin
      :documentation "Free space on top of icon")

   ;; override class slots
   (facilities :allocation :class :initform :move-and-selectable
      :documentation
      "one-of nil, :selectable, :movable, :move-and-selectable, :resizable")
   (outlines :allocation :class :initform nil
      :documentation
  "flag whether complete (solid) icon is moved (default) or just xor the icon"
  )))


(defginafun make-movable-icon
	   (icon-image-or-pathname "image or name of bitmap file"
	    &key (mask nil)        "image or name of bitmap file"
	         (shadow-offset 0) "offset of shadow; 0 = no shadow"
	         (top-margin 0)    "space on top of icon"
		 (bottom-margin shadow-offset)
		                   "space (e.g. for shadow) on bottom of icon"
		 (left-margin 0)   "space on left of icon"
		 (right-margin  shadow-offset)
				   "space (e.g. for shadow) on right of icon"
		 
	         (class 'movable-icon)
		                   "class movable-icon or any subclass of it"
		 (initargs nil)    "further initializations for subclass"
	    &aux icon-image (mask-image nil))

  (description "Create a new movable icon"
	       :constructor-for-class movable-icon
	       :called-by-gina "only in the chess demo application"
	       :called-by-application :sometimes)

  (example (with-application-stopped
	    (setq icon (make-movable-icon "/usr/include/X11/bitmaps/mailfull"
				 :mask "/usr/include/X11/bitmaps/mailfullmsk"
				 :shadow-offset 8))
	    (install icon (main-view (first (document-list *application*))))))


  ;; read images from file when not yet given:
  (setq icon-image (pathname->image *application* icon-image-or-pathname))

  (when mask
    (setq mask-image (pathname->image *application* mask))
    ;; we need an image in z-format for the mask to access pixels:
    (setq mask-image (image->image-z *application* mask-image)))
	      
  (make-direct-manipulation-object
       (+ (xlib:image-width  icon-image) left-margin right-margin)
       (+ (xlib:image-height icon-image) top-margin bottom-margin)
       :class class
       :initargs (more-initargs :icon-image icon-image
				:mask-image mask-image
				:shadow-offset shadow-offset
				:left-margin left-margin
				:top-margin top-margin)))

(defginamethod set-image ((icon movable-icon) 
			  icon-image-or-pathname "image or name of bitmap file"
			  &key (mask nil)        "image or name of bitmap file"
			       (redraw t)
			  &aux icon-image (mask-image nil))
  (description "change the bitmap of the icon (but not its size)"
	       :called-by-application "for dynamically changing the icon"
	       :called-by-gina        "in make-movable-icon to install image")
  
  ;; read images from file when not yet given:
  (setq icon-image (pathname->image *application* icon-image-or-pathname))

  (when mask
    (setq mask-image (pathname->image *application* mask))
    ;; we need an image in z-format for the mask to access pixels:
    (setq mask-image (image->image-z *application* mask-image)))
  
  (setf (icon-image icon) icon-image)
  (setf (mask-image icon) mask-image)
  (setf (icon-pixmap icon) nil)  ;; will be reset by draw-method
  
  (when (and redraw (parent-view icon)) ;; security-check: icon installed?
    (force-redraw icon)))
  
			  
(defginamethod pathname->image ((appl application)
				pathname-or-image
				"if an image is given, it is just returned" 
				&aux image)
  (description "look up cached image for pathname or read file and cache it"
     :called-by-application "for mapping pathnames to images"
     :called-by-gina        "in methods of class movable-icon")

  (when (typep pathname-or-image 'xlib:image)
    ;; Already an image. Nothing to do:
    (return-from pathname->image pathname-or-image))
  (setq image (gethash pathname-or-image (pixmap-table appl)))
  (when (not image)
    (setq image (xlib:read-bitmap-file (find-bitmap pathname-or-image)))
    ;(format t "xlib:read-bitmap-file ~a~%" pathname-or-image)
    (setf (gethash pathname-or-image (pixmap-table appl)) image))
  image)

(defginamethod image->image-z ((appl application)
			       image "image of any format (image-x -xy or -z)"
			       &aux image-z)
   (description
      "look up cached image in z-format or convert to it and cache it"
     :called-by-application "for mapping images to images of z-format"
     :called-by-gina        "in methods of class movable-icon")

   (when (typep image 'xlib:image-z)
     ;; Already of type image-z. Do nothing.
     (return-from image->image-z image))
   ;; lookup for z-image
   (setq image-z (gethash (list image :z) (pixmap-table appl)))
   (when (not image-z)
     ;(format t "Convert image ~a~%" image)     
     (setq image-z (xlib:copy-image image :result-type 'xlib:image-z))
     ;; we store the z-image under (image :z) because under image there could be a pixmap
     (setf (gethash (list image :z) (pixmap-table appl)) image-z))
   image-z)

(defginamethod image->pixmap ((appl application)
			      image "an image (i.e. pixel data on lisp-side)"
			      drawable
      "a drawable which determines the screen where the pixmap will be used"
			      &aux pixmap gcontext)
  (description "lookup cached pixmap for image or create pixmap and cache it"
     :called-by-application "for mapping images to pixmaps"
     :called-by-gina        "in methods of class movable-icon")

  (setq pixmap (gethash image (pixmap-table appl)))
  (when (not pixmap)
    ;(format t "Create pixmap ~a~%" image)
    ;; create a pixmap in the X-server
    (setq pixmap (xlib:create-pixmap
		  :width  (xlib:image-width  image)
		  :height (xlib:image-height image)
		  ;; take depth from pixmap:
		  :depth  (xlib:image-depth image) ;; was: 1
		  :drawable drawable))

    (setq gcontext (xlib:create-gcontext :drawable pixmap :exposures :off))

    ;; draw image into the pixmap
    (xlib:put-image pixmap gcontext image :x 0 :y 0)

    (setf (gethash image (pixmap-table appl)) pixmap))
  pixmap)
  

(defun get-intersecting-rectangle (x0 y0 w0 h0 x1 y1 w1 h1 &aux x y w h)
  (setq x (max x0 x1))
  (setq y (max y0 y1))
  (setq w (if (< x0 x1)
	      (- (min (+ x0 w0) (+ x1 w1)) x1)
	    (- (min (+ x1 w1) (+ x0 w0)) x0)))
  (setq h (if (< y0 y1)
	      (- (min (+ y0 h0) (+ y1 h1)) y1)
	    (- (min (+ y1 h1) (+ y0 h0)) y0)))
  (values x y (if (< w 0) 0 w) (if (< h 0) 0 h)))


(defginamethod draw ((icon movable-icon)
		     count
		     "number of calls to draw method that will immediately follow"
		     x  "upper left corner of rectangle which has to be drawn"
		     y  "upper left corner of rectangle which has to be drawn"
		     width           "size of rectangle which has to be drawn"
		     height          "size of rectangle which has to be drawn"
		     &aux 
		     (view-gc (gcontext (parent-view icon))) 
		     (old-clip-mask (xlib:gcontext-clip-mask view-gc)))
  (description "Draw icon optionally with shadow from stored pixmap"
	       :called-by-gina  "in response to expose events"
	       :default-version "draws icon with or without shadow"
	       :override        :rarely)
  (comment "You can add a deamon to display more information e.g. a text.")
  (declare (ignore count x y width height))

  (with-slots (shadow-offset icon-image left-margin top-margin) icon

     ;; load pixmap when icon is drawn for the first time:
     (when (null (icon-pixmap icon))  (get-pixmap icon))

     (unless (and (listp old-clip-mask) (= (length old-clip-mask) 4))
       (warning "(draw movable-icon): clip mask is not a single rect")
       (return-from draw))
     
     (when (and (mask-pixmap icon) (> shadow-offset 0))
       (multiple-value-bind (x y w h)
	   ;; clip with the global clipping rectangle relative to the icon
	   (get-intersecting-rectangle 0 0 (width icon) (height icon)
				       (- (first old-clip-mask) (x-pos icon))
				       (- (second old-clip-mask) (y-pos icon))
				       (third old-clip-mask)
				       (fourth old-clip-mask))
	 ;; draw shadow
	 ;; set clip mask manually, bug in CLX
	 (setf (xlib:gcontext-clip-mask view-gc) (mask-pixmap icon))
	 (xlib:with-gcontext
	     (view-gc :clip-x (+ (x-pos icon) left-margin shadow-offset)
		      :clip-y (+ (y-pos icon) top-margin shadow-offset))
	   (draw-rectangle icon x y w h t))))
     
     (if (mask-pixmap icon)
	 (multiple-value-bind (x y w h)
	     ;; clip with the global clipping rect relative to the icon pixmap
	     (get-intersecting-rectangle left-margin top-margin
					 (xlib:image-width  icon-image)
					 (xlib:image-height icon-image)
					 (- (first old-clip-mask) (x-pos icon))
					 (- (second old-clip-mask) (y-pos icon))
					 (third old-clip-mask)
					 (fourth old-clip-mask))
	   (setf (xlib:gcontext-clip-mask view-gc) (mask-pixmap icon))
	   (xlib:with-gcontext
	       (view-gc :clip-x (+ (x-pos icon) left-margin)
			:clip-y (+ (y-pos icon) top-margin))
	     (draw-icon-image icon (- x left-margin) (- y top-margin)
			      w h x y))
	   
	   ;; bug in CLX
	   (setf (xlib:gcontext-clip-mask view-gc) old-clip-mask))
       
       ;; else
       (draw-icon-image icon 
			0 0 
			(xlib:image-width  icon-image) 
			(xlib:image-height icon-image)
			left-margin
			top-margin))))

(defginamethod draw-icon-image ((icon movable-icon) 
				source-x source-y width height
				destination-x destination-y
				&aux (gc (gcontext (parent-view icon))))
  (description "copy picture into view"
	       :called-by-gina "in the draw method of movable-icon"
	       :called-by-application "in the draw method of a subclass")
  (comment "source coordinates are relative to the image"
	   "destination coordinates are relative to the icon")
  
  (xlib:with-gcontext
	     (gc :exposures :off
		 :background (if (selected icon)
				 (xlib:gcontext-foreground gc)
			       (xlib:gcontext-background gc))
		 :foreground (if (selected icon)
				 (xlib:gcontext-background gc)
			       (xlib:gcontext-foreground gc)))
    (if (> (xlib:image-depth (icon-image icon)) 1)
	(copy-area (parent-view icon)
		   (icon-pixmap icon)
		   source-x source-y width height 
		   (+ (x-pos icon) destination-x)
		   (+ (y-pos icon) destination-y))
      (copy-plane (parent-view icon)
		   (icon-pixmap icon)
		   1 ;; plane
		   source-x source-y width height 
		   (+ (x-pos icon) destination-x)
		   (+ (y-pos icon) destination-y)))))

(defmethod get-pixmap ((icon movable-icon)
			&aux (view (parent-view icon)))
  "create pixmap as soon as window-id is known"
  ;; used by first call of draw (icon movable-icon)
  (with-slots (x-window) view
       (with-slots (icon-pixmap mask-pixmap icon-image mask-image
				width height) icon
	      (setq icon-pixmap
		(image->pixmap *application* icon-image x-window))
	      (when mask-image
		(setq mask-pixmap
		  (image->pixmap *application* mask-image x-window))))))

(defginamethod draw-outline ((icon movable-icon)
			     new-x "current position"
			     new-y "current position"
			     width "current size"
			     height "current size"
			     &key clear "not used for movable-icon")
  (description "draw icon using xor while being moved"
	       :called-by-gina "during move command"
	       :default-version "draws the icon using xor")
  (comment 
   "function boole-xor is set in gcontext before this method is called.")

  (declare (ignore clear width height))
  (with-slots (icon-pixmap parent-view icon-image left-margin top-margin) icon
	      (xlib:with-gcontext
		  ((gcontext parent-view)
		   :foreground #xffffffff
		   :background 0
		   :function boole-xor)
	       (xlib:copy-plane icon-pixmap
			        (gcontext parent-view)
			        1    ;; depth
			        0 0  ;; source
				(xlib:image-width  icon-image)
				(xlib:image-height icon-image)
				(drawable parent-view)
			        (+ new-x left-margin)
				(+ new-y top-margin)))))

(defginamethod draw-selected ((icon movable-icon) count x y width height)
     (description "do nothing because already done in draw method" )
     (declare (ignore count x y width height)))
	       
(defginamethod point-inside ((icon movable-icon) x y &aux rel-x rel-y)
  (description "check if given point in view coordinates is inside the icon"
	       :called-by-application :rarely
	       :called-by-gina "to test whether mouse click hits icon")
  (comment "Uses mask; calls point-inside of view-object when no mask given.")

  (with-slots (mask-image x-pos y-pos left-margin top-margin) icon
     (when (call-next-method) ;; call method of class view-object
       ;; we are inside the enclosing rectangle:
       (when (null mask-image)
	 (return-from point-inside t))
       ;; lookup hit bit in mask of icon:
       (setq rel-x (- x x-pos left-margin))
       (setq rel-y (- y y-pos top-margin))
       (when (and (>= rel-x 0) (< rel-x (xlib:image-width  mask-image))
		  (>= rel-y 0) (< rel-y (xlib:image-height mask-image)))
	 (= 1 (aref (xlib::image-z-pixarray mask-image) rel-y rel-x))))))
