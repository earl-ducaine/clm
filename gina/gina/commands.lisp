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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;                Commands, Callbacks, ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :gina)

(setq *sccs-id* "@(#)commands.lisp	1.27  11/8/93")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class callback
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass callback ()
   (description "a data object describing an action to do later"
		:application-subclasses :rarely)
  ((function-name :accessor function-name :initarg :function-name
		  :documentation "the function to be called")
   (static-args   :accessor static-args   :initarg :static-args
		  :documentation
		  "a list of arguments which will always be passed to the function")))

(defginamethod print-object ((cb callback) stream)
  (description "specialized printed representation"
	       :called-by-gina "to denote a callback object"
	       :override "to get a special printed representation"
	       :default-version "prints name of the function")
  (format stream "#<Callback ~a>" (function-name cb)))

(defginafun make-callback (function-name "the function to be called"
			    &rest static-args
			    "a list of arguments which will always be passed to the function")
  (description "create a new callback object"
	       :constructor-for-class callback
	       :called-by-application :sometimes)
  (example (execute (make-callback #'list 1 2 3 4) '(5 6 7)))
  (make-instance 'callback
		 :function-name function-name
		 :static-args (copy-list static-args)))

(defginafun print-parms (&rest parms)
  (description
    "Just print the parameters supplied"
    :called-by-gina "when supplied as a callback function"
    :called-by-application :rarely)
  (comment 
   "You can use this function to find out the parameters of a callback.")
  (example (with-application-stopped
	     (setq box (make-modeless-dialog-box "test"))
	     (make-scrollbar box :value-changed-callback #'print-parms)
	     (pop-up box))
	   "Creates a dialog-box with a scrollbar. The new value is printed at each callback.")
  (format t "Parms: ~s~%" parms))

(defginamethod execute 
	((cb callback)
	 dynamic-args
	 "additional arguments for the function stored in the callback object")
  (description "apply the stored function to the static- and dynamic args")
  (with-slots (function-name static-args) cb
     #+(or cmu genera) (setq function-name (coerce function-name 'function))
     (apply function-name (append static-args dynamic-args))))

(defginamethod execute 
        ((callback t)
	 dynamic-args
	 "additional arguments for the function stored in the callback object")
  (description "apply the stored function to the static- and dynamic args")
  (comment
    "Lambda-expressions and compiled functions can be executed just like callback objects.")
  (comment "Callbacks can also be lists of callbacks.")
  ;;lambda-exprs and compiled functions will be applied
  (when callback
    (if (or (typep callback '(or function symbol))
            (and (listp callback) (eql (car callback) 'lambda)))

	(progn
	  #+(or cmu genera) (setq callback (coerce callback 'function))
	  (apply callback dynamic-args))
	  
        ;; else: not a function
	(if (listp callback)
	    ;; multiple callbacks
	    (loop for cb in callback
		  collect (execute cb dynamic-args))
	    ;; else: error
	    (error "Illegal callback argument (~a) to execute" callback)))))

(defginamethod execute-from-widget ((cb callback) widget call-data)
  (description "allow overriding in subclasses, passing a widget")
  (declare (ignore widget))
  (execute cb call-data))

(defginamethod execute-from-widget ((cb t) widget call-data)
  (description "to catch callback lists when passing a widget")
  (if (and (listp cb) (not (eql (car cb) 'lambda)))
    (loop for callb in cb
	do (execute-from-widget callb widget call-data))
    (execute cb call-data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass command ()
   (description "an object representing a single user action"
		:application-subclasses :always
		:gina-subclasses
		"class mouse-down-command and special commands for direct-manipulation"
		:instantiate nil)
   (;; instance-parameters
    (document      :accessor document      :initarg :document
		   :documentation "the document changed by the command")
    (view          :accessor view          :initarg :view
		   :documentation "the view which shows the effect of the command")
    (reverse-undo  :accessor reverse-undo  :initarg :reverse-undo)
                   ;; if the command should be considered its inverse,
                   ;; i.e. if undo should call redoit, and redo undoit
    
    ;; class-parameters
    (name          :accessor name          :initform "Command" :allocation :class
		   :documentation "identification shown in the history scroller")
    (clock-cursor  :accessor clock-cursor  :initform nil       :allocation :class
		   :documentation "flag whether clock cursor is shown during doit")
    (undoable      :accessor undoable      :initform t         :allocation :class
		   :documentation "flag whether an UNDO is possible")
    (causes-change :accessor causes-change :initform t         :allocation :class
		   :documentation "flag whether the command modifies the document")

    ;; instance-variables
    (view-x-offset :accessor view-x-offset
		   :documentation "scroll position of the view when command was issued")
    (view-y-offset :accessor view-y-offset
		   :documentation "scroll position of the view when command was issued")
    (checkpoint    :accessor checkpoint    :initform nil)))

(defginafun make-command (document "the document changed by the command"
		     &key (view (main-view document))
		          "the view which shows the effect of the command"
		          (immediately-submit t)
			  "flag whether command is immediately submitted when created"
			  (create-checkpoint nil)
                          (reverse-undo nil) 
			  (class 'command) "'command or any subclass of it"
			  (initargs nil) "further initializations for command subclass"
		     &aux cmd)
  (description "create a command object"
	       :constructor-for-class command
	       :called-by-application "in the constructor of a command subclass")
  (setq cmd (make-object class initargs :document document :view view
                                        :reverse-undo reverse-undo))

  (when (and view (scroller view))
    ;; store the scrolling position of the view in the cmd
    (update-slots view)
    (setf (view-x-offset cmd) (- 0 (x-pos view)))
    (setf (view-y-offset cmd) (- 0 (y-pos view))))

  ;; create a checkpoint of the document
  (when create-checkpoint
    (setf (checkpoint cmd) (create-checkpoint document)))
  
  ;; execute and register the cmd in the document
  (when (and immediately-submit (executable cmd))
    (submit cmd))
  
  ;; return the new command
  cmd)

(defginamethod submit ((cmd command))
  (description "store command in the document and call doit"
	       :called-by-gina "in make-command"
	       :called-by-application "if a command is not immediately submitted when created"
	       )
  ;; new in Gina 2.2: method will no longer be called, when (not (executable cmd))
  (with-slots (document clock-cursor undoable causes-change) cmd
      (if clock-cursor  
	  (with-clock-cursor (doit cmd))
	  (doit cmd))
      (when causes-change
	;; mark document as modified
	(setf (modified document) t))
	
      (when undoable
	;; store the command object in the document
	(register-command document cmd))
	
      (when (and (not undoable) causes-change)
	;; executing the command makes any undo impossible
	(irreversible-change document))
      ))

(defginamethod executable ((cmd command))
  (description "check if the command is executable"
	       :called-by-gina "before doit is called"
	       :override "to define conditions for the command to be executed"
	       :default-version "returns t, because normal commands are always executable")
  (comment "Especially mouse commands may be executed only if something is hit.")
  t)

(defginamethod doit ((cmd command))
  (description
    "execute the effect of the command"
    :called-by-gina "when the command is submitted"
    :override "to define the special semantics of a command"
    :default-version :do-nothing
    :overridden-by-gina "in the predefined commands for direct manipulation")
  )

(defginamethod scroll-before-undo ((cmd command))
  (description
    "do the scrolling necessary so that the effect of UNDO can be seen"
    :called-by-gina "before method undoit is called"
    :default-version
    "scrolls the view to the position it originally had when the command was executed") 
  (with-slots (view view-x-offset view-y-offset) cmd
    (when (and view (scroller view))
      (scroll-to (scroller view) view-x-offset view-y-offset)
      (xlib:display-finish-output *display*))))

(defmethod undoit :before ((cmd command))
  "do scrolling before undoing the command"
  (scroll-before-undo cmd))

(defginamethod undoit ((cmd command))
  (description "undo effect of the command"
	       :called-by-gina "in response to the UNDO menu entry"
	       :override "to define the inverse of a command"
	       :default-version "tries to restore a checkpoint, if there is one"
	       :overridden-by-gina "in the predefined commands for direct manipulation")
  (with-slots (checkpoint document) cmd
    (when checkpoint
      (restore-checkpoint document checkpoint))))

(defginamethod scroll-before-redo ((cmd command))
  (description
    "do the scrolling necessary so that the effect of REDO can be seen"
    :called-by-gina "before method redoit is called"
    :default-version
    "scrolls the view to the position it originally had when the command was executed"
    :overridden-by-gina "for mouse-commands because of possible auto-scrolling")
  (with-slots (view view-x-offset view-y-offset) cmd
    (when (and view (scroller view))
      (scroll-to (scroller view) view-x-offset view-y-offset))))

(defmethod redoit :before ((cmd command))
  "do scrolling before undoing the command"
  (scroll-before-redo cmd))

(defginamethod redoit ((cmd command))
  (description
    "repeat effect of the command"
    :called-by-gina "when the command is repeated by a REDO"
    :override "if repeating the command is different from doing it for the first time"
    :default-version "calls the method doit"
    :overridden-by-gina "in the predefined commands for direct manipulation")
  (doit cmd))

(defginamethod commit ((cmd command)  undone
		       "flag whether the last action with this command was an UNDO")
  (description
    "free resources used by the command"
    :called-by-gina "when no more undo/redo is possible"
    :override "if there are any resources which must be freed"
    :default-version :do-nothing)
  (declare (ignore undone)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class mouse-down-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass mouse-down-command (command)
   (description "a mouse command with feedback as long as the mouse is down"
		:application-subclasses :always
		:gina-subclasses
		"the special commands for direct-manipulation"
		:instantiate nil)
   (;; instance-parameters
    (start-x       :accessor start-x       :initarg :start-x
		   :documentation "view coordinate where the mouse button went down")
    (start-y       :accessor start-y       :initarg :start-y
		   :documentation "view coordinate where the mouse button went down")
    
    ;; class-parameters
    (hysteresis     :accessor hysteresis     :initform 0 :allocation :class
		    :documentation "minimum mouse movement before command is submitted")
    (auto-scrolling :accessor auto-scrolling :initform t :allocation :class
		    :documentation "flag whether automatic scrolling is desired")
    (idle-timeout  :accessor idle-timeout
		   :initform 0.25
		   :allocation :class
		   :documentation
     "seconds before mouse-idle is called and auto-scrolling is done when mouse is not moved")
    (timer-id             :accessor timer-id             :initform nil
			  :documentation "id for xtk:change-timer and friends")
    (call-doit           :accessor call-doit           :initform t :allocation :class
			 :documentation "flag whether command should really be submitted")
    ;; overrides
    (name          :initform "Mouse Command" :allocation :class
		   :documentation "overrides slot of class command") 
    ;; instance-variables
    (mouse-already-moved :accessor mouse-already-moved :initform nil
			 :documentation "flag if hysteresis has been reached")
    (last-x              :accessor last-x              :initarg :last-x
			 :documentation "view coordinate of last mouse position")
    (last-y              :accessor last-y              :initarg :last-y
			 :documentation "view coordinate of last mouse position")
    ;; experimental for feedback-animation
    (mouse-positions :accessor mouse-positions :initform nil)
    ))

(defginafun make-mouse-down-command
	    (document "the document changed by the command"
	     view     "the view where the mouse feedback is shown"
	     start-x  "view coordinate where the mouse button went down"
	     start-y  "view coordinate where the mouse button went down"
	     &key (cursor :hand) "number or keyword"
	     (class 'mouse-down-command) "'mouse-down-command or any subclass of it"
	     (initargs nil) "further initializations for command subclass"
	     &aux cmd cursor-keyword)
  (description "create a mouse-down-command object"
	       :constructor-for-class mouse-down-command
	       :called-by-application :sometimes)
  (comment "Cursor can be the result of a call to xtk:create-font-cursor"
	   "or xtk:create-pixmap-cursor or a keyword like :hand, :arrow ...")
  (comment "(xtk:all-cursors) lists all available keywords.")
  (declare (special *display*))
  (setq cmd (make-command document
			  :view view
			  :immediately-submit nil
			  :class class
			  :initargs (more-initargs
				      :start-x start-x :start-y start-y
                                      :last-x start-x :last-y start-y)))

  ;; register the mouse command at the application
  (setf (active-mouse-command document) cmd)

  ;; change the cursor for the feedback phase
  (when (not (numberp cursor))
    (setq cursor-keyword cursor)
    ;; check if cursor has already been used in this application
    (setq cursor (gethash cursor-keyword (cursor-table *application*)))
    (when (not cursor)
      ;; cursor used for the first time: create and store in hashtable
      (setq cursor (xtk:create-font-cursor cursor-keyword))
      (setf (gethash cursor-keyword (cursor-table *application*)) cursor)))
  (xtk:change-grab-cursor cursor)
  
  ;; make sure that the current mouse-position is processed
  (motion-notify cmd start-x start-y)

  ;; create timer
  (when (idle-timeout cmd)
	(setf (timer-id cmd)
	      (xtk:create-timer (max 1 (round (* 1000 (idle-timeout cmd))))
                                #'gina-callback
                                (make-callback #'idle-action cmd) 
				;#'handle-timeout cmd
  )))

  ;; return the new command
  cmd)

(defmethod motion-notify ((cmd mouse-down-command) x y
			  &key (finished nil) &aux (error-free nil))
  "react to pointer motion"
  ;;(format t "motion notify ~a ~a " x y)
  (unwind-protect
  (with-slots (start-x start-y last-x last-y view mouse-already-moved
		       hysteresis auto-scrolling) cmd
    ;; check for hysteresis
    (when (or mouse-already-moved
	      (>= (abs (- start-x x)) hysteresis)
	      (>= (abs (- start-y y)) hysteresis))
    
      ;; force x and y into the view
      (when auto-scrolling
	(with-slots (width height) view
		    (setq x (max x 0) y (max y 0))
		    (setq x (min x width) y (min y height))))
    
      ;; clear last feedback if any
      (when mouse-already-moved
	(draw-feedback cmd last-x last-y :clear t))

      ;; hook for application specific transformations
      (multiple-value-setq (x y) (constrain-mouse cmd x y))

      ;; auto scrolling
      (when (and auto-scrolling (scroller view))
	;; scroll the view if necessary
	(show-point (scroller view) x y
		    ;; get the size of the clipper and the position of 
		    ;; the work-area first
		    :ask-for-work-area-pos (not mouse-already-moved)
		    :ask-for-clipper-size  (not mouse-already-moved)
		    ;; just move the X-window most of the time
		    :x-window-only (not finished)
		    ;; always tell the toolkit the final position
		    :force-scrolling finished))

      ;; experimental
      (when (feedback-animation *application*)
	(push (list x y) (mouse-positions cmd)))
      
      ;; process the possibly modified coordinates
      (track-mouse cmd x y :started (not mouse-already-moved) 
		           :finished finished)
    
      ;; draw new feedback
      (when (not finished)
	(draw-feedback cmd x y :clear nil))
    
      ;; store x and y
      (setq last-x x last-y y)
      (setq mouse-already-moved t))
    (setq error-free t))
  ;; cleanup: cancel command in case of an programmer error
  (unless error-free
    (setf (active-mouse-command (document cmd)) nil)
    (when (timer-id cmd)
	(xtk:destroy-timer (timer-id cmd))))
  ))

(defvar *button-1-mask*)
(setq *button-1-mask* (xlib:make-state-mask :button-1))

(defmethod idle-action ((cmd mouse-down-command))
  "the mouse did not move for some time"
  ;;(format t "idle-action ")

  (with-slots (mouse-already-moved auto-scrolling last-x last-y view) cmd
    (when mouse-already-moved  
      ;; auto scrolling
      (when (and auto-scrolling (scroller view))
	;; old pointer position may be invalid because of scrolling
	(multiple-value-bind (x y same-screen child mask) 
	    (xlib:query-pointer (x-window view))
	  (declare (ignore same-screen child))
	  (when (and
		  (not (zerop (logand mask *button-1-mask*)))
		  (not (point-visible (scroller view) x y
				      ;; do not get the size of the clipper 
				      ;; and position of the work-area first
				      :ask-for-work-area-pos nil
				      :ask-for-clipper-size nil)))
	    (motion-notify cmd x y))))

      ;; give subclass a chance to do something
      (mouse-idle cmd last-x last-y)
      )))

(defmethod button-release ((cmd mouse-down-command) x y)
  "button goes up => track phase is over"
  ;;(format t "button release ~a ~a " x y)
  (with-slots (call-doit mouse-already-moved) cmd
    (motion-notify cmd x y :finished t) 

    ;; deinstall the mouse-command and the timer
    (setf (active-mouse-command (document cmd)) nil)
    (when (timer-id cmd)
      (xtk:destroy-timer (timer-id cmd)))

    (if (and call-doit mouse-already-moved (executable cmd)) 
      ;; execute the commands effect
      (submit cmd)
      ;; else: give application a chance to react when not submitted
      (not-submitted cmd))))

(defginamethod constrain-mouse ((cmd mouse-down-command)
				x "current mouse position"
				y "current mouse position")
  (description
    "modify x and y position of mouse pointer"
    :called-by-gina "before mouse feedback is drawn"
    :override "to constrain the mouse to certain locations in the view"
    :default-version "returns x and y unmodified"
    :result "x and y as multiple values")
  (values x y))

(defginamethod draw-feedback ((cmd mouse-down-command)
			      x "current mouse position"
			      y "current mouse position"
			      &key
			      clear "flag whether feedback has to be drawn or cleared")
  (description
    "draw mouse feedback during commands"
    :called-by-gina "each time the mouse has moved"
    :override "to define a special application dependent feedback"
    :default-version "draws a rubberband line from the start- to the current mouse position"
    :overridden-by-gina "in the predefined commands for direct manipulation")
  (comment 
   ":function boole-xor is set in gcontext before this method is called.")
  (declare (ignore clear))
  (with-slots (start-x start-y view) cmd
    (draw-line view start-x start-y x y)
  ))

(defginamethod draw-feedback :around ((cmd mouse-down-command)
			      x "current mouse position"
			      y "current mouse position"
			      &key
			      clear "flag whether feedback has to be drawn or cleared")
  (description "set xor-mode and foreground in the view's gcontext")
  (xlib:with-gcontext ((gcontext (view cmd)) 
                       :function boole-xor
		       :foreground (xor-foreground (view cmd)))
    (call-next-method cmd x y :clear clear)))

(defginamethod track-mouse ((cmd mouse-down-command)
			    x "current mouse position"
			    y "current mouse position"
			    &key
			    (started nil) "flag whether this is the first call of track-mouse"
			    (finished nil) "flag whether this is the last call of track-mouse")
  (description
    "process the possibly modified mouse coordinates"
    :called-by-gina "each time the mouse has moved"
    :override "if each intermediate coordinate is important (e.g. for freehand drawing)"
    :default-version :do-nothing
    :overridden-by-gina "in the predefined commands for direct manipulation")	       
  (declare (ignore x y started finished)))

(defginamethod mouse-idle ((cmd mouse-down-command)
			   x "current mouse position"
			   y "current mouse position")
  (description
    "react to timeout during mouse command"
    :called-by-gina "each time the mouse did not move for some time"
    :override "to define what happens when the mouse button is held down"
    :default-version :do-nothing)
  (declare (ignore x y)))

(defginamethod not-submitted ((cmd mouse-down-command))
  (description
    "react to the fact that command was not submitted"
    :called-by-gina
    "if the command is not submitted when the button goes up, because call-doit is nil"
    :override "to define what happens instead of submitting the command"
    :default-version :do-nothing)
  ;; new in Gina 2.2: will be called also when (not (executable cmd))
  nil)

(defginamethod scroll-before-redo ((cmd mouse-down-command))
  (description
    "do the scrolling necessary so that the effect of REDO can be seen"
    :called-by-gina "before method redoit is called"
    :default-version "moves the point where the mouse-button was released into the view")
  (comment "This method overrides the default behaviour inherited by class command")
  (when (and (view cmd) (scroller (view cmd)))
    (show-point (scroller (view cmd)) (last-x cmd) (last-y cmd))))

;; experimental
(defmethod redoit :before ((cmd mouse-down-command))
   "animate feedback"
   (when (feedback-animation *application*)
     (loop for (x y) in (reverse (mouse-positions cmd))
           for i from 1
      do (multiple-value-setq (x y) (constrain-mouse cmd x y))
         (draw-feedback cmd x y :clear nil)
       (when (and (auto-scrolling cmd) (scroller (view cmd)))
	 ;; scroll the view if necessary
	 (show-point 
	  (scroller (view cmd)) x y
	  ;; get the size of the clipper and the position of the work-area 
	  :ask-for-work-area-pos t
	  :ask-for-clipper-size  t
	  ;; just move the X-window most of the time
	  :x-window-only t
	  ;; always tell the toolkit the final position
	  :force-scrolling t))
       (track-mouse cmd x y
		    :started (= 1 i) 
		    :finished (= (length (mouse-positions cmd)) i))
       ;;(format t "track-mouse ~a ~a ~a~%" x y cmd)
       (xlib:display-force-output *display*)
       (sleep 0.05)
       (draw-feedback cmd x y :clear t))
     (xlib:display-force-output *display*)))

(defginamethod mouse-rectangle ((cmd mouse-down-command) x y)
  (description "calculate the positive rectangle between start and x,y"
     :called-by-application "when a mouse-command may flip over its start"
     :result "x, y, positive width, positive height as multiple values")
  (let ((x-diff (- x (start-x cmd)))
        (y-diff (- y (start-y cmd))))
    (values
       (if (minusp x-diff) x (start-x cmd))
       (if (minusp y-diff) y (start-y cmd))
       (abs x-diff)
       (abs y-diff))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; class object-mover
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass object-mover (mouse-down-command)
   (description "a mouse-down-command to move a direct-manipulation-object")
   (;; overrides
    (name       :initform "Move Object"    :allocation :instance
	 :documentation "overrides default name of class mouse-down-command")
    (hysteresis :initform 5                :allocation :class
	 :documentation "mouse must be moved at least 5 pixels to drag an object")
    ;; instance-parameters
    (move-object :accessor move-object :initarg :move-object
	 :documentation "the object being moved")
    (x-off       :accessor x-off       :initarg :x-off
	 :documentation "point within object where mouse goes down")
    (y-off       :accessor y-off       :initarg :y-off
	 :documentation "point within object where mouse goes down")
    (old-selection :accessor old-selection :initarg :old-selection
         :documentation "selected objects before moving")
    ;; instance-variables
    (new-selection :accessor new-selection :initform :ignore)
    (new-x       :accessor new-x
	 :documentation "view coordinates where object is moved to")
    (new-y       :accessor new-y
         :documentation "view coordinates where object is moved to")))

(defginafun make-object-mover (obj "the object being moved"
				x "point within object where mouse goes down"
				y "point within object where mouse goes down"
				&key (old-selection :ignore)
				     (class 'object-mover)
				     (initargs nil))
  (description "create a new mouse-down-command object to move the specified object"
	       :constructor-for-class object-mover
	       :called-by-application :sometimes)
  (with-slots (parent-view x-pos y-pos) obj
    (make-mouse-down-command
	   (document parent-view) parent-view (+ x-pos x) (+ y-pos y)
	   :class class
	   :initargs (more-initargs
		      :old-selection old-selection
		      :move-object obj :x-off x :y-off y))))

(defginamethod constrain-mouse ((cmd object-mover) x y)
  (description "use the result of constrain-position of the moved object"
               :called-by-gina "each time the mouse has moved")
  (multiple-value-bind (real-new-x real-new-y)
      (constrain-position (move-object cmd) 
                          (- x (x-off cmd)) (- y (y-off cmd)))
    (setf (new-x cmd) real-new-x)
    (setf (new-y cmd) real-new-y)
    (values (+ real-new-x (x-off cmd)) (+ real-new-y (y-off cmd)))))

(defginamethod draw-feedback ((cmd object-mover)
			      x "current mouse position"
			      y "current mouse position"
			      &key
			      clear "flag whether feedback has to be drawn or cleared")
  (description
    "draw mouse feedback while object is moved"
    :called-by-gina "each time the mouse has moved")
  (comment "This methods overrides the default method inherited by class mouse-down-command.")
  (declare (ignore x y))
  (when (outlines (move-object cmd))
    (with-slots (move-object new-x new-y) cmd
      (draw-outline move-object new-x new-y 
		    (width move-object) (height move-object)
		    :clear clear))))

(defginamethod track-mouse ((cmd object-mover) x y &key (started nil) (finished nil))
  (description
    "immediately move the object if desired"
    :called-by-gina "each time the mouse has moved")
  (comment "This methods overrides the default method inherited by class mouse-down-command.")
  (declare (ignore started finished x y))
  (when (not (outlines (move-object cmd)))
    (with-slots (move-object new-x new-y) cmd
      (move move-object new-x new-y)
      (reconfigured-by-user move-object t nil))))

(defginamethod doit ((cmd object-mover))
  (description "finally move the specified view-object")
  (when (outlines (move-object cmd))
    (with-slots (mouse-already-moved move-object new-x new-y) cmd
      (when mouse-already-moved
        (move move-object new-x new-y)
        (reconfigured-by-user move-object t nil)))))

(defginamethod undoit ((cmd object-mover))
  (description "move back the view-object")
  (unless (eq (old-selection cmd) :ignore)
    (setf (new-selection cmd) 
          (selected-objects (parent-view (move-object cmd))))
    (setf (selected-objects (parent-view (move-object cmd))) 
          (old-selection cmd)))
  (with-slots (mouse-already-moved move-object start-x x-off start-y y-off) cmd
    (when mouse-already-moved
      (move move-object (- start-x x-off) (- start-y y-off))
      (reconfigured-by-user move-object t nil))))

(defginamethod redoit ((cmd object-mover))
  (description "move the view-object forward again")
  (unless (eq (new-selection cmd) :ignore)
    (setf (selected-objects (parent-view (move-object cmd))) 
          (new-selection cmd)))
  (with-slots (move-object new-x new-y mouse-already-moved) cmd
    (when mouse-already-moved
      (move move-object new-x new-y)
      (reconfigured-by-user move-object t nil))))

(defmethod not-submitted ((cmd object-mover))
  "handle only selection"
  ;; new in Gina 2.2: will be called also when (not (executable cmd))
  (when (and (selection-undoable (document cmd)) 
	     (not (eq (old-selection cmd) :ignore)))
    (setf (name cmd) "Select One")
    (submit cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; class object-resizer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass object-resizer (mouse-down-command)
   (description "a mouse-down-command to resize a direct-manipulation-object")
   (;; overrides
    (name       :initform "Resize Object"  :allocation :class
	 :documentation "overrides default name of class mouse-down-command")
    (hysteresis :initform 2                :allocation :class
	 :documentation "mouse must be moved at least 2 pixels to drag an object")
    ;; instance-parameters
    (size-object :accessor size-object :initarg :size-object
	  :documentation "the object being resized")
    (old-x       :accessor old-x       :initarg :old-x
	  :documentation "original position of object being resized")
    (old-y       :accessor old-y       :initarg :old-y
	  :documentation "original position of object being resized")
    (old-width   :accessor old-width   :initarg :old-width
	  :documentation "original size of object being resized")
    (old-height  :accessor old-height  :initarg :old-height
	  :documentation "original size of object being resized")
    (move-x      :accessor move-x      :initarg :move-x
	  :documentation "flag whether x-coordinate of the objects position is changed")
    (move-y      :accessor move-y      :initarg :move-y
	  :documentation "flag whether y-coordinate of the objects position is changed")
    ;; instance-variables
    (new-x       :accessor new-x
	  :documentation "new position of object being resized")
    (new-y       :accessor new-y
	  :documentation "new position of object being resized")
    (new-width   :accessor new-width
	  :documentation "new size of object being resized")
    (new-height  :accessor new-height
	  :documentation "new size of object being resized")))

(defginafun make-object-resizer (obj "the object being resized"
				 x "point within object where mouse went down"
				 y "point within object where mouse went down"
				 handle "1, 2, 3, or 4 for the four corners"
                            &key (class 'object-resizer)
			         (initargs nil))
  (description 
   "create a new mouse-down command object to resize the specified object"
	       :constructor-for-class object-resizer
	       :called-by-application :sometimes)
  (with-slots (parent-view x-pos y-pos width height) obj
    (make-mouse-down-command
	   (document parent-view) parent-view (+ x-pos x) (+ y-pos y)
	   :class class
	   :initargs (more-initargs
		       :size-object obj :old-x x-pos :old-y y-pos
                       :old-width width :old-height height
                       :move-x (or (eql handle 1) (eql handle 2))
                       :move-y (or (eql handle 1) (eql handle 4))))))

(defginamethod constrain-mouse ((cmd object-resizer) x y 
				&aux min-size 
				     unconstrained-width unconstrained-height)
  (description "call constrain-size of the object and ensure handle-size")
  (setq min-size (handle-size (size-object cmd)))
  (with-slots (move-x move-y old-width old-height 
               old-x old-y start-x start-y) cmd
    (setq unconstrained-width (if move-x
                                  (+ old-width (- start-x x))
                                  (+ old-width (- x start-x))))
    (setq unconstrained-height (if move-y
                                  (+ old-height (- start-y y))
                                  (+ old-height (- y start-y))))
    (multiple-value-bind (real-new-width real-new-height)
        (constrain-size (size-object cmd)
			(abs unconstrained-width)
			(abs unconstrained-height))
      (setf (new-width cmd) (max min-size real-new-width))
      (setf (new-height cmd) (max min-size real-new-height))
      (setf (new-x cmd) 
	    (if (minusp unconstrained-width) ;; flip over
                (if move-x (+ old-x old-width)
		           (- old-x (new-width cmd)))
	        (if move-x (+ old-x (- old-width (new-width cmd)))
                           old-x)))
      (setf (new-y cmd) 
            (if (minusp unconstrained-height) ;; flip over
                (if move-y (+ old-y old-height)
		           (- old-y (new-height cmd)))
	        (if move-y (+ old-y (- old-height (new-height cmd)))
                           old-y)))
      (values (+ start-x
                 (if move-x old-width (- old-width))
                 (if (eql move-x (minusp unconstrained-width))
		     (new-width cmd)
		     (- (new-width cmd))))
	      (+ start-y
		 (if move-y old-height (- old-height))
		 (if (eql move-y (minusp unconstrained-height))
		     (new-height cmd)
		     (- (new-height cmd))))))))

(defginamethod draw-feedback ((cmd object-resizer) x y &key clear)
  (description "draw outline feedback while resizing if desired")
  (declare (ignore x y))
  (when (outlines (size-object cmd))
    (with-slots (size-object new-x new-y new-width new-height) cmd
      (draw-outline size-object new-x new-y new-width new-height 
		    :clear clear))))

(defginamethod track-mouse ((cmd object-resizer) x y 
                        &key (started nil) (finished nil))
  (description "immediately resize the object if desired")
  (declare (ignore started finished x y))
  (when (not (outlines (size-object cmd)))
    (with-slots (size-object new-x new-y new-width new-height) cmd
      (reconfigure size-object new-x new-y new-width new-height)
      (reconfigured-by-user size-object (or (move-x cmd) (move-y cmd)) t)
  )))

(defginamethod doit ((cmd object-resizer))
  (description "finally resize the specified object")
  (when (outlines (size-object cmd))
    (with-slots (size-object new-x new-y new-width new-height) cmd
      (reconfigure size-object new-x new-y new-width new-height)
      (reconfigured-by-user size-object (or (move-x cmd) (move-y cmd)) t))))

(defginamethod undoit ((cmd object-resizer))
  (description "give the object the original size again")
  (with-slots (size-object old-x old-y old-width old-height) cmd
    (reconfigure size-object old-x old-y old-width old-height)
    (reconfigured-by-user size-object (or (move-x cmd) (move-y cmd)) t)))

(defginamethod redoit ((cmd object-resizer))
  (description "resize the object again")
  (with-slots (size-object new-x new-y new-width new-height) cmd
    (reconfigure size-object new-x new-y new-width new-height)
    (reconfigured-by-user size-object (or (move-x cmd) (move-y cmd)) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; class object-selector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass object-selector (mouse-down-command)
   (description "a mouse-down-command to select direct-manipulation-objects")
  (;; overrides
   (name :initform "Select Objects" :allocation :class
	 :documentation "overrides default name of class mouse-down-command")
   (hysteresis     :initform 0      :allocation :class
	 :documentation "clicking deselects all")
   (undoable       :initform nil    :allocation :instance :initarg :undoable
	 :documentation "no UNDO is possible (nor necessary) for this command")
   (causes-change  :initform nil    :allocation :class
	 :documentation "this command does not modify the document")
   ;; instance variable
   (extend         :accessor extend :initarg :extend
         :documentation "button used (:select :extend :toggle)")
   (old-selection  :accessor old-selection   :initarg :old-selection)
   (new-selection  :accessor new-selection)))

(defginafun make-object-selector (view "the view where the mouse went down"
				  x "mouse down position"
				  y "mouse down position"
                                  extend "button used for selection")
  (description "create a new mouse-down-command object to select objects"
	       :constructor-for-class object-selector
	       :called-by-application :sometimes)
  (make-mouse-down-command (document view) view x y
			   :cursor :crosshair
			   :class 'object-selector
                           :initargs 
			   (list 
			    :extend extend
			    :undoable (selection-undoable (document view))
			    :old-selection (selected-objects view))))

(defginamethod draw-feedback ((cmd object-selector) x y &key clear)
  (description "draw rectangular dashed feedback")
  (declare (ignore clear))
  (with-slots (view) cmd
    (multiple-value-bind (rect-x rect-y rect-w rect-h)
                    (mouse-rectangle cmd x y)
      (xlib:with-gcontext ((gcontext view) :line-style :dash)
        (draw-rectangle view rect-x rect-y rect-w rect-h)))))

(defginamethod doit ((cmd object-selector))
  (description "select all captured view objects")
  (with-slots (last-x last-y view extend) cmd
    (multiple-value-bind (rect-x rect-y rect-w rect-h)
                    (mouse-rectangle cmd last-x last-y)
      (loop for object in (view-objects view)
	    when (and (facilities object)
                      (not (eq (facilities object) :movable)))
	      do (if (inside-rectangle object rect-x rect-y rect-w rect-h)
		     (setf (selected object) 
                           (not (and (eql extend :toggle) (selected object))))
                     (when (eql extend :select)
		           (setf (selected object) nil))))))
  (setf (new-selection cmd) (selected-objects (view cmd))))

(defginamethod undoit ((cmd object-selector))
  (description "reinstall old selection")
  (setf (selected-objects (view cmd)) (old-selection cmd)))

(defginamethod redoit ((cmd object-selector))
  (description "set selection again")
  (setf (selected-objects (view cmd)) (new-selection cmd)))

;; not necessary
;;(defginamethod not-submitted ((cmd object-selector))
;;  (description "only a click, deselect all selected objects")
;;  (when (eql (extend cmd) :select)
;;    (loop for object in (view-objects (view cmd))
;;          when (and (facilities object) 
;;		    (not (eq (facilities object) :movable))
;;		    (selected object))
;;          do (setf (selected object) nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; class single-selector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass single-selector (mouse-down-command)
   (description "a mouse-down-command to select a single object")
  (;; overrides
   (name :initform "Select One" :allocation :class
	 :documentation "overrides default name of class mouse-down-command")
   (hysteresis     :initform 0      :allocation :class
	 :documentation "clicking is sufficient")
   (undoable       :initform nil    :allocation :instance :initarg :undoable
	 :documentation "no UNDO is possible (nor necessary) for this command")
   (causes-change  :initform nil    :allocation :class
	 :documentation "this command does not modify the document")
   ;; instance variable
   (extend         :accessor extend :initarg :extend
         :documentation "button used (:select :extend :toggle)")
   (old-selection  :accessor old-selection   :initarg :old-selection)
   (selected-object  :accessor selected-object :initarg :selected-object)))

(defginafun make-single-selector (obj "the object where the mouse went down"
				  x "mouse down position"
				  y "mouse down position"
                                  extend "button used for selection"
				  &aux (view (parent-view obj)))
  (description "create a new mouse-down-command object to select objects"
	       :constructor-for-class single-selector
	       :called-by-application :sometimes)
  (make-mouse-down-command (document view) view 
			   (+ x (x-pos obj)) (+ y (y-pos obj))
			   :cursor :left-ptr
			   :class 'single-selector
                           :initargs 
			   (list 
			    :extend extend
			    :selected-object obj
			    :undoable (selection-undoable (document view))
			    :old-selection (selected-objects view))))

(defginamethod draw-feedback ((cmd single-selector) x y &key clear)
  (description "draw no feedback")
  (declare (ignore x y clear)))

(defginamethod doit ((cmd single-selector))
  (description "handle the selection")
  (handle-selection (selected-object cmd) (extend cmd)))

(defginamethod undoit ((cmd single-selector))
  (description "reinstall old selection")
  (setf (selected-objects (view cmd)) (old-selection cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class drag-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass drag-command (command)

   (description
     "a command class to implement drag and drop across windows"
     :application-subclasses :always
     :gina-subclasses nil
     :instantiate nil)

   (
   (protocol-id    :accessor protocol-id     :initform :drag :allocation :class
        :documentation "a unique id string for inter-app communication")
   (hysteresis     :accessor hysteresis      :initform 0 :allocation :class
	:documentation "minimum mouse movement before command is submitted")
   (do-tracking    :accessor do-tracking     :initform t   :allocation :class
        :documentation "whether continuous tracking is performed")
   (lazy-tracking  :accessor lazy-tracking   :initform nil :allocation :class
        :documentation "whether track-target is called only once per target")
   (call-doit      :accessor call-doit       :initform t :allocation :class
	:documentation "flag whether command should really be submitted")
   (target-list    :accessor target-list     :initarg :target-list
        :documentation "a list of window-widget-value tripels of poss. targets")
   (shell-to-move  :accessor shell-to-move   :initarg :shell-to-move
        :documentation "an override shell to be moved as feedback")
   (received-from  :accessor received-from   :initarg :received-from
        :documentation "nil or an x-window from another document")
   (mouse-already-moved :accessor mouse-already-moved :initform nil
	:documentation "flag if hysteresis has been reached")
   (source-x       :accessor source-x        :initarg :source-x
        :documentation "the starting x position relative to the source")
   (source-y       :accessor source-y        :initarg :source-y
        :documentation "the starting y position relative to the source")
   (last-x         :accessor last-x          :initarg :last-x)
   (last-y         :accessor last-y          :initarg :last-y)
   (target-x       :accessor target-x
        :documentation "the current x position relative to the target")
   (target-y       :accessor target-y
        :documentation "the current y position relative to the target")
   (current-target :accessor current-target :initform nil
        :documentation "the current target, if any")
   (current-value  :accessor current-value :initform nil
        :documentation "the current value, if any")
   (transfer-value :accessor transfer-value  :initarg :transfer-value
        :documentation "a value returned from the target application")
   ;; for internal use
   (global-x-off   :accessor global-x-off)
   (global-y-off   :accessor global-y-off)
   (x-off          :accessor x-off          :initarg :x-off)
   (y-off          :accessor y-off          :initarg :y-off)
   (target-positions :accessor target-positions :initarg :target-positions)
   (target-shells  :accessor target-shells  :initform nil)
   (foreign-shells :accessor foreign-shells :initform nil)
   (failed-shells  :accessor failed-shells  :initarg :failed-shells)))

(defginafun make-drag-command (
            document "the document modified by the command"
            source "a widget that receives motion events"
            rel-x "the relative x position where the command starts"
            rel-y "the relative y position where the command starts"
            target-list "a list of widget-value pairs belonging to this doc"
       &key (x-off 0) "x offset between shell and source"
            (y-off 0) "y offset between shell and source"
            (received-from nil)
            (cursor :dotbox)
            (shell-to-move nil)
            (transfer-value nil) "the value to transfer to target"
	    (class 'drag-command)
            (initargs nil)
       &aux cmd own-windows own-toplevels)
  (description "create a drag-command object"
	       :constructor-for-class drag-command
	       :called-by-application :sometimes
	       :called-by-gina nil)

  ;; target list must be non-empty
  (multiple-value-setq (target-list own-windows own-toplevels)
                       (sort-target-list target-list))
  (setq cmd (make-command document
                 :immediately-submit nil
                 :class class 
		 :initargs (more-initargs
			    :failed-shells own-windows
			    :received-from received-from
                            :source-x rel-x :source-y rel-y
			    :last-x rel-x :last-y rel-y
			    :x-off x-off :y-off y-off
                            :target-list target-list
                            :target-positions (get-target-positions target-list)
			    :transfer-value transfer-value
                            :shell-to-move (when (not received-from) 
					     shell-to-move))))
  ;(format t "~s~%~s~%" (target-list cmd) (target-positions cmd))

  (multiple-value-bind (x-orig y-orig) (root-coordinates source)
    (setf (global-x-off cmd) x-orig)
    (setf (global-y-off cmd) y-orig)
    (when (shell-to-move cmd)
      (decf (global-x-off cmd) (- rel-x x-off))
      (decf (global-y-off cmd) (- rel-y y-off))))

  ;; register the mouse command at the application
  (setf (active-mouse-command document) cmd)

  ;; change the cursor for the feedback phase
  (unless received-from
    (when (not (numberp cursor))
      ;; check if cursor has already been used in this application
      (let ((cursor-keyword cursor))
        (setq cursor (gethash cursor-keyword (cursor-table *application*)))
        (when (not cursor)
          ;; cursor used for the first time: create and store in hashtable
          (setq cursor (xtk:create-font-cursor cursor-keyword))
          (setf (gethash cursor-keyword (cursor-table *application*)) cursor))))
    (xtk:change-grab-cursor cursor))
  
  ;; make sure that the current mouse-position is processed
  (motion-notify cmd rel-x rel-y)

  ;; collect the positions of possible target shells in source-relative
  ;; coordinates
  (if received-from
      ;; receiver needs only its own shells
      (setf (target-shells cmd)
	    (reverse
                 (loop for child in own-windows
		       for toplevel in own-toplevels
		       with source-win = (get-clx-window source)
                       collect (corner-array toplevel source-win child))))
      ;; sender must collect all windows on screen
      (setf (target-shells cmd)    
            (reverse 
                 (get-all-windows (xlib:screen-root (screen *application*))
                                  (xlib:find-atom *display* (atom-name cmd))
				  (get-clx-window source)
				  ;(xlib:screen-root (screen *application*))
				  own-windows))))
  ;(format t "~{~s~%~}" (target-shells cmd))


  cmd)

(defmethod motion-notify ((cmd drag-command) x y
			  &key (finished nil) 
                          &aux (error-free nil)
                               target-win tg-x tg-y old-target old-value)
  "react to pointer motion"
  (declare (special *display*))
  ;;(format t "motion notify ~a ~a " x y)
  (unwind-protect
  (with-slots (source-x source-y target-x target-y current-target current-value
               mouse-already-moved hysteresis lazy-tracking) cmd
    ;; check for hysteresis
    (when (or mouse-already-moved
	      (>= (abs (- source-x x)) hysteresis)
	      (>= (abs (- source-y y)) hysteresis))

      ;; move the shell as feedback
      ;; must be before get-target-window is called, else the shell
      ;; window will be reported as target
      (when (shell-to-move cmd)
        (move (shell-to-move cmd)
              (+ (global-x-off cmd) x)
              (+ (global-y-off cmd) y))
        (when (not mouse-already-moved)
          (pop-up (shell-to-move cmd)))
        (when finished
          (pop-down (shell-to-move cmd))))

      ;; determine new target
      (when (or (do-tracking cmd) finished)
        (multiple-value-setq (target-win tg-x tg-y)
             (identify-target-window (target-shells cmd) x y))

        (setq old-target current-target)
        (setq old-value current-value)

	;; if tg-x, tg-y are nil, identify-target reports type-error in CMU. cici.
	(when (not target-win) (setq tg-x 0) (setq tg-y 0))

        (multiple-value-setq (current-target current-value tg-x tg-y)
	    (identify-target cmd target-win tg-x tg-y))

        (when (and (not (received-from cmd)) (not current-target) target-win)
          (notify-foreign-shell cmd target-win tg-x tg-y))
      )
      ;; report the upper left corner as position, but use mouse
      ;; position as deciding point
      (when (and target-win (or (shell-to-move cmd) (received-from cmd)))
          (decf tg-x (- (source-x cmd) (x-off cmd)))
	  (decf tg-y (- (source-y cmd) (y-off cmd))))

      (when (do-tracking cmd)
        (when (and mouse-already-moved old-target
                   (or finished
                      (not lazy-tracking)
                      (not (eql old-target current-target))))
            (draw-target-feedback cmd 
                                  old-target target-x target-y old-value
                                  :clear t))

        (when (or (not (equal current-target old-target))
                  (and current-target (not lazy-tracking)))
            (track-target cmd current-target tg-x tg-y current-value))

        (when (and (current-target cmd) (not finished)
                   (not (and lazy-tracking (eql old-target current-target))))
            (draw-target-feedback cmd 
                                  current-target tg-x tg-y current-value
                                  :clear nil))
      )

      (setq target-x tg-x target-y tg-y)
      (setf (last-x cmd) x (last-y cmd) y)
      (setq mouse-already-moved t))

    (setq error-free t))
  ;; cleanup: cancel command in case of an programmer error
  (unless error-free
    (setf (active-mouse-command (document cmd)) nil)
    (when (shell-to-move cmd) (pop-down (shell-to-move cmd))))
  ))

(defmethod draw-feedback ((cmd drag-command) x y &key (clear nil))
  "may be called during expose"
  (draw-target-feedback cmd (current-target cmd) x y (current-value cmd) 
                        :clear clear))

(defginamethod draw-target-feedback ((cmd drag-command) 
               widget "one of the widgets from target-list"
               rel-x "x position relative to widget"
               rel-y "y position relative to widget"
               value "corresponding value from target-list"
          &key (clear nil))
  (description
    "draw feedback in the target"
    :called-by-gina "on pointer motion when over target widget"
    :override "to define individual feedback drawing with xor"
    :default-version :do-nothing)
  (declare (ignore widget rel-x rel-y value clear))
  nil)

(defginamethod track-target ((cmd drag-command) 
               widget "one of the widgets from target-list, or nil if outside"
               rel-x "x position relative to widget (or nil)"
               rel-y "y position relative to widget (or nil)"
               value "corresponding value from target-list")
  (description
    "track mouse motion"
    :called-by-gina
    "on pointer motion when over target widget, or when just left target"
    :override "to execute individual mouse tracking"
    :default-version :do-nothing)
  (comment "The positions are inaccurate if lazy-tracking is enabled.")
  (comment "Outside a target it is called only once with nil parameters,"
           "until a new target is entered, regardless of lazy-tracking.")
  (declare (ignore widget rel-x rel-y value))
  nil)

(defmethod notify-foreign-shell ((cmd drag-command) 
				 target-win target-x target-y)
  (if (member target-win (foreign-shells cmd))
      ;; notify target of mouse motion 
      (xlib:send-event target-win :motion-notify 
                   (xlib:make-event-mask :button-motion)
                   :root (xlib:drawable-root target-win) ;; must be present!
                   :event-window target-win
                   :window target-win :x target-x :y target-y
                   :time 0 :state 0 :root-x 0 :root-y 0)
      ;; else try to identify target for the first time
      (unless (member target-win (failed-shells cmd))
          (push target-win (foreign-shells cmd))
          ;(format t "Yeah: ~s~%" target-win)
          ;; create a new command in the target app
          (setf *drag-n-drop-transfer-value* 
                (list (transfer-value cmd) 
                      (x-window (main-shell (document cmd)))
                      ;(get-clx-window (main-shell (document cmd)))
                      ))
          (xlib:send-event target-win :button-press 
               (xlib:make-event-mask :button-press)
               :time (xlib:find-atom *display* (atom-name cmd))
               :root (xlib:drawable-root target-win) ;; must be present!
               :event-window target-win :window target-win
               :x (- (source-x cmd) (x-off cmd))
	       :y (- (source-y cmd) (y-off cmd))  ;; to init in foreign
	       :state 0 :code 0 :root-x 0 :root-y 0
	       ))))

(defun atom-name (cmd)
  (string-upcase (substitute #\_ #\- (symbol-name (protocol-id cmd)))))

(defmethod button-release ((cmd drag-command) x y)
  "button goes up => track phase is over"
  (declare (ignore x y))
  ;;(format t "button release ~a ~a " x y)
  (with-slots (call-doit mouse-already-moved current-target) cmd
    ;; If the pointer is moved one pixel on a widget border, the MIT server
    ;; does not generate a motion-notify. This is the only case where the
    ;; button-release position differs from the last motion position. In this
    ;; case, use the last motion position to avoid misleading feedback.
    ;; old version:
    ;;(motion-notify cmd x y :finished t)
    (motion-notify cmd (last-x cmd) (last-y cmd) :finished t)

    ;; deinstall the mouse-command
    (setf (active-mouse-command (document cmd)) nil)

    ;; notify foreign shells
    (release-foreign-shells cmd)

    (if (and call-doit mouse-already-moved current-target (executable cmd))
      ;; execute the commands effect
      (submit cmd)
      ;; else: give application a chance to react when not submitted
      (not-submitted cmd))))

(defmethod release-foreign-shells ((cmd drag-command))
  "send button release to all foreign shells that were touched"  
  (unless (received-from cmd)
    (loop for corners in (target-shells cmd)
          for shell = (aref corners 4)
          when (member shell (foreign-shells cmd))
          do (let ((rx (- (last-x cmd) (aref corners 0) (aref corners 5)))
                   (ry (- (last-y cmd) (aref corners 1) (aref corners 6))))
               ;; send one motion to set last-x and last-y
               (xlib:send-event shell :motion-notify 
                 (xlib:make-event-mask :button-motion)
                 :root (xlib:drawable-root shell) ;; must be present!
                 :event-window shell
                 :window shell :x rx :y ry
		 :time 0 :state 0 :root-x 0 :root-y 0)
               (xlib:send-event shell :button-release 
                 (xlib:make-event-mask :button-release)
                 :root (xlib:drawable-root shell) ;; must be present!
                 :state 256     ;; is checked by handle-button-release!
                 :event-window shell
                 :window shell :x rx :y ry
		 :time 0
		 :state 0 :code 0 :root-x 0 :root-y 0))
   )))

(defun get-all-windows (window protocol-atom source own-shells
                        &optional (top-win nil) 
			&aux children map-state override)
  "collect all toplevel-shells that accept drag"
  (declare (special *application* got-the-first-one))
  (unless top-win (setq got-the-first-one nil))
  (setq children (xlib:query-tree window))
  (loop for normal-child in children
        for toplevel = (if top-win top-win normal-child)
        for child-hash = (gethash normal-child (drag-window-hash *application*))
        for shortcut = (and child-hash (not (symbolp child-hash)))
        for child = (if shortcut child-hash normal-child)
        unless (eq child-hash :never)
        append
           (progn
	     (unless (or top-win child-hash)   ;; mark toplevel of subtree
	       (mark-drag-window normal-child :empty))
             (xlib:with-state (child)
                (setq map-state (xlib:window-map-state child))
                (setq override (xlib:window-override-redirect child)))
             (if (eq override :on)
                (mark-drag-window toplevel :never)
                (when (eq map-state :viewable)
                  (if (and (not top-win) (eq child-hash :empty))
		    (when got-the-first-one 
		      (list (corner-array toplevel source nil)))
                    (let ((name (if shortcut 
			  	    :ok 
				    (xlib:get-property child :wm_name))))
                      (if name
                        (multiple-value-bind (supported-commands type)
			              (xlib:get-property child "__GINA_CMDS")
                          (mark-drag-window toplevel child)
		          (if (or (member child own-shells)
	                          (and type
				       (member protocol-atom 
					       supported-commands)))
			      (progn
				(setq got-the-first-one t)
                                (list (corner-array toplevel source child)))
			      (when got-the-first-one
				(list (corner-array toplevel source nil)))))
		        (get-all-windows child protocol-atom source 
				         own-shells toplevel)))))))))

(defun mark-drag-window (window &optional (name-window :never))
  "mark toplevel window in hash table"
  (declare (special *application*))
  (setf (gethash window (drag-window-hash *application*)) name-window)
  nil)

(defun corner-array (toplevel source window)
  "produce geometry information array for a window relative to source"
  (multiple-value-bind (source-x source-y)
          (xlib:translate-coordinates toplevel 0 0 source)
    (let ((new-array (make-array 7)))
      (setf (svref new-array 0) source-x)
      (setf (svref new-array 1) source-y)
      (xlib:with-state (toplevel)
       (setf (svref new-array 2) (+ source-x (xlib:drawable-width toplevel)))
       (setf (svref new-array 3) (+ source-y (xlib:drawable-height toplevel))))
      (setf (svref new-array 4) window)
      (when window
	(multiple-value-bind (win-x win-y)
	    (xlib:translate-coordinates window 0 0 toplevel)
	  (setf (svref new-array 5) win-x)
	  (setf (svref new-array 6) win-y)))
      new-array)))

(defun identify-target-window (array-list x y)
  "find whether a target window in the ordered array matches coordinates"
  (declare (fixnum x y))
  (loop for a in array-list
        for window = (svref a 4)
        when (and (>= x (the fixnum (svref a 0)))
                  (>= y (the fixnum (svref a 1)))
                  (< x (the fixnum (svref a 2)))
                  (< y (the fixnum (svref a 3))))
          do (return (when window
		 (xlib:with-state (window)
		   (when (and (>= x (+ (the fixnum (svref a 0))
				       (the fixnum (svref a 5))))
			      (>= y (+ (the fixnum (svref a 1))
				       (the fixnum (svref a 6))))
			      (< x (+ (the fixnum (svref a 0))
				      (the fixnum (svref a 5))
				      (xlib:drawable-width window)))
			      (< y (+ (the fixnum (svref a 1))
				      (the fixnum (svref a 6))
				      (xlib:drawable-height window))))
		     (values window
			     (- x (the fixnum (svref a 0)) 
				(the fixnum (svref a 5)))
			     (- y (the fixnum (svref a 1))
				(the fixnum (svref a 6))))))))))

(defun sort-target-list (target-list &aux new-list)
  "sort the targets according to the priority of their top-shells"
  (unless (listp target-list)
    (setq target-list (list target-list)))
  (let ((used-shells nil)
        (their-windows nil)
        (their-prios nil)
        (root nil)
        (root-children nil)
        (their-root-children nil))
    (setq new-list 
       (loop for raw-target in target-list
	     for target = (if (listp raw-target) 
			      raw-target
			      (list raw-target nil))
             for shell = (enclosing-shell (first target))
             do (unless (member shell used-shells)
                  (push (get-clx-window shell) their-windows)
                  (unless root
                    (setq root (xlib:drawable-root (first their-windows)))
                    (setq root-children (xlib:query-tree root)))
                  (push (root-parent (first their-windows) root)
			their-root-children)
                  (push (position (first their-root-children) root-children)
			their-prios)
                  ;(push (root-prio (first their-windows) root root-children)
                  ;      their-prios)
                  (update-slots shell)
                  (push shell used-shells))
             when (eql (xlib:window-map-state 
                           (nth (position shell used-shells) their-windows))
                       :viewable)
             collect (cons
                 (nth (position shell used-shells) their-windows) target)))
    (setq new-list (sort new-list #'>
                         :key (coerce `(lambda (triple) 
			         (nth (position (first triple) 
						',their-windows) 
				      ',their-prios))
				      'function)))
    (values new-list their-windows their-root-children)))

(defun root-parent (shell-window root)
  "parent that is direct root child"
  (multiple-value-bind (children parent) (xlib:query-tree shell-window)
    (declare (ignore children))
      (if (eql parent root)
          shell-window
          (root-parent parent root))))

(defun get-target-positions (target-list)
  "get list of rectangles and offsets relative to the respective shells"
  (loop for target in target-list
        for widget = (second target)
        do (update-slots widget)
        collect (position-in-parent (group-widget-id widget) 0 0 30000 30000
                                    (group-widget-id (enclosing-shell widget))
                                    0 0)
  ))

(defun position-in-parent (parent-id x y width height shell-id rel-x rel-y
                           &aux size next-parent)
  ;(format t "~d ~d ~d ~d ~d ~d :" parent-id x y width height shell-id)
  (if (eql shell-id parent-id)
      (let ((result (make-array 6)))
        (setf (svref result 0) x)
	(setf (svref result 1) y)
	(setf (svref result 2) (+ x width))
	(setf (svref result 3) (+ y height))
	(setf (svref result 4) rel-x)
	(setf (svref result 5) rel-y)
	result)
      (progn
        (setq size (xtk:get-values parent-id :x :y :width :height))
	(setq next-parent (xtk:get-parent parent-id))
	(when (eq next-parent shell-id)
	  ;; the child of the shell contains the position of the shell
	  ;; in Motif 1.2. Aaaaarghhh!
	  (setq size (cons 0 (cons 0 (cddr size)))))
        ;(format t "~s~%" size)
        (let* ((px (max 0 (+ (first size) x)))
               (py (max 0 (+ (second size) y))))
          (position-in-parent next-parent px py
                              (- (min (+ x width) (third size)) (max x 0))
			      (- (min (+ y height) (fourth size)) (max y 0))
                              shell-id 
			      (+ rel-x (first size)) 
			      (+ rel-y (second size)))))))

(defun identify-target (cmd target-win tg-x tg-y)
  (declare (fixnum tg-x tg-y))
  (when target-win
    (loop for possible-target in (target-list cmd)
          for target-position in (target-positions cmd)
          when (and (eql (first possible-target) ;; the shell's window
			 target-win)
		    (not (or (< tg-x (the fixnum (svref target-position 0)))
			     (< tg-y (the fixnum (svref target-position 1)))
			     (>= tg-x (the fixnum (svref target-position 2)))
			     (>= tg-y (the fixnum (svref target-position 3))))))
          do (return-from identify-target
	                  (values (second possible-target) 
				  (third possible-target) 
				  (- tg-x (svref target-position 4)) 
				  (- tg-y (svref target-position 5))))))
  (values nil nil tg-x tg-y))
                 
;(defun get-target-window (source-window x y 
;                &optional (target-win (xlib:drawable-root source-window)))
;  "get a shell window from coordinates relative to source window"
;  ;(format t "Target: ~x~%" (xlib:window-id target-win))
;  (multiple-value-bind (dx dy dwin)
;      (xlib:translate-coordinates source-window x y target-win)
;    (when (and dwin (not (zerop (xlib:window-id dwin))))
;        (if (xlib:get-property dwin :wm_name :result-type 'string)
;            (values dwin dx dy)
;            (get-target-window source-window x y dwin)))))

(defginamethod not-submitted ((cmd drag-command))
  (description
    "react to the fact that command was not submitted"
    :called-by-gina
    "on button-release, if no target hit or call-doit is nil"
    :override "to define what happens instead of submitting the command"
    :default-version :do-nothing)
    ;; new in Gina 2.2: will be called also when (not (executable cmd))
  nil)






