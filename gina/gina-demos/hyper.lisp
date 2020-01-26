;;;-*-Mode:LISP;Syntax: Common-Lisp;Package: hyper;Base:10-*-
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
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS WHETHER IN AN ACTION
;;; OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
;;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;;
;;; Authors: Project GINA (spenke@gmd.de)
;;;          P.O. Box 1316
;;;          D-5205 Sankt Augustin 1
;;;

(in-package :GINA)
(defginapackage :hyper)
(in-package :hyper)

(setq *sccs-id* "@(#)hyper.lisp	1.12  11/10/92")
 
;; "Hyper-GINA" is a demo application for GINA
;; It combines concepts from the GINA Interface Builder and from Hypercard
;; Interfaces can be build and immediately be run
;; Lisp code is directly attached to callback and interpreted
;;
;; Facilities of "Hyper-GINA":
;;
;; - mode can be selected on the left similar to graphic editor
;; - in most modes, "elements" can be drawn into the main view
;; - elements are widgets like in the interface builder
;; - elements can be moved around and resized
;; - elements can be operated, but initially have no callbacks
;; - Lisp code can be entered into code elements
;; - pressing the little black button in the upper right corner of the code
;;   element executes the code
;; - the border of each element contains the name of an element and one or
;;   more outlets (at least "value")
;; - name and outlets can be dragged onto other elements using the mouse.
;;   The semantics of such a dragging operation depends on the class of
;;   elements involved. This is implemented by multimethods.
;;   As feedback the operation which will take place if the mouse button
;;   is released is displayed in the message area.
;; - Dragging the name of a code into an element will set the callback
;;   of the element.
;; - Callbacks have no parameters
;; - within code, other elements can be referenced by their name.
;;   Internally element names are replaced by (value <denoted-element>)
;;   before the code is executed. An exception is the view-element: its
;;   name is replaced by (widget <view-element>).
;;   Most elements have an accessor VALUE. The type of the VALUE depends on
;;   the element-type. For example, (setf number3 "25") will convert the
;;   string before storing it in the number-element.
;;   If the default-translation of an element-name is not appropriate you
;;   can explicitly specify (widget <element>) to access the motif-widget
;;   contained in the element (to set its colour for example). For 
;;   list-elements you can write (items list5) to read and write the item-list
;;   See method parse for details of the translation process.
;; - dragging the name of an element into a code inserts the element name
;; - For buttons, by dragging the name of a code into it the 
;;   activate-callback is set
;; - For string, text, number, scale and list, the value-changed-callback
;;   is set by dragging the name of a code into it.
;; - dragging a button name into a list means that the callback of the button
;;   is executed when a list item is double-clicked
;; - using the value-changed-callback of a number-element the typical
;;   spreadsheet effect can be obtained: editing a value immediately may update
;;   a derived value
;; - for views setting the callback means defining its draw method. The name
;;   of the view can be used as the first parameter of methods like
;;   draw-rectangle or force-redraw
;; - the callback of a clock element is executed every second
;; - dragging the value outlet to another element means assigning the value
;;   (including necessary conversions)
;; - dragging the items outlet copies the item list
;; - dragging something onto the items outlet sets the item list
;; - setting the value of a button means setting its label-string
;; - code elements are executed when a stack is openend if the init flag
;;   at the upper right corner is set
;; - you can have several cards: see new card, next card, previous card 
;;   within code you can write (goto 2) to select the second card
;; - try the adjust commands to find out their meaning!! 
;; - in user mode elements cannot be moved or selected
;; - the function play (e.g (PLAY "gong" :volume 30)) plays sounds in
;;   /usr/demo/SOUND/sounds/* on SUN workstations
;;   *all-sounds* is a list of all available sounds
;; - *all-colors* is a list of colors for Motif widgets

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class hyper-application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass hyper-application (application)
  (;; overrides
   (name                 :initform "Hyper-GINA"             :allocation :class)
   (document-type        :initform 'hyper-document :allocation :class)
   (signature            :initform "hyper"                 :allocation :class)
   (file-type            :initform "stack"                 :allocation :class))
  (:documentation "a simple hypercard-like application"))

(defun make-hyper-application ()
  "start the hyper-application"
  (make-application :class 'hyper-application))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class hyper-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass hyper-document (document)
  (;; overrides
   (shell-width  :initform 600)
   (shell-height :initform 500)
   (message :accessor message)
   (cards :accessor cards :initform (list (make-card)))
   (current-card :accessor current-card)
   (element-table :accessor element-table 
		  :initform (make-hash-table :test #'equal))
   (element-counter :accessor element-counter :initform 0))
  (:documentation "application dependent document type"))

(defmethod initialize-instance :after ((doc hyper-document) &rest initargs)
  "initialize a just created instance"
  (declare (ignore initargs))
  (setf (current-card doc) (first (cards doc))))

(defmethod create-windows ((doc hyper-document)
			   &aux scroller form radio-group)
  "create the windows belonging to this document"
  (with-slots (main-shell main-view) doc
    (setq main-shell (make-document-shell doc))
    ;; create a form containing the different parts
    (setq form      (make-form main-shell))
    (setf (message doc) (make-label form "Ready"))
    (setq scroller  (make-scroller form))    
    (setq main-view (make-hyper-view scroller doc))
    (setq radio-group
	  (make-radio-button-group form
				   '(("User" :user-mode)
				     ("Select" :select-mode)
				     ("String" :string-mode)
				     ("Text"   :text-mode)
				     ("Number" :number-mode)
				     ("Button" :button-mode)
				     ("List"   :list-mode)
				     ("Scale"  :scale-mode)
				     ("View"   :view-mode)    
				     ("Code"   :code-mode)
				     ("Result" :result-mode)
				     ("Clock" :clock-mode)
				     )
                           :value-changed-callback
			   (make-callback 'set-mode main-view)
			   :button-resources '(:shadow-thickness 3)
			   ))
    (set-mode main-view :user-mode nil)
    
    ;; layout definition:
    (define-form-constraint (message doc)
			    :top-attachment :form
			    :left-attachment :form
			    :right-attachment :form)
    (define-form-constraint radio-group
			    :top-attachment :widget
			    :top-widget (message doc)
			    :left-attachment :form)
    (define-form-constraint scroller
			    :left-attachment :widget :left-widget radio-group
			    :top-attachment :widget
			    :top-widget (message doc)
			    :right-attachment :form
			    :bottom-attachment :form)
    
    ;; add some menu commands
    (add-menu-command (main-menu main-shell)
		      "Edit" "Clear All" 
		      (make-callback 'make-clear-all-command doc))
    (add-menu-command (main-menu main-shell)
		      "Edit" "Clear" (make-callback 'make-clear-command doc))
    (add-menu-command (main-menu main-shell)
		      "Card" "New Card" (make-callback 'new-card doc))
    (add-menu-command (main-menu main-shell)
		      "Card" "Next Card" (make-callback 'next-card doc))
    (add-menu-command (main-menu main-shell)
		      "Card" "Previous Card" 
		      (make-callback 'previous-card doc))
    (add-menu-command (main-menu main-shell)
		      "Adjust" "Left" 
		      (make-callback 'make-adjust-command doc 
				     "Adjust Left" :left))
    (add-menu-command (main-menu main-shell)
		      "Adjust" "Top" 
		      (make-callback 'make-adjust-command doc 
				     "Adjust Left" :top))
    (add-menu-command (main-menu main-shell)
		      "Adjust" "Right" 
		      (make-callback 'make-adjust-command doc 
				     "Adjust Right" :right))
    (add-menu-command (main-menu main-shell)
		      "Adjust" "Bottom" 
		      (make-callback 'make-adjust-command doc 
				     "Adjust Bottom" :bottom))
    (add-menu-command (main-menu main-shell)
		      "Adjust" "Width" 
		      (make-callback 'make-adjust-command doc 
				     "Adjust Width" :width))
    (add-menu-command (main-menu main-shell)
		      "Adjust" "Height" 
		      (make-callback 'make-adjust-command doc 
				     "Adjust Height" :height))
    (add-menu-command (main-menu main-shell)
		      "Adjust" "Size" 
		      (make-callback 'make-adjust-command doc 
				     "Adjust Size" :height :width))
    (add-menu-command (main-menu main-shell)
		      "Adjust" "Horizontal Spacing" 
		      (make-callback 'make-adjust-command doc 
				     "Adjust Horizontal Spacing" :hspacing))
    (add-menu-command (main-menu main-shell)
		      "Adjust" "Vertical Spacing" 
		      (make-callback 'make-adjust-command doc 
				     "Adjust Vertical Spacing" :vspacing))
    (add-menu-command (main-menu main-shell)
		      "Adjust" "Row" 
		      (make-callback 'make-adjust-command doc 
				     "Adjust Row" :top :hspacing))
    (add-menu-command (main-menu main-shell)
		      "Adjust" "Column" 
		      (make-callback 'make-adjust-command doc 
				     "Adjust Column" :left :vspacing))
    (set-motif-resources (widget (find-menu-entry (main-menu main-shell) 
						  "Adjust" "Column"))
                     :accelerator "<Key>F5"
                     :accelerator-text "F5")
    (set-motif-resources (widget (find-menu-entry (main-menu main-shell) 
						  "Edit" "Undo"))
                     :accelerator "Alt<Key>u"
                     :accelerator-text "Alt+u")
    (set-motif-resources (widget (find-menu-entry (main-menu main-shell) 
						  "Edit" "Redo"))
                     :accelerator "Alt<Key>r"
                     :accelerator-text "Alt+r")
    ))

(defmethod report ((doc hyper-document) message)
  "show message in the window"
  (setf (label-string (message doc)) message))

(defmethod write-to-stream ((doc hyper-document) stream)
  "write the document to the specified stream"
  ;; write number of cards
  (format stream "~d~%" (length (cards doc)))

  (format stream "~d~%" (element-counter doc))
  
  ;; write all cards
  (loop for card in (cards doc)
	do (write-to-stream card stream)))

(defmethod read-from-stream ((doc hyper-document) stream &aux nr-of-cards) 
  "read the document from the specified stream"
  ;; deinstall existing objects
  (loop for object in (view-objects (main-view doc))
	do (deinstall object))
  (setf (cards doc) nil)
  
  ;; read number of cards
  (setq nr-of-cards (read stream))
  (setf (element-counter doc) (read stream))
  (setf (element-table doc) (make-hash-table :test #'equal))

  ;; read all cards
  (let ((*document* doc))
    (declare (special *document*))
    (setf (cards doc)
      (loop repeat nr-of-cards
       as card = (make-card)
       collect card
       do (read-from-stream card stream))))
  (setf (current-card doc) (first (cards doc)))
  (enter (current-card doc) (main-view doc))
  )

(defmethod destroy-windows :after ((doc hyper-document))
  "destroy all timers in clock elements"
  (loop for card in (cards doc)
    do (loop for element in (elements card)
	when (eq (gina::clos-class-of element) 'clock-element)
	do (xtk:destroy-timer (timer-id element))
	 )))

(defmethod new-card ((doc hyper-document) &aux card)
  "create a new card an select it"
  (setq card (make-card))
  (setf (cards doc) (append (cards doc) (list card)))
  (leave (current-card doc))
  (enter card (main-view doc))
  (setf (current-card doc) card))

(defmethod previous-card ((doc hyper-document) &aux prev-card)
  "select another card"
  (if (eq (current-card doc) (first (cards doc)))
      (xlib:bell *display*)
      (progn 
	(setq prev-card 
	  (nth (1- (position (current-card doc) (cards doc))) (cards doc)))
	(leave (current-card doc))
	(enter prev-card (main-view doc))
	(setf (current-card doc) prev-card))))

(defmethod next-card ((doc hyper-document) &aux next-card)
  "select another card"
  (if (eq (current-card doc) (car (last (cards doc))))
      (xlib:bell *display*)
      (progn 
	(setq next-card 
	  (nth (1+ (position (current-card doc) (cards doc))) (cards doc)))
	(leave (current-card doc))
	(enter next-card (main-view doc))
	(setf (current-card doc) next-card))))

(defvar *error-reported*)
(setq *error-reported* nil)
(defmacro reporting-errors ((doc) &body body)
  `(let* ((result-list
	   (multiple-value-list
	       (gina::ignore-errors
		,(append '(progn) body))))
	  (error
	   (second result-list)))
     (setq *error-reported* (when error t))
     (when error 
       (play "laugh" :volume 40)
       (warning-dialog (format nil "~a" error)
		       :document ,doc
		       :dialog-title "Error:"))
     (when (not error)
       (first result-list))))
 
(defmethod parse ((doc hyper-document) code &aux expression)
  "read string as code and replace special identifiers"
  (reporting-errors (doc)
  (unwind-protect
      (progn
	(in-package :hyper)
	 
	(setq expression 
	  (read-from-string (unreplace-returns (value (widget code)))))

	(loop for element in (elements (current-card doc))
	      do 
	  (setq expression 
	    (subst `(value  ',element) 
		   `(value  ,(name-symbol element)) expression 
		   :test #'equal))
	  (setq expression 
	    (subst `(widget ',element) 
		   `(widget ,(name-symbol element)) expression
		   :test #'equal))
	  (setq expression 
	    (subst `(items ',element) 
		   `(items ,(name-symbol element)) expression
		   :test #'equal))
	  (setq expression
	    (if (member (typestring element) '("View") :test #'equal)
		(subst `(widget ',element) (name-symbol element) expression)
	        (subst `(value  ',element) (name-symbol element) expression))))
	expression)
    (in-package :gina))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class card
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass card ()
  ((elements :accessor elements :initform nil)))

(defun make-card ()
  (make-object 'card nil))


(defmethod write-to-stream ((card card) stream)
  "write the card to the specified stream"
  ;; write number of objects
  (format stream "~d~%" (length (elements card)))
  
  ;; write all objects
  (loop for element in (elements card)
	do (write-to-stream element stream)))


(defmethod read-from-stream ((card card) stream &aux nr-of-objects) 
  "read the card from the specified stream"
  
  ;; read number of objects
  (setq nr-of-objects (read stream))

  ;; read positions of objects
  (loop repeat nr-of-objects
   as class = (read stream)
   as new-element = (read-from-stream class stream)
   do 
    (setf (x-pos new-element)(read stream))
    (setf (y-pos new-element)(read stream))
    (push new-element (elements card)))

  ;; replace names of default-buttons by reference
  (loop for elem in (elements card)
   when (and (eq (gina::clos-class-of elem) 'list-element)
             (default-button-element elem))
    do ;; we do not use the accessor here because it is defined too late!!
       (setf (slot-value elem 'default-button-element)
	 (gethash (default-button-element elem) 
		  (element-table (document elem))))
       (setf (default-action-button (widget elem))
	 (widget (default-button-element elem))))
  
  ;; execute initializations
  (loop for elem in (elements card)
   when (and (eq (gina::clos-class-of elem) 'code-element)
             (is-init elem))
    do (setf (current-card (document elem)) card) ;; for parser !!
       (evaluate elem))
	 
  )

(defmethod leave ((card card))
  (loop for elem in (elements card)
	do (deinstall elem)))

(defmethod enter ((card card) view)
  (loop for elem in (elements card)
	do (install elem view (x-pos elem) (y-pos elem))))

(defun goto (card-number &aux card)
  "jump to another card within current document"
  (declare (special *document*))
  (when (not (boundp '*document*))
    (format t "Cannot jump, *document* unbound!!~%"))
  (setq card (nth (1- card-number) (cards *document*)))
  (when card
    (leave (current-card *document*))
    (enter card (main-view *document*))
    (setf (current-card *document*) card)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class adjust-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass adjust-command (command)
  (;; overrides
   (name :initarg :name :allocation :instance)
   (undoable :allocation :instance)
   (causes-change :allocation :instance)
   ;; instance-parameters
   (adjustments :initarg :adjustments)
   ;; instance variables
   (old-coordinates :accessor old-coordinates 
    :documentation "x,y,width and height of objects before adjust")
   (adjusted-elements :accessor adjusted-elements :initform nil))
  (:documentation "a command to adjust selected objects"))

(defun make-adjust-command (document name  &rest adjustments)
  "create a new command object with appropriate parameters"
  (make-command document
		:class 'adjust-command
		:initargs `(:adjustments ,adjustments
			    :name ,name)))

(defmethod doit ((cmd adjust-command) &aux space)
  "align selected elements"
  (with-slots (document adjustments adjusted-elements old-coordinates) cmd
  (when (not adjusted-elements)
    (setq adjusted-elements
      (loop for elem in (elements (current-card document))
       when (selected elem) collect elem))

    ;; determine top/leftmost element
    (setq adjusted-elements
      (sort adjusted-elements 
	    #'(lambda (a b) 
		(< (+ (x-pos a) (y-pos a)) (+ (x-pos b) (y-pos b))))))
    ;; collect old values
    (setq old-coordinates
      (loop for elem in adjusted-elements
       collect (list (x-pos elem) (y-pos elem) (width elem) (height elem))))
    (when (not adjusted-elements)
      ;; nothing to undo
      (setf (undoable cmd) nil)
      (setf (causes-change cmd) nil)))

  (when (member :left adjustments)
    ;; align x position to top/leftmost element
    (loop for elem in (cdr adjusted-elements)
       do (move elem (x-pos (first adjusted-elements))
		     (y-pos elem))))
  (when (member :top adjustments)
    ;; align y position according to top/leftmost element
    (loop for elem in (cdr adjusted-elements)
       do (move elem 
		(x-pos elem)
		(y-pos (first adjusted-elements)))))
  (when (member :right adjustments)
    ;; align x position to right of top/leftmost element
    (loop for elem in (cdr adjusted-elements)
       do (move elem (+ (x-pos (first adjusted-elements))
			(- (width (first adjusted-elements))
			   (width elem)))
		     (y-pos elem))))
  (when (member :bottom adjustments)
    ;; align y position according to bottom top/leftmost element
    (loop for elem in (cdr adjusted-elements)
       do (move elem 
		(x-pos elem)
		(+ (y-pos (first adjusted-elements))
		   (- (height (first adjusted-elements))
		      (height elem))))))
  (when (member :width adjustments)
    ;; align width according to top/leftmost element
    (loop for elem in (cdr adjusted-elements)
       do (resize elem 
		  (width (first adjusted-elements))
		  (height elem))))
  (when (member :height adjustments)
    ;; align height according to top/leftmost element
    (loop for elem in adjusted-elements
       do (resize elem 
		  (width elem)
		  (height (first adjusted-elements)))))

  (when (member :hspacing adjustments)
    ;; align horizontal spacing according to the 2 top/leftmost elements
    (when (> (length adjusted-elements) 2)
      (setq space (- (x-pos (second adjusted-elements))
		     (x-pos (first  adjusted-elements))
		     (width (first  adjusted-elements))))
      (loop 
       for elem in (cddr adjusted-elements)
       for prev in (cdr adjusted-elements)
       do (move elem
		(+ (x-pos prev) (width prev) space)
		(y-pos elem)))))

  (when (member :vspacing adjustments)
    ;; align vertical spacing according to the 2 top/leftmost elements
    (when (> (length adjusted-elements) 2)
      (setq space (- (y-pos (second adjusted-elements))
		     (y-pos (first  adjusted-elements))
		     (height (first  adjusted-elements))))
      (loop 
       for elem in (cddr adjusted-elements)
       for prev in (cdr adjusted-elements)
       do (move elem
		(x-pos elem)
		(+ (y-pos prev) (height prev) space)))))
))

(defmethod undoit ((cmd adjust-command))
  (with-slots (adjusted-elements old-coordinates) cmd
	 (loop for element in adjusted-elements
	       for (x y width height) in old-coordinates
	   do (move element x y)
	      (resize element width height))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class hyper-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass hyper-view (view)
  ((drawing-mode :accessor drawing-mode :initform :user-mode))
  (:documentation "a view with special reaction to clicks"))

(defun make-hyper-view (parent doc)
  (make-view parent :width 1000 :height 1000 :document doc :class 'hyper-view))

(defmethod button-press ((view hyper-view) code repetition x y)
  "react to button-press event in the window"
  (declare (ignore repetition))
  (case (drawing-mode view)
    (:select-mode  (make-object-selector view x y code))
    (:string-mode  (make-string-drawer view x y))
    (:text-mode    (make-text-drawer view x y))
    (:number-mode  (make-number-drawer view x y))
    (:button-mode  (make-button-drawer view x y))
    (:list-mode    (make-list-drawer view x y))
    (:scale-mode   (make-scale-drawer view x y))
    (:view-mode    (make-view-drawer view x y))
    (:code-mode    (make-code-drawer view x y))
    (:result-mode  (make-result-drawer view x y))
    (:clock-mode   (make-clock-drawer view x y))
    ))

(defmethod set-mode ((view hyper-view) new-mode old-mode)
  "depending on the palette, button-press is propagated to the objects or not"
  (with-slots (drawing-mode propagate-button-press) view
    (setq drawing-mode new-mode)
    (setq propagate-button-press
	  (not (member drawing-mode '(:user-mode))))
    (when (eq drawing-mode :user-mode)
      (loop for object in (view-objects view)
       do (setf (selected object) nil)))
    (when (member :user-mode (list new-mode old-mode))
      (force-redraw view))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass element (direct-manipulation-object)
  (;;overrides
   (facilities :initform :resizable :allocation :class
	       :documentation "elements can be moved, selected and resized")
   (outlines   :initform t          :allocation :class
	       :documentation 
	       "just move an outline of the object, not the object itself")

   ;; class variables
   (typestring :accessor typestring :initform "Element" :allocation :class)
   (outlets     :accessor outlets :initform '("value") 
		:allocation :class)
   (min-width   :accessor min-width  :initform 80 :allocation :class)
   (min-height  :accessor min-height :initform 45 :allocation :class)
   ;; instance variables
   (document :accessor document :initarg :document)
   (value :initform nil) ;; no ":accessor value" because of explicit methods!
   (name-string :accessor name-string)
   (name-symbol :accessor name-symbol)
   (widget :accessor widget :initform nil)
   ;;(trigger-list :accessor trigger-list :initform nil)
   (code :accessor code :initform nil))
  )

(defun make-element (width height doc 
		     &key (class 'element)
			  (name-string nil)
		     &aux new)
  (setq new (make-direct-manipulation-object
	     width height
	     :class class
	     :initargs (list :document doc)))
  (setf (width new)  (max (min-width new)  width))
  (setf (height new) (max (min-height new) height))

  (create-the-widget new) ;; method of each subclass
  (resize (widget new) (- (width new) 10) 
	  (- (height new) (+ 10 (* 10 (length (outlets new))))))

  (setf (name-string new)
    (or name-string
	(format nil "~a~d" (typestring new) (incf (element-counter doc)))))
  (setf (name-symbol new) 
    (intern (string-upcase (name-string new)) (find-package 'hyper)))
  (setf (gethash (name-string new) (element-table doc)) new)
  (setf (gethash (name-symbol new) (element-table doc)) new)
  new)

(defmethod install :after ((elem element) (view hyper-view) 
			   &optional (x 0) (y 0) &key (redraw t))
   (declare (ignore redraw))
   (move (widget elem) (+ 5 x) (+ 10 y))
   (manage (widget elem)))

(defmethod create-the-widget ((elem element))
  (setf (widget elem)
	(make-label (main-view (document elem)) "Element" :managed nil)))

(defmethod move :after ((elem element) new-x new-y &key (redraw t))
   (declare (ignore redraw))
   (move (widget elem) (+ 5 new-x) (+ 10 new-y)))

(defmethod resize :after ((elem element) new-width new-height &key (redraw t))
   (declare (ignore redraw))
   (when (widget elem)
     (resize (widget elem) (- new-width 10) 
	     (- new-height (+ 10 (* 10 (length (outlets elem))))))))

(defmethod reconfigure :after ((elem element) new-x new-y new-width new-height
			        &key (redraw t) &allow-other-keys)
  (declare (ignore redraw))
  (move (widget elem) (+ 5 new-x) (+ 10 new-y))
  (resize (widget elem) (- new-width 10) 
	  (- new-height (+ 10 (* 10 (length (outlets elem)))))))
    
(defmethod draw ((elem element) count x y width height)
  "draw element into the view"
  (declare (ignore x y width height))
  (when (and (zerop count)  ;; Ignore all but the last exposure event
             (not (eq :user-mode (drawing-mode (parent-view elem)))))
    (draw-rectangle elem 0 0 (width elem) (height elem))
    (xlib:with-gcontext ((gcontext (parent-view elem)) :font "6x10")
      (draw-glyphs elem 5 8 (name-string elem))
      ;;(draw-glyphs elem 10 (- (height elem) 1) "value")
      ;;(draw-glyphs elem (- (width elem) 15) (- (height elem) 1) "=>")
      (loop for outlet in (outlets elem)
	    for y from 1
	do (draw-glyphs elem 
			10 
			(- (height elem) 1 
			   (* (- (length (outlets elem)) y) 10))
			outlet))
      )))

(defmethod button-press :around 
	   ((obj element)
	    code ;;"which mouse button"
	    repetition ;;"1,2,3,.. for single, double, triple click"
	    x ;;"relative mouse position in the view-object"
	    y ;;"relative mouse position in the view-object"
	    )
     "define special handling for clicks into connect/assign handles"
     (declare (ignore repetition code))
     (cond ((and (< y 10) (> x 5) (< x 40))
	    (make-name-inserter (parent-view obj) 
				(+ (x-pos obj) x) (+ (y-pos obj) y)
				obj))
	   ((and (> y (- (height obj) (* 10 (length (outlets obj)))))
		 (= 0 (which-resize-handle obj x y))
                 (<= x 40))
	    (make-assigner (parent-view obj)  
			   (+ (x-pos obj) x) (+ (y-pos obj) y)
			   :from-element obj
                           :outlet 
                           (nth (1- (ceiling 
				     (- y (- (height obj) 
					     (* 10 (length (outlets obj)))))
				     10))
				(outlets obj))))
	   (t (call-next-method))))

(defmethod deinstall :before ((elem element) &key (redraw t))
   (declare (ignore redraw))
   (when (widget elem)
     (unmanage (widget elem)))
   )

(defmethod value ((elem element))
  "get the value of the widget within the element"
  (value (widget elem)))

(defmethod (setf value) (new-value (elem element))
  "set the value of the widget within the element"
  (setf (value (widget elem)) new-value))

(defmethod blink ((elem1 element) (elem2 element))
  "blink two elements simultaneously"
  (loop repeat 5
   do 
    (setf (selected elem1) nil)
    (setf (selected elem2) nil)
    (xlib:display-force-output *display*)
    (sleep 0.05)
    (setf (selected elem1) t)
    (setf (selected elem2) t)
    (xlib:display-force-output *display*)
    (sleep 0.05)))

(defmethod evaluate-code ((elem element))
  (when (code elem)
    (evaluate (gethash (code elem) (element-table (document elem))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class element-drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass element-drawer (mouse-down-command)
  (;; overrides
   (name :initform "Draw Element"   :allocation :class)
   (hysteresis :initform 5          :allocation :class)
   ;; class variables
   (element-constructor :accessor element-constructor :allocation :class
			:initform 'make-element)
   ;; instance-variables
   (new-element :accessor new-element :initform nil))
  (:documentation "a mouse-down-command to draw an element"))

(defun make-element-drawer (view x y &key (class 'element-drawer))
  (make-mouse-down-command (document view) view x y
			   :cursor :crosshair
			   :class class))

(defmethod constrain-mouse ((cmd element-drawer) x y)
  "make sure width and height of outline is > 0"
  (with-slots (start-x start-y) cmd
    (setq x (max x (1+ start-x)))
    (setq y (max y (1+ start-y))))
  (values x y))

(defmethod draw-feedback ((cmd element-drawer) x y &key clear)
  "draw rectangular feedback"
  (declare (ignore clear))
  (with-slots (start-x start-y view) cmd
    (xlib:with-gcontext ((gcontext view) :line-style :dash)
      (draw-rectangle view start-x start-y (- x start-x)  (- y start-y)))))

(defmethod doit ((cmd element-drawer))
  "install the specified element"
  (with-slots (start-x start-y last-x last-y view new-element 
		       element-constructor) cmd
    (when (not new-element)
      (setq new-element (funcall element-constructor 
				 (- last-x start-x) 
				 (- last-y start-y)
				 (document cmd))))
    (install new-element view start-x start-y)
    (push new-element (elements (current-card (document cmd))))
    ))

(defmethod undoit ((cmd element-drawer))
  "deinstall the specified element again"
  (with-slots (new-element) cmd
    (deinstall new-element)
    (setf (elements (current-card (document cmd)))
	  (delete new-element (elements (current-card (document cmd)))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class value-holder-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass value-holder-element (element)
  ())

;; this is the abstract superclass of all elements which have 
;; - value

;; it is used as a specializer for assign and connect


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class variable-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass variable-element (value-holder-element)
  ())

;; this is the abstract superclass of all elements which have 
;; - value
;; - setf value
;; - value-changed-callback

;; it is used as a specializer for assign and connect
(defmethod (setf code) :after (new-code (elem variable-element))
  "turn callback on and off when code is set and cleared"
   (if (or new-code);; (trigger-list elem)
       (setf (value-changed-callback (widget elem))
	 `(lambda (&rest ignore) (evaluate-code ',elem)))
     (setf (value-changed-callback (widget elem)) nil)))

'(defmethod (setf trigger-list) :after (new-list (elem variable-element))
  "turn callback on and off when trigger-list is set and cleared"
   (if (or new-list (code elem))
       (setf (value-changed-callback (widget elem))
	 `(lambda (&rest ignore) (evaluate-code ',elem)))
     (setf (value-changed-callback (widget elem)) nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class string-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+cmu (defclass text-element (variable-element) ()) ;; forward decl. cici.

(defclass string-element (text-element) ;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ((typestring :accessor typestring :initform "String" :allocation :class)
   ;;(value :initform "")
))

(defun make-string-element (width height doc &key (name-string nil))
  (make-element width height doc :class 'string-element
		:name-string name-string))

(defmethod create-the-widget ((string string-element))
  (setf (widget string)
	     (make-text (main-view (document string))
			;;:scroll-vertical nil
			;;:scroll-horizontal nil
			;;:value (slot-value string 'value)
			:managed nil
			:edit-mode :multi-line-edit)
			))

(defmethod write-to-stream ((string string-element) stream)
  "write textual representation of a string-element to stream"
  (with-slots (x-pos y-pos width height) string
    (format stream ":string-element ~d ~d ~s ~s ~s ~d ~d~%" 
	     width height 
	     (name-string string)
	     (value (widget string))
	     (code string)
	     x-pos y-pos)))

(defmethod read-from-stream ((class (eql :string-element)) stream &aux new)
  "create a string-element from its textual representation"
  (declare (special *document*))
  (setq new
    (make-string-element 
     (read stream) (read stream) *document* ;; width, height
     :name-string (read stream))) 
  (setf (value new) (read stream)) ;; => value-changed-callback
  (setf (code new)  (read stream)) ;; name-string of code
  new)

(defmethod (setf value) (new-value (string string-element))
  "set the value of the widget, convert if necessary"
  (if (stringp new-value)
      (setf (value (widget string)) new-value)
      ;; else: convert
      (setf (value (widget string)) (format nil "~a" new-value))))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class string-drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass string-drawer (element-drawer)
  (;; overrides
   (name :initform "Draw String"      :allocation :class)
   (element-constructor :accessor element-constructor :allocation :class
			:initform 'make-string-element))
  (:documentation "a mouse-down-command to draw a string-element"))

(defun make-string-drawer (view x y)
  (make-element-drawer view x y :class 'string-drawer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class text-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass text-element (variable-element)
  ((typestring :accessor typestring :initform "Text" :allocation :class)
   ;;(value :initform "")
))

(defun make-text-element (width height doc &key (name-string nil))
  (make-element width height doc :class 'text-element
		:name-string name-string))

(defmethod create-the-widget ((text text-element))
  (setf (widget text)
	     (make-scrolled-text (main-view (document text))
				 ;;:scroll-vertical nil
				 ;;:scroll-horizontal nil
				 ;;:value (slot-value text 'value)
				 :managed nil)))

(defmethod write-to-stream ((text text-element) stream)
  "write textual representation of a text-element to stream"
  (with-slots (x-pos y-pos width height) text
    (format stream ":text-element ~d ~d ~s ~s ~s ~d ~d~%" 
	    width height 
	    (name-string text) 
	    (value (widget text)) 
	    (code text)
	    x-pos y-pos)))

(defmethod read-from-stream ((class (eql :text-element)) stream &aux new)
  "create a text-element from its textual representation"
  (declare (special *document*))
  (setf new (make-text-element 
	     (read stream) (read stream) *document* ;; width height
	     :name-string (read stream)))
  (setf (value new) (read stream)) ;; => value-changed-callback
  (setf (code new)  (read stream)) ;; name-string of code
  new)

(defmethod (setf value) (new-value (text text-element))
  "set the value of the widget, convert if necessary"
  (if (stringp new-value)
      (setf (value (widget text)) new-value)
      ;; else: convert
      (setf (value (widget text)) (format nil "~a" new-value))))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class text-drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass text-drawer (element-drawer)
  (;; overrides
   (name :initform "Draw Text"      :allocation :class)
   (element-constructor :accessor element-constructor :allocation :class
			:initform 'make-text-element)
   )
  (:documentation "a mouse-down-command to draw a text-element"))

(defun make-text-drawer (view x y)
  (make-element-drawer view x y :class 'text-drawer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class number-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass number-element (variable-element)
  ((typestring :accessor typestring :initform "Number" :allocation :class)
   ;;(value :initform 0)
))

(defun make-number-element (width height doc &key (name-string nil))
  (make-element width height doc :class 'number-element
		:name-string name-string))

(defmethod create-the-widget ((number number-element))
  (setf (widget number)
	     (make-text (main-view (document number))
			;;:scroll-vertical nil
			;;:scroll-horizontal nil
			:value "0"
			;;:value (format nil "~d" (slot-value number 'value))
			:edit-mode :multi-line-edit
			:managed nil
			)))

(defmethod write-to-stream ((number number-element) stream)
  "write textual representation of a number-element to stream"
  (with-slots (x-pos y-pos width height) number
    (format stream ":number-element ~d ~d ~s ~s ~d ~d ~d~%" 
	    width height 
	    (name-string number)
	    (value number) 
	    (code number)
	    x-pos y-pos)))

(defmethod read-from-stream ((class (eql :number-element)) stream &aux new)
  "create a number-element from its textual representation"
  (declare (special *document*))
  (setf new (make-number-element 
	     (read stream) (read stream) *document* ;; width height
             :name-string (read stream)))
  (setf (value new) (read stream)) ;; => value-changed-callback
  (setf (code new) (read stream)) ;; name-string of code
  new)

;; the value of a number element is always returned as a number
(defmethod value ((number number-element))
  "get the value of the widget within the element"
  (or (parse-integer (value (widget number)) :junk-allowed t) 0))

(defmethod (setf value) (new-value (number number-element) &aux n)
  "set the value of the widget within the element"
  (setq n (if (numberp new-value)
	      new-value
	      (or (parse-integer 
		   (format nil "~a" new-value) :junk-allowed t) 
		  0)))
  (setf (value (widget number))
	(format nil "~d" n))
  n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class number-drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass number-drawer (element-drawer)
  (;; overrides
   (name :initform "Draw Number"      :allocation :class)
   (element-constructor :accessor element-constructor :allocation :class
			:initform 'make-number-element)
   )
  (:documentation "a mouse-down-command to draw a number-element"))

(defun make-number-drawer (view x y)
  (make-element-drawer view x y :class 'number-drawer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class code-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass code-element (value-holder-element)
  ((typestring       :accessor typestring       :initform "Code" :allocation :class)
   (expression :accessor expression :initform :invalid)
   (result     :accessor result     :initform nil)
   (is-init    :accessor is-init    :initform nil)
   (min-width   :accessor min-width  :initform 150 :allocation :class)
   ;;(value      :initform "")
   ;;(result-element :accessor result-element :initform nil)
   ))

(defun make-code-element (width height doc &key (name-string nil))
  (make-element width height doc :class 'code-element
		:name-string name-string))

(defmethod create-the-widget ((code code-element))
  (setf (widget code)
	(make-text (main-view (document code)) 
		   :edit-mode :multi-line-edit
		   :managed nil
		   ;;:value (slot-value code 'value)
		   :motif-resources (list :shadow-thickness 6)))
  (setf (value-changed-callback (widget code))
    (make-callback 'invalidate-expression code)))

(defmethod invalidate-expression ((code code-element) &rest args)
  (declare (ignore args))
  (setf (expression code) :invalid))

(defmethod draw :after ((elem code-element) count x y width height)
  "draw init indicator"
  (declare (ignore x y width height))
  (when (and (zerop count)  ;; Ignore all but the last exposure event
             (not (eq :user-mode (drawing-mode (parent-view elem)))))
    
    (xlib:with-gcontext ((gcontext (parent-view elem)) :font "6x10")
      (draw-rectangle elem (- (width elem) 63) 2 25 7 t)
      ;;(draw-glyphs elem (- (width elem) 61) 8 "eval")
      (draw-glyphs elem (- (width elem) 33) 8 "init")
      (when (not (is-init elem))
	(draw-line elem (- (width elem) 34) 5 (- (width elem) 8) 5))
      )))

(defmethod button-press :around 
	   ((elem code-element)
	    code ;;"which mouse button"
	    repetition ;;"1,2,3,.. for single, double, triple click"
	    x ;;"relative mouse position in the view-object"
	    y ;;"relative mouse position in the view-object"
	    )
     "define special handling for clicks into init flag"
     (declare (ignore repetition))
     (cond ((and (> x (- (width elem) 33)) 
		 (< x (- (width elem) 5))
		 (< y 10))
	    (setf (is-init elem) (not (is-init elem)))
	    (setf (modified (document elem)) t)
	    (force-redraw elem))
	   ((and (> x (- (width elem) 63))
            (< x (- (width elem) 38))
            (< y 10))
	    ;; simulate feedback
	    (when (eq code :extend) ;; middle button, secret option
	      (play "computer" :volume 30))
	    (xlib:with-gcontext ((gcontext (parent-view elem)) 
				 :function boole-clr)
	      (draw-rectangle elem (- (width elem) 63) 2 24 6))
	    (xlib:display-force-output *display*)
	    (sleep 0.2)
	    (draw-rectangle elem (- (width elem) 63) 2 25 7 t)
	    (xlib:display-force-output *display*)
	    
	    ;; evaluate code
	    (with-clock-cursor
	     (evaluate elem)))
	   (t (call-next-method))))
       

(defmethod write-to-stream ((code code-element) stream)
  "write textual representation of a code-element to stream"
  (with-slots (x-pos y-pos width height) code
    (format stream ":code-element ~d ~d ~s ~s ~a ~d ~d~%" 
	    width height 
	    (name-string code)
	    (value (widget code)) 
	    (is-init code)
	    x-pos y-pos)))

(defmethod read-from-stream ((class (eql :code-element)) stream &aux new)
  "create a code-element from its textual representation"
  (declare (special *document*))
  (setf new (make-code-element 
	     (read stream) (read stream) *document* ;; width height
             :name-string (read stream)))
  (setf (value new) (read stream)) ;; value
  (setf (is-init new) (read stream))
  new)

(defmethod double-clicked ((self code-element) code repetition x y)
  "execeute the code when view object is double clicked"
  (declare (ignore repetition x y code))
  (with-clock-cursor
    (evaluate self)))

(defmethod evaluate ((code code-element))
  "evaluate the code"
  (when (eq :invalid (expression code))
    ;; parse the expression
    (setf (expression code) 
      (parse (document code) code))
    (when *error-reported* 
      (setf (expression code) :invalid)))
  
  (when (not (eq :invalid (expression code)))
    "evaluate the expression"
    (let ((*document* (document  code)))
      (declare (special *document*))
      (reporting-errors (*document*)
			(unwind-protect
			    (progn
			      (in-package :hyper)
			      ;;(print (expression code))
			      (setf (result code) (eval (expression code))))
			  (in-package :gina))))
    (result code)))

(defmethod value ((code code-element))
  "evaluate and return the result of last evaluation"
  (evaluate code))


(defmethod (setf value) (new-value (code code-element))
  "set the value of the widget, convert if necessary"
  (if (stringp new-value)
      (setf (value (widget code)) new-value)
      ;; else: convert
      (setf (value (widget code)) (format nil "~a" new-value))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class code-drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass code-drawer (element-drawer)
  (;; overrides
   (name :initform "Draw Code"      :allocation :class)
   (element-constructor :accessor element-constructor :allocation :class
			:initform 'make-code-element))
  (:documentation "a mouse-down-command to draw a code-element"))

(defun make-code-drawer (view x y)
  (make-element-drawer view x y :class 'code-drawer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class result-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass result-element (variable-element)
  ((typestring :accessor typestring :initform "Result" :allocation :class)
   ;;(value :accessor value :initform nil)
))

(defun make-result-element (width height doc  &key (name-string nil))
  (make-element width height doc :class 'result-element
		:name-string name-string))

(defmethod create-the-widget ((result result-element))
  (setf (widget result)
	(make-scrolled-text (main-view (document result))
			    :value "NIL"
			    ;;:value (replace-returns 
				;;    (format nil "~s" (value result)))
			    :editable nil
			    :managed nil
			    )))

(defmethod (setf value) (new-value (result result-element))
  "display the value in the text widget"
  (let ((*print-pretty* t))
  (setf (value (widget result)) (replace-returns (format nil "~s" new-value)))))

(defmethod write-to-stream ((result result-element) stream)
  "write textual representation of a result-element to stream"
  (with-slots (x-pos y-pos width height) result
    (format stream ":result-element ~d ~d ~s ~d ~d~%" 
	    width height 
	    (name-string result)
	    x-pos y-pos)))

(defmethod read-from-stream ((class (eql :result-element)) stream)
  "create a result-element from its textual representation"
  (declare (special *document*))
  (make-result-element 
   (read stream) (read stream) *document* ;; width height
   :name-string (read stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class result-drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass result-drawer (element-drawer)
  (;; overrides
   (name :initform "Draw Result"      :allocation :class)
   (element-constructor :accessor element-constructor :allocation :class
			:initform 'make-result-element)
   )
  (:documentation "a mouse-down-command to draw a result-element"))

(defun make-result-drawer (view x y)
  (make-element-drawer view x y :class 'result-drawer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class clock-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clock-element (element)
  ((typestring :accessor typestring :initform "Clock" :allocation :class)
   (outlets :initform nil :allocation :class) 
   ;;(timeout-callback :accessor timeout-callback :initform nil)
   (timer-id :accessor timer-id)))

(defun make-clock-element (width height doc &key (name-string nil))
  (make-element width height doc :class 'clock-element
		:name-string name-string))

(defmethod create-the-widget ((clock clock-element))
  (setf (widget clock)
	(make-label (main-view (document clock)) " 00:00:00 " :managed nil))
  (setf (timer-id clock)
	(xtk:create-timer 1000 #'tick clock))
  )

(defmethod deinstall :before ((clock clock-element) &key (redraw t))
   (declare (ignore redraw))
   (xtk:destroy-timer (timer-id clock))
   )

(defmethod tick (timer-id (clock clock-element) &aux now (error-free nil))
  "one second is over => refresh the clock"
  (declare (ignore timer-id))
  (setq now (multiple-value-list (decode-universal-time (get-universal-time))))
  (setf (label-string (widget clock))
	(format nil " ~2,'0d:~2,'0d:~2,'0d " 
		(third now) (second now) (first now)))
  (resize (widget clock) (width (widget clock)) (height (widget clock)))
  (unwind-protect
      (progn
	(evaluate-code clock)
	(setq error-free t))
    ;; cleanup: turn of timout-callback in case of error
    (when (or (not error-free) *error-reported*)
      (setf (code clock) nil))))

(defmethod value ((clock clock-element))
  "return the current time a sthe value"
  (label-string (widget clock)))

(defmethod (setf value) (new-value (clock clock-element))
  "this does not make sense"
  (declare (ignore new-value))
  nil)

(defmethod write-to-stream ((clock clock-element) stream)
  "write textual representation of a clock-element to stream"
  (with-slots (x-pos y-pos width height) clock
    (format stream ":clock-element ~d ~d ~s ~s ~d ~d~%" 
	    width height 
	    (name-string clock) (code clock)
	    x-pos y-pos)))

(defmethod read-from-stream ((class (eql :clock-element)) stream &aux new)
  "create a clock-element from its textual representation"
  (declare (special *document*))
  (setf new (make-clock-element  
	     (read stream) (read stream) *document* ;; width height
	     :name-string (read stream)))
  (setf (code new) (read stream)) ;; name-string of code
  new)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class clock-drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clock-drawer (element-drawer)
  (;; overrides
   (name :initform "Draw Clock"      :allocation :class)
   (element-constructor :accessor element-constructor :allocation :class
			:initform 'make-clock-element)
   )
  (:documentation "a mouse-down-command to draw a clock-element"))

(defun make-clock-drawer (view x y)
  (make-element-drawer view x y :class 'clock-drawer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class button-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass button-element (value-holder-element)
  ((typestring :accessor typestring :initform "Button" :allocation :class)
   (value :initform " A BUTTON ")
   ))

(defun make-button-element (width height doc &key (name-string nil))
  (make-element width height doc :class 'button-element
		:name-string name-string))

(defmethod create-the-widget ((button button-element))
  (setf (widget button)
	 (make-push-button (main-view (document button)) 
			   " A BUTTON "
	      :managed nil
              :activate-callback (make-callback 'evaluate-code button))))

(defmethod write-to-stream ((button button-element) stream)
  "write textual representation of a button-element to stream"
  (with-slots (x-pos y-pos width height) button
    (format stream ":button-element ~d ~d ~s ~s ~s ~d ~d~%" 
	    width height 
	    (name-string button) 
	    (code button)
	    (label-string (widget button))
	    x-pos y-pos)))

(defmethod read-from-stream ((class (eql :button-element)) stream &aux new)
  "create a button-element from its textual representation"
  (declare (special *document*))
  (setf new (make-button-element 
	     (read stream) (read stream) *document* ;; width height
             :name-string (read stream)))
  (setf (code new) (read stream)) ;; name-string of code
  (setf (value new) (read stream)) ;; value
  new)


(defmethod value ((button button-element))
  "get the label-string"
  (label-string (widget button)))

(defmethod (setf value) (new-value (button button-element))
  "set the label-string of the widget, convert if necessary"
  (if (stringp new-value)
      (setf (label-string (widget button)) new-value)
      ;; else: convert
      (setf (label-string (widget button)) (format nil "~a" new-value)))
  ;; restor old size of button-widget:
  (resize (widget button) (- (width button) 10) (- (height button) 20))
  (label-string (widget button)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class button-drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass button-drawer (element-drawer)
  (;; overrides
   (name :initform "Draw Button"      :allocation :class)
   (element-constructor :accessor element-constructor :allocation :class
			:initform 'make-button-element)
   )
  (:documentation "a mouse-down-command to draw a button-element"))

(defun make-button-drawer (view x y)
  (make-element-drawer view x y :class 'button-drawer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class list-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass list-element (variable-element)
  ((typestring :accessor typestring :initform "List" :allocation :class)
   (items :accessor items :initform '("Rot" "Gruen" "Blau" "Gelb"))
   (outlets     :accessor outlets :initform '("value" "items") 
		:allocation :class)
   (default-button-element :accessor default-button-element :initform nil)
   (min-width   :accessor min-width  :initform 80 :allocation :class)
   (min-height  :accessor min-height :initform 80 :allocation :class)))

(defun make-list-element (width height doc &key (name-string nil))
  (make-element width height doc :class 'list-element
		:name-string name-string))

(defmethod create-the-widget ((list list-element))
  (setf (widget list)
	 (make-scrollable-selection-list
	  (main-view (document list))
	  '("Rot" "Gruen" "Blau" "Gelb")
	  :managed nil)))

(defmethod set-item-list ((list list-element) new-item-list 
                          &key &allow-other-keys)
  (when (listp new-item-list)
    (set-item-list (widget list)
		   (loop for item in new-item-list
		    when (or (listp item)
			     (stringp item))
		    collect item
		    else
		    collect (list (format nil "~a" item) item)))))

(defmethod (setf items) :after (new-items (list list-element))
  (set-item-list list new-items))

(defmethod write-to-stream ((list list-element) stream)
  "write textual representation of a list-element to stream"
  (with-slots (x-pos y-pos width height) list
    (format stream ":list-element ~d ~d ~s ~s ~s ~d ~d~%" 
	    width height 
	    (name-string list) (code list)
	    (when (default-action-button (widget list))
	      (name-string (default-button-element list)))
	    x-pos y-pos)))

(defmethod read-from-stream ((class (eql :list-element)) stream &aux new)
  "create a list-element from its textual representation"
  (declare (special *document*))
  (setf new (make-list-element 
	     (read stream) (read stream) *document* ;; width height
             :name-string (read stream))) 
  (setf (code new) (read stream)) ;; name-string of code
  (setf (default-button-element new)  (read stream)) ;; name-string
  new)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class list-drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass list-drawer (element-drawer)
  (;; overrides
   (name :initform "Draw List"      :allocation :class)
   (element-constructor :accessor element-constructor :allocation :class
			:initform 'make-list-element))
  (:documentation "a mouse-down-command to draw a list-element"))

(defun make-list-drawer (view x y)
  (make-element-drawer view x y :class 'list-drawer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class scale-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass scale-element (variable-element)
  ((typestring :accessor typestring :initform "Scale" :allocation :class)))

(defun make-scale-element (width height doc &key (name-string nil))
  (make-element width height doc :class 'scale-element
		:name-string name-string))

(defmethod create-the-widget ((scale scale-element))
  (setf (widget scale)
	 (make-scale
	   (main-view (document scale))
	   :orientation (if (> (height scale) (width scale))
			    :vertical :horizontal)
	   :managed nil)))

(defmethod write-to-stream ((scale scale-element) stream)
  "write textual representation of a scale-element to stream"
  (with-slots (x-pos y-pos width height) scale
    (format stream ":scale-element ~d ~d ~s ~s ~d ~d ~d~%" 
	    width height 
	    (name-string scale) 
	    (code scale)
	    (value scale)
	    x-pos y-pos)))

(defmethod read-from-stream ((class (eql :scale-element)) stream &aux new)
  "create a scale-element from its textual representation"
  (declare (special *document*))
  (setf new (make-scale-element 
	     (read stream) (read stream) *document* ;; width height
             :name-string (read stream))) 
  (setf (code new) (read stream)) ;; name-string of code
  (setf (value new) (read stream))
  new)

(defmethod (setf value) (new-value (scale scale-element))
  "set the value of the widget, convert if necessary"
  (setf (value (widget scale))
	(if (numberp new-value)
	    new-value
	    (or (parse-integer new-value :junk-allowed t) 0))))

'(parse-integer "1.2sdf" :junk-allowed t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class scale-drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass scale-drawer (element-drawer)
  (;; overrides
   (name :initform "Draw Scale"      :allocation :class)
   (element-constructor :accessor element-constructor :allocation :class
			:initform 'make-scale-element))
  (:documentation "a mouse-down-command to draw a scale-element"))

(defun make-scale-drawer (view x y)
  (make-element-drawer view x y :class 'scale-drawer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class view-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass user-view (view)
  ((view-element :accessor view-element :initarg :view-element)
   ;;(draw-callback :accessor draw-callback :initform nil)
   ))

(defmethod draw ((view user-view) count x y width height)
  "call the callback supplied by the user as a code block"
  (declare (ignore x y width height))
  (when (zerop count)
    (evaluate-code (view-element view))
    ;;(execute (draw-callback view) (list view))
    ))

(defclass view-element (element)
  ((typestring :accessor typestring :initform "View" :allocation :class)
   (outlets     :accessor outlets :initform nil :allocation :class)))

(defun make-view-element (width height doc &key (name-string nil))
  (make-element width height doc :class 'view-element
		:name-string name-string))

(defmethod create-the-widget ((view view-element))
  (setf (widget view)
	 (make-view
	   (main-view (document view))
           :document (document view)
	   :class 'user-view
	   :width  (- (width view) 10)
	   :height (- (height view) 15)
	   ;;:managed nil
           :initargs (list :view-element view)
	   )))

(defmethod (setf code) :after (new-code (view view-element))
  "call draw-method when code is changed"
  (declare (ignore new-code))
  (force-redraw (widget view)))

(defmethod write-to-stream ((view view-element) stream)
  "write textual representation of a view-element to stream"
  (with-slots (x-pos y-pos width height) view
    (format stream ":view-element ~d ~d ~s ~s ~d ~d~%" 
	    width height 
	    (name-string view) (code view)
	    x-pos y-pos)))

(defmethod read-from-stream ((class (eql :view-element)) stream &aux new)
  "create a view-element from its textual representation"
  (declare (special *document*))
  (setf new (make-view-element 
	     (read stream) (read stream) *document* ;; width height
             :name-string (read stream))) 
  (setf (code new) (read stream)) ;; name-string of code
  new)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class view-drawer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass view-drawer (element-drawer)
  (;; overrides
   (name :initform "Draw View"      :allocation :class)
   (element-constructor :accessor element-constructor :allocation :class
			:initform 'make-view-element))
  (:documentation "a mouse-down-command to draw a scale-element"))

(defun make-view-drawer (view x y)
  (make-element-drawer view x y :class 'view-drawer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class assigner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass assigner (mouse-down-command)
  (;; overrides
   (name :initform "Draw Assignment"      :allocation :class)
   ;; instance variables
   (from-element :accessor from-element :initarg :from-element)
   (outlet       :accessor outlet       :initarg :outlet)
   (to-element   :accessor to-element   :initform nil)
   (inlet        :accessor inlet        :initform nil)
   )
  (:documentation "a mouse-down-command to draw an assignment"))

(defun make-assigner (view x y &key from-element outlet)
  (report (document view) " ")
  (make-mouse-down-command (document view) view x y
			   :class 'assigner
			   :initargs (list :from-element from-element
					   :outlet outlet)))

(defmethod draw-feedback ((cmd assigner) x y &key clear 
			  &aux new-to-element new-inlet)
  (with-slots (start-x start-y view from-element to-element inlet outlet) cmd
    (xlib:with-gcontext ((gcontext view) :font "6x10")
			(draw-line view start-x start-y x y)
			(draw-glyphs view x y (outlet cmd)))
    (when (not clear)
      (setf new-to-element
	(loop for view-object in (view-objects (view cmd))
	 do (when (point-inside view-object (last-x cmd) (last-y cmd))
	      (return view-object))))
      (when (eq new-to-element from-element)
	(setf new-to-element nil))
      (when new-to-element
	"determine inlet"
	(setf new-inlet (if (< y (+ (y-pos new-to-element) 
				    (height new-to-element)
				    -10))
			    (first (outlets new-to-element))
			  (car (last (outlets new-to-element)))))
	;;(print new-inlet)
	)
      (when (or (not (eq new-to-element to-element))
		(not (eq new-inlet inlet)))
	(setf to-element new-to-element)
	(setf inlet new-inlet)
	(if new-to-element
	    (report-assign from-element to-element outlet inlet)
	  (report (document cmd) " ")))
      
      )))

(defmethod doit ((cmd assigner))
  "check if mouse goes up inside an object"
  (with-slots (to-element from-element outlet inlet) cmd
    (when to-element
      ;; two objects hit by the command !!!!
      (assign from-element to-element outlet inlet)
      (blink to-element from-element)
      )))

(defmethod report-assign ((elem1 element) (elem2 element) outlet inlet)
  "default method is called => no semantics defined for the object pair"
  (declare (ignore outlet inlet))
  (report (document (parent-view elem1)) "This does not make sense!"))

(defmethod assign ((elem1 element) (elem2 element) outlet inlet)
  "default method is called => no semantics defined for the object pair"
  (declare (ignore outlet inlet))
  (xlib:bell *display*))

(defmethod report-assign 
              ((elem1 value-holder-element) (elem2 value-holder-element)
	       outlet inlet)
  (report (document (parent-view elem1))
	  (format nil "(setf (~a ~a) (~a ~a))"
		  inlet (name-string elem2) outlet (name-string elem1))))

(defmethod assign ((elem1 value-holder-element) (elem2 value-holder-element)
		   outlet inlet &aux value)
  "Assign the value of one element to the other"
  (setq value (if (equal outlet "items")
		  (items elem1)
		  (value elem1)))
  (if (equal inlet "items")
      (setf (items elem2) value)
    (setf (value elem2) value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class connector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass connector (mouse-down-command)
  (;; overrides
   (name :initform "Draw Connection"      :allocation :class)
   (from-element :accessor from-element :initarg :from-element)
   (to-element   :accessor to-element :initform nil)
   )
  (:documentation "a mouse-down-command to draw a connection"))

'(defun make-connector (view x y &key from-element)
  (make-mouse-down-command (document view) view x y
			   :class 'connector
			   :initargs (list :from-element from-element)))

'(defmethod draw-feedback ((cmd connector) x y &key clear)
  (declare (ignore clear))
  (with-slots (start-x start-y view from-element) cmd
	      (xlib:with-gcontext ((gcontext view) :function boole-xor)
				  (draw-line view start-x start-y x y)
  )))

'(defmethod doit ((cmd connector))
  "check if mouse goes up inside an object"
  (with-slots (from-element to-element) cmd
    (setf to-element
	  (loop for view-object in (view-objects (view cmd))
	        do (when (point-inside view-object (last-x cmd) (last-y cmd))
		  (return view-object))))
    (when (not to-element)
      (report (document cmd) "Nothing hit!")
      (xlib:bell *display*))
    (when to-element
      ;; two objects hit by the command !!!!
      (connect (from-element cmd) to-element)
      ;;(shell-command "play -v40 /usr/demo/SOUND/sounds/gong.au &")
      (blink to-element (from-element cmd))
      ;;(format t "~s --> ~s " (from-element cmd) to-element)
      )))

'(defmethod connect ((elem1 element) (elem2 element))
  "default method is called => no semantics defined for the object pair"
  (xlib:bell *display*)
  (report (document (parent-view elem1)) "This does not make sense!"))

'(defmethod connect ((elem1 variable-element) (elem2 variable-element))
  "Assign the value of one element to the other at each value-changed callback"
  (when (eq elem1 elem2)
    (xlib:bell *display*)
    (report (document (parent-view elem1))
	    "You cannot connect one element to itself!"))
  (when (not (eq elem1 elem2))
    (setf (value elem2) (value elem1))
    (setf (value-changed-callback (widget elem1))
	  `(lambda (new-value &rest ignore)
	     (setf (value ',elem2) (value ',elem1))))
    (report 
     (document (parent-view elem1))
     (format nil 
      "Make value-changed-callback assign the value of the ~a to the ~a!"
      (typestring elem1) (typestring elem2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class name-inserter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass name-inserter (mouse-down-command)
  ((name :initform "Insert name"      :allocation :class)
   (from-element :accessor from-element :initarg :from-element)
   (to-element   :accessor to-element :initform nil)
   ))

(defun make-name-inserter (view x y from-element)
  (report (document view) " ")
  (make-mouse-down-command (document view) view x y
			   :class 'name-inserter
			   :initargs (list :from-element from-element)))

(defmethod draw-feedback ((cmd name-inserter) x y &key clear 
			  &aux new-to-element)
  (with-slots (start-x start-y view from-element to-element) cmd
     (xlib:with-gcontext ((gcontext view) :font "6x10")
	 (draw-glyphs view x y (name-string (from-element cmd))))
     (when (not clear)
       (setf new-to-element
	 (loop for view-object in (view-objects (view cmd))
	       do (when (point-inside view-object (last-x cmd) (last-y cmd))
	                  (return view-object))))
       (when (eq new-to-element from-element)
	 (setf new-to-element nil))
       (when (not (eq new-to-element to-element))
	 (setf to-element new-to-element)
	 (if new-to-element
	     (report-insert from-element to-element)
	   (report (document cmd) " ")))

  )))

(defmethod doit ((cmd name-inserter))
  "check if mouse goes up inside a code object"
  (with-slots (from-element to-element) cmd
  (when to-element
    ;; two objects hit by the command !!!!
    (insert from-element to-element)
    (blink  from-element to-element)
    )))

(defmethod report-insert ((elem1 element) (elem2 element))
  "default method is called => no semantics defined for the object pair"
  (report (document (parent-view elem1)) "This does not make sense!"))

(defmethod insert ((elem1 element) (elem2 element))
  "default method is called => no semantics defined for the object pair"
  (xlib:bell *display*))

(defmethod report-insert ((elem element) (code code-element))
  (report (document (parent-view elem))
	  (format nil "Insert the name ~s into ~a!"
                      (name-string elem) (name-string code))))

(defmethod insert ((elem element) (code code-element))
  "Insert the name of the element into the code"
  (xtk:insert-text (widget-id (widget code))
		   (first (get-motif-resources (widget code) :cursor-position))
		   (name-string elem)))

(defmethod report-insert ((code code-element) (elem element))
  (report (document (parent-view code))
	  (format nil "Set ~a as the callback of ~a!" 
		  (name-string code) (name-string elem))))

(defmethod insert ((code code-element) (elem element))
  "Set the code as the callback of the element"
  (setf (code elem) (name-string code)))

(defmethod report-insert ((button button-element) (list list-element))
  (report (document (parent-view button))
	  (format nil "Make ~a the default button of ~a!" 
		  (name-string button) (name-string list))))

(defmethod insert ((button button-element) (list list-element))
  "make button the default button of the list"
  (setf (default-button-element list) button)
  (setf (default-action-button (widget list))
	(widget button)))

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
	do (deinstall object)
	   (setf (elements (current-card (document cmd)))
		 (delete object (elements (current-card (document cmd)))))))

(defmethod undoit ((cmd clear-all-command))
  "reinstall all objects in the view"
  (loop for object in (objects cmd)
	do (install object (main-view (document cmd))
		    (x-pos object) (y-pos object))
	   (push object (elements (current-card (document cmd))))
	   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class clear-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clear-command (command)
  (;; overrides
   (name :initform "Clear Command" :allocation :class)
   ;; instance-parameters
   (objects :accessor objects :initarg :objects
	    :documentation "objects being cleared"))
  (:documentation "a command to delete the selected objects"))

(defun make-clear-command (document)
  "create a new command object with appropriate parameters"
  (make-command 
   document
   :class 'clear-command
   :initargs `(:objects 
	       ,(loop for object in (view-objects (main-view document))
		 when (selected object) collect object))))

(defmethod doit ((cmd clear-command))
  "delete all objects from the view"
  (loop for object in (objects cmd)
	do (deinstall object)
	   (setf (elements (current-card (document cmd)))
		 (delete object (elements (current-card (document cmd)))))))

(defmethod undoit ((cmd clear-command))
  "reinstall all objects in the view"
  (loop for object in (objects cmd)
	do (install object (main-view (document cmd))
		    (x-pos object) (y-pos object))
	   (setf (elements (current-card (document cmd)))
		 (delete object (elements (current-card (document cmd)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sound and color
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun play (sound-name &key (volume 40))
   "submit a play command to unix"
   ;; 1<=volume<=100
   (#-genera shell-command #+genera print
    (format nil "/usr/demo/SOUND/play -v~d /usr/demo/SOUND/sounds/~a.au &"
	    volume sound-name)))
'(play "gong" :volume 100)

(defvar *all-sounds*)
(setq *all-sounds*
  '("bark" "bong" "bubbles" "busy" "chirp" "clink" "computer" "cowbell" "crash"
    "cuckoo" "doorbell" "drip" "flush" "gong" "laugh" "laughter" "ring" 
    "rooster" "spacemusic" "splat" "train" "whistle"))

(defvar *all-colors*)
(setq *all-colors*
  (loop for c in (xtk:get-x-colors) collect (fourth c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; main program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(register-application "hyper" 'hyper-application "stack")
'(make-hyper-application)





