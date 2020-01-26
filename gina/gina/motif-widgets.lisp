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

(setq *sccs-id* "@(#)motif-widgets.lisp	1.34	11/8/93")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;            Encapsulation of Motif-Widgets in CLOS-Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Design goals:
;; - object-oriented toolbox for Motif-widgets
;; - Encapsulation of Motif-Widgets
;; - Individual Constructor-functions with understandable parameters
;; - Important resources and callbacks are implemented as parameters of 
;;   constructors
;;   Furthermore, a mechanism to set ALL Motif resources and callbacks.
;; - In most cases widget-classes directly correspond to CLOS-classes, 
;;   however, sometimes more special subclasses

;; Realisierung:
;; - pro class eine Funktion make-<widget-name>
;; - erster parameter ist stets der parent
;; - alle moeglichen callbacks werden als slots gespeichert
;; - gaengige callbacks koennen bei der Initialisierung angegeben werden
;;   default ist jeweils der Aufruf einer leeren Methode
;;   daher kann entweder die leere Methode ueberschrieben werden, oder ein 
;;   anderer callback gesetzt werden
;; - alle anderen (ungewoehnlichen) callbacks koennen mit (setf <callback>) 
;;   nachtraeglich gesetzt werden
;; - bei (setf <callback>) wird eine Funktion oder ein Callback Objekt angegeben
;;   bei Aufruf werden 0 oder mehrere widget spezifische Argumente uebergeben
;; - soll eine Methode des widget-Objektes als Callback aufgerufen werden, so
;;   uebergibt man bei (setf <callback>) ein callback-object mit dem 
;;   Methodennamen sowie einem Verweis auf das Objekt. Die Methode hat dann 
;;   genau die von Motif angelieferten Parameter
;; - Die wichtigsten Resourcen sind als Positions- oder Key-Parameter 
;;   dokumentiert
;; - weitere resourcen fuer das zugrundeliegende Motif-Widget koennen stets 
;;   angegeben werden
;;
;; Zusammengesetzte Widgets:
;; - teilweise wird auch ein ganzer Teilbaum von Widgets in einem CLOS-Objekts
;;   eingeschalt. Beispiel: Ein Frame mit einem Label darueber (labeled-frame)
;;   besteht aus
;;                          row-column
;;                          /       \
;;                       label    frame
;;
;;   Das zusammengesetzte Widget verkoerpert bei solchen Teilbaeumen das 
;;   wichtigste widget des Unterbaumes:
;;   labeled-frame ist dann eine Unterklasse von frame und hat Verweise auf
;;   label und row-column Objekte. Es wird aber als group-widget-id die 
;;   widget-id der row-column angegeben. Das hat zur Folge, dass move und 
;;   resize sich (richtigerweise) auf die row-column beziehen. Ebenso 
;;   destroy-Aufrufe und form-constraints. Soll aber ein Sohn in den 
;;   labeled-frame eingehaengt werden, so muss er eigentlich den frame als 
;;   parent haben. Daher kann der slot widget-id-for-sons auf die id des 
;;   frames gesetzt werden
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass widget ()

   (description
     "the abstract superclass of all CLOS-Objects encapsulating Motif widgets"
     :application-subclasses :rarely
     :gina-subclasses "all Motif widgets are subclasses"
     :instantiate nil)

   ((widget-id          :accessor widget-id
	 :documentation "integer returned by CLM create-widget call")
    (group-widget-id    :accessor group-widget-id
	 :documentation "id used when the whole group is meant")
    (widget-id-for-sons :accessor widget-id-for-sons
	 :documentation "may be different from widget-id in compound widgets")
    (parent             :accessor parent
         :documentation "the parent widget object in the hierarchy")
    (name               :accessor name
         :documentation "name string for resource database lookup")
    (width              :accessor width
         :documentation "total width in pixels")
    (height             :accessor height
         :documentation "total height in pixels")
    (x-pos              :accessor x-pos    :initform 0
	 :documentation "the x-coordinate relative to the parent")
    (y-pos              :accessor y-pos    :initform 0
	 :documentation "the y-coordinate relative to the parent")
    ;;(help-callback :accessor help-callback :initform nil)
    ))

;;(defcallback widget help-callback :help :lambda-list "()")

(defginafun make-widget (parent "the parent widget"
			  &key (class 'widget)
		               (initargs nil)
			       (motif-widget-class :push-button)
			       (managed t)
			       (name class)
			       (motif-resources nil)
			  &aux new-widget)
  (description
    "create a new CLOS Object representing a widget"
    :constructor-for-class widget
    :called-by-gina "whenever any kind of widget is created"
    :called-by-application "in the constructor of subclasses")

  (declare (special *application*))
  
  (when (symbolp name)
    (setq name (string-downcase (symbol-name name))))
  
  ;; create CLOS Object
  (setq new-widget (make-object class initargs))

  ;; create Motif widget in the toolkit server
  (setf (widget-id new-widget)
	(xtk:create-named-widget motif-widget-class (widget-id-for-sons parent)
				 name managed motif-resources))
  ;; by default the new widget-id is used for sons and the group 
  (setf (widget-id-for-sons new-widget) (widget-id new-widget))
  (setf (group-widget-id    new-widget) (widget-id new-widget))

  ;; inform (interested) parent of new son
  (son-created parent new-widget)
  
  (setf (parent new-widget) parent)
  (setf (name   new-widget) name)

  ;; handle Control-right clicks by describing the clicked widget
  (when (and (inspect-click *application*)
             (allows-inspect-click parent))
    (add-event-handler new-widget :button-release-mask
		       (make-callback #'inspect-clicked new-widget)))
  
  new-widget)

(defmethod allows-inspect-click ((w widget))
  "if the widget as a parent allows the inspect-click to be installed"
  t)

(defginamethod add-event-handler ((w widget) 
                                  mask "logical or of CLX event masks"
                                  function-or-callback)
  (description
    "add an event handler for some  basic X event"
    :called-by-gina "to install event handlers for the most important events"
    :called-by-application "if more specific events are desired")
  (comment "mask :no-event-mask registers all non-maskable events")
  (xtk:add-event-handler (widget-id w) mask
			 #'gina-callback function-or-callback
			 :non-maskable (eq mask :no-event-mask)))

(defginamethod inspect-clicked ((w widget) &rest parm-list)
  (description
    "the widget is clicked, check for inspect click for debugging"
    :called-by-gina "if inspect-click flag in application enabled")
  (comment
    "parm-list: type x y root-x root-y state button")

  ;;(format t "State: ~s Button: ~s~%" (nth 5 parm-list) (nth 6 parm-list))
  (when (= 1028 (nth 5 parm-list)) ;; Control-right button
    (inspect-dialog w)))

(defginamethod son-created ((w widget) son)
  (description 
    "parent is informed that a new son is created"
    :called-by-gina "whenever a son of this widget is created"
    :default-version :do-nothing)
  (declare (ignore son))
  )

(defginamethod get-motif-resources ((w widget) &rest resources)
  (description 
    "get up-to-date motif-resources from toolkit"
    :called-by-gina :rarely
    :called-by-application "to find out unusual resource values"
    :result "list of requested resource values")
  (example 
    (get-motif-resources frame :shadow-thickness :shadow-type)
    "returns (2 :etched-in)")
  (apply #'xtk:get-values (cons (widget-id w) resources)))

(defginamethod set-motif-resources ((w widget) &rest resource-pairs)
  (description 
    "set motif-resources later than at initialization"
    :called-by-gina :rarely
    :called-by-application "to set non-slot resources in the Motif server")
  (example
    (set-motif-resources frame :shadow-thickness 4 :shadow-type :etched-out))
  (apply #'xtk:set-values (cons (widget-id w) resource-pairs)))

'(defginamacro more-initargs (&rest initarg-pairs)
   (description
     "append more initarg-pairs to local variable initargs"
     :called-by-gina "in constructor-functions of widgets"
     :called-by-application "in the constructor of a new subclass of a widget")
  (example
    (let ((initargs '(:a 3 :b 7))) (more-initargs :x 5 :y (+ 3 4)))
    "returns (:a 3 :b 7 :x 5 :y 7)")
  (append '(append initargs) (list (cons 'list initarg-pairs))))
(eval-when (eval load)
  (export 'motif-resources))
'(defginamacro more-motif-resources (&rest resource-pairs)
  (description
     "prepend more resource-pairs to local variable motif-resources"
     :called-by-gina "in constructor-functions of widgets"
     :called-by-application "in the constructor of a new subclass of a widget")
  (example
    (let ((motif-resources '(:a 3 :b 7))) 
       (more-motif-resources :x 5 :y (+ 3 4)))
    "returns (:x 5 :y :7 a 3 :b 7)")
  (append '(append) (list (cons 'list resource-pairs)) '(motif-resources)))

(defmethod set-callback ((w widget) callback-name function-or-callback)
  "set the callback of the widget"
  (with-slots (widget-id) w
    (xtk:remove-all-callbacks widget-id callback-name)
    (when function-or-callback ;; the nil-callback removes all callbacks
      (xtk:add-callback widget-id callback-name
		    #'widget-callback 
		    (make-array 2 :initial-contents
				(list w function-or-callback))))))

(defun widget-callback (widget-id widget-and-callback &rest call-data)
  "route widget callback to the GINA callback or lisp function"
  (declare (ignore widget-id))
  ;; call-data is a widget dependent list of parameters
  (execute-from-widget (svref widget-and-callback 1) 
		       (svref widget-and-callback 0) call-data))

(defun gina-callback (widget-id function-or-callback &rest call-data)
  "route toolkit callback to the GINA callback or lisp function"
  (declare (ignore widget-id))
  ;; call-data is a widget dependent list of parameters
  (execute function-or-callback call-data))

(defginamethod destroy ((w widget))
  (description 
    "destroy the widget objects and all its sons"
    :called-by-application "to destroy widgets no longer needed")
  (xtk:destroy-widget (group-widget-id w)))

(defginamethod manage ((w widget))
  (description 
    "make widget managed"
    :called-by-application "to make a widget visible")
  (xtk:manage-widgets (group-widget-id w)))

(defginamethod unmanage ((w widget))
  (description 
    "make widget unmanaged"
    :called-by-application "to make a widget temporarily invisible")
  (xtk:unmanage-widgets (group-widget-id w)))

(defginamethod update-slots ((w widget) &aux new-values)
  (description 
    "ask the toolkit server for current size and position values"
    :called-by-application "to bring the cached slots up to date")
  (setq new-values (xtk:get-values (group-widget-id w) :width :height :x :y))
  (with-slots (width height x-pos y-pos) w
    (setq width  (first  new-values))
    (setq height (second new-values))
    (setq x-pos  (third  new-values))
    (setq y-pos  (fourth new-values))))

(defresourceslot widget width :width :mode ())

(defmethod (setf width) :before (new-width (w widget))
   "inform toolkit server that slot has changed"
   (xtk:set-values (group-widget-id w) :width new-width))

(defresourceslot widget height :height :mode ())

(defmethod (setf height) :before (new-height (w widget))
   "inform toolkit server that slot has changed"
   (xtk:set-values (group-widget-id w) :height new-height))

(defresourceslot widget x-pos :x-pos :mode ())

(defmethod (setf x-pos) :before (new-x-pos (w widget))
   "inform toolkit server that slot has changed"
   (xtk:set-values (group-widget-id w) :x new-x-pos))

(defresourceslot widget y-pos :y-pos :mode ())

(defmethod (setf y-pos) :before (new-y-pos (w widget))
   "inform toolkit server that slot has changed"
   (xtk:set-values (group-widget-id w) :y new-y-pos))

(defginamethod move ((widget widget) new-x new-y 
		     &key &allow-other-keys)
  (description 
    "change x and y coordinates"
    :called-by-application "to reposition a widget")
  (with-slots (x-pos y-pos) widget
    (setq x-pos new-x)
    (setq y-pos new-y)
  ;;    (xtk::move-widget (group-widget-id widget) new-x new-y)
  ;;    nicht brutal genug:
    (xtk:set-values (group-widget-id widget) :x new-x :y new-y)
    ))
  
(defginamethod resize ((widget widget) new-width new-height
		       &key &allow-other-keys)
  (description 
    "change width and height"
    :called-by-application "to resize a widget")
  (with-slots (width height) widget
    (setq width  new-width)
    (setq height new-height)
    ;; besser resize-widget???
    (xtk:set-values (group-widget-id widget)
		:width new-width :height new-height)))

(defginamethod root-coordinates ((w widget) &key (x 0) (y 0))
  (description
    "translate coordinates relative to root window"
    :called-by-application 
          "to get the absolute position of a point in a widget"
    :result "(values x y)")
  (let ((coords (xtk:xt-translate-coordinates (group-widget-id w) x y)))
    (values (first coords) (second coords))))

(defginamethod get-clx-window ((w widget))
  (description
    "get the CLX window id of a widget"
    :result "a CLX window")
  (xtk:make-clx-window (group-widget-id w)))

(defginamethod point-inside ((widget widget) 
			     x "x-pos relative to parent widget"
			     y "y-pos relative to parent widget")
  (description
    "test whether point lies inside a widget"
    :called-by-application :sometimes
    :result "flag whether inside")
  (update-slots widget)
  (and (>= x (x-pos widget)) (>= y (y-pos widget))
       (<= x (+ (x-pos widget) (width widget)))
       (<= y (+ (y-pos widget) (height widget)))))

(defginamethod add-to-tab-group ((widget widget))
  (description 
    "add widget to the tab-group of its shell"
    :called-by-gina "to add text-widgets to the shells tab-group"
    :called-by-application "to add other widgets to the shells tab-group")
  (xtk:add-tab-group (widget-id widget)))

(defginamethod remove-from-tab-group ((widget widget))
  (description 
    "remove widget from the tab-group of its shell"
    :called-by-application "to remove a widget from the tab-group")
  (xtk:remove-tab-group (widget-id widget)))

(defginamethod set-keyboard-focus ((widget widget))
  (description
    "try to set the keyboard focus to a widget"
    :called-by-application "to enable keyboard input, e.g. in a view"
    :result "T if focus could be transferred, else NIL")
  (if (zerop (first (xtk:process-traversal (widget-id widget) :current)))
      (not (zerop (first (xtk:process-traversal (widget-id widget) :current))))
      t))

(defginamethod set-sensitivity ((widget widget) new-value)
  (description
    "set the sensitivity, i.e. whether widget reacts to mouse clicks"
    :called-by-application "to mark temporarily inapplicable widgets or groups")
  (if new-value
      (xtk:set-sensitive (group-widget-id widget))
      (xtk:set-insensitive (group-widget-id widget))))

(defginamethod enclosing-shell ((widget widget))
  (description 
    "walk up widget tree until shell"
    :called-by-application "to find out the shell in which this widgets lies"
    :overridden-by-gina "for shell-widgets, which just return themself")
  (enclosing-shell (parent widget)))

;; general method to find the document of each widget

(defginamethod document ((widget widget))
  (description "Determine document of widget"
			  :called-by-application :sometimes
			  :result "the document associated to the widget")
  (when (not (parent widget))
    (error "Cannot determine document of ~s" widget))
  (document (parent widget)))

(defginamethod define-cursor ((widget widget)
	       cursor-nr-or-keyword "number or keyword for the cursor"
	       &aux cursor)
  (description 
    "permanently set the cursor for a widget"
    :called-by-application :sometimes)
  (comment 
    "Cursor can be the result of a call to xtk:create-font-cursor"
    "or xtk:create-pixmap-cursor or a keyword like :hand, :arrow ...")
  (comment 
    "(print (xtk:all-cursors)) lists all available keywords.")
  (example 
    (with-application-stopped
	 (define-cursor (main-view (first (document-list *application*))) 
	     :hand)))
  (example 
    (with-application-stopped
	 (define-cursor (main-view (first (document-list *application*))) 
	     :restore)))
  (example 
    (with-application-stopped
	 (define-cursor (main-view (first (document-list *application*)))
			(xtk:create-pixmap-cursor
			      "/vol/gina/bitmaps/lupe"
			      "/vol/gina/bitmaps/lupe-mask"))))
  (cond ((numberp cursor-nr-or-keyword)
	 (xtk:define-cursor cursor-nr-or-keyword (widget-id widget)))
	
	((eq :restore cursor-nr-or-keyword)
	 (xtk:define-cursor :restore (widget-id widget)))
	(t 
	 ;; we have to find a cursor-id for xtk:define-cursor
	 ;; check if cursor has already been used in this application
	 (setq cursor (gethash cursor-nr-or-keyword 
			       (cursor-table *application*)))
	 (when (not cursor)
	   ;; cursor used for the first time: create and store in hashtable
	   (setq cursor (xtk:create-font-cursor cursor-nr-or-keyword))
	   (setf (gethash cursor-nr-or-keyword (cursor-table *application*)) 
	     cursor))
	 (xtk:define-cursor cursor (widget-id widget)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class shell - the abstract superclass for all shells
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                  shell
;;                             /      |     \
;;               toplevel-shell menu-shell   modal-dialog-shell
;;                /         \
;; basic-document-shell   modeless-dialog-shell
;;          |
;;   document-shell
;;         |
;; document-shell-with-scroller
;;

'(defginaclass shell (widget)

   (description
     "the abstract superclass of all Motif-shells"
     :application-subclasses :rarely
     :gina-subclasses t
     :instantiate nil)

   ((title              :accessor title :initarg :title
         :documentation "title string")
    (views              :accessor views :initform nil
         :documentation "all views within the shell")
    (document           :accessor document       :initform nil
         :initarg :document
         :documentation "the document belonging to the shell, or nil")
    (visible            :accessor visible        :initform nil
         :documentation "whether the shell is up")
    (taps               :accessor taps           :initform nil)
    (drag-protocols     :accessor drag-protocols :initform nil
         :initarg :drag-protocols
         :documentation "a list of keywords of accepted drag-commands")
    (popup-callback     :accessor popup-callback :initform nil)))

;; deamon to call set-callback when slot 'popup-callback is modified 
(defcallback shell popup-callback :popup :lambda-list "()")

;; make-shell does NOT call make-widget even though it is a subclass of widget
(defginafun make-shell (parent "the parent widget" title "the title string"
		   &key (class 'shell)
		        (initargs nil)
		        (motif-widget-class :shell)
			(name class)
		        (motif-resources nil)
		   &aux new-shell)
  (description 
    "create a new CLOS Object representing a shell"
    :constructor-for-class shell
    :called-by-gina "in the constructors for subclasses"
    :called-by-application "in constructors of app-defined subclasses") 
  (declare (special *application*))

  ;; create CLOS Object
  (setq new-shell (make-object class (more-initargs :title title)))

  (when (symbolp name)
    (setq name (string-downcase (symbol-name name))))

  ;;(setq motif-resources (more-motif-resources :allow-shell-resize t))
  (when title
    (setq motif-resources (more-motif-resources :title title)))

  ;; create Motif widget in the toolkit server
  (setf (widget-id new-shell)
	(apply #'xtk:create-popup-shell  
	       (append (list name motif-widget-class
			     (if parent 
                                 (widget-id parent) 
                                 (application-shell *application*)))
		       motif-resources)))

  ;; by default the new widget-id is used for sons 
  (setf (widget-id-for-sons new-shell) (widget-id new-shell))
  (setf (group-widget-id    new-shell) (widget-id new-shell))

  (setf (parent new-shell) parent)
  (setf (name   new-shell) name)

  (when parent
    (make-tap new-shell :always #'eval-taps
	      :register-with (enclosing-shell parent)))

  ;; handle Control-right clicks by describing the clicked widget
  (when (inspect-click *application*)
    (add-event-handler new-shell :button-release-mask
		       (make-callback #'inspect-clicked new-shell)))
  
  (when (and (document new-shell) (drag-protocols new-shell))
    ;; signal accepted drag-commands
    (loop for atom in (drag-protocols new-shell)
          do (xtk:add-protocol-callback (widget-id new-shell)
                       :__gina_cmds atom 'ignore-protocol new-shell))
    ;; install event handlers for foreign draggers
    (add-event-handler new-shell :button-press-mask
                       (make-callback #'gina::handle-button-press 
				      new-shell))
    (add-event-handler new-shell :button-release-mask
                       (make-callback #'gina::handle-button-release 
				      (document new-shell)))
    (add-event-handler new-shell :button-motion-mask
                       (make-callback #'gina::handle-button-motion 
				      (document new-shell))))


  new-shell)

(defun ignore-protocol (shell)
  (format t "Ooops! Protocol is called for ~s!~%" shell))

(defginamethod handle-button-press ((shell shell)
				    &rest event-data "a list of event details")
  (description "low level event handler for button-press events"
	       :called-by-gina "in response to foreign drop events"
	       :default-version "propagates command creation to document"
	       :override "if you are interested in special event details")
  ;(format t "Look: ~s~%" event-data)
  (create-drop-command (document shell) shell
		       (second event-data) (third event-data) ;; x y
                              ;; timestamp used to identify atom
		       (xlib:atom-name *display* (eighth event-data))
		       (first *drag-n-drop-transfer-value*)
		       (second *drag-n-drop-transfer-value*)))

;; simply changing the title slot of a shell will show a new window title
(defresourceslot shell title :title :mode ())

(defmethod (setf title) :before (new-title (shell shell))
   "inform toolkit server that shell title has changed"
   (xtk:set-values (group-widget-id shell) :title new-title))

(defginamethod pop-up ((shell shell))
  (description
    "popup the underlying widget"
    :called-by-application "to make a secondary window visible"
    :overridden-by-gina "in some shell subclasses")
  (xtk:popup (group-widget-id shell) :grab-none)
  (setf (visible shell) t)
  (eval-initial-taps shell :pop-up)
  )

(defginamethod pop-down ((shell shell))
  (description
    "popdown the underlying widget"
    :called-by-application "to make a secondary window disappear"
    :overridden-by-gina "in some shell subclasses")
  (setf (visible shell) nil)
  (xtk:popdown (group-widget-id shell)))

(defginamethod enclosing-shell ((shell shell))
  (description
    "override to return shell itself")
  ;; return the shell itself
  shell)

(defmethod destroy :before ((shell shell))
  (setf (visible shell) nil)
  (when (parent shell)
    (remove-all-taps (enclosing-shell (parent shell)) shell)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class tap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass tap ()
  (description "the connection between app data and a widget")
  ((method-to-call   :accessor method-to-call  :initarg :method-to-call)
   (timestamp       :accessor timestamp       :initform -1)
   (static-params   :accessor static-params   :initarg :static-params)
   (not-initial     :accessor not-initial     :initarg :not-initial)))

(defginafun make-tap (widget "a widget, view-object or menu-entry"
		      command-classes "list of class names"
		      method "a function to call with (widget cmd)"
		 &key (not-initial nil) "only call when cmd executed"
		      (static-params nil) "passed to method"
		      (register-with (enclosing-shell widget))
		      (initargs nil)
		      (class 'tap))
  (description "construct a tap"
	       :called-by-application "to establish a tap")
  (let ((new-tap (make-object class initargs
			      :method-to-call method 
			      :static-params static-params
			      :not-initial not-initial)))
    (insert-tap register-with widget 
		(if command-classes
		    (if (listp command-classes)
			command-classes
		        (list command-classes))
		  '(:always)) 
		new-tap)
    ;; usually needed for view-objects
    (unless (or not-initial (not (visible register-with)))
      (evaluate new-tap widget :create (tap-time register-with)))
    new-tap))

(defginamethod class-identifier ((cmd command))
  (description "identify command by class name for taps"
	       :called-by-gina "to find the taps to evaluate for this command")
  (gina::clos-class-of cmd))

(defginamethod class-identifier ((cmd symbol))
  (description "identify command by class name for taps"
	       :called-by-gina "to find the taps to evaluate for this command")
  cmd)

(defmethod evaluate ((tap tap) widget command timestamp)
  "evaluate a single tap"
  ;; timestamp of tap = 0 indicates removed tap
  (unless (or (zerop (timestamp tap)) (= timestamp (timestamp tap)))
    (setf (timestamp tap) timestamp)
    (apply (method-to-call tap)
	   (cons widget (cons command (static-params tap))))))

(defginamethod eval-taps ((shell shell) cmd)
  (description "evaluate all taps for a specific cmd"
	       :called-by-gina "when a command is executed")
  ;;(format t "Eval taps of ~s for cmd ~s~%" shell cmd)
  (loop for batch-cmd in (reverse (getf (taps shell) :batch))
        for cmd-id = (class-identifier batch-cmd)
        do (eval-taps-for-multiple shell 
				   (list (getf (taps shell) cmd-id)
					 (getf (taps shell) :always))
				   batch-cmd 
				   t (visible shell)))
  (setf (getf (taps shell) :batch) nil)
  (when cmd
    (eval-taps-for-multiple shell 
			    (list (getf (taps shell) (class-identifier cmd))
				  (getf (taps shell) :always))
			    cmd t (visible shell))))

(defmethod batch-taps ((shell shell) cmd)
  "batch taps for later processing"
  (setf (getf (taps shell) :batch) (push cmd (getf (taps shell) :batch))))

(defmethod eval-taps-for-multiple ((shell shell) list-of-taplists cmd
				   check-for-initial 
				   check-for-not-initial
				   &aux timestamp)
  "eval tap for a batch of lists, removing duplicates"
  ;; doesn't work because one tap may delete objects that make re-evaluating
  ;; another one necessary
  ;;(setq timestamp (parse-integer (xtk:last-timestamp-processed)))
  (setq timestamp (getf (taps shell) :timestamp))
  (if timestamp (incf timestamp) (setq timestamp 1))
  (setf (getf (taps shell) :timestamp) timestamp)
 
  (loop for one-list in list-of-taplists
        when (listp one-list)
        do (loop for widget in one-list by #'cddr
	         for taps in (rest one-list) by #'cddr do
	      (loop for tap in taps 
	            when (or (and (not-initial tap) check-for-initial)
			     (and (not (not-initial tap)) 
				  check-for-not-initial)) do
			;(loop for prev-list in list-of-taplists
		        ;            when (eq prev-list one-list)
		        ;              do (return nil)
		        ;            when (and (listp prev-list)
			;	              (member tap 
			;			      (getf prev-list widget)))
			;              do (return t))
			  (evaluate tap widget cmd timestamp)))))

;; cmd in the following is probably a symbol
;; taps that rely on features of a special command can
;; specialize their method according to the command class
;; or, e.g. with (eql :create)

(defmethod eval-initial-taps ((shell shell) cmd)
  "eval initial taps for a shell"
  ;; does not handle case when a tap for a new commmand is added by
  ;; another tap
  (eval-taps-for-multiple shell (taps shell) cmd nil t))

(defmethod tap-time ((shell shell))
  (getf (taps shell) :timestamp))

(defmethod insert-tap ((shell shell) widget classes tap)
  "insert a tap under the shell"
  (loop for class in classes
        for taps-and-widgets = (getf (taps shell) class)
        for existing = (getf taps-and-widgets widget)
        do (setf (getf taps-and-widgets widget)
	         (pushnew tap existing))
	   (setf (getf (taps shell) class)
		 taps-and-widgets)))

(defginamethod remove-all-taps ((shell shell) widget)
  (description "remove all taps for a widget"
	       :called-by-application "when an object is deinstalled")
  (loop for class in (taps shell) by #'cddr
        for taps-and-widgets in (rest (taps shell)) by #'cddr
        unless (member class '(:timestamp :batch))
        do (let ((old-taps (getf taps-and-widgets widget)))
	     (loop for old-tap in old-taps
		   do (setf (timestamp old-tap) 0))
	     (when old-taps
	       (setf (getf taps-and-widgets widget) nil)
               (setf (getf (taps shell) class) taps-and-widgets)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; checking updates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginamacro when-changed ((object) "any CLOS object"
				     assignments "in let style"
				     &rest update-code)
  (description "update slot only when changed"
	       :called-by-application "in a tap update function")
  (comment "The update-code is called when a slot value will change."
	   "The form (update-slots) in the update code updates the slot"
	   "values. You can use another method, such as reconfigure, if"
	   "setting the slots is not sufficient")
  (example (when-changed (widget)
             ((value '(1 2 3))
              (sensitive t))
             (format t "Old value ~s~%" (value widget))
             (update-slots)
             (format t "New value ~s~%" (value widget))))
  (list 'let assignments
     `(unless (and ,@(loop for ass in assignments
			  for slot = (first ass)
			  collect `(equal ,slot
					  (slot-value ,object ',slot))))
       ,@(loop for statement in update-code
	       append (if (equal statement '(update-slots))
		 	  (loop for ass in assignments
			        for slot = (first ass)
			        collect `(setf (,slot ,object) ,slot))
			  (list statement))))))

(defginafun compare-lists (source-list view-list 
			   backward-pointing-slot-or-method
		    "function to be applied to objects in view-list"
		           creator
		    "function to create a view-object for a source object"
		      &key (order-is-interesting t)
		           (reusable-objects nil) "old view objects")
  (description "compare two arbitrary lists with backward pointing slot"
	       :called-by-application "for advanced use in taps"
	       :result 
   "(new view-list, new, old, reordered view objects) as multiple values")
  (example (flet ((backward (string) (parse-integer string))
		  (forward (int) (format nil "~d" int)))
	     (print (multiple-value-list 
			(compare-lists 
			 '(0 12 2 3 4 5 1) 
			 '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10")  
			 #'backward #'forward)))))
  (let ((old-ones nil)
	(reordered-forward nil)
	(reordered-backward nil)
	(new-view-list (make-array (length source-list) :initial-element nil))
	(last-position 0))

    (setq old-ones
      (loop for viewer in view-list
            for view-index from 0
            for back = (funcall backward-pointing-slot-or-method viewer)
            for index = (position back source-list)
            do (when index
		 (setf (svref new-view-list index) (list viewer view-index)))
	    unless index
	    collect viewer))

    (when order-is-interesting
      ;; check forward for reordered
      (loop for index from 0 upto (1- (length source-list))
            for entry = (svref new-view-list index)
            when entry
            do (if (= (second entry) last-position)
		   (incf last-position)
	           (when (> (second entry) last-position)
		     (loop for mark from last-position to (1- (second entry))
		           do (push mark reordered-forward))
		     (setq last-position (1+ (second entry))))))
      ;; check backwards for reordered
      (when reordered-forward
        (setq last-position (1- (length view-list)))
        (loop for index from (1- (length source-list)) downto 0
              for entry = (svref new-view-list index)
              when entry
              do ;(format t "Back ~d: entry is ~d, expected is ~d~%"
	         ;        index (second entry) last-position)
                 (if (= (second entry) last-position)
		     (decf last-position)
                     (when (< (second entry) last-position)
		       (loop for mark from last-position 
		                      downto (1+ (second entry))
		             do (push mark reordered-backward))
		       ;(format t "Mark ~d downto ~d~%" 
		       ;        last-position (1+ (second entry)))
		       (setq last-position (1- (second entry)))))))
      ;; check which one is longer
      (if (< (length reordered-forward) (length reordered-backward))
	  (setq reordered-forward (reverse reordered-forward))
          (setq reordered-forward reordered-backward)))

    (values ;; complete new list
            (loop for source in source-list
	          for index from 0
	          for entry = (svref new-view-list index)
	          unless entry
		  do (setq entry
		           (loop for reuse in reusable-objects
		                 when (eq source 
			      (funcall backward-pointing-slot-or-method reuse))
			           do (return reuse)))
		     (unless entry
		       (setq entry (funcall creator source)))
		     (setf (svref new-view-list index)
		           (list entry -1))
	          collect (first (svref new-view-list index)))
	    ;; new entries
	    (loop repeat (length source-list) ;; i.e. for source in source-list
                  for index from 0
	          for entry = (svref new-view-list index)
	          when (= (second entry) -1)
	          collect (first entry))
	    ;; old entries
	    old-ones 
	    ;; reordered entries
	    (when order-is-interesting
	      (loop for view in view-list
	            for index from 0
	            for to-collect = (and reordered-forward
			  		  (= index (first reordered-forward))
					  view)
	            do (when to-collect 
		         (setq reordered-forward (rest reordered-forward)))
                    when (and to-collect (not (member view old-ones)))
	            collect to-collect))
	  )))


(defginamethod compare-view-objects-with-list ((view view) objects 
					   backward-pointing-slot-or-method
					   creator
					   &key (order-is-interesting t))
  "compare view objects with a list of document objects"
  (multiple-value-bind (new-list new-ones old-ones reordered-ones)
      (compare-lists objects (view-objects view)
		     backward-pointing-slot-or-method creator
		     :order-is-interesting order-is-interesting
		     :reusable-objects (old-view-objects view))
    (loop for old in old-ones do 
	  (deinstall old)
	  (pushnew old (old-view-objects view)))
    (loop for new in new-ones do
      (install new view (x-pos new) (y-pos new) :redraw nil))
    (when reordered-ones
      (setf (view-objects view) new-list))
    (loop for new in new-ones do
      (force-redraw new))
    (loop for new in reordered-ones do
      (force-redraw new))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class toplevel-shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; toplevel-shells
;;;;;;;;;;;;;; - know their underlying X-Window
;; - can set the a cursor for the window
;; - can be exposed by the application
;; - are known by the application
;; - receive a message when closed by the Window Manager
;; - can have the invisible application-shell as there father

'(defginaclass toplevel-shell (shell)

   (description
     "the abstract superclass of all document-shells and modeless dialog boxes"
     :application-subclasses :rarely
     :gina-subclasses t
     :instantiate nil)

   ())

(defginafun make-toplevel-shell (title "the title string"
			    &key (parent nil) 
			         "default: directly under invisible app-shell"
			         (allow-shell-resize nil)
                                   "whether resizable by changing children"
			         (class 'toplevel-shell)
			         (initargs nil)
				 (motif-widget-class :toplevel-shell)
				 (name class)
				 (motif-resources nil)
			    &aux new-toplevel-shell)
  (description 
    "create a new CLOS Object representing a toplevel-shell"
    :constructor-for-class toplevel-shell
    :called-by-gina "in the constructors for subclasses")
  (setq new-toplevel-shell
	(make-shell parent title
		    :class class
		    :initargs initargs
		    :motif-widget-class motif-widget-class
		    :name name
		    :motif-resources (more-motif-resources
				       :allow-shell-resize allow-shell-resize
				       :delete-response :do-nothing)))
  ;; inform the shell, when user has resized it
  (add-event-handler new-toplevel-shell :structure-notify-mask
		     (make-callback #'handle-resize-event new-toplevel-shell))
  
  ;; used by send-message
  (add-event-handler 
     new-toplevel-shell :no-event-mask
     (make-callback #'handle-client-message new-toplevel-shell))
        
  ;; tell the window manager to inform application when shell is to be deleted
  (xtk:add-wm-protocol-callback (group-widget-id new-toplevel-shell)
			    :wm-delete-window #'gina-callback
			    (make-callback #'closed-by-wm new-toplevel-shell))
  new-toplevel-shell)

(defginamethod pop-up :after ((shell toplevel-shell))
  "register shell at the application"
  (declare (special *application*))
  ;; register the toplevel-shell at the application
  (pushnew shell (toplevel-shells *application*)))

(defun handle-resize-event (shell &rest event-data)
  "call GINA method in response to event"
  (declare (ignore event-data))
  (resized-by-user shell))

(defun handle-client-message (shell &rest event-data)
  "read mailbox when client message arrives"
  (declare (ignore shell))
  (when (= (first event-data) 33) ;; client-message
    (read-mailbox *application*)))

(defginamethod resized-by-user ((shell toplevel-shell))
  (description
    "hook for reaction to user resizing the shell"
    :override :sometimes
    :default-version "calls update-slots")
  (comment
    "May currently be called even if size unchanged")
  ;; default: just ask for the new size
  (update-slots shell)

  ;; after resizing from the upper-left corner sometimes wrong position
  ;;(format t "New Size: ~d ~d ~d ~d~%" 
	  ;;(x-pos shell) (y-pos shell) (width shell) (height shell))
  )

(defginamethod expose ((shell toplevel-shell))
  (description
    "bring window to front and set input focus"
    :called-by-application "to focus attention on a secondary window")
  (comment
    "Should be called only by the process owning the shell.")
  (declare (special *display*))  
  (xtk::raise-widget (widget-id shell)))

;; for the moment: popdown instead of destroy-widget
(defginamethod destroy ((shell toplevel-shell))
  (description
    "destroy the shell and all its children"
    :called-by-application "to destroy a secondary window")
  (declare (special *application* *display*))
  
  ;; tell the toolkitserver to destroy the window
  (xtk:popdown (group-widget-id shell))
  (with-slots (toplevel-shells) *application*
    ;; unregister shell at application
    (setq toplevel-shells (delete shell toplevel-shells))))

(defginamethod closed-by-wm ((shell toplevel-shell))
  (description
    "shell is being closed by the window manager"
    :override :sometimes
    :overridden-by-gina "in some subclasses"
    :default-version "calls destroy")
  (destroy shell))

;; now method for all widgets !!
;; wirkt nicht auf children mit eigenem cursor !!
;(defginamethod define-cursor ((shell toplevel-shell) cursor-nr-or-keyword)
;  "set the cursor for a toplevel-shell"
;  (declare (special *display*))
;  (setf (xlib:window-cursor (x-window shell))
;	(make-cursor cursor-nr-or-keyword))
;  (xlib:display-force-output *display*))
;
;(print (xtk:all-cursors))
;(with-application-stopped (define-cursor (main-shell (first (document-list
;						*application*))) :watch))
;(with-application-stopped (define-cursor (main-shell (first (document-list
;						*application*))) :restore))

;'(defginamacro with-clock-cursor (&body body)
;   (description
;     "use clock cursor for the execution of body in all application windows"
;     :called-by-application "to signal a time-consuming operation")
;   (example
;     (with-clock-cursor (sleep 10)))
;   ;; later: grab-pointer
;   ;; also: save old cursor !!
;   `(unwind-protect
;	(progn
;	  (loop for shell in (toplevel-shells *application*)
;		do (define-cursor shell :watch))
;	  . ,body)
;      ;; cleanup-action
;      (loop for shell in (toplevel-shells *application*)
;	    do (define-cursor shell :restore))
;      ))

'(defginamacro with-cursor ((cursor) &body body)
   (description
     "use cursor for the execution of body in all application windows"
     :called-by-application :sometimes)
   (example
     (with-cursor (:circle) (sleep 10)))
   `(let ((affected-shells (toplevel-shells *application*)))
      (unwind-protect
	(progn
	  (push ,cursor (cursor-stack *application*))
	  ;;(format t "set-cursor: ~s~%" affected-shells)
	  (loop for shell in affected-shells    
		do (define-cursor shell ,cursor))
	  ,@body)
	;; cleanup-action
	(pop (cursor-stack *application*))
	(let ((old-cursor (first (cursor-stack *application*))))
	  ;;(format t "ret-cursor: ~s ~s~%" affected-shells old-cursor)
	  (loop for shell in affected-shells
	   do (define-cursor shell (or old-cursor :restore))))
	)))

'(with-application-stopped 
  (with-cursor (:circle) 
   (sleep 5)
   (with-cursor (:hand) (sleep 5))
   (sleep 5)))

'(with-application-stopped 
  (with-cursor (:circle) 
   (sleep 5)
   (with-cursor (:circle) (sleep 5))
   (sleep 5)))

'(defginamacro with-clock-cursor (&body body)
   (description
     "use clock cursor for the execution of body in all application windows"
     :called-by-application "to signal a time-consuming operation")
   (example
     (with-clock-cursor (sleep 10)))
   `(with-cursor (:watch)
     ,(cons 'progn body)))

'(with-application-stopped (with-clock-cursor (sleep 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class modeless-dialog-shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass modeless-dialog-shell (toplevel-shell)

   (description
     "the Motif dialog-shell widget used for modeless dialogs"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ((dialog-box         :accessor dialog-box
         :documentation "its child, must be set after creation")))

(defginafun make-modeless-dialog-shell (title "the title string"
	        &key (parent-shell
			  (main-shell (first (document-list *application*))))
                       "default: main-shell of first document of application"
		     (allow-shell-resize nil)
                       "whether resizable by changing children"
		     (class 'modeless-dialog-shell)
		     (initargs nil)
		     (motif-widget-class :dialog-shell)
		     (name class)
		     (motif-resources nil))
  (description 
    "create CLOS Object for a dialog-shell for modeless dialogs"
    :constructor-for-class modeless-dialog-shell)
  (make-toplevel-shell title
		       :parent parent-shell
		       :allow-shell-resize allow-shell-resize
		       :class class
		       :initargs initargs
		       :motif-widget-class motif-widget-class
		       :name name
		       :motif-resources motif-resources))

(defginamethod pop-up ((shell modeless-dialog-shell))
  (description
    "popup the underlying widget"
    :called-by-application "to make a secondary window visible"
    :overridden-by-gina "in some shell subclasses")
  (declare (special *application*))
  (when (dialog-box shell)
      (xtk:manage-popup-child (widget-id (dialog-box shell)))
      (setf (visible shell) t)
      (eval-initial-taps shell :pop-up)
      (pushnew shell (toplevel-shells *application*))))

(defginamethod pop-down ((shell modeless-dialog-shell))
  (description
    "popdown the underlying widget"
    :called-by-application "to make a secondary window disappear"
    :overridden-by-gina "in some shell subclasses")
  (declare (special *application*))
  (when (dialog-box shell)
      (setf (visible shell) nil)
      (xtk:unmanage-popup-child (widget-id (dialog-box shell)))
      (setf (toplevel-shells *application*)
            (delete shell (toplevel-shells *application*)))))

(defmethod closed-by-wm ((shell modeless-dialog-shell))
  "propagate message to dialog-box"
  (closed-by-wm (dialog-box shell)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class modal-dialog-shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass modal-dialog-shell (shell)

   (description
     "the Motif dialog-shell widget used for modal dialogs"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ())

(defginafun make-modal-dialog-shell (title "the title string"
		&key (parent-shell
			  (main-shell (first (document-list *application*))))
                       "default: main shell of first document of application"
		     (allow-shell-resize nil)
                       "whether resizable by changing children"
		     (class 'modal-dialog-shell)
		     (initargs nil)
		     (motif-widget-class :dialog-shell)
		     (name class)
		     (motif-resources nil))
  (description 
    "create CLOS Object for a dialog-shell for modal dialogs"
    :constructor-for-class modal-dialog-shell)
  (make-shell parent-shell title
	      :class class
	      :initargs initargs
	      :motif-widget-class motif-widget-class
	      :name name
	      :motif-resources (more-motif-resources
				 :allow-shell-resize allow-shell-resize)))

(defginamethod closed-by-wm ((shell modal-dialog-shell))
  (description
    "modal-dialog-shells cannot be closed by the window manager"
    :default-version "just beeps")
  (declare (special *display*))
  (xlib:bell *display*))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class menu-shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass menu-shell (shell)
   (description
     "the Motif menu-shell widget"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ())

(defginafun make-menu-shell (menubar 
                              "the parent widget, a menu bar or menu shell"
			&key (class 'menu-shell)
			     (initargs nil)
			     (motif-widget-class :menu-shell)
			     (name class)
			     (motif-resources nil))
  (description 
    "create a new CLOS Object representing a menu-shell"
    :constructor-for-class menu-shell
    :called-by-application :sometimes)
  (make-shell menubar nil
	      :class class
	      :initargs initargs
	      :motif-widget-class motif-widget-class
	      :name name
	      :motif-resources (more-motif-resources
				 :width 5 :height 5
				 ;;:save-under nil ;; necessary for screen dumps
				 :allow-shell-resize t
				 :override-redirect t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  class basic-document-shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass basic-document-shell (toplevel-shell)

   (description
     "a toplevel-shell for a document without a menubar"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ((document           :accessor document :initarg :document
	 :documentation "the document the shell belongs to")
    (x-window           :accessor x-window :initform nil
         :documentation "the window to send messages to")))

(defginafun make-basic-document-shell (document "the document object"
				 &key (width :auto) ":auto or fixed width"
                                      (height :auto) ":auto or fixed height"
				      (title (name *application*))
                                        "title string"
		                      (allow-shell-resize nil)
                                        "whether resizable by changing children"
                                      (drag-protocols (drag-protocols document))
                                        "keyword list of accepted drag-commands"
				      (class 'basic-document-shell)
				      (initargs nil)
				      (motif-widget-class :toplevel-shell)
				      (name class)
				      (motif-resources nil))
  (description 
    "create a new toplevel-shell for a document without a menubar"
    :constructor-for-class basic-document-shell
    :called-by-application :sometimes)
  (declare (special *application*))

  ;; by default shell-size is determined by motif geometry management
  (when (numberp width)
    ;; explicit width specified
    (setq motif-resources (more-motif-resources :width width)))
  (when (numberp height)
    ;; explicit height specified
    (setq motif-resources (more-motif-resources :height height)))
  
  (let ((new-shell (make-toplevel-shell title
		       :class class
                       :allow-shell-resize allow-shell-resize
		       :initargs (more-initargs 
                                    :document document 
				    :drag-protocols drag-protocols)
		       :motif-widget-class motif-widget-class
		       :name name
		       :motif-resources motif-resources)))
    new-shell))

(defmethod pop-up :after ((shell basic-document-shell))
   "register at document"
   (declare (special *display*))
   (setf (x-window shell) (xtk:make-clx-window (group-widget-id shell)))
 ;         (xtk:make-x-window (xtk:get-window-id (group-widget-id shell))
 ;                            *display*)
   (pushnew shell (toplevel-shells (document shell))))

(defginamethod destroy :before ((shell basic-document-shell))
  "update list of toplevel-shells of the document"
  (with-slots (toplevel-shells) (document shell)
    (setq toplevel-shells (delete shell toplevel-shells))))

(defginamethod closed-by-wm ((shell basic-document-shell))
  (description
    "closing a document shell means closing its document"
    :override :sometimes
    :default-version "closes the document")
  (declare (special *display*))
  ;;(format t "closed-by-wm")
  (if (not (active-modal-dialogs (document shell)))
      (do-close (document shell))
      (xlib:bell *display*)))

;(defginamethod (setf width) :after (new-width (shell basic-document-shell))
;  "the shell size has been changed => document modified"
;  (declare (ignore new-width))
;  (setf (modified (document shell)) t))

;(defginamethod (setf height) :after (new-height (shell basic-document-shell))
;  "the window size has been changed => document modified"
;  (declare (ignore new-height))
;  (setf (modified (document shell)) t))

;; perhaps later:
;(defginamethod resized-by-user :after ((shell basic-document-shell))
;  "the window size has been changed => document modified"
;  (setf (modified (document shell)) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  class document-shell 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass document-shell (basic-document-shell)

   (description
     "a toplevel-shell with a main-menu, representing a document"
     :application-subclasses "for application-specific main windows"
     :gina-subclasses t)

   ((main-window        :accessor main-window
         :documentation "automatically created main window")
    (main-menu   :accessor main-menu
         :documentation "automatically created menu with default entries")))

(defginafun make-document-shell (document "the document object"
			     &key (width :auto) ":auto or fixed width"
                                  (height :auto) ":auto or fixed height"
                                  (is-main-shell t)
                                    "whether register as main shell of document"
                                  (with-menu t)
                                    "whether create the standard menu bar"
		                  (allow-shell-resize nil)
                                    "whether resizable by changing children"
                                  (drag-protocols (drag-protocols document))
                                    "keyword list of accepted drag-commands"
			          (class 'document-shell)
				  (initargs nil)
				  (motif-widget-class :toplevel-shell)
				  (name class)
				  (motif-resources nil)
			     &aux new-shell title)
  (description 
    "create toplevel-shell with a menu-bar, representing a document"
    :constructor-for-class document-shell
    :called-by-application :sometimes)
  (declare (special *application*))

  ;; use shell-width and shell-height stored in document as a default
  (with-slots (shell-width shell-height) document
    (when (eq width :auto)
      (setq width shell-width))
    (when (eq height :auto)
      (setq height shell-height)))

  (setq title (format nil "~a: ~a" (name *application*) (name document)))
  
  (setq new-shell
	(make-basic-document-shell document
				   :title title
				   :width width :height height
                                   :allow-shell-resize allow-shell-resize
				   :drag-protocols drag-protocols
				   :class class
				   :initargs initargs
				   :motif-widget-class motif-widget-class
				   :name name
				   :motif-resources 
                                       (more-motif-resources
					:icon-name (name document))))
  
  (with-slots (main-window main-menu widget-id-for-sons) new-shell
    ;; create main-window
    (setq main-window (make-main-window new-shell))

    ;; create-main-menu
    (when with-menu
        (setq main-menu (make-main-menu main-window))
        (standard-menu-items document new-shell))

    (when is-main-shell
        (setf (main-shell document) new-shell))

    ;; creating sons of the shell should be actually sons of the main-window
    (setq widget-id-for-sons  (widget-id main-window))
    
    ;; return the new window
    new-shell))

(defun x-parent (x-window)
  "find parent of an x-window"
  (second (multiple-value-list
	    (xlib:query-tree x-window))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class  document-shell-with-scroller
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass document-shell-with-scroller (document-shell)

   (description
     "a document-shell with a single scroller"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ((scroller           :accessor scroller
         :documentation "automatically created scroller as main window child")))

(defginafun make-document-shell-with-scroller (document "the document object"
				 &key (width :auto) ":auto or fixed width"
                                      (height :auto) ":auto or fixed height"
		                      (allow-shell-resize nil)
                                        "whether resizable by changing children"
                                      (drag-protocols (drag-protocols document))
                                        "keyword list of accepted drag-commands"
				      (class 'document-shell-with-scroller)
				      (initargs nil)
				      (motif-widget-class :toplevel-shell)
				      (name class)
				      (motif-resources nil)
				 &aux new-shell)
  (description 
    "create a new document-shell with a single scroller"
    :constructor-for-class document-shell-with-scroller
    :called-by-application :sometimes)
  (setq new-shell (make-document-shell document
				       :width width :height height
				       :class class
                                       :allow-shell-resize allow-shell-resize
				       :drag-protocols drag-protocols
				       :initargs initargs
				       :motif-widget-class motif-widget-class
				       :name name
				       :motif-resources motif-resources))
  (with-slots (scroller widget-id-for-sons) new-shell
    ;; create the single scroller
    (setq scroller (make-scroller new-shell))

    ;; creating sons of the shell should be actually sons of the scroller
    (setq widget-id-for-sons (widget-id-for-sons scroller))

    ;; return the new window
    new-shell))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class label
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass label (widget)

   (description
     "the Motif label widget"
     :application-subclasses :rarely
     :gina-subclasses t)

   ((label-string       :accessor label-string :initarg :label-string
	 :documentation "the string shown")
    (label-type         :accessor label-type   :initarg :label-type
         :documentation "either :string or :pixmap")))

(defginafun make-label (parent "the parent widget"
                        label-string "the label string"
		   &key (alignment :center) 
                          ":center :end :beginning"
		        (recompute-size t)
                          "whether request new size if label string changed"
			(label-type :string) 
                          "if :pixmap, label-string is regarded as filename"
			(sensitive t)
		        (class 'label)
		        (initargs nil)
			(motif-widget-class :label)
			(name class)
			(managed t)
			(motif-resources nil))
  (description 
    "create a new label widget"
    :constructor-for-class label
    :called-by-application :sometimes)
  (when (eq label-type :pixmap)
    (setq motif-resources 
      (more-motif-resources 
       :label-pixmap (find-bitmap label-string :for-clm t))))
  (when (eq label-type :string)
    (setq motif-resources (more-motif-resources :label-string label-string)))
    
  (make-widget parent
	       :class class
	       :initargs (more-initargs :label-string label-string
                                        :label-type label-type)
	       :motif-widget-class motif-widget-class
	       :name name
	       :managed managed
	       :motif-resources (more-motif-resources
				  :label-type label-type
				  :sensitive sensitive
				  :recompute-size recompute-size
				  :alignment alignment)))

;(defresourceslot label label-string :label-string :mode (:write))

(defresourceslot label label-string :label-string :mode ())

(defmethod (setf label-string) :after (new-label-string (label label))
  "inform the toolkit server that label has changed"
  (when (eq (label-type label) :string)
        (set-motif-resources label :label-string new-label-string))
  (when (eq (label-type label) :pixmap)
        (set-motif-resources label 
			     :label-pixmap 
			     (find-bitmap new-label-string :for-clm t))))

(defresourceslot label label-type :label-type :mode ())

(defmethod (setf label-type) :after (new-label-type (label label))
  "inform the toolkit server that label-type has changed"
  (set-motif-resources label :label-type new-label-type)
  (setf (label-string label) (label-string label)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class drag-label
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass drag-label (label)
  (description "a picture label for drag and drop"
       :application-subclasses :rarely)
  ((drag-shell   :accessor drag-shell
       :documentation "a shell with duplicate image to move around")
   (cursor       :accessor cursor      :initform :dotbox
       :documentation "a cursor created from the label pixmap")
   (activate-callback :accessor activate-callback :initform nil)))

(defcallback drag-label activate-callback nil :lambda-list (x y))

(defginafun make-drag-label (parent 
                             document "the document to register commands with"
                             pixmap "name of a bitmap file"
                        &key (cursor-mask nil) 
			       "a bitmap file as cursor mask, or nil"
			     (activate-callback nil) 
			       "a callback that creates a drag-command"
			     (sensitive t)
			     (class 'drag-label)
                             (initargs nil)
			     (motif-widget-class :label)
			     (name class)
			     (managed t)
			     (motif-resources nil)
			     (shell-resources '(:border-width 1))
                        &aux new-label)
  (description 
    "create a new drag label"
    :constructor-for-class drag-label
    :called-by-application :sometimes)
  ;(declare (special *application*))
  (setq new-label (make-label parent pixmap :label-type :pixmap 
			      :sensitive sensitive
                              :class class :initargs initargs
			      :motif-widget-class motif-widget-class
			      :name name :managed managed
			      :motif-resources motif-resources))
  (setf (drag-shell new-label)
        (make-shell new-label nil :motif-widget-class 'override-shell
                          :motif-resources shell-resources))
  (make-label (drag-shell new-label) pixmap :label-type :pixmap
	      :sensitive sensitive :motif-widget-class motif-widget-class
	      :name name :motif-resources motif-resources)
  (when cursor-mask
      (setf (cursor new-label)
            (xtk:create-pixmap-cursor (find-bitmap pixmap :for-clm t)
				      (find-bitmap cursor-mask :for-clm t))))
  (add-event-handler new-label :button-release-mask
                     (make-callback #'handle-button-release document))
  (add-event-handler new-label :button-motion-mask
                     (make-callback #'handle-button-motion document))
  (add-event-handler new-label :button-press-mask
                     (make-callback #'handle-button-press new-label))
  (when activate-callback
      (setf (activate-callback new-label) activate-callback))
  new-label)

(defmethod handle-button-press ((label drag-label) &rest event-data)
  (execute (activate-callback label) 
           (list (second event-data) (third event-data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class toggle-button
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass toggle-button (label)

   (description
     "the Motif toggle button widget"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ((value              :accessor value :initarg :value
	 :documentation "whether currently selected or not")
    (value-changed-callback :accessor value-changed-callback 
                            :initarg :value-changed-callback)
    (arm-callback       :accessor arm-callback :initform nil)
    (disarm-callback    :accessor disarm-callback :initform nil)))

(defcallback toggle-button value-changed-callback 
                           :value-changed :lambda-list (value))
(defcallback toggle-button arm-callback :arm :lambda-list (value))
(defcallback toggle-button disarm-callback :disarm :lambda-list (value))

(defresourceslot toggle-button value :set)

(defginafun make-toggle-button (parent "the parent widget" 
                                string "the label-string"
		       &key (alignment :center) 
                              ":center :end :beginning"
		            (indicator-type :n-of-many) 
                              ":n-of-many or :one-of-many"
		            (label-type :string) 
			      "if :pixmap, label-string is regarded as filename"
			    (indicator-on (eq label-type :string))
                              "if display toggle indicator"
			    (value nil) "initial value"
			    (value-changed-callback :call-method)
                              ":call-method or a callback"
			    (spacing 4) "between toggle and label"
                            (recompute-size t)
                              "whether request new size if label-string changed"
			    (sensitive t)
			    (class 'toggle-button)
			    (initargs nil)
			    (motif-widget-class :toggle-button)
			    (name class)
			    (managed t)
			    (motif-resources nil)
		       &aux new-toggle-button)
  (description 
    "make a new toggle-button widget"
    :constructor-for-class toggle-button
    :called-by-application :sometimes)
  (declare (ignore alignment)) ;; intentionally not passed to make-label???
  (when (not indicator-on)
    "add shadows so that the button appears to be pressed in"
    (setq motif-resources (more-motif-resources :shadow-thickness 2)))
  
  (setq new-toggle-button
	(make-label parent string
		    :label-type label-type
                    :recompute-size recompute-size
		    :class class
		    :initargs (more-initargs :value value)
		    :motif-widget-class motif-widget-class
		    :name name
		    :managed managed
		    :motif-resources (more-motif-resources
				       :indicator-type indicator-type
				       :indicator-on indicator-on
				       :set value
				       :spacing spacing
				       :sensitive sensitive
				       )))

  (when value-changed-callback
    (if (eq :call-method value-changed-callback)
	(setf (value-changed-callback new-toggle-button)
	      (make-callback #'execute-value-changed-callback 
                             new-toggle-button))
	(setf (value-changed-callback new-toggle-button) 
              value-changed-callback)))
  
  new-toggle-button)

(defginamethod execute-value-changed-callback ((button toggle-button) 
                                           set "state of toggle"
					   &optional ignore1 ignore2)
  (description
    "semantics of a users mouse click into the toggle-button"
    :override "in application-specific subclasses"
    :default-version :do-nothing)
  (comment
    "This method is only called if :call-method is specified on creation.")
  (declare (ignore set ignore1 ignore2))
  ;;(format t "Set: ~s~%" set)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class push-button
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass push-button (label)

   (description
     "the Motif push button widget"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ((activate-callback :accessor activate-callback :initform nil)
    (arm-callback :accessor arm-callback :initform nil)
    (disarm-callback :accessor disarm-callback :initform nil)))

(defcallback push-button activate-callback :activate :lambda-list "()")
(defcallback push-button arm-callback :arm :lambda-list "()")
(defcallback push-button disarm-callback :disarm :lambda-list "()")

(defginafun make-push-button (parent "the parent widget"
                              string "the label-string"
		     &key (show-as-default 0) 
                            "size of frame for default buttons"
		          (label-type :string)
		            "if :pixmap, label-string is regarded as filename"
			  (activate-callback :call-method)
                            ":call-method or a callback"
			  (alignment :center) ":center :end :beginning"
		          (recompute-size t)
                            "whether request new size if label-string changed"
		          (sensitive t)
		          (class 'push-button)
		          (initargs nil)
		          (motif-widget-class :push-button)
		          (name class)
		          (managed t)
		          (motif-resources nil)
		     &aux new-push-button)
  (description 
    "make a new push-button widget"
    :constructor-for-class push-button
    :called-by-application :sometimes)
  (setq new-push-button
	(make-label parent string
		    :label-type label-type
		    :alignment alignment
		    :recompute-size recompute-size
		    :sensitive sensitive
		    :class class
		    :initargs initargs
		    :motif-widget-class motif-widget-class
		    :name name
		    :managed managed
		    :motif-resources (more-motif-resources
				       :show-as-default show-as-default)))

  (when activate-callback
    (if (eq :call-method activate-callback)
	(setf (activate-callback new-push-button)
	      (make-callback #'execute-activate-callback new-push-button))
	(setf (activate-callback new-push-button) activate-callback)))
  
  new-push-button)

(defginamethod execute-activate-callback ((pb push-button))
  (description
    "semantics of a users mouse click into the push-button"
    :override "in application-specific subclasses"
    :default-version :do-nothing)
  (comment
    "This method is only called if :call-method is specified on creation.")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class cascade-button
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass cascade-button (label)

   (description
     "the Motif cascade button widget"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ((activate-callback  :accessor activate-callback  :initform nil)
    (cascading-callback :accessor cascading-callback :initform nil)))

(defcallback cascade-button activate-callback :activate :lambda-list "()")
(defcallback cascade-button cascading-callback :cascading :lambda-list "()")

(defginafun make-cascade-button (parent "the parent widget"
                                 string "the label string"
                                 submenu "the pull-down submenu"
		      &key (label-type :string) 
		             "if :pixmap, label-string is regarded as filename"
			   (sensitive t)
			   (class 'cascade-button)
		           (initargs nil)
			   (motif-widget-class :cascade-button)
			   (name class)
			   (managed t)
			   (motif-resources nil))
  (description 
    "make a new cascade-button widget"
    :constructor-for-class cascade-button
    :called-by-application :sometimes)
  (make-label parent string
	      :label-type label-type
	      :sensitive sensitive
	      :class class
	      :initargs initargs
	      :motif-widget-class motif-widget-class
	      :name name
	      :managed managed
	      :motif-resources (more-motif-resources
				 :sub-menu-id (widget-id submenu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; class beep-button
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;; Beispiel fuer eine Unterklasse eines Widgets
;;; - neue defaults fuer vorgesehene Parameter (string="Beep")
;;; - Verwendung weiterer Resourcen (:width)
;;; - Verwendung weiterer Callbacks (:arm)
;;; - Spezielles Verhalten durch Ueberschreiben einer Methode
;;;     (execute-activate-callback)
;;; - Spezielles Verhalten durch setzen eines Callbacks (:arm)
;
;
;'(defginaclass beep-button (push-button)
;   "A Push-Button that beeps"
;   ())
;
;(defginafun make-beep-button (parent
;			 &key (class 'beep-button)
;			      (initargs nil)
;			      (motif-resources nil)
;			 &aux new-beep-button)
;  (description "make a pushbutton that beeps"
;	       :constructor-for-class beep-button
;	       :called-by-application :sometimes)
;  (setq new-beep-button
;	(make-push-button parent "Beep"
;			  :show-as-default 5
;			  :class class
;			  :initargs initargs
;			  :motif-resources (more-motif-resources
;					     :width 400)))
;  ;; beep button also beeps when armed
;  (setf (arm-callback new-beep-button)
;	(make-callback #'execute-activate-callback new-beep-button))
;  
;  new-beep-button)
;
;(defginamethod execute-activate-callback ((bb beep-button))
;  "beep when user clicks"
;  (declare (special *display*))
;  (xlib:bell *display*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class arrow-button
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass arrow-button (widget)

   (description
     "the Motif arrow button widget"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ((activate-callback :accessor activate-callback :initform nil)
    (arm-callback :accessor arm-callback :initform nil)
    (disarm-callback :accessor disarm-callback :initform nil)))

(defcallback arrow-button activate-callback :activate :lambda-list "()")
(defcallback arrow-button arm-callback :arm :lambda-list "()")
(defcallback arrow-button disarm-callback :disarm :lambda-list "()")

(defginafun make-arrow-button (parent "the parent widget"
			  &key
			  (arrow-direction :arrow-up)
			    ":arrow-up, :arrow-down, :arrow-left, :arrow-right"
			  (activate-callback :call-method)
                            ":call-method or a callback"
			  (sensitive t)
			  (class 'arrow-button)
			  (initargs nil)
			  (motif-widget-class :arrow-button)
			  (name class)
			  (managed t)
			  (motif-resources nil)
			  &aux new-arrow-button)
  (description 
    "make a new arrow-button widget"
    :constructor-for-class arrow-button
    :called-by-application :sometimes)
  (setq new-arrow-button
	(make-widget parent
		     :class class
		     :initargs initargs
		     :motif-widget-class motif-widget-class
		     :name name
		     :managed managed
		     :motif-resources (more-motif-resources
					:arrow-direction arrow-direction
					:sensitive sensitive)))
  (when activate-callback
    (if (eq :call-method activate-callback)
	(setf (activate-callback new-arrow-button)
	      (make-callback #'execute-activate-callback new-arrow-button))
	(setf (activate-callback new-arrow-button) activate-callback)))
  
  new-arrow-button)

(defginamethod execute-activate-callback ((ab arrow-button))
  (description
    "semantics of a users mouse click into the arrow-button"
    :override "in application-specific subclasses"
    :default-version :do-nothing)
  (comment
    "This method is only called if :call-method is specified on creation.")
  ;; this default method may be overridden in subclasses with special semantics
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class scale
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass scale (widget)

   (description
     "the Motif scale widget"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ((value              :accessor value :initarg :value
         :documentation "current value between min and max")
    (maximum            :accessor maximum :initarg :maximum
         :documentation "maximum value")
    (minimum            :accessor minimum :initarg :minimum
         :documentation "minimum value")
    (value-changed-callback :accessor value-changed-callback 
                            :initarg :value-changed-callback)
    (drag-callback :accessor drag-callback :initform nil)))

(defcallback scale value-changed-callback :value-changed 
                   :lambda-list (new-value))
(defcallback scale drag-callback :drag :lambda-list (new-value))
(defresourceslot scale value :value)

(defginafun make-scale (parent "the parent widget"
		   &key (orientation :vertical) 
                          ":vertical or :horizontal"
		        (processing-direction (if (eq orientation :vertical)
						  :max-on-top :max-on-right))
                          ":max-on-top :max-on-right :max-on-top :max-on-bottom"
		        (title-string "") "identifying label"
		        (minimum 0) "minimum value"
			(maximum 100) "maximum value"
			(value 0) "initial value"
			(value-changed-callback :call-method)
                          ":call-method or a callback"
			(show-value t)
                          "whether current value is displayed as a number"
		        (sensitive t)
			(class 'scale)
			(initargs nil)
			(motif-widget-class :scale)
			(name class)
			(managed t)
			(motif-resources nil)
		   &aux new-scale)
  (description 
    "make a new scale widget"
    :constructor-for-class scale
    :called-by-application :sometimes)
  (setq new-scale
	(make-widget parent
		    :class class
		    :initargs (more-initargs
				:minimum minimum
				:maximum maximum
				:value value)
		    :motif-widget-class motif-widget-class
		    :name name
		    :managed managed
		    :motif-resources (more-motif-resources
				      :title-string title-string
				      :orientation orientation
				      :processing-direction processing-direction
				      :minimum minimum
				      :maximum maximum
				      :value value
				      :show-value show-value
				      :sensitive sensitive)))

  (when value-changed-callback
    (if (eq :call-method value-changed-callback)
	(setf (value-changed-callback new-scale)
	      (make-callback #'execute-value-changed-callback new-scale))
	(setf (value-changed-callback new-scale) value-changed-callback)))
  
  new-scale)

(defginamethod execute-value-changed-callback ((scale scale) new-value
					       &optional ignore1 ignore2)
  (description
    "semantics of a users modification of the current value"
    :override "in application-specific subclasses"
    :default-version :do-nothing)
  (comment
    "This method is only called if :call-method is specified on creation.")
  (declare (ignore new-value ignore1 ignore2))
  ;;(format t "New value: ~s~%"new-value)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class scrollbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass scrollbar (widget)

   (description
     "the Motif scroll bar widget"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ((value              :accessor value     :initarg :value
         :documentation "current value between min and max")
    (maximum            :accessor maximum   :initarg :maximum
         :documentation "maximum value in application-defined units")
    (minimum            :accessor minimum   :initarg :minimum
         :documentation "minimum value in application-defined units")
    (increment          :accessor increment :initarg :increment
         :documentation "single-step increment in application-defined units")
    (page-increment     :accessor page-increment :initarg :page-increment
         :documentation "page-step increment in application-defined units")
    (slider-size        :accessor slider-size   :initarg :slider-size
         :documentation "slider size in application-defined units")
    (value-changed-callback :accessor value-changed-callback 
                            :initarg :value-changed-callback)
    (decrement-callback      :accessor decrement-callback      :initform nil)
    (increment-callback      :accessor increment-callback      :initform nil)
    (page-decrement-callback :accessor page-decrement-callback :initform nil)
    (page-increment-callback :accessor page-increment-callback :initform nil)
    (to-bottom-callback      :accessor to-bottom-callback      :initform nil)
    (to-top-callback         :accessor to-top-callback         :initform nil)
    (drag-callback           :accessor drag-callback           :initform nil)
    ))

(defresourceslot scrollbar value :value)
(defresourceslot scrollbar maximum :maximum)
(defresourceslot scrollbar minimum :minimum)
(defresourceslot scrollbar increment :increment)
(defresourceslot scrollbar page-increment :page-increment)
(defresourceslot scrollbar slider-size :slider-size)

(defcallback scrollbar value-changed-callback  :value-changed
	     :lambda-list (new-value))
(defcallback scrollbar drag-callback           :drag
	     :lambda-list (new-value))
(defcallback scrollbar decrement-callback      :decrement
	     :lambda-list (new-value))
(defcallback scrollbar increment-callback      :increment
	     :lambda-list (new-value))
(defcallback scrollbar page-decrement-callback :page-decrement
	     :lambda-list (new-value))
(defcallback scrollbar page-increment-callback :page-increment
	     :lambda-list (new-value))
(defcallback scrollbar to-bottom-callback      :to-bottom
	     :lambda-list (new-value button-press-pos))
(defcallback scrollbar to-top-callback         :to-top
	     :lambda-list (new-value button-press-pos))

(defginafun make-scrollbar (parent "the parent widget"
		       &key (orientation :vertical) ":vertical or :horizontal"
		            (minimum 0) "initial minimum"
			    (maximum 100) "initial maximum"
			    (value 0) "initial value"
			    (increment 1) "initial increment"
			    (page-increment 10) "initial page-increment"
			    (slider-size 10) "initial slider-size"
			    (value-changed-callback :call-method)
                              ":call-method or a callback"
			    (class 'scrollbar)
			    (initargs nil)
			    (motif-widget-class :scroll-bar)
			    (name class)
			    (managed t)
			    (motif-resources nil)
		       &aux new-scrollbar)
  (description 
    "make a new scrollbar widget"
    :constructor-for-class scrollbar
    :called-by-application :sometimes)
  (setq new-scrollbar
	(make-widget parent
		    :class class
		    :initargs (more-initargs
				:minimum minimum
				:maximum maximum
				:value value
				:increment increment
				:page-increment page-increment
				:slider-size slider-size)
		    :motif-widget-class motif-widget-class
		    :name name
		    :managed managed
		    :motif-resources (more-motif-resources
				       :orientation orientation
				       :minimum minimum
				       :maximum maximum
				       :value value
				       :increment increment
				       :page-increment page-increment
				       :slider-size slider-size
				       )))

  (when value-changed-callback
    (if (eq :call-method value-changed-callback)
	(setf (value-changed-callback new-scrollbar)
	      (make-callback #'execute-value-changed-callback new-scrollbar))
	(setf (value-changed-callback new-scrollbar) value-changed-callback)))
  
  new-scrollbar)

(defginamethod execute-value-changed-callback ((scrollbar scrollbar) new-value
					   &optional ignore1 ignore2)
  (description
    "semantics of a users modification of the current value"
    :override "in application-specific subclasses"
    :default-version :do-nothing)
  (comment
    "This method is only called if :call-method is specified on creation.")
  (declare (ignore new-value ignore1 ignore2))
  ;;(format t "New value: ~s~%"new-value)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class main-menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass main-menu (menu-bar)

   (description
     "a menu-bar plus its popup shells"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ((topics            :accessor topics            :initform nil)))

(defginafun make-main-menu (parent "the parent widget"
                              &key (class 'main-menu)
			           (initargs nil)
				   (motif-widget-class :row-column)
				   (name class)
				   (managed t)
				   (motif-resources nil))
  (description 
    "create a new main-menu widget"
    :constructor-for-class main-menu
    :called-by-application :sometimes)
  (make-menu-bar parent
		 :class class
		 :initargs initargs
		 :motif-widget-class motif-widget-class
		 :name name
		 :managed managed
		 :motif-resources motif-resources))

'(defclass topic ()
  ((name               :accessor name              :initarg :name)
   (box                :accessor box               :initarg :box)
   (cascade            :accessor cascade           :initarg :cascade)
   (items              :accessor items             :initform nil))
  (:documentation ""))

'(defclass item ()
  ((name               :accessor name              :initarg :name)
   (entry              :accessor entry             :initarg :entry))
  (:documentation ""))

(defginamethod insert-menu-entry ((menu main-menu) 
                                  topic-name "identifying string and menu title"
                                  item-name "identifying string for the entry"
                                  entry "object of a subclass of entry"
                                  &key (before :end)
                                         ":end or the name of another entry"
                                  &aux topic item)
  (description
    "create a menu entry under the named topic"
    :called-by-application "to add an entry to a menu")
  (example
    (with-application-stopped
      (insert-menu-entry 
        (main-menu (main-shell (first (document-list *application*))))
        "Edit" "Clear"
        (make-toggle-entry "Clear Nil" nil)
        :before "Undo"))
    "inserts an entry in the Edit menu before the Undo entry"
    "the entry is a toggle button showing \"Clear Nil\" and an empty callback"
    "the entry can later be referenced by the two keys \"Edit\" and \"Clear\"")
  (setf (parent-menu entry) menu)
  (setq topic (loop for top in (topics menu)
                    when (equal (name top) topic-name) do (return top)))
  (if (not topic)
      ;; generate new topic
      (let* ((menu-shell (make-menu-shell menu))
	     (menu-box   (make-menu-pulldown menu-shell))
             (casc-btn   (make-cascade-button menu topic-name menu-box)))
          (setq topic 
                (make-instance 'topic :name topic-name 
                                      :box menu-box :cascade casc-btn))
	  (setf (topics menu) (append (topics menu) (list topic)))
          (setq item (make-instance 'item :name item-name :entry entry))
          (setf (items topic) (list item))
          (create-entry (entry item) menu-box))
      ;; find item
      (let ((item (loop for i in (items topic)
                        when (equal (name i) item-name) do (return i))))
        (when item
            (setf (items topic) (remove item (items topic)))
            (destroy-entry (entry item)))
        (setq item (make-instance 'item :name item-name :entry entry))
        (if (eql before :end)
            (progn
                (setf (items topic) (append (items topic) (list item)))
                (create-separator (entry item) (box topic))
                (create-entry (entry item) (box topic)))
            (progn
                (loop for i in (items topic) do (destroy-entry (entry i)))
                (setf (items topic)
                    (loop for i in (items topic)
                          when (equal (name i) before) collect item
                          collect i))
                (loop for i in (items topic)
                      for count from 0
                      do (unless (zerop count)
                             (create-separator (entry i) (box topic)))
                         (create-entry (entry i) (box topic))))))))

(defginamethod remove-menu-entry ((menu main-menu) 
                                  topic-name "string identifying topic"
                                  item-name "string identifying entry"
                                  &aux topic item)
  (description
    "remove the menu entry"
    :called-by-application "to remove a standard menu entry"
    :result "the removed entry object")
  (setq topic (loop for top in (topics menu)
                    when (equal (name top) topic-name) do (return top)))
  (when topic
      (setq item (loop for i in (items topic)
                       when (equal (name i) item-name) do (return i)))
      (when item
          (setf (items topic) (remove item (items topic)))
          (destroy-entry (entry item))
          (unless (items topic)
              (setf (topics menu) (remove topic (topics menu)))
              (destroy (cascade topic))
              (destroy (parent (box topic))))
          (entry item))))

(defginamethod find-menu-entry ((menu main-menu) 
                                topic-name "string identifying topic"
                                item-name "string identifying entry"
                                &aux topic)
  (description
    "find the entry corresponding to the named item"
    :called-by-application "to find the entry object"
    :result "the entry object identified by topic and item, or nil")
  (setq topic (loop for top in (topics menu)
                    when (equal (name top) topic-name) do (return top)))
  (when topic
      (loop for i in (items topic)
                       when (equal (name i) item-name) do (return (entry i)))))  

(defginamethod add-menu-command ((menu main-menu) 
                                 topic "string identifying topic"
                                 item-name "string identifying entry"
                                 callback "the callback activated by the entry"
				 &key (separator-before nil)
				      (accelerator nil)
				      (acc-modifier :alt))
  (description
    "add a button menu entry"
    :called-by-application "to add a simple menu entry")
  (comment
    "This method is a convenience interface to insert a button entry")
  (insert-menu-entry menu topic item-name
                     (make-button-entry item-name callback
					:separator-before separator-before
					:accelerator accelerator
					:acc-modifier acc-modifier)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; entry classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass menu-entry ()

   (description
     "the parent class for menu entries"
     :application-subclasses :rarely
     :gina-subclasses t
     :instantiate nil)

  ((widget             :accessor widget          :initform nil)
   (parent-menu        :accessor parent-menu     :initform nil)
   (sensitive          :accessor sensitive       :initform t
        :documentation "the sensitivity flag")
   (separator-before   :accessor separator-before :initarg :separator-before
        :documentation "whether create a separating line before this entry")
   (separator          :accessor separator       :initform nil)
   (callback           :accessor callback        :initarg :callback
        :documentation "the callback associated with the entry")))

(defmethod enclosing-shell ((entry menu-entry))
  "determine the next higher shell widget"
  (and (parent-menu entry)
       (enclosing-shell (parent-menu entry))))

(defmethod create-separator ((entry menu-entry) parent)
  "create the separator if required"
  (when (separator-before entry)
      (setf (separator entry)
            (make-separator parent))))

(defmethod destroy-entry ((entry menu-entry))
  "destroy the widget belonging to a menu entry"
  (when (widget entry)
      (when (separator entry)
          (destroy (separator entry))
          (setf (separator entry) nil))
      (destroy (widget entry))
      (setf (widget entry) nil)))

(defginamethod (setf sensitive) :after (new-value (entry menu-entry))
  "set sensitivity of entry if widget exists"
  (when (widget entry)
      (set-motif-resources (widget entry) :sensitive new-value)))

(defmethod call-callback ((entry menu-entry) &rest rest)
  "call the registered entry callback"
  (execute-from-widget (callback entry) entry rest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; button entry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass button-entry (menu-entry)

   (description
     "a button entry in menus"
     :application-subclasses :rarely
     :gina-subclasses t)

  ((label-string       :accessor label-string    :initarg :label-string
        :documentation "the label string shown")
   (accelerator        :accessor accelerator     :initarg :accelerator
        :documentation "a string denoting an accelerator key, e.g. \"F5\"")
   (acc-modifier       :accessor acc-modifier    :initarg :acc-modifier
        :documentation "modifier, one of :alt :shift :ctrl")))

(defcallback button-entry callback nil :lambda-list "()")

(defginafun make-button-entry (string "initial label string"
                               callback "to be activated by the entry"
                          &key (separator-before nil)
                               (accelerator nil)
                               (acc-modifier :alt))
  (description
    "create a button entry instance"
    :constructor-for-class button-entry)
  (make-instance 'button-entry 
                 :label-string string :callback callback
                 :separator-before separator-before
                 :accelerator accelerator :acc-modifier acc-modifier))

(defmethod create-entry ((entry button-entry) parent)
  "create the widget belonging to a button entry"
  (setf (widget entry)
        (make-push-button parent (label-string entry)
                          :activate-callback 
                              (make-callback 'call-callback entry)))
  (set-accelerator entry))

(defginamethod (setf label-string) :after (new-value (entry button-entry))
  "propagate label string to widget"
  (when (widget entry)
      (setf (label-string (widget entry)) new-value)))

(defginamethod (setf accelerator) :after (new-value (entry button-entry))
  "propagate new accelerator to widget"
  (declare (ignore new-value))
  (set-accelerator entry))

(defginamethod (setf acc-modifier) :after (new-value (entry button-entry))
  "propagate new modifier to widget"
  (declare (ignore new-value))
  (set-accelerator entry))

(defmethod set-accelerator ((entry button-entry))
  (when (and (widget entry) (accelerator entry))
    (set-motif-resources (widget entry) 
        :accelerator 
        (format nil "~a<Key>~a:" (mod-representation (acc-modifier entry))
                           (accelerator entry))
        :accelerator-text
        (format nil "~a+~a" (mod-representation (acc-modifier entry))
                       (accelerator entry)))))

(defun mod-representation (modifier)
  (case modifier
     ((:shift) "Shift") ((:ctrl) "Ctrl") ((:alt) "Alt") (t "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; toggle entry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass toggle-entry (button-entry)

   (description
     "a toggle button entry in menus"
     :application-subclasses :rarely
     :gina-subclasses nil)

  ((value              :accessor value             :initarg :value
        :documentation "the current toggle value")))

(defcallback toggle-entry callback nil :lambda-list (value))

(defginafun make-toggle-entry (string "initial label string"
                               callback "to be activated by the entry"
                          &key (value nil) "initial value"
                               (separator-before nil)
                               (accelerator nil)
                               (acc-modifier :alt))
  (description
    "create a toggle button entry instance"
    :constructor-for-class toggle-entry)
  (make-instance 'toggle-entry :label-string string :callback callback
                               :value value
                               :separator-before separator-before
                               :accelerator accelerator
                               :acc-modifier acc-modifier))

(defmethod create-entry ((entry toggle-entry) parent)
  "create the widget belonging to the toggle button entry"
  (setf (widget entry)
        (make-toggle-button parent (label-string entry)
			    :value (value entry)
                            :value-changed-callback
			    (make-callback 'call-callback entry)))
  (set-accelerator entry))

(defginamethod (setf value) :after (new-value (entry toggle-entry))
  "propagate value changes to the widget"
  (when (widget entry)
      (setf (value (widget entry)) new-value)))

(defmethod call-callback :before ((entry toggle-entry) &rest rest)
  "update the value slot"
  (with-slots (value) entry
    (setq value (first rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; toggle-group-entry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass toggle-group-entry (button-entry)

   (description
     "a group of toggle buttons in a menu"
     :application-subclasses :rarely
     :gina-subclasses t)

   ((parent             :accessor parent          :initform nil)
    (is-submenu         :accessor is-submenu :initarg :is-submenu
         :documentation "whether use a submenu or expand in place")
    (item-list          :accessor item-list  :initarg :item-list
         :documentation "list of strings or (string value) pairs")
    (initial-value      :accessor initial-value  :initarg :initial-value
         :documentation "initial string or value")
    (button-resources   :accessor button-resources :initarg :button-resources
         :documentation "motif-resources for buttons")
    (button-label-type  :accessor button-label-type :initarg :button-label-type
         :documentation "label-type for buttons")
    (value              :accessor value :initform nil
	 :documentation "a subset of value-list")
    (value-list         :accessor value-list
         :documentation "list of values extracted from item list")
    (toggle-buttons     :accessor toggle-buttons :initform nil)
    (separators         :accessor separators :initform nil)
    (menu-shell         :accessor menu-shell :initform nil)))

(defcallback toggle-group-entry callback nil :lambda-list (value toggled-value set))

(defginafun make-toggle-group-entry (label-string "label-string if submenu"
                                     item-list 
                                       "list of strings or (string value) pairs"
                                     callback "a value-changed callback"
				&key (separator-before nil)
                                     (is-submenu t)
                                        "whether use submenu or expand in place"
                                     (initial-value nil) 
                                        "a subset of value-list"
				     (button-label-type :string) 
                                        ":string or :pixmap"
				     (button-resources nil)
                                        "motif-resources for buttons")
  (description
    "create a toggle group entry instance"
    :constructor-for-class toggle-group-entry)
  (example
    (make-toggle-group-entry 
      "Style" '(("Bold" :bold) ("Italic" :italic)) 
      (make-callback '(lambda (value toggled-value set) ()))
      :initial-value '(:bold)))
  (make-instance 'toggle-group-entry :callback callback
                 :label-string label-string
                 :separator-before separator-before
                 :is-submenu is-submenu
                 :item-list item-list
                 :initial-value initial-value
                 :button-label-type button-label-type
                 :button-resources button-resources))

(defmethod create-entry ((entry toggle-group-entry) parent)
  "creating widgets for this menu entry"
  (setf (parent entry) parent)
  (if (is-submenu entry)
      (let* ((menu-shell (make-menu-shell (parent parent)))
	     (menu-box   (make-menu-pulldown menu-shell)))
        (setf (menu-shell entry) menu-shell)
        (setf (widget entry)
              (make-cascade-button parent (label-string entry) menu-box))
        (setf (parent entry) menu-box)
        (set-item-list entry (item-list entry) 
                       :selected-value (initial-value entry)))
      (set-item-list entry (item-list entry) 
                     :selected-value (initial-value entry))))

(defmethod destroy-entry ((entry toggle-group-entry))
  "destroy the widgets belonging to the entry"
  (set-item-list entry nil :selected-value nil)
  (when (menu-shell entry) (destroy (menu-shell entry)))
  (call-next-method))
  
(defginamethod (setf value) :before (new-value (group toggle-group-entry))
  "the application has changed the selected values"
  (update-values group new-value))

(defmethod update-values ((group toggle-group-entry) new-value)
  "set new button states"
  (with-slots (value-list toggle-buttons) group
    ;; set all buttons according to new-values
    (loop for value  in value-list
	  for button in toggle-buttons
          when button
	  do (setf (value button)
		   (when (member value new-value :test #'equal) t)))))

(defginamethod set-item-list ((group toggle-group-entry) 
                               new-item-list 
                                 "list of strings or (string value) pairs"
			  &key (selected-value nil)
                                 "a subset of the values in the item list"
			       managed "ignored")
  (description
    "the item-list has changed"
    :called-by-application "to change the item list of the menu entry")
  (declare (ignore managed))
  (update-item-list group (parent group) new-item-list 
                    :selected-value selected-value
                    :indicator-type :n-of-many))

(defmethod button-toggled ((group toggle-group-entry) toggled-value set)
  "a new toggle button is selected by the user"
  (with-slots (value callback) group
    (if set
	(setq value (cons   toggled-value value))
	(setq value (remove toggled-value value)))
    (execute-from-widget callback group (list value toggled-value set))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; radio-group-entry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass radio-group-entry (toggle-group-entry)

   (description
     "a radio button group in a menu"
     :application-subclasses :rarely
     :gina-subclasses nil)

  ())

(defcallback radio-group-entry callback nil :lambda-list (value old-value))

(defginafun make-radio-group-entry (label-string "label-string if submenu"
                                    item-list 
                                      "list of strings or (string value) pairs"
                                    callback "a value-changed callback"
			       &key (separator-before nil)
                                    (is-submenu t)
                                       "whether use submenu or expand in place"
                                    (initial-value :use-first) 
                                       "a value from value-list or :use-first"
				    (button-label-type :string) 
                                       ":string or :pixmap"
				    (button-resources nil)
                                       "motif-resources for buttons")
  (description
    "create a toggle group entry instance"
    :constructor-for-class radio-group-entry)
  (example
    (make-radio-group-entry 
      "Font" '(("Courier" :courier) ("Times" :times)) 
      (make-callback '(lambda (new-value old-value) ()))
      :initial-value :times))
  (make-instance 'radio-group-entry :callback callback
                 :label-string label-string
                 :separator-before separator-before
                 :is-submenu is-submenu
                 :item-list item-list
                 :initial-value initial-value
                 :button-label-type button-label-type
                 :button-resources button-resources))

(defmethod update-values ((group radio-group-entry) new-value)
  "set new button states"
  (with-slots (value-list toggle-buttons) group
    ;; reset the currently selected button and set the newly selected button
    (loop for value  in value-list
	  for button in toggle-buttons
	  when (equal value (value group))
	    do (setf (value button) nil)
	  when (equal value new-value)
	    do (setf (value button) t))))

(defginamethod set-item-list ((group radio-group-entry) 
                              new-item-list 
                                "list of strings or (string value) pairs"
			 &key (selected-value :use-first)
                                "a value from value-list or :use-first"
			      managed "ignored")
  (description
    "the item-list has changed"
    :called-by-application "to change the item list of the menu entry")
  (declare (ignore managed))
  (update-item-list group (parent group) new-item-list 
                    :selected-value selected-value
                    :indicator-type :one-of-many))

(defmethod button-toggled ((group radio-group-entry) new-value set 
                               &aux old-value)
  "a new toggle button is selected by the user"
  (with-slots (value callback) group
    (if set
      (progn
          (update-values group new-value)  ;; because no radio-behaviour
          (setq old-value value)
          (setq value new-value)
          (execute-from-widget callback group (list new-value old-value)))
      (update-values group value))))  ;; prevent resetting the last toggle

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class menu-bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass menu-bar (widget)

   (description
     "a special kind of row-column widget"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ())

(defginafun make-menu-bar (parent "the parent widget"
                             &key (class 'menu-bar)
			          (initargs nil)
				  (motif-widget-class :row-column)
				  (name class)
				  (managed t)
				  (motif-resources nil)
		             &aux new-menu-bar)
  (description 
    "make a new MenuBar widget"
    :constructor-for-class menu-bar
    :called-by-application :sometimes)
  (setq new-menu-bar
	(make-widget parent
		     :class class
		     :initargs initargs
		     :motif-widget-class motif-widget-class
		     :name name
		     :managed managed
		     :motif-resources (more-motif-resources
					:row-column-type :menu-bar)))
  new-menu-bar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class option-menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass option-menu (widget)

   (description
     "the Motif option menu widget"
     :application-subclasses :rarely
     :gina-subclasses nil)

  ((value              :accessor value :initform nil
        :documentation "the current value from the value-list")
   (value-list         :accessor value-list :initform nil
        :documentation "list of values extracted from the item list")
   (option-shell       :accessor option-shell
        :documentation "the menu shell for the option menu pane")
   (button-box         :accessor button-box
        :documentation "the option menu pane")
   (option-buttons     :accessor option-buttons :initform nil
        :documentation "the list of buttons in the pane")
   (value-changed-callback        :accessor value-changed-callback 
                                  :initform nil)))

(defcallback option-menu value-changed-callback nil
	     :lambda-list (value old-value))

(defginafun make-option-menu (parent "the parent widget"
                              item-list
                                "a list of strings or (string value) pairs"
			 &key (label-string nil)
                                "the label string on the left side"
			      (value-changed-callback :call-method)
                                ":call-method or a callback"
			      (initial-value :use-first)
                                "a value from the value-list or :use-first"
			      (class 'option-menu)
			      (initargs nil)
			      (motif-widget-class :row-column)
			      (name class)
			      (managed t)
			      (motif-resources nil)
			 &aux new tmp-shell tmp-box tmp-button)
  (description 
    "create a new option-menu"
    :constructor-for-class option-menu
    :called-by-application :sometimes)
  (when (not label-string) (setq label-string ""))
  (setq tmp-shell  (make-menu-shell parent))
  (setq tmp-box    (make-menu-pulldown tmp-shell))
  ;; create one dummy button so that the geometry management
  ;; will be right when creating the option menu
  (setq tmp-button (make-push-button tmp-box
				     ;; first label string
				     (if item-list
					 (if (stringp (first item-list))
					     (first item-list)
					     (first (first item-list)))
				         "Dummy")
				     ;; will be filled in later:
				     :activate-callback nil))

  ;; create the option-menu itself
  (setq new (make-widget parent
			 :class class
			 :initargs initargs
			 :motif-widget-class motif-widget-class
			 :name name
			 :managed managed
			 :motif-resources (more-motif-resources
					    :label-string label-string
					    :sub-menu-id (widget-id tmp-box)
					    :row-column-type :menu-option)))
  (with-slots (option-shell button-box value value-list option-buttons) new
    (setq option-shell tmp-shell)
    (setq button-box tmp-box)
    (setq option-buttons (list tmp-button))

    ;; create a push-button for each item
    (set-item-list new item-list :selected-value initial-value)
     
    (when value-changed-callback
      (if (eq :call-method value-changed-callback)
	  (setf (value-changed-callback new)
		(make-callback #'execute-value-changed-callback new))
	  (setf (value-changed-callback new) value-changed-callback))))
  new)

(defginamethod set-item-list ((option-menu option-menu) 
                              new-item-list
                                "a list of strings or (string value) pairs"
			 &key (selected-value :use-first)
                                "a value from the value-list or :use-first"
			      managed "ignored"
			 &aux val string)
  (description
    "the item-list has changed"
    :called-by-application "to change the list of options")
  (declare (ignore managed))
  (with-slots (value-list option-buttons button-box) option-menu
    
    (when option-buttons
      ;; destroy first one so that buttons rotate and label string updates
      ;; but only when it is not the dummy one at creation time
      (when (and (activate-callback (first option-buttons))
                 (selected-button-would-remain option-menu selected-value
					       new-item-list))
        (unmanage (first option-buttons))
        (destroy (first option-buttons))
        (setq option-buttons (rest option-buttons)))
      ;; destroy existing buttons
      (loop for button in (nthcdr (length new-item-list) option-buttons)
	    do (unmanage button)
	       (destroy button) ;; old BUG in MOTIF
	       ))
    
    (setq value-list nil)
    (setq option-buttons
          (loop for item in new-item-list
	        for i from 0
		for reuse-button = (nth i option-buttons) 
		do (if (listp item)
		       (setq string (first item) val (second item))
		       (setq string item val item))
		   (setq value-list (append value-list (list val)))
	        collect (make-one-option-button option-menu string val 
						reuse-button)))
    
    (when (eq selected-value :use-first)
      (setq selected-value (first value-list)))
    (setf (value option-menu) selected-value) ;; deamon sets button states !!
    ))

(defun selected-button-would-remain (option-menu selected-value item-list
						 &aux button-id)
  (setq button-id (first (get-motif-resources option-menu :menu-history)))
  (when (eq selected-value :use-first)
	(setq selected-value (if (listp (first item-list))
				 (second (first item-list))
			         (first item-list))))
  (loop for item in item-list
        for value = (if (listp item) (second item) item)
	for option-button in (option-buttons option-menu)
	do (when (and (equal value selected-value)
                      (eql (widget-id option-button) button-id))
		 (return-from selected-button-would-remain t)))
  nil)

(defun make-one-option-button (option-menu string val reuse-button)
  "make a new button, but reuse old buttons and modify string and value"
  (unless reuse-button
    (return-from make-one-option-button
      (make-push-button (button-box option-menu) string
			:activate-callback 
			(make-callback #'value-changed option-menu val))))
  (setf (label-string reuse-button) string)
  (unless (activate-callback reuse-button)
    (setf (activate-callback reuse-button)
      (make-callback #'value-changed option-menu val))
    (return-from make-one-option-button reuse-button))
  (setf (second (static-args (activate-callback reuse-button))) val)
  reuse-button)

(defmethod value-changed ((option-menu option-menu) new-value 
                              &aux old-value)
  "a new toggle button is selected by the user"
  (with-slots (value value-changed-callback) option-menu
    (setq old-value value)
    (setq value new-value)
    (execute-from-widget value-changed-callback option-menu 
			 (list new-value old-value))))


(defginamethod execute-value-changed-callback ((option-menu option-menu) 
                                               new-value
					       &optional old-value ignore)
  (description
    "reaction when a new option is selected by the user"
    :override "in application-specific subclasses"
    :default-version :do-nothing)
  (declare (ignore old-value new-value ignore))
  ;; old-value is always supplied for option menus !!
  ;;(format t "option-menu: ~a --> ~a " old-value new-value)
  )

(defginamethod (setf value) :after (new-value (option-menu option-menu))
  "the value is changed by the application program, not the user"
  (loop for value  in (value-list option-menu)
	for option-button in (option-buttons option-menu)
	do (when (equal value new-value)
	     (set-motif-resources option-menu 
                                  :menu-history (widget-id option-button))
	     (return))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class menu-pulldown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass menu-pulldown (widget)

   (description
     "the Motif row-column widget operating as a menu pane"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ())

(defginafun make-menu-pulldown (parent "the parent widget"
			   &key (class 'menu-pulldown)
			        (initargs nil)
				(motif-widget-class :row-column)
				(name class)
				(motif-resources nil))
  (description 
    "make a new pull-down menu widget"
    :constructor-for-class menu-pulldown
    :called-by-application :sometimes)
  (make-widget parent
	       :class class
	       :initargs initargs
	       :motif-widget-class motif-widget-class
	       :managed nil
	       :name name
	       :motif-resources (more-motif-resources
				  :row-column-type :menu-pulldown)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class main-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass main-window (widget)

   (description
     "the Motif main-window widget"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ())

(defginafun make-main-window (parent "the parent widget"
			 &key (class 'main-window)
			      (initargs nil)
			      (motif-widget-class :main-window)
			      (name class)
			      (managed t)
			      (motif-resources nil)
			 &aux new-main-window)
  (description 
    "make a new main-window widget"
    :constructor-for-class main-window
    :called-by-application :sometimes)
  (setq new-main-window
	(make-widget parent
		     :class class
		     :initargs initargs
		     :motif-widget-class motif-widget-class
		     :name name
		     :managed managed
		     :motif-resources motif-resources))
  new-main-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class scroller
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass scroller (widget)

   (description
     "the Motif scrolled-window widget"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ((horizontal-scrollbar :accessor horizontal-scrollbar :initform nil
         :documentation "scroll bar object or nil (read-only)")
    (vertical-scrollbar   :accessor vertical-scrollbar :initform nil
         :documentation "scroll bar object or nil (read-only)")
    (work-area            :accessor work-area :initform nil
         :documentation "the work area child (read-only)")
    (clipper              :accessor clipper :initform nil)))

(defginafun make-scroller (parent "the parent widget"
		      &key 
                      (scrolling-policy :automatic)
                        ":automatic or :application-defined"
		      (class 'scroller)
		      (initargs nil)
		      (motif-widget-class :scrolled-window)
		      (name class)
		      (managed t)
		      (motif-resources nil)
		      &aux new-scroller)
  (description 
    "make a new scroller widget"
    :constructor-for-class scroller
    :called-by-application :sometimes)
  (setq new-scroller
	(make-widget parent
		     :class class
		     :initargs initargs
		     :motif-widget-class motif-widget-class
		     :name name
		     :managed managed
		     :motif-resources (more-motif-resources
					:scrolling-policy scrolling-policy
					)))  
  new-scroller)

;; make 'make-scrolled-window a synonym for 'make-scroller
(eval-when (eval load)
  (export 'make-scrolled-window))
(setf (symbol-function 'make-scrolled-window) (symbol-function 'make-scroller))

(defginamethod son-created ((scroller scroller) son &aux ids id)
  (description
    "in this case the scroller is informed that the work-area is created")

  (setf (work-area scroller) son)

  ;; determine the widget-ids of the implicitly created scrollbars and build
  ;; CLOS-Objects around them, so that properties of the scrollbars can be 
  ;; modified

  (setq ids (get-motif-resources scroller 
                                 :horizontal-scroll-bar :vertical-scroll-bar 
                                 :clip-window))
  ;; CLM returns -1 for a NULL widget !

  (setq id (first ids))
  (when (> id 0)                                   ;;(not (zerop id))
    (with-slots (horizontal-scrollbar) scroller
      (setf horizontal-scrollbar (make-instance 'scrollbar))
      (setf (widget-id horizontal-scrollbar) id)
      (setf (widget-id-for-sons horizontal-scrollbar) id)
      (setf (group-widget-id horizontal-scrollbar) id)
      (setf (parent horizontal-scrollbar) scroller)
      (value horizontal-scrollbar)))               ;; initialize the value slot

  (setq id (second ids))
  (when (> id 0)                                   ;;(not (zerop id))
    (with-slots (vertical-scrollbar) scroller
      (setf vertical-scrollbar (make-instance 'scrollbar))
      (setf (widget-id vertical-scrollbar) id)
      (setf (widget-id-for-sons vertical-scrollbar) id)
      (setf (group-widget-id vertical-scrollbar) id)
      (setf (parent vertical-scrollbar) scroller)
      (value vertical-scrollbar)))                 ;; initialize the value slot

  ;; also encapsulate the implicitly created clip-window in a CLOS-object
  (setq id (third ids))
  (when (> id 0)                                   ;;(not (zerop id))
    (with-slots (clipper) scroller
      (setf clipper (make-instance 'widget))
      (setf (widget-id clipper) id)
      (setf (widget-id-for-sons clipper) id)
      (setf (group-widget-id clipper) id)
      (setf (parent clipper) scroller))))

(defginamethod scroll-to ((scroller scroller) x-offset y-offset
		      &key (x-window-only nil))
  (description
    "initiate scrolling from the application side"
    :called-by-application "to scroll to a new position")
  (declare (special *display*))
  (if x-window-only
      (progn
	;; use CLX calls to move the work-area
	(setf (xlib:drawable-x (x-window (work-area scroller))) (- 0 x-offset))
	(setf (xlib:drawable-y (x-window (work-area scroller))) (- 0 y-offset))
	;; update slots of work-area
	(with-slots (x-pos y-pos) (work-area scroller)
	  (setq x-pos (- 0 x-offset) y-pos (- 0 y-offset))))
      ;; else use toolkit server to move the work-area
      (progn
	;; make sure that feedback is cleared before view is scrolled
	(xlib:display-finish-output *display*)
	(xtk::move-widget (group-widget-id (work-area scroller)) 
                         (- 0 x-offset) (- 0 y-offset))))
  
  ;; move the sliders
  (unless (eq (value (horizontal-scrollbar scroller)) x-offset)
    (setf (value (horizontal-scrollbar scroller)) x-offset)
    (execute (drag-callback (horizontal-scrollbar scroller)) (list x-offset)))

  (unless (eq (value (vertical-scrollbar scroller)) y-offset)
    (setf (value (vertical-scrollbar scroller)) y-offset)
    (execute (drag-callback (vertical-scrollbar scroller)) (list y-offset))))

(defginamethod show-point ((scroller scroller) x y
		       &key (ask-for-work-area-pos t)
		            (ask-for-clipper-size  t)
			    (x-window-only nil)
			    (force-scrolling nil)
	    #+genera-8 &allow-other-keys
		       &aux old-x-offset old-y-offset new-x-offset new-y-offset
		            (do-scrolling force-scrolling))
  (description
    "do minimal scrolling to make the specified point visible")

  (with-slots (clipper work-area) scroller
    ;; get up-to-date information about widget size and position 
    ;; from the toolkit server
    (when ask-for-work-area-pos
      (update-slots work-area))
    (when ask-for-clipper-size
      (update-slots clipper))

    (setq old-x-offset (- 0 (x-pos work-area)))
    (setq old-y-offset (- 0 (y-pos work-area)))
    (setq new-x-offset old-x-offset)
    (setq new-y-offset old-y-offset)
    
    (if (< x old-x-offset)
	(progn (setq do-scrolling t)
	       (setq new-x-offset x))
	;; else
	(when (>= x (+ (width  clipper) old-x-offset))
	  (setq do-scrolling t)
	  (setq new-x-offset (- x (width clipper)))))
    (if (< y old-y-offset)
	(progn (setq do-scrolling t)
	       (setq new-y-offset y))
	;; else
	(when (>= y (+ (height clipper) old-y-offset))
	  (setq do-scrolling t)
	  (setq new-y-offset (- y (height clipper)))))
    
    (when do-scrolling
      ;;(format t "scroll-to ~s ~s~%" new-x-offset new-y-offset)
      (scroll-to scroller new-x-offset new-y-offset 
                 :x-window-only x-window-only))

    ;; return whether scrolling took place
    do-scrolling))

(defginamethod point-visible ((scroller scroller) x y
			  &key (ask-for-work-area-pos t)
		               (ask-for-clipper-size  t)
			  &aux old-x-offset old-y-offset)
  (description
    "check whether the specified point is visible")

  (with-slots (clipper work-area) scroller
    ;; get up-to-date information about widget size and position 
    ;; from the toolkit server
    (when ask-for-work-area-pos
      (update-slots work-area))
    (when ask-for-clipper-size
      (update-slots clipper))

    (setq old-x-offset (- 0 (x-pos work-area)))
    (setq old-y-offset (- 0 (y-pos work-area)))
    
    (not (or (< x old-x-offset)
	     (>= x (+ (width  clipper) old-x-offset))
	     (< y old-y-offset)
	     (>= y (+ (height clipper) old-y-offset))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class separator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass separator (widget)

   (description
     "the Motif separator widget"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ())

(defginafun make-separator (parent "the parent widget"
		       &key (orientation :horizontal) ":horizontal or :vertical"
;		            (attach-to nil)
		            (margin 0) "between end of line and end of widget"
			    (separator-type :shadow-etched-in) "type of line"
			    (class 'separator)
			    (initargs nil)
			    (motif-widget-class :separator)
			    (name class)
			    (managed t)
			    (motif-resources nil)
		       &aux new-separator)
  (description 
    "make a new separator widget"
    :constructor-for-class separator
    :called-by-application :sometimes)
  (comment
    "separator-type may be one of:"
    ":no-line"
    ":single-line :double-line"
    ":single-dashed-line :double-dashed-line"
    ":shadow-etched-in :shadow-etched-out")
  (setq new-separator
	(make-widget parent
		     :class class
		     :initargs initargs
		     :motif-widget-class motif-widget-class
		     :name name
		     :managed managed
		     :motif-resources (more-motif-resources
					:orientation orientation
					:margin margin
					:separator-type separator-type
					)))
;  (when attach-to
;    (if (eq orientation :horizontal)
;      (define-form-constraint new-separator
;			      :left-attachment :form :right-attachment :form
;			      :top-attachment :widget :top-widget attach-to)
;      (define-form-constraint new-separator
;			      :top-attachment :form :bottom-attachment :form
;			      :left-attachment :widget :left-widget attach-to)))
  new-separator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass frame (widget)

   (description
     "the Motif frame widget"
     :application-subclasses :rarely
     :gina-subclasses t)

   ())

(defginafun make-frame (parent
		   &key (margin-width 0)
		        (margin-height 0)
			(shadow-type :in) ":in :out :etched-in :etched-out"
			(class 'frame)
			(initargs nil)
			(motif-widget-class :frame)
			(name class)
			(managed t)
			(motif-resources nil)
		   &aux new-frame)
  (description 
    "make a new frame widget"
    :constructor-for-class frame
    :called-by-application :sometimes)
  (setq new-frame
	(make-widget parent
		     :class class
		     :initargs initargs
		     :motif-widget-class motif-widget-class
		     :name name
		     :managed managed
		     :motif-resources (more-motif-resources
					:margin-width  margin-width
					:margin-height margin-height
					:shadow-type shadow-type)))
  new-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class paned-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass paned-window (widget)

   (description
     "the Motif paned window widget"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ())

(defginafun make-paned-window (parent "the parent widget"
                                 &key (spacing 8) "between panes"
			              (class 'paned-window)
				      (initargs nil)
				      (motif-widget-class :paned-window)
				      (name class)
				      (managed t)
				      (motif-resources nil)
			  &aux new-paned-window)
  (description 
    "make a new paned-window widget"
    :constructor-for-class paned-window
    :called-by-application :sometimes)
  (setq new-paned-window
	(make-widget parent
		     :class class
		     :initargs initargs
		     :motif-widget-class motif-widget-class
		     :name name
		     :managed managed
		     :motif-resources (more-motif-resources
					:spacing spacing)))
  new-paned-window)

(defginafun define-pane-constraint (widget "a child of the pane"
			       &key (minimum 1000 minimum-specified)
                                      "the minimum size of this pane"
			            (maximum 1000 maximum-specified)
                                      "the maximum size of this pane"
				    (allow-resize nil allow-resize-specified)
				    (skip-adjust nil skip-adjust-specified)
			       &aux constraint-resources)
  (description
    "define constraint for child of a paned-window widget"
    :called-by-application "to set constraints for a paned-window child")
  (setq constraint-resources nil)
  (when minimum-specified
    (setq constraint-resources
	  (append (list :pane-minimum minimum) constraint-resources)))
  (when maximum-specified
    (setq constraint-resources
	  (append (list :pane-maximum maximum) constraint-resources)))
  (when allow-resize-specified
    (setq constraint-resources
	  (append (list :allow-resize allow-resize) constraint-resources)))
  (when skip-adjust-specified
    (setq constraint-resources
	  (append (list :skip-adjust skip-adjust) constraint-resources)))
  
  (apply #'xtk:set-values (cons (group-widget-id widget) constraint-resources)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class row-column
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass row-column (widget)

   (description
     "the Motif row-column widget"
     :application-subclasses :rarely
     :gina-subclasses t)

   ())

(defginafun make-row-column (parent "the parent widget"
                               &key (orientation :vertical) 
                                      ":vertical or :horizontal"
			            (num-columns 1)
                                      "more than one implies column packing"
				    (packing nil)
                                      "nil=dynamic, :pack-column or :pack-tight"
                                    (entry-alignment :beginning)
                                      ":none, :beginning, :center or :end"
			            (spacing 1) "between children"
                                    (margin-width 2)
                                    (margin-height 2)
			            (class 'row-column)
			            (initargs nil)
				    (motif-widget-class :row-column)
				    (name class)
				    (managed t)
				    (motif-resources nil)
			       &aux new-row-column)
  (description 
    "make a new row-column widget"
    :constructor-for-class row-column
    :called-by-application :sometimes)
  (if packing
     (setq motif-resources (more-motif-resources :packing packing))
     (setq motif-resources
	   (more-motif-resources :packing (if (> num-columns 1) 
                                 :pack-column :pack-tight))))
  (if (eql entry-alignment :none)
     (setq motif-resources (more-motif-resources :is-aligned nil))
     (setq motif-resources
           (more-motif-resources :is-aligned t 
                                 :entry-alignment entry-alignment)))
  
  (setq new-row-column
	(make-widget parent
		     :class class
		     :initargs initargs
		     :motif-widget-class motif-widget-class
		     :name name
		     :managed managed
		     :motif-resources (more-motif-resources
					:orientation orientation
					:num-columns num-columns
					:spacing spacing
                                        :margin-width margin-width
                                        :margin-height margin-height)))
  new-row-column)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class labeled-frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass labeled-frame (frame)

   (description
     "a frame with a label on top"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ((column             :accessor column
         :documentation "automatically created row-column")
    (label              :accessor label
         :documentation "automatically created label-child")))

(defginafun make-labeled-frame (parent "the parent widget"
                                label-string "the label string shown"
			   &key (shadow-type :etched-in) 
                                  ":in :out :etched-in :etched-out"
			        (class 'labeled-frame)
				(initargs nil)
				(motif-widget-class :frame)
				(name class)
				(managed t)
				(motif-resources nil)
				(label-resources nil)
                                  "motif-resources for the label"
				(column-resources nil)
                                  "motif-resources for the row-column"
			   &aux new-labeled-frame new-column new-label)
  (description 
    "make a new labeled-frame widget"
    :constructor-for-class labeled-frame
    :called-by-application :sometimes)
  (setq new-column (make-row-column parent
				    :orientation :vertical
				    :name name
				    :managed managed
				    :motif-resources column-resources))

  (setq new-label (make-label new-column label-string
			      :motif-resources label-resources))

  (setq new-labeled-frame (make-frame new-column
				      :shadow-type shadow-type
				      :class class
				      :initargs initargs
				      :motif-widget-class motif-widget-class
				      :motif-resources motif-resources))
  
  (with-slots (label column group-widget-id) new-labeled-frame
    (setq label new-label)
    (setq column new-column)
    (setq group-widget-id (widget-id column)))
  ;; set parent to skip scroller in between
  (setf (parent new-labeled-frame) parent)
  
  ;; return the enclosing row-colum widget
  new-labeled-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; handling item lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun update-item-list (group parent new-item-list
			 &key (selected-value nil) 
                              (indicator-type :n-of-many)
			 &aux val string optimize)
  "the item-list has changed"
  (with-slots (value-list toggle-buttons separators button-resources 
               button-label-type) group
    (setq optimize
          (and (not separators)
               (not (loop for item in new-item-list
                          when (eq item :separator)
                            do (return t)))))
    (when toggle-buttons
      ;; destroy existing buttons
      (loop for button in (nthcdr (if optimize (length new-item-list) 0)
                                  toggle-buttons)
	    do (unmanage button)
               (destroy button) ;; BUG in MOTIF: Buttons are not destroyed
	       ))

    (when separators
      ;; destroy separators
      (loop for separator in separators
	    do (unmanage separator)
               (destroy separator) ;; BUG in MOTIF: Buttons are not destroyed
               )
      (setq separators nil))
    
    (setq value-list nil)
    (setq toggle-buttons
	  (loop for item in new-item-list
                for i from 0
                for reuse-toggle = (nth i toggle-buttons)
		do (if (listp item)
		       (setq string (first item) val (second item))
		       (setq string (if (stringp item) item 
                                                       (format nil "~a" item))
                             val item))
		   (if (eq :separator string)
		       (push (make-separator parent) separators)
		       (setq value-list (append value-list (list val))))
		when (not (eq :separator string))
		  collect (if reuse-toggle
                            (progn
                              (unless (equal (label-string reuse-toggle) string)
                                  (setf (label-string reuse-toggle) string))
			      ;; modify existing callback object!
			      (setf (second 
				     (static-args 
				      (value-changed-callback reuse-toggle)))
				val)
                              reuse-toggle)
                            (make-toggle-button
			      parent string
			      :indicator-type indicator-type
			      :label-type button-label-type
			      :motif-resources button-resources 
			      :value-changed-callback
			      (make-callback #'button-toggled group val)))))
    
    (when (eq selected-value :use-first)
        (setq selected-value (first value-list)))
    ;; deamon sets button states !!
    (if (listp selected-value)
        (setf (value group) (copy-list selected-value))
        (setf (value group) selected-value))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class toggle-button-group
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass toggle-button-group (row-column)

   (description
     "a group of toggle buttons with optional label and frame"
     :application-subclasses :rarely
     :gina-subclasses t)

   ((button-resources   :accessor button-resources :initarg :button-resources
         :documentation "motif-resources for buttons")
    (button-label-type  :accessor button-label-type :initarg :button-label-type
         :documentation "label-type for buttons")
    (value              :accessor value :initform nil
	 :documentation "a subset of value-list")
    (value-list         :accessor value-list
         :documentation "the list of values extracted from the item list")
    (value-changed-callback :accessor value-changed-callback :initform nil)
    (main-column        :accessor main-column :initform nil
         :documentation "the enclosing row-column")
    (label              :accessor label :initform nil
         :documentation "the title label widget")
    (frame              :accessor frame :initform nil
         :documentation "the frame widget")
    (toggle-buttons     :accessor toggle-buttons :initform nil
         :documentation "the list of toggle buttons")
    (separators         :accessor separators :initform nil
         :documentation "the list of separators")))

(defcallback toggle-button-group value-changed-callback nil
	     :lambda-list (value toggled-value set))

(defginafun make-toggle-button-group (parent "the parent widget"
                                 item-list 
                                   "a list of strings or (string value) pairs"
		            &key (label-string nil) "as title"
				 (value-changed-callback :call-method)
                                   ":call-method or a callback"
				 (initial-value nil)
                                   "subset of values in item-list"
				 (with-frame t)
                                   "whether enclosing frame visible"
				 (shadow-type :etched-in) 
                                   ":in :out :etched-in :etched-out"
			         (spacing 1) "between buttons"
				 (num-columns 1) "for row-column"
				 (orientation :vertical) 
                                   ":vertical or :horizontal"
				 (button-label-type :string) 
                                   ":string or :pixmap"
				 (class 'toggle-button-group)
				 (initargs nil)
                                 (name class)
                                 (managed t)
				 (motif-resources nil)
				 (button-resources nil)
                                   "motif-resources for buttons"
			    &aux new-toggle-button-group main-column 
                                 label frame)
  (description
    "make new group of toggle buttons with optional label and frame"
    :constructor-for-class toggle-button-group
    :called-by-application :sometimes)
  ;; make a column containing label and frame
  (setq main-column
	(make-row-column parent :orientation :vertical 
                                :managed managed
                                :name name))
  (when label-string
    (setq label (make-label main-column label-string)))

  (setq frame (make-frame main-column :shadow-type shadow-type))
  ;; initial :shadow-thickness 0 not accepted by Motif
  (unless with-frame
    (set-motif-resources frame :shadow-thickness 0))

  ;; the toggle-button-group is embodied by the column of buttons:
  (setq new-toggle-button-group
	(make-row-column frame
			 :orientation orientation
			 :num-columns num-columns
			 ;;:packing :pack-column
			 :spacing spacing
			 :class class
			 :initargs (more-initargs 
                                    :button-resources button-resources
				    :button-label-type button-label-type)
			 :motif-resources motif-resources))

  ;; store references to component widgets
  (setf (main-column new-toggle-button-group) main-column)
  (setf (label new-toggle-button-group) label)
  (setf (frame new-toggle-button-group) frame)
  (setf (group-widget-id new-toggle-button-group) (widget-id main-column))
  ;; set parent to skip widgets in between
  (setf (parent new-toggle-button-group) parent)

  (when value-changed-callback
    (if (eq :call-method value-changed-callback)
	(setf (value-changed-callback new-toggle-button-group)
	      (make-callback #'execute-value-changed-callback 
                             new-toggle-button-group))
	(setf (value-changed-callback new-toggle-button-group) 
              value-changed-callback)))
    
  ;; create the toggle buttons
  (set-item-list new-toggle-button-group item-list 
                 :selected-value initial-value :managed managed)
  
  new-toggle-button-group)

(defginamethod (setf value) :before (new-value (group toggle-button-group))
  "the application has changed the selected values"
  (update-values group new-value))

(defmethod update-values ((group toggle-button-group) new-value)
  "set new button states"
  (with-slots (value-list toggle-buttons) group
    ;; set all buttons according to new-values
    (loop for value  in value-list
	  for button in toggle-buttons
	  do (setf (value button)
		   (when (member value new-value :test #'equal) t)))))

(defginamethod set-item-list ((group toggle-button-group) 
                              new-item-list
                                "a list of strings or (string value) pairs"
			 &key (selected-value nil) 
                                "a subset of the values in item-list"
                              (managed t))
  (description
    "the item-list has changed"
    :called-by-application "to change the list of items")
  (example
    (set-item-list toggle-button-group 
                   '(("Bold" :bold) ("Italic" :italic) ("Underline" :under))
                   :selected-values '(:italic :under)))
  (when managed (unmanage group))
  (update-item-list group group new-item-list :selected-value selected-value
                    :indicator-type :n-of-many)
  (when managed (manage group)))

(defmethod button-toggled ((group toggle-button-group) toggled-value set)
  "a new toggle button is selected by the user"
  (with-slots (value value-changed-callback) group
    (if set
	(setq value (cons   toggled-value value))
	(setq value (remove toggled-value value)))
    (execute-from-widget value-changed-callback group
			 (list value toggled-value set))))

(defginamethod execute-value-changed-callback ((group toggle-button-group) 
                                               new-value
					       &optional toggled-value set)
  (description
    "reaction when a button is toggled by the user"
    :override :sometimes
    :default-version :do-nothing)
  (declare (ignore new-value toggled-value set))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class radio-button-group
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass radio-button-group (toggle-button-group)

   (description
     "a group of radio buttons with optional label and frame"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ())

(defcallback radio-button-group value-changed-callback nil
	     :lambda-list (value old-value))

(defginafun make-radio-button-group (parent "the parent widget"
                                 item-list 
                                   "a list of strings or (string value) pairs"
		            &key (label-string nil) "as title"
				 (value-changed-callback :call-method)
                                   ":call-method or a callback"
				 (initial-value :use-first)
                                   "a value from the item-list or :use-first"
				 (with-frame t)
                                   "whether enclosing frame visible"
				 (shadow-type :etched-in) 
                                   ":in :out :etched-in :etched-out"
			         (spacing 1) "between buttons"
				 (num-columns 1) "for row-column"
				 (orientation :vertical) 
                                   ":vertical or :horizontal"
				 (button-label-type :string) 
                                   ":string or :pixmap"
				 (class 'radio-button-group)
				 (initargs nil)
                                 (name class)
                                 (managed t)
				 (motif-resources nil)
				 (button-resources nil)
                                   "motif-resources for buttons")
  (description 
    "make new group of toggle buttons with optional label and frame"
    :constructor-for-class radio-button-group
    :called-by-application :sometimes)
  (make-toggle-button-group parent item-list
                            :label-string label-string
                            :value-changed-callback value-changed-callback
                            :initial-value initial-value
                            :with-frame with-frame
                            :shadow-type shadow-type
                            :spacing spacing
                            :num-columns num-columns
                            :orientation orientation
                            :button-label-type button-label-type
                            :class class
                            :initargs initargs
                            :name name
                            :managed managed
                            :motif-resources 
                                (more-motif-resources :radio-behavior t)
                            :button-resources button-resources))

(defmethod update-values ((group radio-button-group) new-value)
  "set new button states"
  (with-slots (value-list toggle-buttons) group
    ;; reset the currently selected button and set the newly selected button
    (loop for value  in value-list
	  for button in toggle-buttons
	  when (equal value (value group))
	    do (setf (value button) nil)
	  when (equal value new-value)
	    do (setf (value button) t))))

(defginamethod set-item-list ((group radio-button-group) 
                              new-item-list
                                "a list of strings or (string value) pairs"
			 &key (selected-value :use-first) 
                                "a value from the item list or :use-first"
                              (managed t))
  (description
    "the item-list has changed"
    :called-by-application "to change the list of items")
  (when managed (unmanage group))
  (update-item-list group group new-item-list :selected-value selected-value
                    :indicator-type :one-of-many)
  (when managed (manage group)))

(defmethod button-toggled ((group radio-button-group) new-value set 
                               &aux old-value)
  "a new toggle button is selected by the user"
  (with-slots (value value-changed-callback) group
    (when set
      (setq old-value value)
      (setq value new-value)
      (execute-from-widget value-changed-callback group 
			   (list new-value old-value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class selection-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass selection-list (widget)

   (description
     "the Motif List widget"
     :application-subclasses :rarely
     :gina-subclasses t)

   ((default-action-button  
         :accessor default-action-button :initarg :default-action-button
         :documentation "button to activate on double-click")
    (selection-policy   :accessor selection-policy :initarg :selection-policy
         :documentation ":single :multiple :browse :extended")
    (value              :accessor value :initform nil
	 :documentation "a single value or a subset of value-list")
    (value-list         :accessor value-list
         :documentation "the list of values extracted from the item list")
    (string-list        :accessor string-list
         :documentation "the list of strings extracted from the item list")
    (value-changed-callback      :accessor value-changed-callback      
                                 :initform nil)
    (default-action-callback     :accessor default-action-callback     
                                 :initform nil)
    (browse-selection-callback   :accessor browse-selection-callback   
                                 :initform nil)
    (single-selection-callback   :accessor single-selection-callback   
                                 :initform nil)
    (extended-selection-callback :accessor extended-selection-callback 
                                 :initform nil)
    (multiple-selection-callback :accessor multiple-selection-callback 
                                 :initform nil)))

(defcallback selection-list value-changed-callback nil
	     :lambda-list (value old-value))
(defcallback selection-list default-action-callback     :default-action
	     :lambda-list (item item-no)
	     :hidden t)
(defcallback selection-list browse-selection-callback   :browse-selection
	     :lambda-list (item item-no)
	     :hidden t)
(defcallback selection-list single-selection-callback   :single-selection
	     :lambda-list (item item-no)
	     :hidden t)
(defcallback selection-list extended-selection-callback :extended-selection
	     :lambda-list (type count &rest items)
	     :hidden t)
(defcallback selection-list multiple-selection-callback :multiple-selection
	     :lambda-list (count &rest items)
	     :hidden t)

(defginafun make-selection-list (parent "the parent widget"
                                 item-list
                                   "list of strings or (string value) pairs"
			    &key (default-action-button nil)
                                   "button to activate on double-click"
			         (initial-value :use-first)
                                   "a value or subset from the value-list"
			         (visible-item-count 10)
                                   "determines the initial height"
			         (width 100) "initial width if allowed"
				 (selection-policy :browse) 
                                   ":single :multiple :browse :extended"
			         (class 'selection-list)
			         (initargs nil)
			         (motif-widget-class :list)
				 (name class)
				 (managed t)
				 (motif-resources nil)
			    &aux new-selection-list)
  (description 
    "make a new list widget"
    :constructor-for-class selection-list
    :called-by-application :sometimes)
  (setq new-selection-list
	(make-widget parent
		     :class class
		     :initargs (more-initargs
				 :default-action-button default-action-button
				 :selection-policy selection-policy)
		     :motif-widget-class motif-widget-class
		     :name name
		     :managed managed
		     :motif-resources (more-motif-resources
					:visible-item-count visible-item-count
					:selection-policy selection-policy
					:list-size-policy :constant
					:width width
					:scroll-bar-display-policy :static)))
  ;; set callbacks to execute a method
  (setf (default-action-callback new-selection-list)
	(make-callback #'execute-default-action-callback new-selection-list))
  (setf (browse-selection-callback new-selection-list)
	(make-callback #'execute-browse-selection-callback new-selection-list))
  (setf (single-selection-callback new-selection-list)
	(make-callback #'execute-single-selection-callback new-selection-list))
  (setf (multiple-selection-callback new-selection-list)
	(make-callback #'execute-multiple-selection-callback 
                       new-selection-list))
  (setf (extended-selection-callback new-selection-list)
	(make-callback #'execute-extended-selection-callback 
                       new-selection-list))
  
  ;; set the item-list of the list widget
  (set-item-list new-selection-list item-list :selected-value initial-value)
  
  new-selection-list)

(defginamethod (setf value) :after (new-value (selection-list selection-list)
					      &aux single-value)
  "the application has changed the selected value"
  (with-slots (value-list string-list selection-policy) selection-list
    ;; determine type of value
    (setq single-value (member selection-policy '(:browse :single)))

    ;; first deselect all items
    (xtk:list-deselect-all-items (widget-id selection-list))
    
    ;; set to :multiple temporarily because list-select-pos acts weird
    (when (eq selection-policy :extended)
      (set-motif-resources selection-list :selection-policy :multiple))
    
    ;; select one or more items
    (loop for val in value-list
	  for pos from 1
	  when (if single-value
		   (equal val new-value)
		   (member val new-value :test #'equal))
	    do (xtk:list-select-pos (widget-id selection-list) pos)
	       (when single-value (return))
	       )
    
    (when (eq selection-policy :extended)
      (set-motif-resources selection-list :selection-policy :extended))))

(defginamethod set-item-list ((selection-list selection-list) 
                              new-item-list 
                                "list of strings or (string value) pairs"
			 &key (selected-value :use-first)
                                "a value or subset from value-list"
			      managed "ignored")
  (description
    "the item-list has changed"
    :called-by-application "to change the list of selectable values")
  (declare (ignore managed))
  ;;(when (null new-item-list) (setq new-item-list '("       ")))
  (with-slots (value value-list string-list selection-policy) selection-list
    ;; recompute slots
    (setq string-list nil value-list nil)
    (loop for item in new-item-list
	  do (if (listp item)
		 (progn
		   (push (first  item) string-list)
		   (push (second item) value-list))
		 (progn
		   (push (if (stringp item) item (format nil "~a" item)) 
                         string-list)
		   (push item value-list))))
    (setq string-list (reverse string-list))
    (setq value-list  (reverse value-list))

    ;; try it with an empty list, should work now
    ;(when (null string-list) (setq string-list '("       ")))
    ;; inform motif-widget
    (apply #'xtk:set-items
	   (cons (widget-id selection-list) string-list))

    ;; select desired item
    (if (eq selected-value :use-first)
	(setf (value selection-list)
	      (if (member selection-policy '(:browse :single))
		  (first value-list)
		  nil))
      (if (listp selected-value)
        (setf (value selection-list) (copy-list selected-value))
        (setf (value selection-list) selected-value)))
    ))

(defginamethod show-item ((selection-list selection-list) value-to-show)
  (description
    "scroll to show item at top"
    :called-by-application "to show a certain item at the top")
  (with-slots (value-list string-list) selection-list
    (loop for value  in value-list
	  for string in string-list
	  when (equal value value-to-show)
	    do (xtk:list-set-top-item (widget-id selection-list) string)
	    and do (return nil))))

(defmethod execute-default-action-callback ((list selection-list) 
                                                item item-no
						&aux new-value old-value)
  "an new item is selected, store it in a slot"
  (declare (ignore item))
  ;;(format t "Double-click: ~a ~a~%" item item-no)
  (with-slots (value value-list default-action-button
		     selection-policy value-changed-callback) list
    (setq old-value value)
    (setq new-value (nth (1- item-no) value-list))
    (setf (value list) ;; activate deamon, because Motif 
                       ;; does NOT change selection !!
	  (case selection-policy
	    ((:browse :single) new-value)
	    (:extended         (list new-value))
	    (:multiple (if (member new-value value :test #'equal)
			   (delete new-value value)
			   (cons new-value value)))))
    (if default-action-button
	;; simulate button press of default button
	(execute-from-widget (activate-callback default-action-button)
			     list nil)
	;; else: handle like simple selction
	(execute-from-widget value-changed-callback list 
			     (list value old-value)))))

(defmethod execute-browse-selection-callback ((list selection-list) 
                                                  item item-no
						  &aux old-value)
  "an new item is selected, store it in a slot"
  (declare (ignore item))
  ;;(format t "~s ~a~%" item item-no)
  (with-slots (value value-list value-changed-callback) list
    (setq old-value value)
    (setq value (nth (1- item-no) value-list))
    (execute-from-widget value-changed-callback list (list value old-value))))

(defmethod execute-single-selection-callback ((list selection-list) 
                                                  item item-no
					          &aux old-value)
  "an new item is selected, store it in a slot"
  (declare (ignore item))
  ;;(format t "~s ~a~%" item item-no)
  (with-slots (value value-list value-changed-callback) list
    (setq old-value value)
    (setq value (nth (1- item-no) value-list))
    (execute-from-widget value-changed-callback list (list value old-value))))

(defmethod execute-multiple-selection-callback ((list selection-list) 
                                                    count &rest items
						    &aux old-value)
  "an new set of items is selected, store it in a slot"
  (declare (ignore count))
  ;;(format t "~a ~s~%" count items)
  (with-slots (value value-list string-list value-changed-callback) list
    (setq old-value value)
    (setq value
	  (loop for single-value in value-list
		for string in string-list 
		when (member string items :test #'equal)
		  collect single-value))
    (execute-from-widget value-changed-callback list (list value old-value))))

(defmethod execute-extended-selection-callback ((list selection-list) 
                                                    type count
						    &rest items
						    &aux old-value)
  "an new set of items is selected, store it in a slot"
  (declare (ignore type count))
  ;;(format t "~s ~a ~s~%" type count items)
  (with-slots (value value-list string-list value-changed-callback) list
    (setq old-value value)
    (setq value
	  (loop for single-value in value-list
		for string in string-list 
		when (member string items :test #'equal)
		  collect single-value))
    (execute-from-widget value-changed-callback list (list value old-value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class scrollable-selection-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass scrollable-selection-list (selection-list)

   (description
     "a selection-list inside a scroller"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ((scroller :accessor scroller :documentation "surrounding the list")))

(defginafun make-scrollable-selection-list (parent "the parent widget"
                                 item-list
                                   "list of strings or (string value) pairs"
			    &key (default-action-button nil)
                                   "button to activate on double-click"
			         (initial-value :use-first)
                                   "a value or subset from the value-list"
			         (visible-item-count 10)
                                   "determines the initial height"
			         (width 100) "initial width if allowed"
				 (selection-policy :browse) 
                                   ":single :multiple :browse :extended"
				 (class 'scrollable-selection-list)
				 (initargs nil)
				 (motif-widget-class :list)
				 (name class)
				 (managed t)
				 (motif-resources nil)
			    &aux new-scroller 
                                 new-scrollable-selection-list)
  (description 
    "make a new scroller containing a selection-list"
    :constructor-for-class scrollable-selection-list
    :called-by-application :sometimes)
  (setq new-scroller
	(make-scroller parent
		       :scrolling-policy :application-defined
		       :name name
		       :managed managed))
  (setq new-scrollable-selection-list
	(make-selection-list new-scroller item-list
			     :default-action-button default-action-button
			     :initial-value initial-value
			     :visible-item-count visible-item-count
			     :selection-policy selection-policy
			     :width width
			     :class class
			     :initargs initargs
			     :motif-widget-class motif-widget-class
			     :motif-resources motif-resources))
  (setf (scroller new-scrollable-selection-list) new-scroller)
  (setf (group-widget-id new-scrollable-selection-list) 
        (widget-id new-scroller))
  ;; set parent to skip scroller in between
  (setf (parent new-scrollable-selection-list) parent)
  new-scrollable-selection-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class bulletin-board
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass bulletin-board (widget)

   (description
     "the Motif bulletin board widget"
     :application-subclasses :rarely
     :gina-subclasses t)

   ((map-callback :accessor map-callback :initform nil)
    (unmap-callback :accessor unmap-callback :initform nil)))

(defcallback bulletin-board map-callback   :map   :lambda-list "()")
(defcallback bulletin-board unmap-callback :unmap :lambda-list "()")

(defginafun make-bulletin-board (parent "the parent widget"
			    &key (auto-unmanage nil)
                                   "if button activation removes dialog"
			         (class 'bulletin-board)
			         (initargs nil)
				 (motif-widget-class :bulletin-board)
				 (name class)
				 (managed nil)
				 (motif-resources nil))
  (description 
    "create a new CLOS Object representing a BulletinBoard"
    :constructor-for-class bulletin-board
    :called-by-application :sometimes)
  (make-widget parent
	       :class class
	       :initargs initargs
	       :motif-widget-class motif-widget-class
	       :name name
	       :managed managed
	       :motif-resources (more-motif-resources 
                                 :auto-unmanage auto-unmanage)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass form (bulletin-board)

   (description
     "the Motif form widget"
     :application-subclasses :rarely
     :gina-subclasses t)

   ())

(defginafun make-form (parent "the parent widget"
                         &key (auto-unmanage nil)
                                "if button activation removes dialog"
		              (class 'form)
			      (initargs nil)
			      (motif-widget-class :form)
			      (name class)
			      (managed t)
			      (motif-resources nil))
  (description 
    "make a new form widget"
    :constructor-for-class form
    :called-by-application :sometimes)
  (make-bulletin-board parent
		       :auto-unmanage auto-unmanage
		       :class class
		       :initargs initargs
		       :motif-widget-class motif-widget-class
		       :name name
		       :managed managed
		       :motif-resources (more-motif-resources
					  ;;:rubber-positioning t
					  )))
(defginafun define-form-constraint (widget "a child of the form"
			       &rest constraint-resources
                                       "list of motif constraint resources"
			       &aux restlist)
  (description
    "define layout constraint for child of a form widget"
    :called-by-application "to define attachments for form children")
  (comment
    "Every side of a widget can be individually attached in the following ways:"
    ":none :form :opposite-form :widget :opposite-widget :position :self"
    "2-5 use the :(dir)-offset, :position uses :(dir)-position,"
    "4-5 use the :(dir)-widget parameters in the list")
  (example
    (define-form-constraint child
       :top-attachment :form :top-offset 4
       :left-attachment widget :left-widget another-child
       :right-attachment :position :right-position 50
       :bottom-attachment :none))
  ;; replace widget objects by widget id's
  (setq restlist constraint-resources)
  (loop for parm in constraint-resources
	do (setq restlist (cdr restlist))
	when (member parm 
                     '(:left-widget :right-widget :bottom-widget :top-widget))
	  do (rplaca restlist (group-widget-id (first restlist))))
  ;;(format t "~a~%" constraint-resources)
  (apply #'xtk:set-values (cons (group-widget-id widget) constraint-resources)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass text (widget)

   (description
     "the Motif text widget"
     :application-subclasses :rarely
     :gina-subclasses t)

   ((value                  :accessor value                  :initarg :value
         :documentation "the current string value")
    (value-changed-callback :accessor value-changed-callback :initform nil)
    (modify-verify-callback :accessor modify-verify-callback :initform nil)
    (motion-verify-callback :accessor motion-verify-callback :initform nil)
    (losing-focus-callback  :accessor losing-focus-callback  :initform nil)
    (activate-callback      :accessor activate-callback      :initform nil)))


(defcallback text value-changed-callback :value-changed
	     :lambda-list "() but (value) for single-line texts")
(defcallback text modify-verify-callback :modify-verify
	     :lambda-list 
                 (current-insert new-insert start-pos end-pos text length))
(defcallback text motion-verify-callback :motion-verify
	     :lambda-list (current-insert new-insert))
(defcallback text losing-focus-callback  :losing-focus
	     :lambda-list (current-insert new-insert start-pos end-pos))
(defcallback text activate-callback      :activate
	     :lambda-list "()")

(defresourceslot text value :value)

(defginafun make-text (parent "the parent widget"
		  &key (value "") "initial value"
		       (editable t) "if editable"
		       (columns 20) "determines initial width"
		       (rows 1) "determines initial height"
		       (edit-mode :single-line-edit) 
                         ":single-line-edit or :multi-line-edit"
		       (class 'text)
		       (initargs nil)
		       (motif-widget-class :text)
		       (name class)
		       (managed t)
		       (motif-resources nil)
		       &aux new-text)
  (description 
    "make a new text widget"
    :constructor-for-class text
    :called-by-application :sometimes)
  (setq new-text
	(make-widget parent
		     :class class
		     :initargs (more-initargs :value value)
		     :motif-widget-class motif-widget-class
		     :name name
		     :managed managed
		     :motif-resources (more-motif-resources
					:value value
					:editable editable
					:columns columns
					:rows rows
					:edit-mode edit-mode)))

  ;; make text reachable by TAB-character
  (when editable (add-to-tab-group new-text))
  
  new-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class scrolled-text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass scrolled-text (text)

   (description
     "a text widget inside a scroller"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ((scroller :accessor scroller :documentation "surrounding the list")))

(defginafun make-scrolled-text (parent "the parent widget"
		  &key (value "") "initial value"
		       (editable t) "if editable"
		       (columns 20) "determines initial width"
		       (rows 10) "determines initial height"
                       (scroll-vertical t) "if vertical scrollbar"
                       (scroll-horizontal t) "if horizontal scrollbar"
		       (class 'scrolled-text)
		       (initargs nil)
		       (motif-widget-class :text)
		       (name class)
		       (managed t)
		       (motif-resources nil)
		       &aux new-scroller new-text)
  (description 
    "make a new scroller containing a text widget"
    :constructor-for-class text
    :called-by-application :sometimes)
  (setq new-scroller
        (make-scroller parent
                       :scrolling-policy :application-defined
                       :name name
                       :managed managed))
  (setq new-text
        (make-text new-scroller
                   :value value
                   :editable editable
                   :columns columns
                   :rows rows
                   :edit-mode :multi-line-edit
                   :class class
                   :initargs initargs
                   :motif-widget-class motif-widget-class
                   :motif-resources 
                     (more-motif-resources 
                         :scroll-horizontal scroll-horizontal
                         :scroll-vertical scroll-vertical
                     )))
  (setf (scroller new-text) new-scroller)
  (setf (group-widget-id new-text)
        (widget-id new-scroller))
  ;; set parent to skip scoller in between
  (setf (parent new-text) parent)
  new-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class labeled-text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass labeled-text (text)

   (description
     "a text widget with a label"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ((label              :accessor label
         :documentation "the label widget")
    (row-column         :accessor row-column
         :documentation "the enclosing row-column")
    (label-position     :accessor label-position :initarg :label-position
         :documentation ":top or :left")))

(defginafun make-labeled-text (parent "the parent widget"
                               label-string "the label shown"
			  &key (value "") "initial value"
		               (editable t) "if editable"
		               (columns 20) "determines initial width"
		               (edit-mode :single-line-edit) 
                                 ":single-line-edit or :multi-line-edit"
			       (label-position :left)
                                 ":top or :left"
			       (class 'labeled-text)
			       (initargs nil)
			       (motif-widget-class :text)
			       (name class)
			       (managed t)
			       (motif-resources nil)
			       (row-column-resources nil)
                                 "motif-resources for row-column"
			       (label-resources nil)
                                 "motif-resources for the label"
			  &aux new-row-column new-label new-labeled-text)
  (description 
    "create a new labeled text"
    :constructor-for-class labeled-text
    :called-by-application :sometimes)
  (setf new-row-column
	(make-row-column parent
			 :orientation (if (eq label-position :left) 
                                          :horizontal 
                                          :vertical)
			 :name name
			 :managed managed
			 :motif-resources row-column-resources))

  (setq new-label 
	(make-label new-row-column label-string
		    :motif-resources label-resources))
  
  (setq new-labeled-text
	(make-text new-row-column
		   :value value
		   :editable editable
		   :columns columns
		   :edit-mode edit-mode
		   :class class
		   :initargs (more-initargs :label-position label-position)
		   :motif-widget-class motif-widget-class
		   :motif-resources motif-resources))
  
  (with-slots (label row-column group-widget-id) new-labeled-text
    (setq label new-label)
    (setq row-column new-row-column)
    (setq group-widget-id (widget-id new-row-column)))
  ;; set parent to skip scroller in between
  (setf (parent new-labeled-text) parent)
  
  new-labeled-text)

(defginamethod (setf label-position) :after 
                                     (new-label-position (lt labeled-text))
  "modify orientation of row-column"
  (set-motif-resources (row-column lt)
		       :orientation (if (eq new-label-position :left) 
                                        :horizontal 
                                        :vertical)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  class dialog-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass dialog-box (form)

   (description
     "a superclass for gina dialog boxes"
     :instantiate nil
     :gina-subclasses t)

   ((document           :accessor document     :initarg :document
         :documentation "the document associated with the dialog box")
    (dialog-shell       :accessor dialog-shell :initarg :dialog-shell
         :documentation "the shell widget around the dialog")
    (relative-widget    :accessor relative-widget :initform nil
         :documentation "a widget used for relative popup positioning")
    (relative-x         :accessor relative-x      :initform :center
     :documentation "one of :center :left :left-align :right-align :right")
    (relative-y         :accessor relative-y      :initform :center
     :documentation "one of :center :top :top-align :bottom-align :bottom")
    (result             :accessor result         :initform nil
	 :documentation "value returned from modal dialog")))

(defginamethod closed-by-wm ((box dialog-box))
  (description
    "dialog-box is being closed by the window manager"
    :override :sometimes
    :default-version "calls pop-down")
  (pop-down box))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  class modeless-dialog-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass modeless-dialog-box (dialog-box)

   (description
     "a form with a shell around it for modeless dialogs"
     :application-subclasses "for application-specific modeless dialogs"
     :gina-subclasses nil)

   ((default-button     :accessor default-button :initform nil
         :documentation "the default button in the dialog, if any")))

(defginafun make-modeless-dialog-box (title "the title string"
				&key (document 
                                       (first (document-list *application*)))
                                       "default: first document of application"
				     (resize nil)
                                       "whether include resize handles"
				     (allow-shell-resize nil)
                                       "whether resizeable by changing children"
				     (return-is-accelerator t)
                                       "whether return activates default button"
                                     (drag-protocols nil)
                                       "keyword list of accepted drag-commands"
			             (class 'modeless-dialog-box)
			             (initargs nil)
			             (motif-widget-class :form)
			             (name class)
			             (motif-resources nil)
			             (dialog-shell-resources nil)
                                       "motif-resources for dialog-shell"
			         &aux new-modeless-dialog-box new-dialog-shell)
  (description 
    "make a form with a shell around it for modeless dialogs"
    :constructor-for-class modeless-dialog-box
    :called-by-application :sometimes)
  (declare (special *application*))
  (setq new-dialog-shell
	(make-modeless-dialog-shell title
				    :parent-shell (main-shell document)
				    :allow-shell-resize allow-shell-resize
				    :name name
				    :motif-resources dialog-shell-resources
				    :initargs 
				      (list :drag-protocols drag-protocols)))
  
  (setf (document new-dialog-shell) document)
  
  (when (not return-is-accelerator)
    ;; no default button can be activated
    (setq motif-resources (more-motif-resources :accelerators "")))
  
  (setq new-modeless-dialog-box
	(make-form new-dialog-shell
		   :auto-unmanage nil
		   :class class
		   :initargs (more-initargs 
                              :dialog-shell new-dialog-shell
                              :document document)
		   :motif-widget-class motif-widget-class
		   :managed nil
		   :motif-resources (more-motif-resources
				      :dialog-style :modeless
				      :no-resize (not resize)
				      :resize-policy :grow)))

  ;; callback for relative positioning on popup
  (setf (map-callback new-modeless-dialog-box)
        (make-callback 'position-dialog-box new-modeless-dialog-box))

  ;; set backward-pointer from dialog-shell to dialog-box
  (setf (dialog-box new-dialog-shell) new-modeless-dialog-box)
  
  new-modeless-dialog-box)

(defginamethod (setf default-button) :after 
                              (new-default-button (box modeless-dialog-box))
  "set default-button resource in the toolkit"
  (set-motif-resources box :default-button (widget-id new-default-button)))

(defginamethod pop-up ((box modeless-dialog-box))
  (description
    "show the modeless dialog box"
    :called-by-application "to make a dialog appear")
  (pop-up (dialog-shell box)))

(defginamethod pop-down ((box modeless-dialog-box))
  (description
    "hide the modeless dialog box"
    :called-by-application "to make a dialog disappear")
  (pop-down (dialog-shell box)))

(defun position-dialog-box (box)
  "move dialog before popup relative to another widget"
  (when (relative-widget box)
     (set-motif-resources box :default-position nil)
     (update-slots box)
     (update-slots (relative-widget box))
     (multiple-value-bind (root-x root-y)
           (root-coordinates (relative-widget box))
       (move box
             (case (relative-x box)
               ((:left)   (- root-x 25 (width box)))
               ((:left-align)  (- root-x 11))
               ((:center) (- root-x 11
                             (- (round (/ (width box) 2))
                                (round (/ (width (relative-widget box)) 2)))))
               ((:right-align) (+ root-x -11 (- (width (relative-widget box))
                                               (width box))))
               (t         (+ root-x (width (relative-widget box)) 13)))
             (case (relative-y box)
               ((:top)    (- root-y 42 (height box)))
               ((:top-align)  (- root-y 27))
               ((:center) (- root-y 27
                             (- (round (/ (height box) 2))
                                (round (/ (height (relative-widget box)) 2)))))
               ((:bottom-align) (+ root-y -27 (- (height (relative-widget box))
                                                 (height box))))
               (t         (+ root-y (height (relative-widget box)) 13)))))))

'(progn
   (setq *box* (make-modeless-dialog-box "a modeless dialog box"
		     :motif-resources (list :width 200 :height 150)))
   (make-beep-button *box*)
   (pop-up *box*))
'(pop-down *box*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  class modal-dialog-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass modal-dialog-box (dialog-box)

   (description
     "a form with a shell around it for modal dialogs"
     :application-subclasses "for application-specific modal dialogs"
     :gina-subclasses nil)

   ((default-button     :accessor default-button :initform nil
         :documentation "the default button in the dialog, if any")))

(defginafun make-modal-dialog-box (title "the title string"
			     &key (document 
                                      (first (document-list *application*)))
                                    "default: the first document of application"
			          (resize nil)
                                    "whether include resize handles"
			          (allow-shell-resize nil)
                                    "whether resized by changing children"
			          (return-is-accelerator t)
                                    "whether return activates default button"
                                  (drag-protocols nil)
                                    "keyword list of accepted drag-commands"
			          (class 'modal-dialog-box)
			          (initargs nil)
			          (motif-widget-class :form)
			          (name class)
			          (motif-resources nil)
			          (dialog-shell-resources nil)
                                    "motif-resources for dialog-shell"
			     &aux new-modal-dialog-box new-dialog-shell)
  (description 
    "make a form with a shell around it for modal dialogs"
    :constructor-for-class modal-dialog-box
    :called-by-application :sometimes)
  (declare (special *application*))
  (setq new-dialog-shell
	(make-modal-dialog-shell title
	      :parent-shell (if (active-modal-dialogs document)
				(dialog-shell
				     (first (active-modal-dialogs document)))
				(main-shell document))
	       :allow-shell-resize allow-shell-resize
	       :name name
	       :motif-resources dialog-shell-resources
               :initargs (list :drag-protocols drag-protocols)))

  (when (not return-is-accelerator)
    ;; no default button can be activated
    (setq motif-resources (more-motif-resources :accelerators "")))
  
  (setq new-modal-dialog-box
	(make-form new-dialog-shell
		   :auto-unmanage nil ;; t => Absturz !?
		   :class class
		   :initargs (more-initargs :dialog-shell new-dialog-shell
					    :document document)
		   :motif-widget-class motif-widget-class
		   :managed nil
		   :motif-resources (more-motif-resources
				      :dialog-style :full-application-modal
				      :no-resize (not resize)
				      :resize-policy :grow)))

  ;; callback for relative positioning on popup
  (setf (map-callback new-modal-dialog-box)
        (make-callback 'position-dialog-box new-modal-dialog-box))
  
  new-modal-dialog-box)

(defginamethod (setf default-button) :after 
                                     (new-default-button (box modal-dialog-box))
  "set default button resource in the toolkit"
  (set-motif-resources box :default-button (widget-id new-default-button)))

(defginamethod pop-up ((box modal-dialog-box))
  (description
    "bring up the modal dialog"
    :called-by-application "to show a modal dialog and wait for the result"
    :result "the result slot")
  (declare (special *application*))
  (with-slots (widget-id result document) box
    (unwind-protect
      (progn
	(with-clock-cursor
	 (xtk:realize-widget widget-id)
	 (push (dialog-shell box) (toplevel-shells *application*))
	 (push box (active-modal-dialogs document)))
	(setf (visible (dialog-shell box)) t)
	(eval-initial-taps (dialog-shell box) :pop-up)
	
	;; bring up dialog and wait until answered
	(xtk:alert-dialog widget-id))
      
      ;;cleanup
      (pop (active-modal-dialogs document))
      (setf (toplevel-shells *application*) 
	(delete (dialog-shell box) (toplevel-shells *application*)))
      )
    ;; return result slot of dialog box
    result))

(defginamethod pop-down ((box modal-dialog-box))
  (description
    "end the modal dialog"
    :called-by-application "to end the modal dialog")
  (setf (visible (dialog-shell box)) nil)
  (xtk:unmanage-popup-child (widget-id box)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class tool-dialog-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass tool-dialog-box (dialog-box)

   (description
     "a form with a shell around it for tool dialogs with a menu"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ((main-menu          :accessor main-menu
         :documentation "the automatically created menu bar")))

(defginafun make-tool-dialog-box (title "the title string"
			    &key (document 
                                    (first (document-list *application*)))
                                   "default: first document of application"
				 (allow-shell-resize nil)
                                   "whether resized by changing children"
                                 (drag-protocols nil)
                                   "keyword list of accepted drag-commands"
				 (class 'tool-dialog-box)
				 (initargs nil)
				 (motif-widget-class :form)
				 (name class)
				 (motif-resources nil)
				 (dialog-shell-resources nil)
                                   "motif-resources for dialog shell"
			    &aux new-tool-dialog-box new-dialog-shell)
  (description 
    "make a form with a shell an a menu bar for tool dialogs"
    :constructor-for-class tool-dialog-box
    :called-by-application :sometimes)
  (setq new-dialog-shell
	(make-modeless-dialog-shell title
				    :parent-shell (main-shell document)
				    :allow-shell-resize allow-shell-resize
				    :name name
				    :motif-resources dialog-shell-resources
				    :initargs
				      (list :drag-protocols drag-protocols)))
  
  ;; no default button can be activated
  (setq motif-resources (more-motif-resources :accelerators ""))
  
  (setq new-tool-dialog-box
	(make-form new-dialog-shell
		   :class class
		   :auto-unmanage nil
		   :initargs (more-initargs :dialog-shell new-dialog-shell
                                            :document document)
		   :motif-widget-class motif-widget-class
		   :managed nil
		   :motif-resources (more-motif-resources
                                      ;:default-position nil
				      :dialog-style :modeless
				      :no-resize nil
				      :resize-policy :grow)))

  ;; callback for relative positioning on popup
  (setf (map-callback new-tool-dialog-box)
        (make-callback 'position-dialog-box new-tool-dialog-box))

  (setf (main-menu new-tool-dialog-box) (make-main-menu new-tool-dialog-box))
  (define-form-constraint (main-menu new-tool-dialog-box)
              :left-attachment :form  :top-attachment :form)

  ;; add close command
  (add-menu-command (main-menu new-tool-dialog-box)
                    "File" "Close Tool"
                    (make-callback 'pop-down new-tool-dialog-box))

  ;; set backward-pointer from dialog-shell to dialog-box
  (setf (dialog-box new-dialog-shell) new-tool-dialog-box)
  
  new-tool-dialog-box)

(defginamethod pop-up ((box tool-dialog-box))
  (description
    "show the tool dialog box"
    :called-by-application "to make a tool dialog appear")
  ;(when (not (slot-boundp (dialog-shell box) 'x-pos))
  ;      (setf (x-pos box)
  ;            (+ (x-pos (parent (dialog-shell box))) 50))
  ;      (setf (y-pos box)
  ;            (+ (y-pos (parent (dialog-shell box))) 50)))
  (pop-up (dialog-shell box))
  (define-form-constraint (main-menu box) :right-attachment :form))

(defginamethod pop-down ((box tool-dialog-box))
  (description
    "hide the tool dialog box"
    :called-by-application "to make a tool dialog disappear")
  (pop-down (dialog-shell box)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class dialog-dismiss-button
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass dialog-dismiss-button (push-button)

   (description
     "a push-button to dismiss a dialog box"
     :application-subclasses :rarely
     :gina-subclasses nil)

  ((dialog-box         :accessor dialog-box :initarg :dialog-box
	:documentation "modal dialog box containing the button")
   (result             :accessor result :initarg :result
        :documentation "assigned to result-variable when button clicked")))

(defginafun make-dialog-dismiss-button (parent "the parent widget"
                                        string "the label string"
				   &key (dialog-box parent)
                                          "the dialog box to be notified"
				        (default-button nil)
                                          "whether it is the default button"
					(result string)
                                          "the result value to assign"
					(class 'dialog-dismiss-button)
					(initargs nil)
					(motif-widget-class :push-button)
					(name class)
					(managed t)
					(motif-resources nil)
				   &aux new-dialog-dismiss-button)
  (description 
    "make a new dialog-dismiss-button widget"
    :constructor-for-class dialog-dismiss-button
    :called-by-application :sometimes)
  (setq new-dialog-dismiss-button
	(make-push-button parent string
			  :show-as-default (if default-button 1 0)
			  :class class
			  :initargs (more-initargs :dialog-box dialog-box
						   :result result)
			  :motif-widget-class motif-widget-class
			  :name name
			  :managed managed
			  :motif-resources motif-resources))
  (when (and default-button dialog-box)
    (setf (default-button dialog-box) new-dialog-dismiss-button)
    (set-motif-resources new-dialog-dismiss-button :traversal-on t))

  new-dialog-dismiss-button)

(defginamethod execute-activate-callback ((button dialog-dismiss-button))
  (description
    "store result in dialog box"
    :override :sometimes
    :default-version "assigns result and removes the dialog")
  (with-slots (dialog-box result) button
    (when dialog-box
      (setf (result dialog-box) result)
      ;; auto-unmanage only works for direct children
      (pop-down dialog-box))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class dialog-dismiss-button-row
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass dialog-dismiss-button-row (row-column)

   (description
     "a row of dialog-dismiss-buttons with the leftmost as default"
     :application-subclasses :rarely
     :gina-subclasses nil)

   ((buttons            :accessor buttons
         :documentation "the list of buttons")))

(defginafun make-dialog-dismiss-button-row (parent "the parent widget"
			     &key (item-list '(("  Yes " :yes)
				     	       ("  No  " :no)
				      	       ("Cancel" :cancel)))
                                    "a list of strings or (string value) pairs"
				  (dialog-box parent)
                                    "the dialog box to be notified"
				  (spacing 30)
                                    "between buttons"
				  (class 'dialog-dismiss-button-row)
				  (initargs nil)
				  (motif-widget-class :row-column)
				  (name class)
				  (managed t)
				  (motif-resources nil)
			     &aux new-dialog-dismiss-button-row) 
  (description 
    "make row of dialog-dismiss-buttons with the leftmost as default"
    :constructor-for-class dialog-dismiss-button-row
    :called-by-application :sometimes)
  (setq new-dialog-dismiss-button-row
	(make-row-column dialog-box
			 :spacing spacing
			 :orientation :horizontal
			 :class class
			 :initargs initargs
			 :motif-widget-class motif-widget-class
			 :name name
			 :managed managed
			 :motif-resources (more-motif-resources
					    ;;:packing :pack-none
					    )))
  
  ;; create a push-button for each item pair
  (setf (buttons new-dialog-dismiss-button-row)
	(loop for item-pair in item-list
	      collect (make-dialog-dismiss-button
			new-dialog-dismiss-button-row (first item-pair)
			:dialog-box dialog-box
			:default-button (equal item-pair (first item-list))
			:result (second item-pair))))
  ;; return the row-colummn widget
  new-dialog-dismiss-button-row)

(defmethod allows-inspect-click ((w dialog-dismiss-button-row))
  "if the widget as a parent allows the inspect-click to be installed"
  nil)

