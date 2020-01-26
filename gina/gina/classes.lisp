;;; -*- Mode:LISP;Syntax: Common-Lisp;Package: GINA ;Base:10-*-

(in-package :gina) 

#+cmu (defclass menu-bar () ()
            (:documentation "forward declaration for CMU-CL 16e"))
#+cmu (defclass modeless-dialog-box () ()
            (:documentation "forward declaration for CMU-CL 16e"))

(defginaclass application nil
 (description "the superclass of all GINA applications" :application-subclasses
  :always :gina-subclasses "only in the demo applications" :instantiate
  "to run the empty application")
 ((name :accessor name :initform "GINA" :allocation :class :documentation
   "name of application")
  (document-type :accessor document-type :initform 'document :allocation :class
   :documentation "class of application dependent documents")
  (signature :accessor signature :initform "GINA" :allocation :class
   :documentation "internal identification of application")
  (file-type :accessor file-type :initform "empty" :allocation :class
   :documentation "type field of pathnames for saved documents")
  (process-per-document :accessor process-per-document :initform nil
   :allocation :class)
  (display-host :accessor display-host :initarg :display-host :documentation
   "where the X-server runs")
  (display-number :accessor display-number :initarg :display-number
   :documentation "number of display on host where X-server runs")
  (screen-number :accessor screen-number :initarg :screen-number :documentation
   "number of screen on host where X-server runs")
  (toolkit-host :accessor toolkit-host :initarg :toolkit-host :documentation
   "where the toolkit-server runs")
  (debug-out :accessor debug-out :initarg :debug-out :documentation
   "flag for debugging-output")
  (document-list :accessor document-list :initform nil :documentation
   "the document objects currently open")
  (document-counter :accessor document-counter :initform 0)
  (process :accessor process :initform nil :documentation
   "the process running this application")
  (mailbox :accessor mailbox :initform nil :documentation
   "a list of messages, sent by other applications")
  (display :accessor display :initform nil :documentation
   "the CLX-display used by this application")
  (clx-synchronous :accessor clx-synchronous :initform *clx-synchronous*
   :documentation "flag if CLX is run in synchronous mode")
  (xtk-connection :accessor xtk-connection :initform nil :documentation
   "the Motif-connection used by this application")
  (application-shell :accessor application-shell :documentation
   "invisible shell, root of widget hierarchy")
  (toplevel-shells :accessor toplevel-shells :initform nil :documentation
   "List of all toplevel shells")
  (screen :accessor screen)
  (standard-font :accessor standard-font :initform "9x15")
  (cursor-font :accessor cursor-font)
  (cursor-table :accessor cursor-table :initform (make-hash-table))
  (pixmap-table :accessor pixmap-table :initform
   (make-hash-table :test #'equal))
  (drag-window-hash :accessor drag-window-hash :initform (make-hash-table))
  (semaphor :accessor semaphor :initform (xtk::make-process-lock))
  (idle-timeout :accessor idle-timeout :initform nil :documentation
   "time interval in seconds between calls of method idle-action")
  (timer-id :accessor timer-id :initform nil :documentation
   "id for xtk:change-timer and friends")
  (timer-active-in-handler :accessor timer-active-in-handler :initform nil
   :documentation "keyword parameter for xtk:create-timer")
  (inspect-click :accessor inspect-click :initform *inspect-click*
   :documentation "flag whether inspect-click is enabled")
  (debug-menu :accessor debug-menu :initform *debug-menu* :documentation
   "flag whether debug menu is present")
  (just-print-errors :accessor just-print-errors :initform t :documentation
   "flag whether errors are just printed or debugger is invoked")
  (feedback-animation :accessor feedback-animation :initform nil :documentation
   "flag for experimental mouse-feedback animation")
  (cursor-stack :accessor cursor-stack :initform nil))) 
(defginaclass document nil
 (description "the superclass of all documents" :application-subclasses :always
  :gina-subclasses "only in the demo applications" :instantiate nil)
 ((file-pathname :accessor file-pathname :initarg :file-pathname :documentation
   "where the document is stored")
  (wildcard :accessor wildcard :initarg :wildcard :documentation
   "directory where the document is stored")
  (next-number :accessor next-number :documentation
   "used for 'Untitled <number>'")
  (name :accessor name :documentation "the name part of the pathname")
  (modified :accessor modified :initform nil :documentation
   "flag if document was modified since last save")
  (undo-commands :accessor undo-commands :initform nil :documentation
   "list of undoable commands")
  (redo-commands :accessor redo-commands :initform nil :documentation
   "list of redoable commands")
  (max-history :accessor max-history :initform nil :allocation :class
   :documentation
   "maximum number of past commands stored or NIL for unlimited history")
  (active-mouse-command :accessor active-mouse-command :initform nil
   :documentation "pointer to the currently active mouse-command")
  (toplevel-shells :accessor toplevel-shells :initform nil :documentation
   "list of all toplevel shells")
  (views :accessor views :initform nil :documentation
   "all views showing the document contents")
  (main-view :accessor main-view :initform nil :documentation
   "the application can store the most important view here")
  (main-shell :accessor main-shell :documentation
   "the toplevel-window of the document")
  (shell-width :accessor shell-width :initform :auto :documentation
   "width of the shell for this document")
  (shell-height :accessor shell-height :initform :auto :documentation
   "height of the shell for this document")
  (history-scroller :accessor history-scroller :initform nil)
  (inspector-hash-table :accessor inspector-hash-table :initform
   (make-hash-table))
  (active-modal-dialogs :accessor active-modal-dialogs :initform nil
   :documentation "stack of modal dialogs currently visible")
  (undo-menu-entry :accessor undo-menu-entry :initform nil :documentation
   "dynamically changing menu entry for UNDO")
  (redo-menu-entry :accessor redo-menu-entry :initform nil :documentation
   "dynamically changing menu entry for REDO")
  (docs-menu-entry :accessor docs-menu-entry :initform nil :documentation
   "changing menu entry for DOCUMENTS")
  (save-menu-entry :accessor save-menu-entry :initform nil :documentation
   "sometimes insensitive menu entry for SAVE")
  (revert-menu-entry :accessor revert-menu-entry :initform nil :documentation
   "sometimes insensitive menu entry for REVERT")
  (open-dialog-box :accessor open-dialog-box :initform nil)
  (save-dialog-box :accessor save-dialog-box :initform nil)
  (semaphor :accessor semaphor :initform (xtk::make-process-lock))
  (background-processes :accessor background-processes :initform nil
   :documentation "list of all running background processes")
  (drag-protocols :accessor drag-protocols :initform nil :documentation
   "a list of keywords of accepted drag-commands")
  (animator :accessor animator :initform nil)
  (selection-undoable :accessor selection-undoable :allocation :class :initform
   nil))) 
(defclass history-scroller (modeless-dialog-box)
          ((document :accessor document :initarg :document)
           (scrollbar :accessor scrollbar)
           (previous-button :accessor previous-button)
           (next-button :accessor next-button)
           (cmd-num-label :accessor cmd-num-label)
           (play-field :accessor play-field))
          (:documentation
           "a dialog box for scrolling through the command history")) 
(defginaclass widget nil
 (description
  "the abstract superclass of all CLOS-Objects encapsulating Motif widgets"
  :application-subclasses :rarely :gina-subclasses
  "all Motif widgets are subclasses" :instantiate nil)
 ((widget-id :accessor widget-id :documentation
   "integer returned by CLM create-widget call")
  (group-widget-id :accessor group-widget-id :documentation
   "id used when the whole group is meant")
  (widget-id-for-sons :accessor widget-id-for-sons :documentation
   "may be different from widget-id in compound widgets")
  (parent :accessor parent :documentation
   "the parent widget object in the hierarchy")
  (name :accessor name :documentation
   "name string for resource database lookup")
  (width :accessor width :documentation "total width in pixels")
  (height :accessor height :documentation "total height in pixels")
  (x-pos :accessor x-pos :initform 0 :documentation
   "the x-coordinate relative to the parent")
  (y-pos :accessor y-pos :initform 0 :documentation
   "the y-coordinate relative to the parent"))) 
(defginamacro more-initargs (&rest initarg-pairs)
 (description "append more initarg-pairs to local variable initargs"
  :called-by-gina "in constructor-functions of widgets" :called-by-application
  "in the constructor of a new subclass of a widget")
 (example (let ((initargs '(:a 3 :b 7))) (more-initargs :x 5 :y (+ 3 4)))
  "returns (:a 3 :b 7 :x 5 :y 7)")
 (append '(append initargs) (list (cons 'list initarg-pairs)))) 
(defginamacro more-motif-resources (&rest resource-pairs)
 (description "prepend more resource-pairs to local variable motif-resources"
  :called-by-gina "in constructor-functions of widgets" :called-by-application
  "in the constructor of a new subclass of a widget")
 (example
  (let ((motif-resources '(:a 3 :b 7))) (more-motif-resources :x 5 :y (+ 3 4)))
  "returns (:x 5 :y :7 a 3 :b 7)")
 (append '(append) (list (cons 'list resource-pairs)) '(motif-resources))) 
(defginaclass shell (widget)
 (description "the abstract superclass of all Motif-shells"
  :application-subclasses :rarely :gina-subclasses t :instantiate nil)
 ((title :accessor title :initarg :title :documentation "title string")
  (views :accessor views :initform nil :documentation
   "all views within the shell")
  (document :accessor document :initform nil :initarg :document :documentation
   "the document belonging to the shell, or nil")
  (visible :accessor visible :initform nil :documentation
   "whether the shell is up")
  (taps :accessor taps :initform nil)
  (drag-protocols :accessor drag-protocols :initform nil :initarg
   :drag-protocols :documentation
   "a list of keywords of accepted drag-commands")
  (popup-callback :accessor popup-callback :initform nil))) 
(defginaclass tap nil
 (description "the connection between app data and a widget")
 ((method-to-call :accessor method-to-call :initarg :method-to-call)
  (timestamp :accessor timestamp :initform -1)
  (static-params :accessor static-params :initarg :static-params)
  (not-initial :accessor not-initial :initarg :not-initial))) 
(defginamacro when-changed
 ((object) "any CLOS object" assignments "in let style" &rest update-code)
 (description "update slot only when changed" :called-by-application
  "in a tap update function")
 (comment "The update-code is called when a slot value will change."
  "The form (update-slots) in the update code updates the slot"
  "values. You can use another method, such as reconfigure, if"
  "setting the slots is not sufficient")
 (example
  (when-changed (widget) ((value '(1 2 3)) (sensitive t))
   (format t "Old value ~s~%" (value widget)) (update-slots)
   (format t "New value ~s~%" (value widget))))
 (list 'let assignments
       `(unless (and ,@(loop for ass in assignments for slot = (first ass)
                             collect
                             `(equal ,slot (slot-value ,object ',slot))))
          ,@(loop for statement in update-code append
                  (if (equal statement '(update-slots))
                      (loop for ass in assignments for slot = (first ass)
                            collect `(setf (,slot ,object) ,slot))
                    (list statement)))))) 
(defginaclass toplevel-shell (shell)
 (description
  "the abstract superclass of all document-shells and modeless dialog boxes"
  :application-subclasses :rarely :gina-subclasses t :instantiate nil)
 nil) 
(defginamacro with-cursor ((cursor) &body body)
 (description "use cursor for the execution of body in all application windows"
  :called-by-application :sometimes)
 (example (with-cursor (:circle) (sleep 10)))
 `(let ((affected-shells (toplevel-shells *application*)))
    (unwind-protect
        (progn (push ,cursor (cursor-stack *application*))
               (loop for shell in affected-shells do
                     (define-cursor shell ,cursor))
               ,@body)
      (pop (cursor-stack *application*))
      (let ((old-cursor (first (cursor-stack *application*))))
        (loop for shell in affected-shells do
              (define-cursor shell (or old-cursor :restore))))))) 
(defginamacro with-clock-cursor (&body body)
 (description
  "use clock cursor for the execution of body in all application windows"
  :called-by-application "to signal a time-consuming operation")
 (example (with-clock-cursor (sleep 10)))
 `(with-cursor (:watch) ,(cons 'progn body))) 
(defginaclass modeless-dialog-shell (toplevel-shell)
 (description "the Motif dialog-shell widget used for modeless dialogs"
  :application-subclasses :rarely :gina-subclasses nil)
 ((dialog-box :accessor dialog-box :documentation
   "its child, must be set after creation"))) 
(defginaclass modal-dialog-shell (shell)
 (description "the Motif dialog-shell widget used for modal dialogs"
  :application-subclasses :rarely :gina-subclasses nil)
 nil) 
(defginaclass menu-shell (shell)
 (description "the Motif menu-shell widget" :application-subclasses :rarely
  :gina-subclasses nil)
 nil) 
(defginaclass basic-document-shell (toplevel-shell)
 (description "a toplevel-shell for a document without a menubar"
  :application-subclasses :rarely :gina-subclasses nil)
 ((document :accessor document :initarg :document :documentation
   "the document the shell belongs to")
  (x-window :accessor x-window :initform nil :documentation
   "the window to send messages to"))) 
(defginaclass document-shell (basic-document-shell)
 (description "a toplevel-shell with a main-menu, representing a document"
  :application-subclasses "for application-specific main windows"
  :gina-subclasses t)
 ((main-window :accessor main-window :documentation
   "automatically created main window")
  (main-menu :accessor main-menu :documentation
   "automatically created menu with default entries"))) 
(defginaclass document-shell-with-scroller (document-shell)
 (description "a document-shell with a single scroller" :application-subclasses
  :rarely :gina-subclasses nil)
 ((scroller :accessor scroller :documentation
   "automatically created scroller as main window child"))) 
(defginaclass label (widget)
 (description "the Motif label widget" :application-subclasses :rarely
  :gina-subclasses t)
 ((label-string :accessor label-string :initarg :label-string :documentation
   "the string shown")
  (label-type :accessor label-type :initarg :label-type :documentation
   "either :string or :pixmap"))) 
(defginaclass drag-label (label)
 (description "a picture label for drag and drop" :application-subclasses
  :rarely)
 ((drag-shell :accessor drag-shell :documentation
   "a shell with duplicate image to move around")
  (cursor :accessor cursor :initform :dotbox :documentation
   "a cursor created from the label pixmap")
  (activate-callback :accessor activate-callback :initform nil))) 
(defginaclass toggle-button (label)
 (description "the Motif toggle button widget" :application-subclasses :rarely
  :gina-subclasses nil)
 ((value :accessor value :initarg :value :documentation
   "whether currently selected or not")
  (value-changed-callback :accessor value-changed-callback :initarg
   :value-changed-callback)
  (arm-callback :accessor arm-callback :initform nil)
  (disarm-callback :accessor disarm-callback :initform nil))) 
(defginaclass push-button (label)
 (description "the Motif push button widget" :application-subclasses :rarely
  :gina-subclasses nil)
 ((activate-callback :accessor activate-callback :initform nil)
  (arm-callback :accessor arm-callback :initform nil)
  (disarm-callback :accessor disarm-callback :initform nil))) 
(defginaclass cascade-button (label)
 (description "the Motif cascade button widget" :application-subclasses :rarely
  :gina-subclasses nil)
 ((activate-callback :accessor activate-callback :initform nil)
  (cascading-callback :accessor cascading-callback :initform nil))) 
(defginaclass arrow-button (widget)
 (description "the Motif arrow button widget" :application-subclasses :rarely
  :gina-subclasses nil)
 ((activate-callback :accessor activate-callback :initform nil)
  (arm-callback :accessor arm-callback :initform nil)
  (disarm-callback :accessor disarm-callback :initform nil))) 
(defginaclass scale (widget)
 (description "the Motif scale widget" :application-subclasses :rarely
  :gina-subclasses nil)
 ((value :accessor value :initarg :value :documentation
   "current value between min and max")
  (maximum :accessor maximum :initarg :maximum :documentation "maximum value")
  (minimum :accessor minimum :initarg :minimum :documentation "minimum value")
  (value-changed-callback :accessor value-changed-callback :initarg
   :value-changed-callback)
  (drag-callback :accessor drag-callback :initform nil))) 
(defginaclass scrollbar (widget)
 (description "the Motif scroll bar widget" :application-subclasses :rarely
  :gina-subclasses nil)
 ((value :accessor value :initarg :value :documentation
   "current value between min and max")
  (maximum :accessor maximum :initarg :maximum :documentation
   "maximum value in application-defined units")
  (minimum :accessor minimum :initarg :minimum :documentation
   "minimum value in application-defined units")
  (increment :accessor increment :initarg :increment :documentation
   "single-step increment in application-defined units")
  (page-increment :accessor page-increment :initarg :page-increment
   :documentation "page-step increment in application-defined units")
  (slider-size :accessor slider-size :initarg :slider-size :documentation
   "slider size in application-defined units")
  (value-changed-callback :accessor value-changed-callback :initarg
   :value-changed-callback)
  (decrement-callback :accessor decrement-callback :initform nil)
  (increment-callback :accessor increment-callback :initform nil)
  (page-decrement-callback :accessor page-decrement-callback :initform nil)
  (page-increment-callback :accessor page-increment-callback :initform nil)
  (to-bottom-callback :accessor to-bottom-callback :initform nil)
  (to-top-callback :accessor to-top-callback :initform nil)
  (drag-callback :accessor drag-callback :initform nil))) 
(defginaclass main-menu (menu-bar)
 (description "a menu-bar plus its popup shells" :application-subclasses
  :rarely :gina-subclasses nil)
 ((topics :accessor topics :initform nil))) 
(defclass topic nil
          ((name :accessor name :initarg :name)
           (box :accessor box :initarg :box)
           (cascade :accessor cascade :initarg :cascade)
           (items :accessor items :initform nil))
          (:documentation "")) 
(defclass item nil
          ((name :accessor name :initarg :name)
           (entry :accessor entry :initarg :entry))
          (:documentation "")) 
(defginaclass menu-entry nil
 (description "the parent class for menu entries" :application-subclasses
  :rarely :gina-subclasses t :instantiate nil)
 ((widget :accessor widget :initform nil)
  (parent-menu :accessor parent-menu :initform nil)
  (sensitive :accessor sensitive :initform t :documentation
   "the sensitivity flag")
  (separator-before :accessor separator-before :initarg :separator-before
   :documentation "whether create a separating line before this entry")
  (separator :accessor separator :initform nil)
  (callback :accessor callback :initarg :callback :documentation
   "the callback associated with the entry"))) 
(defginaclass button-entry (menu-entry)
 (description "a button entry in menus" :application-subclasses :rarely
  :gina-subclasses t)
 ((label-string :accessor label-string :initarg :label-string :documentation
   "the label string shown")
  (accelerator :accessor accelerator :initarg :accelerator :documentation
   "a string denoting an accelerator key, e.g. \"F5\"")
  (acc-modifier :accessor acc-modifier :initarg :acc-modifier :documentation
   "modifier, one of :alt :shift :ctrl"))) 
(defginaclass toggle-entry (button-entry)
 (description "a toggle button entry in menus" :application-subclasses :rarely
  :gina-subclasses nil)
 ((value :accessor value :initarg :value :documentation
   "the current toggle value"))) 
(defginaclass toggle-group-entry (button-entry)
 (description "a group of toggle buttons in a menu" :application-subclasses
  :rarely :gina-subclasses t)
 ((parent :accessor parent :initform nil)
  (is-submenu :accessor is-submenu :initarg :is-submenu :documentation
   "whether use a submenu or expand in place")
  (item-list :accessor item-list :initarg :item-list :documentation
   "list of strings or (string value) pairs")
  (initial-value :accessor initial-value :initarg :initial-value :documentation
   "initial string or value")
  (button-resources :accessor button-resources :initarg :button-resources
   :documentation "motif-resources for buttons")
  (button-label-type :accessor button-label-type :initarg :button-label-type
   :documentation "label-type for buttons")
  (value :accessor value :initform nil :documentation "a subset of value-list")
  (value-list :accessor value-list :documentation
   "list of values extracted from item list")
  (toggle-buttons :accessor toggle-buttons :initform nil)
  (separators :accessor separators :initform nil)
  (menu-shell :accessor menu-shell :initform nil))) 
(defginaclass radio-group-entry (toggle-group-entry)
 (description "a radio button group in a menu" :application-subclasses :rarely
  :gina-subclasses nil)
 nil) 
(defginaclass menu-bar (widget)
 (description "a special kind of row-column widget" :application-subclasses
  :rarely :gina-subclasses nil)
 nil) 
(defginaclass option-menu (widget)
 (description "the Motif option menu widget" :application-subclasses :rarely
  :gina-subclasses nil)
 ((value :accessor value :initform nil :documentation
   "the current value from the value-list")
  (value-list :accessor value-list :initform nil :documentation
   "list of values extracted from the item list")
  (option-shell :accessor option-shell :documentation
   "the menu shell for the option menu pane")
  (button-box :accessor button-box :documentation "the option menu pane")
  (option-buttons :accessor option-buttons :initform nil :documentation
   "the list of buttons in the pane")
  (value-changed-callback :accessor value-changed-callback :initform nil))) 
(defginaclass menu-pulldown (widget)
 (description "the Motif row-column widget operating as a menu pane"
  :application-subclasses :rarely :gina-subclasses nil)
 nil) 
(defginaclass main-window (widget)
 (description "the Motif main-window widget" :application-subclasses :rarely
  :gina-subclasses nil)
 nil) 
(defginaclass scroller (widget)
 (description "the Motif scrolled-window widget" :application-subclasses
  :rarely :gina-subclasses nil)
 ((horizontal-scrollbar :accessor horizontal-scrollbar :initform nil
   :documentation "scroll bar object or nil (read-only)")
  (vertical-scrollbar :accessor vertical-scrollbar :initform nil :documentation
   "scroll bar object or nil (read-only)")
  (work-area :accessor work-area :initform nil :documentation
   "the work area child (read-only)")
  (clipper :accessor clipper :initform nil))) 
(defginaclass separator (widget)
 (description "the Motif separator widget" :application-subclasses :rarely
  :gina-subclasses nil)
 nil) 
(defginaclass frame (widget)
 (description "the Motif frame widget" :application-subclasses :rarely
  :gina-subclasses t)
 nil) 
(defginaclass paned-window (widget)
 (description "the Motif paned window widget" :application-subclasses :rarely
  :gina-subclasses nil)
 nil) 
(defginaclass row-column (widget)
 (description "the Motif row-column widget" :application-subclasses :rarely
  :gina-subclasses t)
 nil) 
(defginaclass labeled-frame (frame)
 (description "a frame with a label on top" :application-subclasses :rarely
  :gina-subclasses nil)
 ((column :accessor column :documentation "automatically created row-column")
  (label :accessor label :documentation "automatically created label-child"))) 
(defginaclass toggle-button-group (row-column)
 (description "a group of toggle buttons with optional label and frame"
  :application-subclasses :rarely :gina-subclasses t)
 ((button-resources :accessor button-resources :initarg :button-resources
   :documentation "motif-resources for buttons")
  (button-label-type :accessor button-label-type :initarg :button-label-type
   :documentation "label-type for buttons")
  (value :accessor value :initform nil :documentation "a subset of value-list")
  (value-list :accessor value-list :documentation
   "the list of values extracted from the item list")
  (value-changed-callback :accessor value-changed-callback :initform nil)
  (main-column :accessor main-column :initform nil :documentation
   "the enclosing row-column")
  (label :accessor label :initform nil :documentation "the title label widget")
  (frame :accessor frame :initform nil :documentation "the frame widget")
  (toggle-buttons :accessor toggle-buttons :initform nil :documentation
   "the list of toggle buttons")
  (separators :accessor separators :initform nil :documentation
   "the list of separators"))) 
(defginaclass radio-button-group (toggle-button-group)
 (description "a group of radio buttons with optional label and frame"
  :application-subclasses :rarely :gina-subclasses nil)
 nil) 
(defginaclass selection-list (widget)
 (description "the Motif List widget" :application-subclasses :rarely
  :gina-subclasses t)
 ((default-action-button :accessor default-action-button :initarg
   :default-action-button :documentation "button to activate on double-click")
  (selection-policy :accessor selection-policy :initarg :selection-policy
   :documentation ":single :multiple :browse :extended")
  (value :accessor value :initform nil :documentation
   "a single value or a subset of value-list")
  (value-list :accessor value-list :documentation
   "the list of values extracted from the item list")
  (string-list :accessor string-list :documentation
   "the list of strings extracted from the item list")
  (value-changed-callback :accessor value-changed-callback :initform nil)
  (default-action-callback :accessor default-action-callback :initform nil)
  (browse-selection-callback :accessor browse-selection-callback :initform nil)
  (single-selection-callback :accessor single-selection-callback :initform nil)
  (extended-selection-callback :accessor extended-selection-callback :initform
   nil)
  (multiple-selection-callback :accessor multiple-selection-callback :initform
   nil))) 
(defginaclass scrollable-selection-list (selection-list)
 (description "a selection-list inside a scroller" :application-subclasses
  :rarely :gina-subclasses nil)
 ((scroller :accessor scroller :documentation "surrounding the list"))) 
(defginaclass bulletin-board (widget)
 (description "the Motif bulletin board widget" :application-subclasses :rarely
  :gina-subclasses t)
 ((map-callback :accessor map-callback :initform nil)
  (unmap-callback :accessor unmap-callback :initform nil))) 
(defginaclass form (bulletin-board)
 (description "the Motif form widget" :application-subclasses :rarely
  :gina-subclasses t)
 nil) 
(defginaclass text (widget)
 (description "the Motif text widget" :application-subclasses :rarely
  :gina-subclasses t)
 ((value :accessor value :initarg :value :documentation
   "the current string value")
  (value-changed-callback :accessor value-changed-callback :initform nil)
  (modify-verify-callback :accessor modify-verify-callback :initform nil)
  (motion-verify-callback :accessor motion-verify-callback :initform nil)
  (losing-focus-callback :accessor losing-focus-callback :initform nil)
  (activate-callback :accessor activate-callback :initform nil))) 
(defginaclass scrolled-text (text)
 (description "a text widget inside a scroller" :application-subclasses :rarely
  :gina-subclasses nil)
 ((scroller :accessor scroller :documentation "surrounding the list"))) 
(defginaclass labeled-text (text)
 (description "a text widget with a label" :application-subclasses :rarely
  :gina-subclasses nil)
 ((label :accessor label :documentation "the label widget")
  (row-column :accessor row-column :documentation "the enclosing row-column")
  (label-position :accessor label-position :initarg :label-position
   :documentation ":top or :left"))) 
(defginaclass dialog-box (form)
 (description "a superclass for gina dialog boxes" :instantiate nil
  :gina-subclasses t)
 ((document :accessor document :initarg :document :documentation
   "the document associated with the dialog box")
  (dialog-shell :accessor dialog-shell :initarg :dialog-shell :documentation
   "the shell widget around the dialog")
  (relative-widget :accessor relative-widget :initform nil :documentation
   "a widget used for relative popup positioning")
  (relative-x :accessor relative-x :initform :center :documentation
   "one of :center :left :left-align :right-align :right")
  (relative-y :accessor relative-y :initform :center :documentation
   "one of :center :top :top-align :bottom-align :bottom")
  (result :accessor result :initform nil :documentation
   "value returned from modal dialog"))) 
(defginaclass modeless-dialog-box (dialog-box)
 (description "a form with a shell around it for modeless dialogs"
  :application-subclasses "for application-specific modeless dialogs"
  :gina-subclasses nil)
 ((default-button :accessor default-button :initform nil :documentation
   "the default button in the dialog, if any"))) 
(defginaclass modal-dialog-box (dialog-box)
 (description "a form with a shell around it for modal dialogs"
  :application-subclasses "for application-specific modal dialogs"
  :gina-subclasses nil)
 ((default-button :accessor default-button :initform nil :documentation
   "the default button in the dialog, if any"))) 
(defginaclass tool-dialog-box (dialog-box)
 (description "a form with a shell around it for tool dialogs with a menu"
  :application-subclasses :rarely :gina-subclasses nil)
 ((main-menu :accessor main-menu :documentation
   "the automatically created menu bar"))) 
(defginaclass dialog-dismiss-button (push-button)
 (description "a push-button to dismiss a dialog box" :application-subclasses
  :rarely :gina-subclasses nil)
 ((dialog-box :accessor dialog-box :initarg :dialog-box :documentation
   "modal dialog box containing the button")
  (result :accessor result :initarg :result :documentation
   "assigned to result-variable when button clicked"))) 
(defginaclass dialog-dismiss-button-row (row-column)
 (description "a row of dialog-dismiss-buttons with the leftmost as default"
  :application-subclasses :rarely :gina-subclasses nil)
 ((buttons :accessor buttons :documentation "the list of buttons"))) 
(defclass open-dialog-box (modal-dialog-box)
          ((file-type :accessor file-type :initarg :file-type)
           (directory-menu :accessor directory-menu)
           (file-list :accessor file-list))
          (:documentation "a modal dialog box to select an existing file")) 
(defclass save-dialog-box (modal-dialog-box)
          ((file-type :accessor file-type :initarg :file-type)
           (directory-menu :accessor directory-menu)
           (file-list :accessor file-list) (text-field :accessor text-field))
          (:documentation "a modal dialog box to select a new file")) 
(defclass inspector-box (modeless-dialog-box)
          ((path-text :accessor path-text) (class-label :accessor class-label)
           (object-list :accessor object-list) (slot-list :accessor slot-list)
           (button-row :accessor button-row)
           (inspect-button :accessor inspect-button)
           (inspect-button2 :accessor inspect-button2)
           (browser-button :accessor browser-button))
          (:documentation "a dialog box showing object contents")) 
(defginaclass view (widget)
 (description "a drawing area where document contents are shown"
  :application-subclasses :always :gina-subclasses
  "only in the demo applications" :instantiate "to make very simple tests")
 ((document :accessor document :initarg :document :documentation
   "the document shown in the view")
  (scroller :accessor scroller :initarg :scroller :documentation
   "the scroller containing the view, if any")
  (width :accessor width :initarg :width :documentation
   "width of the view in pixels")
  (height :accessor height :initarg :height :documentation
   "height of the view in pixels")
  (backing-store :accessor backing-store :initarg :backing-store :documentation
   "flag, whether backing-store is desired")
  (resize-callback :accessor resize-callback :initform nil)
  (destroy-callback :accessor destroy-callback :initform nil)
  (x-window :accessor x-window :initform nil :documentation
   "CLX-structure denoting the underlying window")
  (double-buffering :accessor double-buffering :initarg :double-buffering
   :initform nil)
  (pixmap-buffer :accessor pixmap-buffer)
  (background :accessor background :initform nil) (drawable :accessor drawable)
  (name-of-font :accessor name-of-font :initform "9x15" :documentation
   "name of default font for this view")
  (font :accessor font :documentation "default font for this view")
  (gcontext :accessor gcontext :initform nil :documentation
   "CLX graphics context used for drawing primitives")
  (printing :accessor printing :initform nil :documentation
   "flag, whether drawing is currently done for printing")
  (view-objects :accessor view-objects :initform nil :documentation
   "list of objects displayed in the view")
  (object-cache :accessor object-cache :initform nil)
  (old-view-objects :accessor old-view-objects :initform nil)
  (propagate-button-press :accessor propagate-button-press :initform t
   :documentation
   "flag, whether button-press is propagated to the view-objects")
  (repeated-click :accessor repeated-click :initform 1 :documentation
   "Number of very recent clicks")
  (last-click-time :accessor last-click-time :initform nil :documentation
   "Internal real-time of last button-press event")
  (double-click-interval :accessor double-click-interval :allocation :class
   :initform 250 :documentation
   "maximum time interval for double clicks in milliseconds"))) 
(defginaclass view-object nil
 (description "the superclass of all objects shown in views"
  :application-subclasses :always :gina-subclasses
  "objects which can be moved and resized with the mouse" :instantiate
  "for testing only")
 ((width :accessor width :initarg :width :documentation
   "extension of enclosing rectangle in pixels")
  (height :accessor height :initarg :height :documentation
   "extension of enclosing rectangle in pixels")
  (mouse-sensitive :accessor mouse-sensitive :initarg :mouse-sensitive
   :documentation "whether button-press is to be propagated to this object")
  (parent-view :accessor parent-view :initform nil :documentation
   "the view containing the object")
  (x-pos :accessor x-pos :initform nil :documentation
   "the x-coordinate relative to the containing view")
  (y-pos :accessor y-pos :initform nil :documentation
   "the y-coordinate relative to the containing view")
  (facilities :accessor facilities :initform nil :allocation :class
   :documentation
   "one-of nil, :selectable, :movable, :move-and-selectable, :resizable")
  (selected :reader selected :initform nil :documentation
   "flag whether object is currently selected")
  (outlines :accessor outlines :initform t :allocation :class :documentation
   "flag whether complete object is moved or just an outline")
  (handle-size :accessor handle-size :initform 5 :allocation :class
   :documentation "size of little rectangles indicating selected state"))) 
(defginamacro point-inside-rectangle (x y rect-x rect-y rect-width rect-height)
 (description "check if a given point lies within a certain rectangle")
 `(and (>= ,x ,rect-x) (>= ,y ,rect-y) (<= ,x (+ ,rect-x ,rect-width))
       (<= ,y (+ ,rect-y ,rect-height)))) 
(defginaclass direct-manipulation-object (view-object)
 (description "a view-object with direct-manipulation capabilities"
  :application-subclasses :always :gina-subclasses
  "movable icons and in the demo applications" :instantiate "for testing only")
 ((facilities :accessor facilities :initform :resizable :allocation :class
   :documentation
   "one-of nil, :selectable, :movable, :move-and-selectable, :resizable"))) 
(defginaclass movable-icon (direct-manipulation-object)
 (description "a direct manipulation-object displaying a pixmap"
  :application-subclasses :sometimes :gina-subclasses
  "only in the chess demo application" :instantiate t)
 ((icon-pixmap :accessor icon-pixmap :initform nil :documentation
   "Pixmap of icon on X-server side")
  (mask-pixmap :accessor mask-pixmap :initform nil :documentation
   "Pixmap of mask on X-server side")
  (icon-image :accessor icon-image :initarg :icon-image :documentation
   "Image structure of icon as read from file")
  (mask-image :accessor mask-image :initarg :mask-image :documentation
   "Image structure of mask as read from file")
  (shadow-offset :accessor shadow-offset :initarg :shadow-offset :documentation
   "Offset of shadow (0 = no shadow)")
  (left-margin :accessor left-margin :initarg :left-margin :documentation
   "Free space on left of icon")
  (top-margin :accessor top-margin :initarg :top-margin :documentation
   "Free space on top of icon")
  (facilities :allocation :class :initform :move-and-selectable :documentation
   "one-of nil, :selectable, :movable, :move-and-selectable, :resizable")
  (outlines :allocation :class :initform nil :documentation
   "flag whether complete (solid) icon is moved (default) or just xor the icon"))) 
(defginaclass callback nil
 (description "a data object describing an action to do later"
  :application-subclasses :rarely)
 ((function-name :accessor function-name :initarg :function-name :documentation
   "the function to be called")
  (static-args :accessor static-args :initarg :static-args :documentation
   "a list of arguments which will always be passed to the function"))) 
(defginaclass command nil
 (description "an object representing a single user action"
  :application-subclasses :always :gina-subclasses
  "class mouse-down-command and special commands for direct-manipulation"
  :instantiate nil)
 ((document :accessor document :initarg :document :documentation
   "the document changed by the command")
  (view :accessor view :initarg :view :documentation
   "the view which shows the effect of the command")
  (reverse-undo :accessor reverse-undo :initarg :reverse-undo)
  (name :accessor name :initform "Command" :allocation :class :documentation
   "identification shown in the history scroller")
  (clock-cursor :accessor clock-cursor :initform nil :allocation :class
   :documentation "flag whether clock cursor is shown during doit")
  (undoable :accessor undoable :initform t :allocation :class :documentation
   "flag whether an UNDO is possible")
  (causes-change :accessor causes-change :initform t :allocation :class
   :documentation "flag whether the command modifies the document")
  (view-x-offset :accessor view-x-offset :documentation
   "scroll position of the view when command was issued")
  (view-y-offset :accessor view-y-offset :documentation
   "scroll position of the view when command was issued")
  (checkpoint :accessor checkpoint :initform nil))) 
(defginaclass mouse-down-command (command)
 (description "a mouse command with feedback as long as the mouse is down"
  :application-subclasses :always :gina-subclasses
  "the special commands for direct-manipulation" :instantiate nil)
 ((start-x :accessor start-x :initarg :start-x :documentation
   "view coordinate where the mouse button went down")
  (start-y :accessor start-y :initarg :start-y :documentation
   "view coordinate where the mouse button went down")
  (hysteresis :accessor hysteresis :initform 0 :allocation :class
   :documentation "minimum mouse movement before command is submitted")
  (auto-scrolling :accessor auto-scrolling :initform t :allocation :class
   :documentation "flag whether automatic scrolling is desired")
  (idle-timeout :accessor idle-timeout :initform 0.25 :allocation :class
   :documentation
   "seconds before mouse-idle is called and auto-scrolling is done when mouse is not moved")
  (timer-id :accessor timer-id :initform nil :documentation
   "id for xtk:change-timer and friends")
  (call-doit :accessor call-doit :initform t :allocation :class :documentation
   "flag whether command should really be submitted")
  (name :initform "Mouse Command" :allocation :class :documentation
   "overrides slot of class command")
  (mouse-already-moved :accessor mouse-already-moved :initform nil
   :documentation "flag if hysteresis has been reached")
  (last-x :accessor last-x :initarg :last-x :documentation
   "view coordinate of last mouse position")
  (last-y :accessor last-y :initarg :last-y :documentation
   "view coordinate of last mouse position")
  (mouse-positions :accessor mouse-positions :initform nil))) 
(defginaclass object-mover (mouse-down-command)
 (description "a mouse-down-command to move a direct-manipulation-object")
 ((name :initform "Move Object" :allocation :instance :documentation
   "overrides default name of class mouse-down-command")
  (hysteresis :initform 5 :allocation :class :documentation
   "mouse must be moved at least 5 pixels to drag an object")
  (move-object :accessor move-object :initarg :move-object :documentation
   "the object being moved")
  (x-off :accessor x-off :initarg :x-off :documentation
   "point within object where mouse goes down")
  (y-off :accessor y-off :initarg :y-off :documentation
   "point within object where mouse goes down")
  (old-selection :accessor old-selection :initarg :old-selection :documentation
   "selected objects before moving")
  (new-selection :accessor new-selection :initform :ignore)
  (new-x :accessor new-x :documentation
   "view coordinates where object is moved to")
  (new-y :accessor new-y :documentation
   "view coordinates where object is moved to"))) 
(defginaclass object-resizer (mouse-down-command)
 (description "a mouse-down-command to resize a direct-manipulation-object")
 ((name :initform "Resize Object" :allocation :class :documentation
   "overrides default name of class mouse-down-command")
  (hysteresis :initform 2 :allocation :class :documentation
   "mouse must be moved at least 2 pixels to drag an object")
  (size-object :accessor size-object :initarg :size-object :documentation
   "the object being resized")
  (old-x :accessor old-x :initarg :old-x :documentation
   "original position of object being resized")
  (old-y :accessor old-y :initarg :old-y :documentation
   "original position of object being resized")
  (old-width :accessor old-width :initarg :old-width :documentation
   "original size of object being resized")
  (old-height :accessor old-height :initarg :old-height :documentation
   "original size of object being resized")
  (move-x :accessor move-x :initarg :move-x :documentation
   "flag whether x-coordinate of the objects position is changed")
  (move-y :accessor move-y :initarg :move-y :documentation
   "flag whether y-coordinate of the objects position is changed")
  (new-x :accessor new-x :documentation "new position of object being resized")
  (new-y :accessor new-y :documentation "new position of object being resized")
  (new-width :accessor new-width :documentation
   "new size of object being resized")
  (new-height :accessor new-height :documentation
   "new size of object being resized"))) 
(defginaclass object-selector (mouse-down-command)
 (description "a mouse-down-command to select direct-manipulation-objects")
 ((name :initform "Select Objects" :allocation :class :documentation
   "overrides default name of class mouse-down-command")
  (hysteresis :initform 0 :allocation :class :documentation
   "clicking deselects all")
  (undoable :initform nil :allocation :instance :initarg :undoable
   :documentation "no UNDO is possible (nor necessary) for this command")
  (causes-change :initform nil :allocation :class :documentation
   "this command does not modify the document")
  (extend :accessor extend :initarg :extend :documentation
   "button used (:select :extend :toggle)")
  (old-selection :accessor old-selection :initarg :old-selection)
  (new-selection :accessor new-selection))) 
(defginaclass single-selector (mouse-down-command)
 (description "a mouse-down-command to select a single object")
 ((name :initform "Select One" :allocation :class :documentation
   "overrides default name of class mouse-down-command")
  (hysteresis :initform 0 :allocation :class :documentation
   "clicking is sufficient")
  (undoable :initform nil :allocation :instance :initarg :undoable
   :documentation "no UNDO is possible (nor necessary) for this command")
  (causes-change :initform nil :allocation :class :documentation
   "this command does not modify the document")
  (extend :accessor extend :initarg :extend :documentation
   "button used (:select :extend :toggle)")
  (old-selection :accessor old-selection :initarg :old-selection)
  (selected-object :accessor selected-object :initarg :selected-object))) 
(defginaclass drag-command (command)
 (description "a command class to implement drag and drop across windows"
  :application-subclasses :always :gina-subclasses nil :instantiate nil)
 ((protocol-id :accessor protocol-id :initform :drag :allocation :class
   :documentation "a unique id string for inter-app communication")
  (hysteresis :accessor hysteresis :initform 0 :allocation :class
   :documentation "minimum mouse movement before command is submitted")
  (do-tracking :accessor do-tracking :initform t :allocation :class
   :documentation "whether continuous tracking is performed")
  (lazy-tracking :accessor lazy-tracking :initform nil :allocation :class
   :documentation "whether track-target is called only once per target")
  (call-doit :accessor call-doit :initform t :allocation :class :documentation
   "flag whether command should really be submitted")
  (target-list :accessor target-list :initarg :target-list :documentation
   "a list of window-widget-value tripels of poss. targets")
  (shell-to-move :accessor shell-to-move :initarg :shell-to-move :documentation
   "an override shell to be moved as feedback")
  (received-from :accessor received-from :initarg :received-from :documentation
   "nil or an x-window from another document")
  (mouse-already-moved :accessor mouse-already-moved :initform nil
   :documentation "flag if hysteresis has been reached")
  (source-x :accessor source-x :initarg :source-x :documentation
   "the starting x position relative to the source")
  (source-y :accessor source-y :initarg :source-y :documentation
   "the starting y position relative to the source")
  (last-x :accessor last-x :initarg :last-x)
  (last-y :accessor last-y :initarg :last-y)
  (target-x :accessor target-x :documentation
   "the current x position relative to the target")
  (target-y :accessor target-y :documentation
   "the current y position relative to the target")
  (current-target :accessor current-target :initform nil :documentation
   "the current target, if any")
  (current-value :accessor current-value :initform nil :documentation
   "the current value, if any")
  (transfer-value :accessor transfer-value :initarg :transfer-value
   :documentation "a value returned from the target application")
  (global-x-off :accessor global-x-off) (global-y-off :accessor global-y-off)
  (x-off :accessor x-off :initarg :x-off)
  (y-off :accessor y-off :initarg :y-off)
  (target-positions :accessor target-positions :initarg :target-positions)
  (target-shells :accessor target-shells :initform nil)
  (foreign-shells :accessor foreign-shells :initform nil)
  (failed-shells :accessor failed-shells :initarg :failed-shells))) 
(defginamacro in-background-process
 ((document &key (terminate-on-error t)) &body body)
 (description "execute body in a background process" :called-by-gina nil
  :called-by-application :sometimes)
 (comment "It returns the background process created.")
 (comment "See also method kill-background-process of class document")
 (example
  (with-application-stopped
   (in-background-process ((first (document-list *application*)))
    (replay-history (first (document-list *application*))))))
 (let ((application-var (gensym))
       (display-var (gensym))
       (motif-connection-var (gensym)))
   `(xtk::process-run-function
      (format nil "~a on ~a Background" (name *application*)
              (display-host *application*))
      #'(lambda (,application-var ,display-var ,motif-connection-var)
          (let ((*application* ,application-var)
                (*display* ,display-var)
                (xtk:*x-display* ,display-var)
                (xtk:*motif-connection* ,motif-connection-var))
            (declare (special *application* *display* xtk:*x-display*
                      xtk:*motif-connection*))
            (unwind-protect
                (progn (with-document (,document)
                        (push (xtk::current-process)
                              (background-processes ,document)))
                       ,(if terminate-on-error
                            `(catch 'xtk:proceed-from-error
                               (handler-bind ((error #'error-handler))
                                 ,(cons 'progn body)))
                          (cons 'progn body)))
              (with-document (,document)
               (setf (background-processes ,document)
                     (remove (xtk::current-process)
                             (background-processes ,document))))
              (xlib:display-force-output *display*))))
      *application* *display* xtk:*motif-connection*))) 
(defginamacro with-application (&body body)
 (description "execute body with exclusive access to the application"
  :called-by-gina t :called-by-application :sometimes)
 (example (with-application (print (eval (semaphor *application*)))))
 `(xtk::with-process-lock ((semaphor *application*)) ,@body)) 
(defginamacro with-document ((document) &body body)
 (description "execute body with exclusive access to the document"
  :called-by-gina t :called-by-application :sometimes)
 (example
  (with-document ((first (document-list *application*))) (print "Hallo")))
 `(xtk::with-process-lock ((semaphor ,document)) ,@body)) 
(defginamacro with-clm-connection (&body body)
 (description "prevent clm calls by other processes for body" :called-by-gina t
  :called-by-application :sometimes)
 `(xtk::with-process-lock
    ((xtk::toolkit-connection-lock xtk:*motif-connection*)) ,@body)) 
(defginamacro with-progress-bar
 ((document &key (modal t) (title "Progress Bar")
   (message "Performing your request ...") (abortable t) (kill-on-abort nil)
   (centered t) (with-scale t))
  &body body)
 (description "execute body while displaying a progress bar" :called-by-gina
  nil :called-by-application :sometimes)
 (comment "modal: whether all other input is blocked")
 (comment "title: title string of the dialog box")
 (comment "message: message displayed in the box")
 (comment "abortable: whether user can abort the action")
 (comment "kill-on-abort: whether process is killed or abort flag is set only")
 (comment "centered: whether dialog box is centered over the document")
 (comment "with-scale: whether a scale indicating progress is displayed")
 (example
  (with-application-stopped
   (with-progress-bar
    ((first (document-list *application*)) :kill-on-abort t :centered nil)
    (loop for i from 1 to 100 do (sleep 0.1)
          (indicate-progress i :new-message
           (format nil "~d more steps to go ..." (- 100 i)))))))
 (example
  (with-application-stopped
   (with-progress-bar
    ((first (document-list *application*)) :kill-on-abort nil)
    (loop for i from 1 to 10 until (progress-bar-aborted) do (sleep 1)
          (indicate-progress (* 10 i))))))
 (let ((box-variable (gensym)))
   `(let ((,box-variable
           (make-progress-bar-box ,document ,title ,message :abortable
            ,abortable :kill-on-abort ,(and kill-on-abort abortable) :modal
            ,modal :centered ,centered :with-scale ,with-scale)))
      (pop-up ,box-variable)
      (setf (background-process ,box-variable)
            (in-background-process (,document)
             (let ((*progress-bar* ,box-variable))
               (declare (special *progress-bar*))
               (unwind-protect
                   ,(cons 'progn body)
                 (pop-down *progress-bar*)
                 (destroy *progress-bar*)))))))) 
