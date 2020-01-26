;;; -*- Mode:LISP;Syntax: Common-Lisp;Package: ib ;Base:10-*-

;;; Copyright 1990 GMD

(in-package :ib)

(defclass ib-application (application)
  (;; overrides
   (name                 :initform "Interface Builder"   :allocation :class)
   (document-type        :initform 'ib-document          :allocation :class)
   (signature            :initform "ibuild"              :allocation :class)
   (file-type            :initform "ibuild"              :allocation :class)
   ;(inspect-click        :initform nil :allocation :class)
   ;; instance variable
   (palette-shell        :accessor palette-shell       :initform nil))
  (:documentation "the interface builder application"))

(defclass ib-document (document)
  (;; instance variables
   (class-counters :accessor class-counters :initform nil)
   (main-info      :accessor main-info)      
   (lisp-details   :accessor lisp-details  :initform (make-lisp-code-details))
   (cplus-details  :accessor cplus-details :initform (make-cplus-code-details))
   ;; overrides
   (drag-protocols :accessor drag-protocols :allocation :class
                         :initform '(:widget_drag))
   (shell-width    :initform 700)
   (shell-height   :initform 550))
  (:documentation "Interface Builder document type"))

(defclass general-code-details ()
  ((detail-name    :reader detail-name      :initform :unspecified-code-details
                   :allocation :class)
   (code-file      :accessor code-file      :initform "ib-demo")
   (active         :accessor active         :initform nil))
  (:documentation "information nedded for all code generators"))

(defclass class-counter ()
  ((widget-class  :accessor widget-class   :initarg :widget-class)
   (next          :accessor next           :initform 0))
  (:documentation "Next number to use for a certain widget class"))

(defclass widget-info ()
  ((document     :accessor document     :initarg :document)
   (children     :accessor children     :initform nil)
   (parent-info  :accessor parent-info  :initarg :parent-info :initform nil)
   ;
   (x-pos                :accessor x-pos        :initform nil)
   (y-pos                :accessor y-pos        :initform nil)
   (width                :accessor width        :initform nil)
   (height               :accessor height       :initform nil)
   ;
   (plate                :accessor plate        :initform nil)
   (res-dialog           :accessor res-dialog   :initform nil)
   (resource-values      :accessor resource-values :initform nil)
   (resource-widgets     :accessor resource-widgets :initform nil)
   (constraints          :accessor constraints :initform nil)
   ;
   ; The following slots have :allocation :class and must be overridden in
   ; the subclasses
   ;
   (res-title            :accessor res-title
			 :initform "Attributes" :allocation :class)
   (create-function      :accessor create-function
			 :initform nil :allocation :class)
   (positional-names     :accessor positional-names
			 :initform nil :allocation :class)
   (key-names            :accessor key-names
			 :initform nil :allocation :class)
   (required-key-names   :accessor required-key-names
			 :initform nil :allocation :class)
   (important-names      :accessor important-names
                         :initform nil :allocation :class)
   (callback-names       :accessor callback-names
                         :initform nil :allocation :class)
   (num-widgets          :accessor num-widgets
			 :initform 0 :allocation :class)
   (widget-class         :accessor widget-class
			 :initform 'widget :allocation :class)
   ;
   ; The following slots have :allocation :class and are initialized in
   ; the constructor function
   ;
   (all-resources        :accessor all-resources
			 :initform nil :allocation :class)
   (num-resources        :accessor num-resources
			 :initform 0 :allocation :class)
   (key-resources        :accessor key-resources
			 :allocation :class :initform nil)
   (positional-resources :accessor positional-resources
			 :allocation :class :initform nil)
   (motif-resources      :accessor motif-resources
			 :allocation :class :initform nil))
  (:documentation "A widget object in the Interface Builder"))

(defclass motif-resource ()
  ((resource-name :accessor resource-name :initarg :resource-name)
   (default-value :accessor default-value :initarg :default-value )
   (access-mode   :accessor access-mode   :initarg :access-mode)
   (print-length  :accessor print-length  :initform 20 :allocation :class)
   (table-index   :accessor table-index)
   (in-output     :accessor in-output     :initform nil))
  (:documentation "A resource widget"))

(defclass boolean-resource (motif-resource)
  ()
  (:documentation "A boolean resource"))

(defclass item-list-resource (motif-resource)
  ()
  (:documentation "A list of items"))

(defclass color-resource (motif-resource)
  ((color-wheel   :accessor color-wheel :initform nil))
  (:documentation "A color resource"))

(defclass text-resource (motif-resource)
  ((columns :accessor columns :initform 20 :initarg :columns))
  (:documentation "A text resource"))

(defclass immediate-text-resource (text-resource)
  ((columns :accessor columns :initform 20 :initarg :columns))
  (:documentation "A text resource"))

(defclass numeric-resource (text-resource)
  ()
  (:documentation "A numeric resource"))

(defclass limited-numeric-resource (motif-resource)
  ((minimum :accessor minimum :initform 0 :initarg :minimum)
   (maximum :accessor maximum :initform 100 :initarg :maximum))
  (:documentation "A limited numeric resource"))

(defclass enumerative-resource (motif-resource)
  ((possible-values :accessor possible-values :initarg :possible-values)
   (cplus-prefix    :accessor cplus-prefix  :initarg :cplus-prefix))
  (:documentation "An enumerative resource"))

(defclass callback-resource (motif-resource)
  ((default-params :accessor default-params :initarg :default-params))
  (:documentation "A callback"))

(defclass attachment ()
  ((atype     :accessor atype      :initform :none)
   (offset    :accessor offset     :initform 0)
   (form-pos  :accessor form-pos   :initform 0)
   (widget    :accessor widget     :initform nil))
  (:documentation "Attachments for one direction"))

(defclass ib-constraints ()
  ((constraint-name   :accessor constraint-name   :initform "General"
                      :allocation :class))
  (:documentation "Superclass for widget constraints"))

(defclass ib-form-constraints (ib-constraints)
  ((constraint-name   :accessor constraint-name   :initform "Form"
                      :allocation :class)
   (bottom   :accessor bottom    :initform   (make-attachment))
   (left     :accessor left      :initform   (make-attachment))
   (right    :accessor right     :initform   (make-attachment))
   (top      :accessor top       :initform   (make-attachment)))
  (:documentation "Form constraints for a widget in the interface builder"))

(defclass ib-pane-constraints (ib-constraints)
  ((constraint-name  :accessor constraint-name  :initform "Paned Window"
                     :allocation :class)
   (minimum          :accessor minimum          :initform 1)
   (maximum          :accessor maximum          :initform 1000)
   (skip-adjust      :accessor skip-adjust      :initform nil)
   (allow-resize     :accessor allow-resize     :initform nil))
  (:documentation "Pane constraints for a widget in the interface builder"))

(defclass core-info (widget-info)
  ()
  (:documentation ""))

(defclass primitive-info (core-info)
  ()
  (:documentation ""))

(defclass separator-info (primitive-info)
  ((create-function :initform 'make-separator         :allocation :class)
   (widget-class    :initform 'separator :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title       :initform "Separator Attributes"  :allocation :class)
   (key-names       :initform '(:orientation :margin :separator-type) 
                                                      :allocation :class)
   (positional-names :allocation :class)
   (all-resources :allocation :class)
   (num-resources :allocation :class)
   (key-resources :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources :allocation :class))
  (:documentation "a widget-info holding a separator"))

(defclass label-info (primitive-info)
  ((create-function  :initform 'make-label        :allocation :class)
   (widget-class    :initform 'label :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title        :initform "Label Attributes" :allocation :class)
   (positional-names :initform '(:label-string)   :allocation :class)
   (key-names        :initform '(:alignment :label-type :recompute-size
				 :sensitive)      :allocation :class)
 ;(important-names  :initform '(:background)    :allocation :class)
   (all-resources :allocation :class)
   (num-resources :allocation :class)
   (key-resources :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources :allocation :class))
  (:documentation "a widget-info holding a label"))

(defclass push-button-info (label-info)
  ((create-function  :initform 'make-push-button        :allocation :class)
   (widget-class    :initform 'push-button :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title        :initform "Push-Button Attributes" :allocation :class)
   (positional-names :initform '(:label-string)         :allocation :class)
   (key-names        :initform '(:alignment :label-type :show-as-default
				:recompute-size :sensitive)
		     :allocation :class)
   (callback-names   :initform '(:activate-callback)    :allocation :class)
   (all-resources :allocation :class)
   (num-resources :allocation :class)
   (key-resources :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources :allocation :class))
  (:documentation "a push-button with additional behaviour for ib"))

(defclass toggle-button-info (label-info)
  ((create-function  :initform 'make-toggle-button        :allocation :class)
   (widget-class    :initform 'toggle-button              :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title        :initform "Toggle-Button Attributes" :allocation :class)
   (positional-names :initform '(:label-string)           :allocation :class)
   (key-names        :initform '(:indicator-type :label-type :indicator-on
			 	 :spacing :alignment :recompute-size 
                                 :sensitive)              :allocation :class)
   (callback-names   :initform '(:value-changed-callback) :allocation :class)
   (all-resources :allocation :class)
   (num-resources :allocation :class)
   (key-resources :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources :allocation :class))
  (:documentation "a toggle-button with additional behaviour for ib"))

(defclass text-info (primitive-info)
  ((create-function :initform 'make-text        :allocation :class)
   (widget-class    :initform 'text :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title       :initform "Text Attributes" :allocation :class)
   (key-names       :initform '(:value :columns :rows :edit-mode :editable)
		    :allocation :class)
   (positional-names :allocation :class)
   (important-names :initform '(:margin-width :margin-height)
                    :allocation :class)
   (callback-names :initform '(:value-changed-callback) :allocation :class)
   (all-resources :allocation :class)
   (num-resources :allocation :class)
   (key-resources :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources :allocation :class))
  (:documentation "a widget-info holding a text widget"))

(defclass scrolled-text-info (text-info)
  ((create-function :initform 'make-scrolled-text        :allocation :class)
   (widget-class    :initform 'scrolled-text :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title       :initform "Scrolled Text Attributes" :allocation :class)
   (key-names       :initform '(:value :columns :rows :editable
                                :scroll-vertical :scroll-horizontal)
		    :allocation :class)
   (positional-names :allocation :class)
   (important-names :initform '(:margin-width :margin-height)
                    :allocation :class)
   (callback-names :initform '(:value-changed-callback) :allocation :class)
   (all-resources :allocation :class)
   (num-resources :allocation :class)
   (key-resources :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources :allocation :class))
  (:documentation "a widget-info holding a scrolled text widget"))

(defclass labeled-text-info (text-info)
  ((create-function  :initform 'make-labeled-text        :allocation :class)
   (widget-class    :initform  'labeled-text :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title        :initform "Labeled-text Attributes" :allocation :class)
   (positional-names :initform '(:label-string)         :allocation :class)
   (key-names        :initform '(:value :editable :columns :edit-mode
				 :label-position) :allocation :class)
   (all-resources :allocation :class)
   (num-resources :allocation :class)
   (key-resources :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources :allocation :class))
  (:documentation "a widget-info holding a labeled-text"))

(defclass scrollbar-info (primitive-info)
  ((create-function :initform 'make-scrollbar :allocation :class)
   (widget-class    :initform 'scrollbar :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title       :initform "Scrollbar Attributes" :allocation :class)
   (key-names       :initform '(:orientation :minimum :maximum :value
				:increment :page-increment :slider-size)
		    :allocation :class)
   (positional-names :allocation :class)
   (callback-names  :initform '(:value-changed-callback :drag-callback)
                    :allocation :class)
   (all-resources :allocation :class)
   (num-resources :allocation :class)
   (key-resources :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources :allocation :class))
  (:documentation "a widget-info holding a scrollbar"))

(defclass manager-info (core-info)
  ()
  (:documentation ""))

(defclass scale-info (manager-info)
  ((create-function :initform 'make-scale :allocation :class)
   (widget-class    :initform 'scale :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title       :initform "Scale Attributes" :allocation :class)
   (key-names       :initform '(:orientation :processing-direction
				:title-string :sensitive
				:minimum :maximum :value :show-value)
		    :allocation :class)
   (positional-names :allocation :class)
   (callback-names  :initform '(:value-changed-callback :drag-callback)
                    :allocation :class)
   (all-resources :allocation :class)
   (num-resources :allocation :class)
   (key-resources :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources :allocation :class))
  (:documentation "a widget-info holding a scale"))

(defclass bulletin-board-info (manager-info)
  ()
  (:documentation ""))

(defclass form-info (bulletin-board-info)
  ((create-function :initform 'make-form        :allocation :class)
   (widget-class    :initform 'form :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title       :initform "Form Attributes" :allocation :class)
   (positional-names :allocation :class)
   (key-names :allocation :class)
   ;(important-names :allocation :class 
   ;                 :initform '(:margin-width :margin-height))
   (all-resources :allocation :class)
   (num-resources :allocation :class)
   (key-resources :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources :allocation :class))
  (:documentation "a form with additional behaviour for ib"))

(defclass row-column-info (manager-info)
  ((create-function :initform 'make-row-column             :allocation :class)
   (widget-class    :initform 'row-column :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title       :initform "Row-Column Attributes"      :allocation :class)
   (key-names       :initform '(:orientation :num-columns :spacing 
                                :margin-width :margin-height :entry-alignment)
                              :allocation :class)
   (important-names :initform '(:adjust-last :adjust-margin
                                :resize-width :resize-height)
	                      :allocation :class)
   (positional-names :allocation :class)
   (all-resources :allocation :class)
   (num-resources :allocation :class)
   (key-resources :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources :allocation :class))
  (:documentation "a row-column with additional behaviour for ib"))

(defclass frame-info (manager-info)
  ((create-function :initform 'make-frame        :allocation :class)
   (widget-class    :initform 'frame :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title       :initform "Frame Attributes" :allocation :class)
   (key-names       :initform '(:margin-width :margin-height :shadow-type)
		    :allocation :class)
   (positional-names :allocation :class)
   (all-resources :allocation :class)
   (num-resources :allocation :class)
   (key-resources :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources :allocation :class))
  (:documentation "a frame with additional behaviour for Interface Builder"))

(defclass option-menu-info (manager-info)
  ((create-function    :initform 'make-option-menu        :allocation :class)
   (widget-class    :initform 'option-menu :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title          :initform "Option-menu Attributes" :allocation :class)
   (positional-names   :initform '(:item-list)            :allocation :class)
   (key-names          :initform '(:label-string)         :allocation :class)
   (required-key-names :initform '(:label-string)         :allocation :class)
   (callback-names     :initform '(:value-changed-callback) :allocation :class)
   (all-resources :allocation :class)
   (num-resources :allocation :class)
   (key-resources :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources :allocation :class)
   ;; new slot for use in templates
   (template-children :accessor template-children :initform nil))
  (:documentation "a widget-info holding an option-menu"))

(defclass selection-list-info (primitive-info)
  ((create-function  :initform 'make-selection-list        :allocation :class)
   (widget-class    :initform 'selection-list :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title        :initform "Selection list Attributes" :allocation :class)
   (positional-names :initform '(:item-list)               :allocation :class)
   (key-names        :initform '( :visible-item-count
				 :selection-policy)        :allocation :class)
   (required-key-names :initform nil         :allocation :class)
   (callback-names     :initform '(:value-changed-callback) :allocation :class)
   (all-resources        :allocation :class)
   (num-resources        :allocation :class)
   (key-resources        :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources      :allocation :class))
  (:documentation "a widget-info holding a selection-list"))

(defclass scrollable-selection-list-info (selection-list-info)
  ((create-function  :initform 'make-scrollable-selection-list
		     :allocation :class)
   (widget-class    :initform 'scrollable-selection-list :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title        :initform "Scrollable selection list Attributes"
		     :allocation :class)
   (positional-names :initform '(:item-list)
		     :allocation :class)
   (key-names        :initform '(:visible-item-count :selection-policy)
		     :allocation :class)
   (required-key-names :initform '(:scroll-bar-display-policy)
                       :allocation :class)
   (important-names  :initform '(:scroll-bar-display-policy)
                     :allocation :class)
   (all-resources        :allocation :class)
   (num-resources        :allocation :class)
   (key-resources        :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources      :allocation :class))
  (:documentation "a widget-info holding a scrollable-selection-list"))

(defclass scroller-info (manager-info)
  ((create-function :initform 'make-scroller        :allocation :class)
   (widget-class    :initform 'scroller :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title       :initform "Scroller-Attributes" :allocation :class)
   (positional-names :allocation :class)
   (key-names :allocation :class)
   (all-resources :allocation :class)
   (num-resources :allocation :class)
   (key-resources :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources :allocation :class))
  (:documentation "a scroller with additional behaviour for ib"))

(defclass paned-window-info (manager-info)
  (
   (create-function :initform 'make-paned-window        :allocation :class)
   (widget-class    :initform 'paned-window :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title       :initform "Paned Window Attributes" :allocation :class)
   (positional-names :allocation :class)
   (key-names :allocation :class)
   (all-resources :allocation :class)
   (num-resources :allocation :class)
   (key-resources :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources :allocation :class))
  (:documentation "a paned-window with additional behaviour for ib"))

(defclass toggle-button-group-info (row-column-info)
  ((create-function  :initform 'make-toggle-button-group    :allocation :class)
   (widget-class    :initform 'toggle-button-group :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title        :initform "Toggle Button Group Attributes"
		     :allocation :class)
   (positional-names :initform '(:item-list)               :allocation :class)
   (key-names        :initform '(:label-string :button-label-type :with-frame
				 :shadow-type :spacing :orientation)
		     :allocation :class)
   (required-key-names :initform '(:label-string)         :allocation :class)
   (callback-names     :initform '(:value-changed-callback) :allocation :class)
   (all-resources :allocation :class)
   (num-resources :allocation :class)
   (key-resources :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources :allocation :class))
  (:documentation "a widget-info holding a toggle-button-group"))

(defclass radio-button-group-info (toggle-button-group-info)
  ((create-function  :initform 'make-radio-button-group    :allocation :class)
   (widget-class    :initform 'radio-button-group :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title        :initform "Radio Button Group Attributes"
		     :allocation :class)
   (positional-names :initform '(:item-list)               :allocation :class)
   (key-names        :initform '(:label-string :button-label-type :with-frame 
				 :shadow-type :spacing :orientation)
		     :allocation :class)
   (required-key-names :initform '(:label-string)         :allocation :class)
   (all-resources :allocation :class)
   (num-resources :allocation :class)
   (key-resources :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources :allocation :class))
  (:documentation "a widget-info holding a radio-button-group"))

(defclass modeless-dialog-box-info (form-info)
  ((create-function  :initform 'make-modeless-dialog-box  :allocation :class)
   (widget-class    :initform 'modeless-dialog-box        :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title        :initform "Modeless Dialog Attributes" :allocation :class)
   (positional-names :initform '(:title)                  :allocation :class)
   (key-names        :initform '(:class :resize :return-is-accelerator
                                 :allow-shell-resize)     :allocation :class)
   (all-resources :allocation :class)
   (num-resources :allocation :class)
   (key-resources :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources :allocation :class))
  (:documentation "a modeless-dialog-box with additional behaviour for ib"))

(defclass modal-dialog-box-info (form-info)
  ((create-function  :initform 'make-modal-dialog-box  :allocation :class)
   (widget-class    :initform 'modal-dialog-box        :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title        :initform "Modal Dialog Attributes" :allocation :class)
   (positional-names :initform '(:title)                  :allocation :class)
   (key-names        :initform '(:class :resize :return-is-accelerator
                                 :allow-shell-resize)     :allocation :class)
   (all-resources :allocation :class)
   (num-resources :allocation :class)
   (key-resources :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources :allocation :class))
  (:documentation "a modal-dialog-box with additional behaviour for ib"))

(defclass tool-dialog-box-info (form-info)
  ((create-function  :initform 'make-tool-dialog-box      :allocation :class)
   (widget-class    :initform 'tool-dialog-box            :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title        :initform "Tool Dialog Attributes"   :allocation :class)
   (positional-names :initform '(:title)                  :allocation :class)
   (key-names        :initform '(:class :resize :return-is-accelerator
                                 :allow-shell-resize)     :allocation :class)
   (all-resources :allocation :class)
   (num-resources :allocation :class)
   (key-resources :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources :allocation :class))
  (:documentation "a tool-dialog-box with additional behaviour for ib"))

(defclass document-shell-info (core-info)
  ((create-function  :initform 'make-document-shell       :allocation :class)
   (widget-class    :initform 'document-shell             :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title        :initform "Document Shell Attributes" :allocation :class)
   (positional-names :initform '()                         :allocation :class)
   (key-names        :initform '(:class :is-main-shell
                                 :with-menu)               :allocation :class)
   (all-resources :allocation :class)
   (num-resources :allocation :class)
   (key-resources :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources :allocation :class))
  (:documentation "a document-shell with additional behaviour for ib"))

(defclass view-info (manager-info)
  ((create-function  :initform 'make-prototype-view       :allocation :class)
   (widget-class    :initform 'view                       :allocation :class)
   (num-widgets     :initform 0 :allocation :class)
   (res-title        :initform "View Attributes"          :allocation :class)
   (positional-names :initform '()                        :allocation :class)
   (key-names        :initform '(:class :needs-doc)       :allocation :class)
   (all-resources :allocation :class)
   (num-resources :allocation :class)
   (key-resources :allocation :class)
   (positional-resources :allocation :class)
   (motif-resources :allocation :class))
  (:documentation "a view with additional behaviour for ib"))

(defclass ib-view (view)
  ((main-plate   :accessor main-plate    :initform nil)
   (hack-init :accessor hack-init :initform nil))
  (:documentation "a view with special reaction to clicks"))

(defclass prototype-view (view)
  ()
  (:documentation "a view to use in the IB"))

(defclass widget-plate (view-object)
  (;; overrides in the future
   ;(facilities    :initform :resizable    :allocation :class)
   ;(outlines      :initform nil           :allocation :class)
   ;; new slots
   (info          :accessor info          :initarg :info)
   (gina-widget   :accessor gina-widget   :initform nil)
   (symbolic-view :accessor symbolic-view :initform nil)
   (is-symbolic   :accessor is-symbolic   :initform nil)
   (click-time    :accessor click-time    :initform 0))
  (:documentation ""))

(defclass clear-command (command)
  (;; overrides
   (name :initform "Delete Widgets" :allocation :class)
   ;; instance-parameters
   (selected-plates :accessor selected-plates :initarg :selected-plates))
  (:documentation "a command to delete all selected widget-plates and widgets"))

(defclass clear-all-command (clear-command)
  (;; overrides
   (name :initform "Delete All" :allocation :class))
  (:documentation "a command to delete all selected-plates and widgets"))

(defclass dialog-type-command (command)
  (;; overrides
   (name :initform "Change Dialog Type" :allocation :class)
   ;; instance-parameters
   (old-info  :accessor old-info :initarg :old-info)
   (old-type  :accessor old-type :initarg :old-type)
   (new-type  :accessor new-type :initarg :new-type))
  (:documentation "a command to change the dialog-type of the document"))

(defclass change-resource-command (command)
  (;; overrides
   ;; name is instance-specific
   (name        :initarg :name       :allocation :instance)
   ;; instance-parameters
   (info        :accessor info       :initarg :info)
   (resource    :accessor resource   :initarg :resource)
   (new-value   :accessor new-value  :initarg :new-value)
   (old-value   :accessor old-value  :initform nil))
  (:documentation "a command to change resource values"))

(defclass change-multi-resource-command (change-resource-command)
  ())

(defclass prog-lang-command (command)
  (;; overrides
   (name :initform "Change Save Language" :allocation :class)
   ;; instance-parameters
   (new-lang-is-lisp  :accessor new-lang-is-lisp :initarg :new-lang-is-lisp))
  (:documentation "a command to change the save language of the document"))

(defclass change-package-command (command)
  (;; overrides
   (name        :initform "Change Package"  :allocation :class)
   ;; instance-parameters
   (new-value   :accessor new-value  :initarg :new-value)
   (old-value   :accessor old-value  :initarg :old-value))
  (:documentation "a command to change the code package"))

(defclass change-doc-class-command (command)
  (;; overrides
   (name        :initform "Change Document Class"  :allocation :class)
   ;; instance-parameters
   (new-value   :accessor new-value  :initarg :new-value)
   (old-value   :accessor old-value  :initarg :old-value))
  (:documentation "a command to change the C++ doc-class"))

(defclass widget-selector (mouse-down-command)
  (;; overrides
   (name       :initform "Select Widget" :allocation :class)
   (undoable      :accessor undoable       :initform nil :allocation :class)
   (causes-change :accessor causes-change  :initform nil :allocation :class)
   ;; new
   (extend        :accessor extend         :initarg :extend))
  (:documentation "a mouse-down-command to select one or more widgets"))

(defclass widget-resizer (mouse-down-command)
  (;; overrides
   (name       :initform "Resize Widget"  :allocation :class)
   (hysteresis :initform 5                :allocation :class)
   ;; instance-parameters
   (widget-plate :accessor widget-plate :initarg :widget-plate)
   (old-width  :accessor old-width  :initarg :old-width)
   (old-height :accessor old-height :initarg :old-height)
   ;; instance-variables
   (new-width :accessor new-width)
   (new-height :accessor new-height))
  (:documentation "a mouse-down-command to resize a widget-plate"))

(defclass widget-mover (mouse-down-command)
  (;; overrides
   (name       :initform "Move Widgets" :allocation :class)
   (hysteresis :initform 5                :allocation :class)
   ;; instance-parameters
   (widget-plates :accessor widget-plates :initarg :widget-plates)
  )
  (:documentation "a mouse-down-command to move a widget-plate around"))

(defclass one-element-group-command (command)
  ((selected-plates :accessor selected-plates :initarg :selected-plates)
   (group-plates    :accessor group-plates    :initform nil))
  (:documentation "a command to group with one-element managers"))

(defclass frame-group-command (one-element-group-command)
  (;;overrides
   (name         :initform "Make Frame"      :allocation :class))
  (:documentation "a command to group with a frame"))

(defclass scroller-group-command (one-element-group-command)
  (;;overrides
   (name         :initform "Make Scroller"      :allocation :class))
  (:documentation "a command to group with a frame"))

(defclass n-element-group-command (one-element-group-command)
  ()
  (:documentation "a command to group with n-element managers"))

(defclass row-column-group-command (n-element-group-command)
  (;;overrides
   (name         :initform "Make Row-Column"      :allocation :class)
   ;;new
   (orientation  :accessor orientation            :initarg :orientation))
  (:documentation "a command to group with a row-column"))

(defclass form-group-command (n-element-group-command)
  (;;overrides
   (name         :initform "Make Form"      :allocation :class)
   ;;new slots
   (child-rows    :accessor child-rows)
   (child-columns :accessor child-columns))
  (:documentation "a command to group with a form"))

(defclass paned-window-group-command (n-element-group-command)
  (;;overrides
   (name         :initform "Make Paned-Window"      :allocation :class))
  (:documentation "a command to group with a paned-window"))

(defclass ungroup-command (one-element-group-command)
  (;; overrides
   (name :initform "Ungroup Widgets" :allocation :class))
  (:documentation "a command to ungroup a compound widget"))

(defclass lisp-code-details (general-code-details)
  ((detail-name    :reader detail-name      :initform :lisp-code-details
                   :allocation :class)
   (what-package   :accessor what-package   :initform "ib-demo"))
  (:documentation "contains information needed to generate lisp code"))

(defclass cplus-code-details (general-code-details)
  ((detail-name    :reader detail-name      :initform :cplus-code-details
                   :allocation :class)
   (header-name    :accessor header-name    :initform "GnNewDialog")
   (doc-class      :accessor doc-class      :initform "GnDocument"))
  (:documentation "contains information needed to generate C++ code"))

(defclass ib-main-window (document-shell)
  ((view          :accessor view)
   (class-option  :accessor class-option)
   (temp          :accessor temp          :initform nil)
   ;(palette       :accessor palette)
   (scroller      :accessor scroller)
   (grab-row      :accessor grab-row)
   (remove-grab   :accessor remove-grab)
   (delete-grab   :accessor delete-grab)
   (edit-grab     :accessor edit-grab)
   (coder-tool    :accessor coder-tool     :initform nil)
   (tree-tool     :accessor tree-tool      :initform nil)
   (preferences   :accessor preferences)
   (feedback-view :accessor feedback-view))
  (:documentation "The IB main window"))

(defclass ib-coder-window (tool-dialog-box)
  ((package-text     :accessor package-text)
   (doc-class-text   :accessor doc-class-text)
   (code-display     :accessor code-display)
   (lisp-toggle      :accessor lisp-toggle)
   (show-lisp        :accessor show-lisp :initform t)
   (setting-value    :accessor setting-value :initform nil))
  (:documentation "The IB coder tool window"))

(defclass ib-tree-window (tool-dialog-box)
  ((selection-list      :accessor selection-list)
   (name-text           :accessor name-text)
   (label-text          :accessor label-text))
  (:documentation "The IB coder tool window"))

(defclass resource-dialog-box (modeless-dialog-box)
  ((constraint-editor :accessor constraint-editor :initform nil)))

(defclass constraint-editor ()
  ((child-option      :accessor child-option)
   (current-child     :accessor current-child))
  (:documentation "to save callback information for constraint editing"))

(defclass pane-constraint-editor (constraint-editor)
  ((min-scale         :accessor min-scale)
   (max-scale         :accessor max-scale)
   (skip-toggle       :accessor skip-toggle)
   (resize-toggle     :accessor resize-toggle))
  (:documentation "edit status for pane constraints"))

(defclass form-constraint-editor (constraint-editor)
  ((left              :accessor left)
   (right             :accessor right)
   (top               :accessor top)
   (bottom            :accessor bottom))
  (:documentation "edit status for form constraints"))

(defclass color-wheel ()
  ((current-color :accessor current-color :initarg :current-color)
   (current-red   :accessor current-red :initform 0)
   (current-green :accessor current-green :initform 0)
   (current-blue  :accessor current-blue :initform 0)
   (plate :accessor plate :initarg :plate)
   (resource :accessor resource :initarg :resource)
   (intensity :accessor intensity :initarg :intensity)
   (red-scale :accessor red-scale :initarg :red-scale)
   (green-scale :accessor green-scale :initarg :green-scale)
   (blue-scale :accessor blue-scale :initarg :blue-scale)
   (dialog-box :accessor dialog-box :initarg :dialog-box))
  (:documentation "The color wheel"))

(defclass drag-template ()
  ((drag-widget  :accessor drag-widget)
   (drag-shell   :accessor drag-shell)
   (drag-copy    :accessor drag-copy)
   (target-view  :accessor target-view :initarg :target-view)
   (document     :accessor document :initarg :document)
   (info         :accessor info        :initarg :info)
   (info-constructor   :accessor info-constructor 
                       :initarg :info-constructor))
  (:documentation "a widget as template"))

(defclass widget-dragger (drag-command)
  (;; overrides
   (name :initform "Create Widget"  :allocation :class)
   (hysteresis :initform 0          :allocation :class)
   (do-tracking :allocation :class :initform nil)
   (protocol-id :allocation :class :initform :widget_drag)
   ;; instance variables
   (installed-object :accessor installed-object :initform nil))
  (:documentation "a mouse-down-command to move a template"))

(defclass grab-info-command (drag-command)
 (;;overrides
  (name    :initform "Grab Info"  :allocation :class)
  (call-doit :allocation :class :initform nil)
  ;; new slots
  (animate :accessor animate :initform t :allocation :class)
  (label   :accessor label)
  (info    :accessor info   :initform nil)
  ))

(defclass animator ()
   ((cmd            :accessor cmd      :initarg :cmd)
    (ashell  :accessor ashell  :initform nil)   ;; the shell to animate
    (timer   :accessor timer   :initform nil)   ;; timer for animation
    (bcount  :accessor bcount  :initform nil)
    (x-inc   :accessor x-inc   :initform nil)
    (y-inc   :accessor y-inc   :initform nil)
    (x-root  :accessor x-root)
    (y-root  :accessor y-root)))

(defclass grab-resources-command (grab-info-command)
 (;;overrides
  (name    :initform "Grab Resources"  :allocation :class)
  (causes-change  :initform nil :allocation :class)
  (undoable       :initform nil :allocation :class)
  (animate        :initform nil :allocation :class)
  ))

(defclass delete-command (grab-info-command)
 (;;overrides
  (name    :initform "Cut Widget"  :allocation :class)
  ;;new slots
  (old-parent   :accessor old-parent)
  (old-position :accessor old-position)
  ))

(defclass copy-command (grab-info-command)
 (;;overrides
  (name    :initform "Copy Widget"  :allocation :class)
  ))

(defclass paste-command (grab-info-command)
 (;;overrides
  (name    :initform "Paste Widget"  :allocation :class)
  (animate        :initform nil :allocation :class)
  ;;new slots
  (new-info    :accessor new-info     :initform nil)
  (new-parent  :accessor new-parent   :initform nil)
  (new-pos     :accessor new-pos      :initform nil)   
  ))

(defclass feedback-view (view)
  ()
  (:documentation "a view showing feedback strings"))

(defclass form-view (view)
  ((form-plate   :accessor form-plate    :initarg :form-plate)
   (form-info    :accessor form-info     :initarg :form-info)
   (zig-zag-mask :accessor zig-zag-mask  :initform nil)
   (line-mask    :accessor line-mask     :initform nil))
  (:documentation "a view editing form constraints"))

(defclass form-child-plate (view-object)
  ((info         :accessor info         :initarg :info)
   (plate        :accessor plate        :initarg :plate)
   (left-ruler   :accessor left-ruler   :initform nil)
   (right-ruler  :accessor right-ruler  :initform nil)
   (top-ruler    :accessor top-ruler    :initform nil)
   (bottom-ruler :accessor bottom-ruler :initform nil)))

(defclass ruler (view-object)
  ((side            :accessor side            :initarg :side)
   (leader          :accessor leader          :initarg :leader)
   (members         :accessor members         :initarg :members)
   (fixed-interval  :accessor fixed-interval  :initform nil)
   (variable-member :accessor variable-member :initform nil)
   (pos             :accessor pos)
   (atype           :accessor atype           :initarg :atype)
                    ;; one of :form :opposite-form :position :none
   ))

(defclass side-attach-command (mouse-down-command)
  (;; overrides
   (name       :initform "Reattach Side" :allocation :class)
   (hysteresis :initform 5                :allocation :class)
   ;; class parameters
   (show-ruler  :accessor show-ruler :initform nil :allocation :class)
   ;; instance-parameters
   (widget-plate :accessor widget-plate :initarg :widget-plate)
   (side  :accessor side :initarg :side)
   (min-pos :accessor min-pos)
   (max-pos :accessor max-pos)
   (inside-obj :accessor inside-obj :initform nil))
  (:documentation "superclass for changing individual side attachments"))

(defclass form-spacing-command (side-attach-command)
  (;; overrides
   (name       :initform "Change Spacing" :allocation :class)
   (show-ruler :initform nil              :allocation :class)
   ;; instance-variables
   (old-offset  :accessor old-offset)
   (new-offset  :accessor new-offset)
   (preceding-plate :accessor preceding-plate  :initform nil)))

(defclass new-ruler-command (side-attach-command)
  (;; overrides
   (name       :initform "Attach Side" :allocation :class)
   (show-ruler :initform t             :allocation :class)
   ;; instance variables
   (new-ruler  :accessor new-ruler)
   (old-size   :accessor old-size)))

(defclass detach-side-command (side-attach-command)
  (;; overrides
   (name       :initform "Detach Side" :allocation :class)
   (show-ruler :initform nil           :allocation :class)
   ;; instance variables
   (old-attachment  :accessor old-attachment)
   (old-ruler       :accessor old-ruler    :initform nil)
   (old-pos         :accessor old-pos)))

(defclass form-child-mover (mouse-down-command)
  (;; overrides
   (name       :initform "Move Form Child" :allocation :class)
   (hysteresis :initform 5                :allocation :class)
   ;; instance-parameters
   (widget-plate :accessor widget-plate :initarg :widget-plate)
   (old-x :accessor old-x :initarg :old-x)
   (old-y :accessor old-y :initarg :old-y)
   ;; instance-variables
   (new-x :accessor new-x)
   (new-y :accessor new-y)
   (x-off :accessor x-off :initarg :x-off)
   (y-off :accessor y-off :initarg :y-off)
  )
  (:documentation "a mouse-down-command to move a widget-plate around"))

(defclass ruler-mover (mouse-down-command)
  (;; overrides
   (name       :initform "Move Ruler" :allocation :class)
   (hysteresis :initform 5                :allocation :class)
   ;; instance-parameters
   (ruler :accessor ruler :initarg :ruler)
   (old-pos    :accessor old-pos  :initarg :old-pos)
   (new-pos    :accessor new-pos))
  (:documentation "a mouse-down-command to move a ruler around"))

(defclass ruler-type-changer (command)
  ((name         :initform "Change Ruler Type"      :allocation :class)
   (ruler        :accessor ruler :initarg :ruler)
   (old-pos      :accessor old-pos   :initarg :old-pos)
   (old-type     :accessor old-type  :initarg :old-type)
   (new-pos      :accessor new-pos)
   (new-type     :accessor new-type  :initform :none))
  (:documentation "A command to change the type of ruler"))

(defclass color-spec (modeless-dialog-box)
  ((current-color :accessor current-color :initarg :current-color)
   (current-red   :accessor current-red :initform 0)
   (current-green :accessor current-green :initform 0)
   (current-blue  :accessor current-blue :initform 0)
   (resource :accessor resource :initarg :resource)
   (selection :accessor selection)
   (intensity :accessor intensity)
   (red-scale :accessor red-scale)
   (green-scale :accessor green-scale)
   (blue-scale :accessor blue-scale))
  (:documentation "The color wheel"))

