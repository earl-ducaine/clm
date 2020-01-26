;;;-*-Mode:LISP;Syntax: Common-Lisp; Package:ib; -*-
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
;;;          Germany

(in-package :ib)

(setq *sccs-id* "@(#)class-tree.lisp	1.16	11/8/93")

;;; The info subclasses are arranged roughly like the widget classes they
;;; model. The differences are encoded as shared slots or methods.
;;;
;;; How to add a new widget class:
;;;     - create a <class>-info class, redefining all shared slots of its
;;;       parent class
;;;     - define a constructor make-<class>-info (parent)
;;;     - define the method initialize-resources, creating all new
;;;       resource objects - if you need a new resource representation type,
;;;       add it in resources.lisp.
;;;     - override make-template, creating a widget that can be used in a
;;;       palette (i.e. with NULL translations).
;;;     - add a template entry for the palette in main-window.lisp or
;;;       add a menu entry for a manager (tree-cmds.lisp).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'core-info
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass core-info (widget-info)
  ()
  (:documentation ""))

(defmethod initialize-resources ((info core-info))
  (call-next-method)
  (add-resources
   info (list (make-color-resource :background "white" 
                                   :access-mode :set-multiple)
	       (make-text-resource :background-pixmap "")
	       (make-color-resource :border-color "black")
	       (make-text-resource :border-pixmap "")
	       (make-boolean-resource :mapped-when-managed t)
	       (make-boolean-resource :sensitive t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'primitive
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass primitive-info (core-info)
  ()
  (:documentation ""))

(defmethod initialize-resources ((info primitive-info))
  (call-next-method)
  (add-resources
   info (list (make-color-resource :bottom-shadow-color "")
	       (make-text-resource :bottom-shadow-pixmap "")
	       (make-color-resource :foreground "")
	       (make-color-resource :highlight-color "black")
	       (make-text-resource :highlight-pixmap "")
	       (make-color-resource :top-shadow-color "")
	       (make-text-resource :top-shadow-pixmap "")
	       (make-limited-numeric-resource :highlight-thickness 0 0 10)
	       (make-limited-numeric-resource :shadow-thickness 2 0 10)
	       (make-enumerative-resource :unit-type :pixels
					  '(:pixels :100th-millimeters
					    :1000th-inches :100th-points
					    :100th-font-units))
	       (make-boolean-resource :highlight-on-enter nil)
	       (make-boolean-resource :traversal-on nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'separator
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass separator-info (primitive-info)
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

(defun make-separator-info (parent)
  (make-widget-info parent 'separator-info))

(defmethod initialize-resources ((info separator-info))
  (call-next-method)
  (add-resources
   info (list (make-enumerative-resource :orientation :horizontal
					  '(:horizontal :vertical))
	       (make-enumerative-resource :separator-type :shadow-etched-in
					  '(:single-line :double-line :no-line
					    :single-dashed-line
					    :double-dashed-line
					    :shadow-etched-in
					    :shadow-etched-out))
	       (make-limited-numeric-resource :margin 0 0 50))))

(defmethod make-template ((info separator-info) parent)
  (make-separator parent :orientation (named-resource-value info :orientation)))

(defmethod prefers-variable-size ((info separator-info) horizontal)
  (if (eql (named-resource-value info :orientation) :horizontal)
      horizontal
      (not horizontal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'label
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass label-info (primitive-info)
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

(defun make-label-info (parent)
  (make-widget-info parent 'label-info))

(defmethod initialize-resources ((info label-info))
  (call-next-method)
  (override-default info :shadow-thickness 0)
  (add-resources
   info (list (make-immediate-text-resource :label-string "A label"
                                          :access-mode 'set-label-string)
	       (make-text-resource :font-list "fixed")
	       (make-enumerative-resource :alignment :center
					  '(:beginning :center :end)
                                          :cplus-prefix "XmALIGNMENT_")
	       (make-enumerative-resource :label-type :string
					  '(:string :pixmap)
                                            :access-mode 'set-label-type)
	       (make-text-resource :label-pixmap "")
	       (make-boolean-resource :recompute-size t))))

(defmethod set-label-string ((widget label) string)
  (setf (label-string widget) string))

(defmethod set-label-type ((widget label) string)
  (setf (label-type widget) string))

(defmethod make-template ((info label-info) parent)
  (make-label parent (named-resource-value info :label-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'push-button
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass push-button-info (label-info)
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

(defun make-push-button-info (parent)
  (make-widget-info parent 'push-button-info))

(defmethod initialize-resources ((info push-button-info))
  (call-next-method)
  (override-default info :label-string "A Push-button")
  (override-default info :export :abstract)
  (add-resources
    info (list (make-color-resource :arm-color "")
		(make-text-resource :arm-pixmap "")
                (make-callback-resource :activate-callback nil)
		(make-boolean-resource :fill-on-arm t)
		(make-limited-numeric-resource :show-as-default 0 0 10))))

(defmethod make-template ((info push-button-info) parent)
  (make-push-button parent (named-resource-value info :label-string)
                    :motif-resources '(:translations "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'toggle-button
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass toggle-button-info (label-info)
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

(defun make-toggle-button-info (parent)
  (make-widget-info parent 'toggle-button-info))

(defmethod initialize-resources ((info toggle-button-info))
  (call-next-method)
  (override-default info :label-string "A Toggle-button")
  (override-default info :export :abstract)
  (add-resources
   info (list (make-boolean-resource :fill-on-select t)
	       (make-boolean-resource :indicator-on t)
	       (make-boolean-resource :set t)
	       (make-boolean-resource :visible-when-of t)
               (make-callback-resource :value-changed-callback 
                                       '("value" "old-value"))
	       (make-enumerative-resource :indicator-type :n-of-many
					  '(:n-of-many :one-of-many))
	       (make-text-resource :select-color "")
	       (make-text-resource :select-pixmap "")
	       (make-text-resource :select-insensitive-pixmap "")
	       (make-limited-numeric-resource :spacing 4 0 10))))

(defmethod make-template ((info toggle-button-info) parent)
  (make-toggle-button parent (named-resource-value info :label-string)
                      :motif-resources '(:translations "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'text-info
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass text-info (primitive-info)
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

(defun make-text-info (parent)
  (make-widget-info parent 'text-info))

(defmethod initialize-resources ((info text-info))
  (call-next-method)
  (override-default info :traversal-on t)
  (override-default info :export :widget)
  (add-resources
   info (list (make-boolean-resource :auto-show-cursor-position t)
	       (make-text-resource :font-list "fixed")
	       (make-boolean-resource :editable t)
               (make-callback-resource :value-changed-callback '("value"))
	       (make-enumerative-resource :edit-mode :single-line-edit
					  '(:single-line-edit :multi-line-edit))
	       (make-limited-numeric-resource :margin-height 3 0 20)
	       (make-limited-numeric-resource :margin-width 3 0 20)
	       (make-limited-numeric-resource :columns 20 1 150)
	       (make-limited-numeric-resource :rows 1 1 80)
	       (make-immediate-text-resource :value ""))))

(defmethod make-template ((info text-info) parent)
  (make-text parent 
             :columns (named-resource-value info :columns)
             :edit-mode (named-resource-value info :edit-mode)
             :motif-resources '(:translations "")))

(defmethod prefers-variable-size ((info text-info) horizontal)
  (if (eql (named-resource-value info :edit-mode) :multi-line-edit)
      t
      horizontal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'scrolled-text-info
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass scrolled-text-info (text-info)
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

(defun make-scrolled-text-info (parent)
  (make-widget-info parent 'scrolled-text-info))

(defmethod all-widgets ((info scrolled-text-info) widget)
  (list (scroller widget) 
        (horizontal-scrollbar (scroller widget))
        (vertical-scrollbar (scroller widget))))

(defmethod initialize-resources ((info scrolled-text-info))
  (call-next-method)
  (override-default info :edit-mode :multi-line-edit)
  (override-default info :rows 10)
  (add-resources
   info (list (make-boolean-resource :scroll-vertical t)
              (make-boolean-resource :scroll-horizontal t))))

(defmethod make-template ((info scrolled-text-info) parent)
  (make-scrolled-text parent 
             :columns (named-resource-value info :columns)
             :rows (named-resource-value info :rows)
             :motif-resources '(:translations "")))

(defmethod prefers-variable-size ((info scrolled-text-info) horizontal)
  (declare (ignore horizontal))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'labeled-text
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass labeled-text-info (text-info)
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

(defun make-labeled-text-info (parent)
  (make-widget-info parent 'labeled-text-info))

(defmethod all-widgets ((info labeled-text-info) widget)
  (list (label widget) (row-column widget)))

(defmethod is-row-column-or-subclass ((info labeled-text-info))
  "to test for resize hack for row-columns"
  t)

(defmethod initialize-resources ((info labeled-text-info))
  (call-next-method)
  (add-resources
   info (list (make-immediate-text-resource :label-string "A labeled text"
                                             :access-mode 'set-label-string)
	       (make-immediate-text-resource :value "")
	       (make-boolean-resource :editable t)
	       (make-limited-numeric-resource :columns 20 1 150)
	       (make-enumerative-resource :edit-mode :single-line-edit
					  '(:single-line-edit :multi-line-edit))
	       (make-enumerative-resource :label-position :left
					  '(:top :left)
                                          :access-mode 'set-label-position))))

(defmethod set-label-string ((text labeled-text) string)
  (set-motif-resources (label text) :label-string string))

(defmethod set-label-position ((text labeled-text) pos)
  (setf (label-position text) pos))

(defmethod make-template ((info labeled-text-info) parent)
  (make-labeled-text parent (named-resource-value info :label-string)
             :columns (named-resource-value info :columns)
             :edit-mode (named-resource-value info :edit-mode)
             :motif-resources '(:translations "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'scrollbar
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass scrollbar-info (primitive-info)
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

(defun make-scrollbar-info (parent)
  (make-widget-info parent 'scrollbar-info))

(defmethod initialize-resources ((info scrollbar-info))
  (call-next-method)
  (override-default info :export :widget)
  (add-resources
   info (list (make-boolean-resource :show-arrows t)
	       (make-numeric-resource :increment 1)
	       (make-numeric-resource :maximum 100)
	       (make-numeric-resource :minimum 0)
	       (make-numeric-resource :page-increment 10)
	       (make-numeric-resource :slider-size 10)
	       (make-numeric-resource :value 0)
               (make-callback-resource :value-changed-callback '("value"))
               (make-callback-resource :drag-callback '("value"))
	       (make-limited-numeric-resource :initial-delay 250 1 1000)
	       (make-limited-numeric-resource :repeat-delay 50 1 500)
	       (make-enumerative-resource :orientation :vertical
					  '(:vertical :horizontal))
	       (make-enumerative-resource :processing-direction :max-on-bottom
					  '(:max-on-bottom :max-on-right
					    :max-on-left   :max-on-top)))))

(defmethod make-template ((info scrollbar-info) parent)
  (make-scrollbar parent 
                  :orientation (named-resource-value info :orientation)
                  :motif-resources '(:translations "")))

(defmethod prefers-variable-size ((info scrollbar-info) horizontal)
  (if (eql (named-resource-value info :orientation) :horizontal)
      horizontal
      (not horizontal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'manager
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass manager-info (core-info)
  ()
  (:documentation ""))

(defmethod initialize-resources ((info manager-info))
  (call-next-method)
  (add-resources
   info (list (make-color-resource :bottom-shadow-color "")
	       (make-text-resource :bottom-shadow-pixmap "")
	       (make-color-resource :foreground "")
	       (make-color-resource :highlight-color "black")
	       (make-text-resource :highlight-pixmap "")
	       (make-color-resource :top-shadow-color "")
	       (make-text-resource :top-shadow-pixmap "")
	       (make-limited-numeric-resource :shadow-thickness 2 0 10)
	       (make-enumerative-resource :unit-type :pixels
					  '(:pixels :100th-millimeters
					    :1000th-inches
					    :100th-points :100th-font-units)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'scale
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass scale-info (manager-info)
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

(defun make-scale-info (parent)
  (make-widget-info parent 'scale-info))

(defmethod initialize-resources ((info scale-info))
  (call-next-method)
  (override-default info :export :widget)
  (add-resources
   info (list (make-boolean-resource :highlight-on-enter nil)
	       (make-text-resource :font-list "fixed")
	       (make-limited-numeric-resource :highlight-thickness 0 0 10)
	       (make-numeric-resource :maximum 100)
	       (make-numeric-resource :minimum 0)
	       (make-numeric-resource :value 0)
               (make-callback-resource :value-changed-callback '("value"))
               (make-callback-resource :drag-callback '("value"))
	       (make-enumerative-resource :orientation :vertical
					  '(:vertical :horizontal))
	       (make-enumerative-resource :processing-direction :max-on-bottom
					  '(:max-on-bottom :max-on-right
					    :max-on-left :max-on-top))
	       (make-limited-numeric-resource :scale-width 0 0 10)
	       (make-limited-numeric-resource :scale-height 0 0 10)
	       (make-immediate-text-resource :title-string "")
	       (make-boolean-resource :show-value nil)
	       (make-boolean-resource :traversal-on nil))))

(defmethod make-template ((info scale-info) parent)
  (make-scale parent 
              :orientation (named-resource-value info :orientation)
              :motif-resources '(:translations "")))

(defmethod prefers-variable-size ((info scale-info) horizontal)
  (if (eql (named-resource-value info :orientation) :horizontal)
      horizontal
      (not horizontal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'bulletin-board
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass bulletin-board-info (manager-info)
  ()
  (:documentation ""))

(defmethod initialize-resources ((info bulletin-board-info))
  (call-next-method)
  (add-resources
   info (list (make-text-resource :dialog-title "Your title here")
	       (make-boolean-resource :allow-overlap t)
	       (make-boolean-resource :auto-unmanage t)
	       (make-boolean-resource :no-resize nil)
	       (make-boolean-resource :default-position t)
	       (make-text-resource :button-font-list "")
	       (make-text-resource :label-font-list "")
	       (make-text-resource :text-font-list "")
	       (make-limited-numeric-resource :margin-height 10 0 100)
	       (make-limited-numeric-resource :margin-width 10 0 100)
	       (make-enumerative-resource :dialog-style :modeless
					  '(:system-modal :application-modal
					    :modeless :work-area)
                                          :cplus-prefix "XmDIALOG_")
	       (make-enumerative-resource :resize-policy :none
					  '(:none :any :grow)
                                          :cplus-prefix "XmRESIZE_")
	       (make-enumerative-resource :shadow-type :in
					  '(:in :out :etched-in :etched-out)
                                          :cplus-prefix "XmSHADOW_"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'form
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass form-info (bulletin-board-info)
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

(defun make-form-info (parent)
  (make-widget-info parent 'form-info))

(defmethod initialize-resources ((info form-info))
  (call-next-method)
  (add-resources
   info (list (make-limited-numeric-resource :horizontal-spacing 0 0 100)
	       (make-limited-numeric-resource :vertical-spacing 0 0 100)
	       (make-boolean-resource :rubber-positioning nil))))

(defmethod prefers-variable-size ((info form-info) horizontal)
  (declare (ignore horizontal))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'row-column
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass row-column-info (manager-info)
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

(defun make-row-column-info (parent)
  (make-widget-info parent 'row-column-info))

(defmethod is-row-column-or-subclass ((info row-column-info))
  "to test for resize hack for row-columns"
  t)

(defmethod initialize-resources ((info row-column-info))
  (call-next-method)
  (add-resources
   info (list (make-boolean-resource :adjust-last t)
	       (make-boolean-resource :adjust-margin t)
	       (make-enumerative-resource :entry-alignment :beginning
					  '(:none :beginning :center :end)
                                          :access-mode 'set-entry-alignment
                                          :cplus-prefix "XmALIGNMENT_")
	       (make-limited-numeric-resource :entry-border 0 0 100)
   ;	       (make-boolean-resource :is-aligned t)
	       (make-boolean-resource :is-homogeneous nil)
	       (make-limited-numeric-resource :margin-width 2 0 100)
	       (make-limited-numeric-resource :margin-height 2 0 100)
	       (make-limited-numeric-resource :num-columns 1 1 10
                                              :access-mode 'set-num-columns)
	       (make-enumerative-resource :orientation :vertical
					  '(:horizontal :vertical))
	       (make-enumerative-resource :packing :pack-tight
					  '(:pack-tight :pack-none
					    :pack-column))
	       (make-boolean-resource :radio-always-one t)
	       (make-boolean-resource :radio-behavior t)
	       (make-boolean-resource :resize-height t)
	       (make-boolean-resource :resize-width t)
	       (make-limited-numeric-resource :spacing 1 0 100))))

(defmethod set-entry-alignment ((widget row-column) alignment)
  (if (eql alignment :none)
      (set-motif-resources widget :is-aligned nil)
      (set-motif-resources widget :is-aligned t :entry-alignment alignment)))

(defmethod set-num-columns ((widget row-column) num-columns)
  (set-motif-resources widget :num-columns num-columns
                       :packing (if (eq num-columns 1)
				    :pack-tight :pack-column)))

(defmethod prefers-variable-size ((info row-column-info) horizontal)
  (and (if (eql (named-resource-value info :orientation) :horizontal)
           (not horizontal)
            horizontal)
       (prefers-variable-size (first (children info)) horizontal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'frame-info
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass frame-info (manager-info)
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

(defun make-frame-info (parent)
  (make-widget-info parent 'frame-info))

(defmethod initialize-resources ((info frame-info))
  (call-next-method)
  (add-resources
   info (list (make-limited-numeric-resource :margin-width 0 0 10)
	       (make-limited-numeric-resource :margin-height 0 0 10)
	       (make-enumerative-resource :shadow-type :in
					  '(:in :out :etched-in :etched-out)
                                          :cplus-prefix "XmSHADOW_"))))

(defmethod prefers-variable-size ((info frame-info) horizontal)
  (when (children info)
    (prefers-variable-size (first (children info)) horizontal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'option-menu
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass option-menu-info (manager-info)
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

(defun make-option-menu-info (parent)
  (make-widget-info parent 'option-menu-info))

(defmethod initialize-resources ((info option-menu-info))
  (call-next-method)
  (override-default info :export :widget)
  (add-resources
   info (list (make-item-list-resource :item-list '("Mercedes" "Audi" "Ford"))
               (make-callback-resource :value-changed-callback 
                                       '("value" "old-value"))
               (make-immediate-text-resource :label-string "Automarke"))))

(defmethod is-row-column-or-subclass ((info option-menu-info))
  "to test for resize hack for row-columns"
  t)

(defmethod make-template ((info option-menu-info) parent)
  (let* ((row (make-row-column parent :orientation :horizontal
                               :margin-width 3 :margin-height 3 :spacing 3)))
    (setf (template-children info)
          (list
            (make-label row (named-resource-value info :label-string))
            (make-push-button row
                        (concatenate 'string
                           (first (named-resource-value info :item-list))
                           "  o ")
                        :motif-resources '(:translations ""))))
    row))
;; real option menu sets OwnerEventMask and thus is undraggable
;  (make-option-menu parent (named-resource-value info :item-list)
;                    :label-string (named-resource-value info :label-string)
;                    :motif-resources '(:translations "")))

(defmethod all-widgets ((info option-menu-info) widget)
  (when (and (template-children info)
             (eql (parent (first (template-children info))) widget))
    ;;only when template
    (template-children info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'selection-list-info
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass selection-list-info (primitive-info)
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

(defun make-selection-list-info (parent)
  (make-widget-info parent 'selection-list-info))

(defmethod initialize-resources ((info selection-list-info))
  (call-next-method)
  (override-default info :export :widget)
  (add-resources
   info (list (make-item-list-resource :item-list '("BMW" "Opel" "Mazda"))
               ;;(make-immediate-text-resource :label-string "Automarke")
               (make-callback-resource :value-changed-callback 
                                       '("value" "old-value"))
               (make-limited-numeric-resource :visible-item-count 10 1 20)
	       (make-enumerative-resource :selection-policy :browse
					  '(:browse :single :multiple
					    :extended)
					  :access-mode 'set-selection-policy)
               (make-limited-numeric-resource :width 100 10 1000)
               (make-enumerative-resource :list-size-policy :variable
                                          '(:constant :variable 
					    :resize-if-possible))
               (make-limited-numeric-resource :list-spacing 0 0 10)
               (make-limited-numeric-resource :list-margin-width 0 0 10)
               (make-limited-numeric-resource :list-margin-height 0 0 10))))

(defmethod set-selection-policy ((info selection-list-info) value)
  (setf (selection-policy info) value))

(defmethod make-template ((info selection-list-info) parent)
  (make-selection-list parent (named-resource-value info :item-list)
       :visible-item-count (named-resource-value info :visible-item-count)
       :motif-resources '(:translations "")))

(defmethod prefers-variable-size ((info selection-list-info) horizontal)
  (declare (ignore horizontal))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'scrollable-selection-list
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass scrollable-selection-list-info (selection-list-info)
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

(defun make-scrollable-selection-list-info (parent)
  (make-widget-info parent 'scrollable-selection-list-info))

(defmethod all-widgets ((info scrollable-selection-list-info) widget)
  (list (scroller widget) 
        (horizontal-scrollbar (scroller widget))
        (vertical-scrollbar (scroller widget))))

(defmethod initialize-resources ((info scrollable-selection-list-info))
  (call-next-method)
  (add-resources info 
     (list (make-enumerative-resource :scroll-bar-display-policy
                :static '(:as-needed :static)))))

(defmethod make-template ((info scrollable-selection-list-info) parent)
  (make-scrollable-selection-list parent (named-resource-value info :item-list)
       :visible-item-count (named-resource-value info :visible-item-count)
       :motif-resources '(:translations "")))

(defmethod prefers-variable-size ((info scrollable-selection-list-info) 
                                  horizontal)
  (declare (ignore horizontal))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class scroller-info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass scroller-info (manager-info)
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

(defun make-scroller-info (parent)
  (make-widget-info parent 'scroller-info))

(defmethod all-widgets ((info scroller-info) widget)
  (list (horizontal-scrollbar widget) (vertical-scrollbar widget)))

(defmethod initialize-resources ((info scroller-info))
  (call-next-method)
  (override-default info :export :widget)
  (add-resources info nil))

(defmethod prefers-variable-size ((info scroller-info) horizontal)
  (declare (ignore horizontal))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class paned-window-info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass paned-window-info (manager-info)
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

(defun make-paned-window-info (parent)
  (make-widget-info parent 'paned-window-info))

(defmethod initialize-resources ((info paned-window-info))
  (call-next-method)
  (add-resources info nil))

(defmethod is-constraint-widget ((info paned-window-info))
  "if this info is a constraint manager"
  t)

(defmethod prefers-variable-size ((info paned-window-info) horizontal)
  (declare (ignore horizontal))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'toggle-button-group-info
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass toggle-button-group-info (row-column-info)
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

(defun make-toggle-button-group-info (parent)
  (make-widget-info parent 'toggle-button-group-info))

(defmethod all-widgets ((info toggle-button-group-info) widget)
  (append
      (list (main-column widget) (label widget) (frame widget))
      (toggle-buttons widget) (separators widget)))

(defmethod initialize-resources ((info toggle-button-group-info))
  (call-next-method)
  (override-default info :export :widget)
  (add-resources
   info (list (make-item-list-resource :item-list '("BMW" "Mercedes" "Mazda"))
               (make-immediate-text-resource :label-string "Automarke"
                                             :access-mode 'set-label-string)
               (make-callback-resource :value-changed-callback 
                                       '("value" "toggled-value" "set"))
	       (make-enumerative-resource :button-label-type :string
					  '(:string :pixmap)
                                          :access-mode 'set-button-label-type)
	       (make-enumerative-resource :shadow-type :etched-in
					  '(:in :out :etched-in :etched-out)
                                          :cplus-prefix "XmSHADOW_"
                                          :access-mode 'set-shadow-type)
               (make-boolean-resource :with-frame t
                                      :access-mode 'set-with-frame))))

(defmethod set-label-string ((group toggle-button-group) string)
  (set-motif-resources (label group) :label-string string))

(defmethod set-button-label-type ((group toggle-button-group) type)
  (setf (button-label-type group) type)
  (loop for button in (toggle-buttons group)
        do (setf (label-type button) type)))

(defmethod set-shadow-type ((group toggle-button-group) type)
  (set-motif-resources (frame group) :shadow-type type))

(defmethod set-with-frame ((group toggle-button-group) flag)
  (if flag
      (set-motif-resources (frame group) :shadow-thickness 2)
      (set-motif-resources (frame group) :shadow-thickness 0)))

(defmethod make-template ((info toggle-button-group-info) parent)
  (make-toggle-button-group parent (named-resource-value info :item-list)
                     :label-string (named-resource-value info :label-string)
                     :motif-resources '(:translations "")
                     :button-resources '(:translations "")))

(defmethod prefers-variable-size ((info toggle-button-group-info) horizontal)
  (declare (ignore horizontal))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'radio-button-group-info
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass radio-button-group-info (toggle-button-group-info)
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

(defun make-radio-button-group-info (parent)
  (make-widget-info parent 'radio-button-group-info))

(defmethod initialize-resources ((info radio-button-group-info))
  (call-next-method)
  (let ((old-callback (gethash :value-changed-callback (all-resources info)))
        (new-callback (make-callback-resource :value-changed-callback 
                                              '("value" "old-value"))))
    (setf (table-index new-callback) (table-index old-callback))
    (setf (gethash :value-changed-callback (all-resources info)) new-callback))
  (add-resources
   info nil))

(defmethod make-template ((info radio-button-group-info) parent)
  (make-radio-button-group parent (named-resource-value info :item-list)
                       :label-string (named-resource-value info :label-string)
                       :motif-resources '(:translations "")
                       :button-resources '(:translations "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'modeless-dialog-box
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass modeless-dialog-box-info (form-info)
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

(defun make-modeless-dialog-box-info (document)
  (make-widget-info nil 'modeless-dialog-box-info :document document))

(defmethod initialize-resources ((info modeless-dialog-box-info))
  (call-next-method)
  (add-resources
    info (list (make-immediate-text-resource :class "demo-modeless-dialog"
                                              :access-mode :ignore)
                (make-immediate-text-resource :title "Dialog"
                                              :access-mode 'set-new-title)
                (make-boolean-resource :resize nil
                                       :access-mode 'set-new-resize)
                (make-boolean-resource :return-is-accelerator t
                                       :access-mode :ignore)
                (make-boolean-resource :allow-shell-resize nil
                                       :access-mode 'set-allow-shell-resize))))

(defmethod set-dialog-title ((widget modeless-dialog-box) new-title)
  (set-motif-resources (dialog-shell widget) :title new-title))

(defmethod set-new-resize ((widget modeless-dialog-box) new-resize)
  (set-motif-resources widget :no-resize (not new-resize)))

(defmethod set-allow-shell-resize ((widget modeless-dialog-box) new-resize)
  (set-motif-resources (dialog-shell widget) :allow-shell-resize new-resize))

(defmethod is-constraint-widget ((info form-info))
  "if this info is a constraint manager"
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'modal-dialog-box
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass modal-dialog-box-info (form-info)
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

(defun make-modal-dialog-box-info (document)
  (make-widget-info nil 'modal-dialog-box-info :document document))

(defmethod initialize-resources ((info modal-dialog-box-info))
  (call-next-method)
  (add-resources
    info (list (make-immediate-text-resource :class "demo-modal-dialog"
                                              :access-mode :ignore)
                (make-immediate-text-resource :title "Dialog"
                                              :access-mode 'set-new-title)
                (make-boolean-resource :resize nil
                                       :access-mode 'set-new-resize)
                (make-boolean-resource :return-is-accelerator t
                                       :access-mode :ignore)
                (make-boolean-resource :allow-shell-resize nil
                                       :access-mode 'set-allow-shell-resize))))

(defmethod set-dialog-title ((widget modal-dialog-box) new-title)
  (set-motif-resources (dialog-shell widget) :title new-title))

(defmethod set-new-resize ((widget modal-dialog-box) new-resize)
  (set-motif-resources widget :no-resize (not new-resize)))

(defmethod set-allow-shell-resize ((widget modal-dialog-box) new-resize)
  (set-motif-resources (dialog-shell widget) :allow-shell-resize new-resize))

(defmethod is-constraint-widget ((info modal-dialog-box-info))
  "if this info is a constraint manager"
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'tool-dialog-box
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass tool-dialog-box-info (form-info)
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

(defun make-tool-dialog-box-info (document)
  (make-widget-info nil 'tool-dialog-box-info :document document))

(defmethod initialize-resources ((info tool-dialog-box-info))
  (call-next-method)
  (add-resources
    info (list (make-immediate-text-resource :class "demo-tool-dialog"
                                              :access-mode :ignore)
                (make-immediate-text-resource :title "Tool"
                                              :access-mode 'set-new-title)
                (make-boolean-resource :resize t
                                       :access-mode 'set-new-resize)
                (make-boolean-resource :return-is-accelerator t
                                       :access-mode :ignore)
                (make-boolean-resource :allow-shell-resize nil
                                       :access-mode 'set-allow-shell-resize))))

(defmethod set-dialog-title ((widget tool-dialog-box) new-title)
  (set-motif-resources (dialog-shell widget) :title new-title))

(defmethod set-new-resize ((widget tool-dialog-box) new-resize)
  (set-motif-resources widget :no-resize (not new-resize)))

(defmethod set-allow-shell-resize ((widget tool-dialog-box) new-resize)
  (set-motif-resources (dialog-shell widget) :allow-shell-resize new-resize))

(defmethod is-constraint-widget ((info tool-dialog-box-info))
  "if this info is a constraint manager"
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'document-shell
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass document-shell-info (core-info)
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

(defun make-document-shell-info (document)
  (make-widget-info nil 'document-shell-info :document document))

(defmethod initialize-resources ((info document-shell-info))
  (call-next-method)
  (add-resources
    info (list (make-immediate-text-resource :class "demo-document-shell"
                                              :access-mode :ignore)
                (make-boolean-resource :is-main-shell t
                                       :access-mode :ignore)
                (make-boolean-resource :with-menu t
                                       :access-mode :ignore))))

(defmethod is-constraint-widget ((info document-shell-info))
  "if this info is a constraint manager"
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'view
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass view-info (manager-info)
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

(defun make-view-info (parent)
  (make-widget-info parent 'view-info))

(defmethod special-params-for-install ((info view-info))
  (list :width (- (width info) 9) :height (- (height info) 9)))

(defmethod initialize-resources ((info view-info))
  (call-next-method)
  (override-default info :export :widget)
  (add-resources
    info (list (make-immediate-text-resource :class "my-view"
                                              :access-mode :ignore)
                (make-boolean-resource :needs-doc t :access-mode :ignore))))

(defmethod make-template ((info view-info) parent)
  (make-prototype-view parent :width 50 :height 50)
  ;; remove the event handlers
  )

(defmethod prefers-variable-size ((info view-info) horizontal)
  (declare (ignore horizontal))
  t)
