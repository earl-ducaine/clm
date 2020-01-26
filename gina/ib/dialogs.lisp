;;;-*-Mode:LISP;Syntax: Common-Lisp;Package:ib;Base:10-*-
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

(setq *sccs-id* "@(#)dialogs.lisp	1.13	11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; resource dialogs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass resource-dialog-box (modeless-dialog-box)
  ((constraint-editor :accessor constraint-editor :initform nil)))

(defmethod create-resource-dialog ((info widget-info) &aux 
				   column r-column b-column dismiss-button)
  "pop up a dialog to edit the resources"
  
  (setf (res-dialog info) 
	(make-modeless-dialog-box (res-title info) 
				  :document (document info)
                                  :class 'resource-dialog-box
  ))
  (when (plate info)
    (setf (relative-widget (res-dialog info)) (gina-widget (plate info)))
    (setf (relative-x (res-dialog info)) :right)
    (setf (relative-y (res-dialog info)) :center))
  
  (setf column (make-row-column (res-dialog info) :spacing 15
				:orientation :vertical))
  (setf r-column (make-row-column column :orientation :vertical))
  (make-separator column)
  (setf b-column (make-row-column column :spacing 15
				  :orientation :horizontal
                                  :motif-resources '(:packing :pack-column
                                      :entry-alignment :center)))

  (loop for name in '(:name :export) do
        (create-resource-widget (find-resource info name) r-column info))
  (loop for resource in (positional-resources info) do
	(create-resource-widget resource r-column info))
  (loop for resource in (key-resources info) do
	(create-resource-widget resource r-column info))
  (loop for resource in (important-resources info) do
        (create-resource-widget resource r-column info))
  (loop for resource in (callback-resources info) do
        (create-resource-widget resource r-column info))
  (setf (constraint-editor (res-dialog info))
        (create-constraint-editor info r-column))

  (setq dismiss-button
        (make-push-button b-column " Dismiss "
             :activate-callback (make-callback #'pop-down (res-dialog info))
             :motif-resources '(:show-as-default 1 :traversal-on t)))
  (setf (default-button (res-dialog info)) dismiss-button)
  )

(defmethod destroy-resource-dialog ((info widget-info))
  (when (res-dialog info)
        (unmanage (res-dialog info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Constraints dialogs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass constraint-editor ()
  ((child-option      :accessor child-option)
   (current-child     :accessor current-child))
  (:documentation "to save callback information for constraint editing"))

'(defclass pane-constraint-editor (constraint-editor)
  ((min-scale         :accessor min-scale)
   (max-scale         :accessor max-scale)
   (skip-toggle       :accessor skip-toggle)
   (resize-toggle     :accessor resize-toggle))
  (:documentation "edit status for pane constraints"))

'(defclass form-constraint-editor (constraint-editor)
  ((left              :accessor left)
   (right             :accessor right)
   (top               :accessor top)
   (bottom            :accessor bottom))
  (:documentation "edit status for form constraints"))

(defmethod create-constraint-editor ((info widget-info) r-column)
  "no constraint editing for ordinary widget classes"
  (declare (ignore r-column))
  nil)

(defmethod create-constraint-editor ((info tool-dialog-box-info) r-column)
  "no constraint editing for ordinary widget classes"
  (declare (ignore r-column))
  nil)

(defmethod create-constraint-editor ((info modeless-dialog-box-info) r-column)
  "no constraint editing for ordinary widget classes"
  (declare (ignore r-column))
  nil)

(defmethod create-constraint-editor ((info modal-dialog-box-info) r-column)
  "no constraint editing for ordinary widget classes"
  (declare (ignore r-column))
  nil)

(defmethod create-child-selection ((info widget-info) column editor)
  "create option menu to select children for constraint editing"
  (setf (child-option editor)
        (make-option-menu column
                          (loop for child in (children info)
                                collect (widget-name child))
                          :label-string "Children:"))
  (setf (value-changed-callback (child-option editor))
        (make-callback #'show-child-constraints info editor))
  (setf (current-child editor) (first (children info))))

(defmethod update-child-selection ((info widget-info))
  (when (and (res-dialog info)
             (constraint-editor (res-dialog info)))
    (set-item-list (child-option (constraint-editor (res-dialog info)))
             (loop for child in (children info)
                   collect (widget-name child)))
    (setf (current-child (constraint-editor (res-dialog info)))
          (first (children info)))
    (update-constraint-values info (constraint-editor (res-dialog info)))))

(defmethod show-child-constraints ((info widget-info) editor &rest rest)
  "callback to switch to different child"
  (loop for child in (children info)
        when (equal (widget-name child) (first rest))
        do   (setf (current-child editor) child))
  (update-constraint-values info editor))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Pane constraints ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod create-constraint-editor ((info paned-window-info) column
                                     &aux editor col2)
  "create pane constraints editing part of dialog"
  (setq editor (make-instance 'pane-constraint-editor))
  (create-child-selection info column editor)

  (setf (min-scale editor)
        (make-scale column :orientation :horizontal
                           :minimum 1 :maximum 1000
                           :title-string "Minimum"))
  (setf (value-changed-callback (min-scale editor))
        (make-callback #'set-constraint-value editor :minimum))
  (setf (max-scale editor)
        (make-scale column :orientation :horizontal
                           :minimum 1 :maximum 1000
                           :title-string "Maximum"))
  (setf (value-changed-callback (max-scale editor))
        (make-callback #'set-constraint-value editor :maximum))

  (setq col2 (make-row-column column :orientation :horizontal))
  (setf (skip-toggle editor)
        (make-toggle-button col2 "Skip-Adjust"))
  (setf (value-changed-callback (skip-toggle editor))
        (make-callback #'set-constraint-value editor :skip-adjust))
  (setf (resize-toggle editor)
        (make-toggle-button col2 "Allow-Resize"))
  (setf (value-changed-callback (resize-toggle editor))
        (make-callback #'set-constraint-value editor :allow-resize))

  (update-constraint-values info editor)
  editor)

(defmethod set-constraint-value ((editor pane-constraint-editor) field 
                                 &rest rest
                                 &aux constraints)
  "callback to set changed resource values"
  (setq constraints (constraints (current-child editor)))
  (case field
        (:minimum      (setf (minimum constraints) (first rest)))
        (:maximum      (setf (maximum constraints) (first rest)))
        (:skip-adjust  (setf (skip-adjust constraints) (first rest)))
        (:allow-resize (setf (allow-resize constraints) (first rest))))
  (update-constraints constraints (plate (current-child editor)))
  (adapt-to-widget-size (plate (find-toplevel-object (current-child editor)))))

(defmethod update-constraint-values ((info paned-window-info) editor
                                     &aux constraints)
  "update values when selected child changes"
  (setq constraints (constraints (current-child editor)))
  (setf (value (min-scale editor)) (minimum constraints))
  (setf (value (max-scale editor)) (maximum constraints))
  (setf (value (skip-toggle editor)) (skip-adjust constraints))
  (setf (value (resize-toggle editor)) (allow-resize constraints)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Form constraints ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod create-constraint-editor ((info form-info) column
                                     &aux editor toggle)
  "create form constraint editing part of dialog"  
  (setq editor (make-instance 'form-constraint-editor))
  (create-child-selection info column editor)

  (setf (top editor)
        (make-constraint-widgets editor column "Top" #'top))
  (setf (left editor)
        (make-constraint-widgets editor column "Left" #'left))
  (setf (right editor)
        (make-constraint-widgets editor column "Right" #'right))
  (setf (bottom editor)
        (make-constraint-widgets editor column "Bottom" #'bottom))

  ;; create hook for form view
  (setq toggle (gina:make-toggle-button column "Show Constraints" :value nil))
  (setf (value-changed-callback toggle)
        (make-callback #'switch-symbolic-state (plate info)))

  (update-constraint-values info editor)
  editor)

(defmethod make-constraint-widgets ((editor form-constraint-editor) parent 
                                    label-string attachment-func
                                    &aux frame column option1 scale option2)
  "create widgets for one attachment direction"
  (make-label parent label-string)
  (setq frame (make-frame parent :shadow-type :etched-in))
  (setq column (make-row-column frame))
  (setq option1 
        (make-option-menu column '(("None"            :none)
                                   ("Form"            :form)
                                   ("Opposite Form"   :opposite-form)
                                   ("Widget"          :widget)
                                   ("Opposite Widget" :opposite-widget)
                                   ("Position"        :position)
                                   ("Self"            :self))
                 :label-string "Attachment"))
  (when (<= (length (value-list (child-option editor))) 1)
      (set-motif-resources (fourth (option-buttons option1)) :sensitive nil)
      (set-motif-resources (fifth (option-buttons option1)) :sensitive nil))
  (setq scale 
        (make-scale column :orientation :horizontal
                           :minimum 0 :maximum 100
                           :title-string "Position"))
  (setq option2 
        (make-option-menu column (value-list (child-option editor))
                          :label-string "Widget"))
  (setf (value-changed-callback option1)
        (make-callback #'set-attachment-type editor attachment-func))
  (setf (value-changed-callback scale)
        (make-callback #'set-offset-or-position editor attachment-func))
  (setf (value-changed-callback option2)
        (make-callback #'set-attached-widget editor attachment-func))
  (list option1 scale option2))

(defmethod set-attachment-type ((editor form-constraint-editor) 
                                attachment-func &rest rest
                                &aux attachment scale)
  "callback to change attachment type"
  (setq attachment 
        (funcall attachment-func (constraints (current-child editor))))
  (setq scale 
        (second (funcall attachment-func editor)))
  (setf (atype attachment) (first rest))
  (case (first rest)
     ((:position :self) 
             (setf (value scale) (form-pos attachment))
             (setf (offset attachment) 0)
             (set-motif-resources scale :title-string "Position"))
     (t
             (setf (value scale) (offset attachment))
             (set-motif-resources scale :title-string "Offset")))
  (update-constraints (constraints (current-child editor))
                      (plate (current-child editor)))
  (adapt-to-widget-size (plate (find-toplevel-object (current-child editor)))))

(defmethod set-offset-or-position ((editor form-constraint-editor) 
                                   attachment-func &rest rest
                                   &aux attachment)
  "callback to change offset or position"
  (setq attachment 
        (funcall attachment-func (constraints (current-child editor))))
  (case (atype attachment)
     ((:position :self)    (setf (form-pos attachment) (first rest)))
     (t                    (setf (offset attachment) (first rest))))
  (update-constraints (constraints (current-child editor))
                      (plate (current-child editor)))
  (adapt-to-widget-size (plate (find-toplevel-object (current-child editor)))))

(defmethod set-attached-widget ((editor form-constraint-editor)
                                attachment-func &rest rest
                                &aux attachment)
  "callback to change attached widget"
  (setq attachment 
        (funcall attachment-func (constraints (current-child editor))))
  (find-attached-widget attachment 
                        (children (parent-info (current-child editor))) 
                        (first rest))
  (update-constraints (constraints (current-child editor))
                      (plate (current-child editor)))
  (adapt-to-widget-size (plate (find-toplevel-object (current-child editor)))))

(defmethod update-constraint-values ((info form-info) editor
                                     &aux child constraints)
  "update values when selected child changes"
  (setq child (current-child editor))
  (setq constraints (constraints child))
  (update-attachment child (left constraints) (left editor))
  (update-attachment child (right constraints) (right editor))
  (update-attachment child (top constraints) (top editor))
  (update-attachment child (bottom constraints) (bottom editor))
)

(defmethod update-attachment ((info widget-info) attachment widget-list)
  "display current values for one attachment direction"
  (let ((option1 (first widget-list))
        (scale   (second widget-list))
        (option2 (third widget-list)))
    (setf (value option1) (atype attachment))
    (case (atype attachment)
          ((:position :self)
               (setf (value scale) (form-pos attachment))
               (set-motif-resources scale :title-string "Position"))
          (t
               (when (> (offset attachment) 100)
                     (set-motif-resources scale :maximum (offset attachment)))
               (setf (value scale) (offset attachment))
               (set-motif-resources scale :title-string "Offset")))

    ;; check child list for validity
    (when (or (/= (length (value-list option2)) 
                  (length (children (parent-info info))))
              (loop for name in (value-list option2)
                   for child in (children (parent-info info))
                   when (not (equal name (widget-name child)))
                     do (return t)))
      (set-item-list option2
                     (loop for child in (children (parent-info info))
                           collect (widget-name child))))

    ;; set widget option sensitivity
    (loop for button in (option-buttons option2)
          for child in (children (parent-info info))
          do (set-motif-resources button :sensitive (not (eql child info))))

    (if (widget attachment)
        (setf (value option2) (widget-name (widget attachment)))
        ;; else only the first time
        (progn
          (setf (value option2)
              ;; more intelligent default based on top-bottom in the future
              (if (equal (widget-name info) (first (value-list option2)))
                  (second (value-list option2))
                  (first (value-list option2))))
          (find-attached-widget attachment (children (parent-info info))
                                (value option2))))))
