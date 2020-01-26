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

(setq *sccs-id* "@(#)resources.lisp	1.14	11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'resource
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Every Motif resource field is present as an instance of this class or
;;; its subclasses.

'(defclass motif-resource ()
  ((resource-name :accessor resource-name :initarg :resource-name)
   (default-value :accessor default-value :initarg :default-value )
   (access-mode   :accessor access-mode   :initarg :access-mode)
   (print-length  :accessor print-length  :initform 20 :allocation :class)
   (table-index   :accessor table-index)
   (in-output     :accessor in-output     :initform nil))
  (:documentation "A resource widget"))

;; Define a setf-method for accessing the resource-value of a resource-slot
(defun resource-value (resource info)
  (if (resource-values info)
      (svref (resource-values info) (table-index resource))
    (default-value resource)))

(defun set-resource-value (resource info new-value)
  (when (null (resource-values info))
    (setf (resource-values info) (make-array (num-resources info)))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (setf (svref (resource-values info) (table-index value))
		   (default-value value)))
	     (all-resources info)))
  (setf (svref (resource-values info) (table-index resource)) new-value))

(defsetf resource-value set-resource-value)

;; Define a setf-method for accessing the resource-widget of a resource-slot
(defun resource-widget (resource info)
  (svref (resource-widgets info) (table-index resource)))

(defun set-resource-widget (resource info new-value)
  (when (null (resource-widgets info))
    (setf (resource-widgets info) (make-array (num-resources info))))
  (setf (svref (resource-widgets info) (table-index resource)) new-value))

(defsetf resource-widget set-resource-widget)

(defun make-motif-resource (resource-name default-value access-mode
					  &key (class 'motif-resource) 
					  (initargs nil))
  "create a new resource object"
  (apply #'make-instance (append (list class
				       :default-value default-value
				       :access-mode access-mode
				       :resource-name resource-name
				       :default-value default-value)
				 initargs)))

(defmethod print-name ((resource motif-resource))
  (format nil (format nil "~~~d,,a" (print-length resource))
	  (resource-name resource)))

(defmethod override-default-value ((resource motif-resource) new-value)
  (setf (default-value resource) new-value))

(defmethod resource-modified ((resource motif-resource) (info widget-info))
  (not (equal (resource-value resource info) (default-value resource))))

(defmethod display-resource-value ((resource motif-resource)
				   (info widget-info) value)
  "Display changed resource value in dialog"
  (declare (ignore value))
  (warning-dialog "Resource editing not implemented yet"))

(defmethod create-resource-widget ((resource motif-resource) parent info)
  "Make a widget for editing the resource"
  (declare (ignore parent info))
  (warning-dialog "Resource editing not implemented yet"))

(defmethod handle-new-resource-value ((resource motif-resource)
				      (plate widget-plate) value)
  "display a changed resource value in the widget"
  (when (gina-widget plate)
	(case (access-mode resource)
	      (:set-motif-resources
	       (set-motif-resources (gina-widget plate) (resource-name resource)
				    value))
              (:set-multiple
               (set-motif-resources (gina-widget plate) (resource-name resource)
				    value)
               (loop for wi in (all-widgets (info plate) (gina-widget plate)) 
                     when wi do
                       (set-motif-resources wi (resource-name resource) value)))
              (:ignore          )
	      (t
	       (funcall (access-mode resource) (gina-widget plate)
			value)))
	(adapt-to-widget-size (plate (find-toplevel-object (info plate))))))

(defmethod update-resource-widget ((resource motif-resource) 
                                   (info widget-info) value)
  "display a changed resource value in the resource dialog, if any"
  (when (and (res-dialog info)
             (resource-widget resource info))
      (setf (in-output resource) t)
      (display-resource-value resource info value)
      (setf (in-output resource) nil)))

(defun set-new-resource-value (resource info &rest rest)
  "create a resource-change-command if new value requested by user"
  (unless (or (in-output resource)
              (equal (first rest) (resource-value resource info)))
    (make-change-resource-command (document info) info resource (first rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'boolean-resource
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass boolean-resource (motif-resource)
  ()
  (:documentation "A boolean resource"))

(defun make-boolean-resource (resource-name default-value
			       &key (access-mode :set-motif-resources))
  (make-motif-resource resource-name default-value access-mode
		       :class 'boolean-resource))

(defmethod create-resource-widget ((resource boolean-resource) parent info)
  (setf (resource-widget resource info)
	(make-toggle-button parent (print-name resource)
			    :value (resource-value resource info)))
  (setf (value-changed-callback (resource-widget resource info))
	(make-callback #'set-new-resource-value resource info)))

(defmethod display-resource-value ((resource boolean-resource)
				   (info widget-info) value)
  "Display changed resource value in dialog"
  (setf (value (resource-widget resource info)) value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'item-list-resource
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass item-list-resource (motif-resource)
  ()
  (:documentation "A list of items"))

(defun make-item-list-resource (resource-name default-value
				&key (access-mode 'set-item-list))
  (make-motif-resource resource-name default-value access-mode
		       :class 'item-list-resource))

(defmethod create-resource-widget ((resource item-list-resource) parent info
                                   &aux scroller)
  (setq scroller
    (make-scroller parent :scrolling-policy :application-defined))
  (setf (resource-widget resource info)
    (make-text scroller 
               :value (text-from-item-list (resource-value resource info))
               :edit-mode :multi-line-edit :rows 4
                                    ;;:label-string (print-name resource)
				    ))
  (setf (value-changed-callback (resource-widget resource info))
        (make-callback #'set-new-item-list resource info)))

(defun text-from-item-list (item-list)
  "transform an item list into a multi-line text"
  (replace-returns
      (format nil "~{~a~%~}" 
         (loop for item in item-list
               collect (if (stringp item)
                           item
                           (item-representation item))))))

(defun item-representation (item)
  "create the presentation string of a single item pair"
  (concatenate 'simple-string 
               (first item)
               (string-downcase (format nil "#~s" (second item)))))

(defun item-conversion (item &aux delimit value)
  "create an item pair from a single text line"
  (setq delimit (position #\# item))
  (when delimit 
      (setq value 
            (ignore-errors
                (read-from-string (subseq item (1+ delimit))))))
  (if delimit
      (if value
          (list (list (subseq item 0 delimit) value))
          (list (subseq item 0 delimit)))
      (when (> (length item) 0) (list item))))
  
(defun set-new-item-list (resource info &rest rest &aux next val item-list)
  "create an item list from a multi-line text"
  (declare (ignore rest))
  (setq next 0)
  (setq val (unreplace-returns (value (resource-widget resource info))))
  (setq item-list
    (loop for  end = (position #\newline val :start next)
          while end
          append (item-conversion (subseq val next end))
          do (setq next (1+ end))))
  (when (< next (length val)) 
      (setq item-list 
            (append item-list (item-conversion (subseq val next)))))
  (set-new-resource-value resource info item-list))
  
(defmethod display-resource-value ((resource item-list-resource)
				   (info widget-info) value)
  "Display changed resource value in dialog"
  (setf (value (resource-widget resource info))
        (text-from-item-list value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'color-resource
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass color-resource (motif-resource)
  ((color-wheel   :accessor color-wheel :initform nil))
  (:documentation "A color resource"))

(defun make-color-resource (resource-name default-value
			    &key (access-mode :set-motif-resources))
  (make-motif-resource resource-name default-value access-mode
		       :class 'color-resource))

(defmethod create-resource-widget ((resource color-resource) parent info
				   &aux row)
  (setf row (make-row-column parent :orientation :horizontal))
  (make-push-button row (print-name resource)
		    :activate-callback
		    (make-callback #'use-color-wheel resource info))
  (setf (resource-widget resource info)
	(make-label row (resource-value resource info))))

(defmethod use-color-wheel ((resource color-resource) info)
  (unless (color-wheel resource)
	  (setf (color-wheel resource) 
		(make-color-wheel resource info)))
  (pop-up (dialog-box (color-wheel resource))))

(defmethod display-resource-value ((resource color-resource)
				   (info widget-info) value)
  "Display changed resource value in dialog"
  ;; don't know
  (declare (ignore value))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'text-resource
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass text-resource (motif-resource)
  ((columns :accessor columns :initform 20 :initarg :columns))
  (:documentation "A text resource"))

(defun make-text-resource (resource-name default-value
			   &key (access-mode :set-motif-resources)
				(columns 20) 
				(class 'text-resource))
  (make-motif-resource resource-name default-value access-mode
		       :class class :initargs `(:columns ,columns)))

(defmethod create-resource-widget ((resource text-resource) parent info)
  (setf (resource-widget resource info)
	(make-labeled-text parent (print-name resource)
			   :columns (columns resource)
			   :value (format nil "~d"
					  (resource-value resource info))))
  (setf (activate-callback (resource-widget resource info))
	(make-callback #'set-new-resource-value resource info)))

(defmethod display-resource-value ((resource text-resource)
				   (info widget-info) value)
  "Display changed resource value in dialog"
  (setf (value (resource-widget resource info)) value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'immediate-text-resource
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass immediate-text-resource (text-resource)
  ((columns :accessor columns :initform 20 :initarg :columns))
  (:documentation "A text resource"))

(defun make-immediate-text-resource (resource-name default-value
				     &key (access-mode :set-motif-resources)
					  (columns 20) 
					  (class 'immediate-text-resource))
  (make-text-resource resource-name default-value
		      :access-mode access-mode :columns columns :class class))

(defmethod create-resource-widget :after ((resource immediate-text-resource) 
					  parent info)
  (declare (ignore parent))
  (setf (value-changed-callback (resource-widget resource info))
	(make-callback #'set-new-resource-value resource info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'numeric-resource
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass numeric-resource (text-resource)
  ()
  (:documentation "A numeric resource"))

(defun  make-numeric-resource (resource-name default-value
			       &key (access-mode :set-motif-resources)
				    (columns 10))
  (make-text-resource resource-name default-value 
		      :access-mode access-mode
		      :columns columns
		      :class 'numeric-resource))

(defmethod create-resource-widget ((resource numeric-resource) parent info)
  (setf (resource-widget resource info)
    (make-labeled-text parent (print-name resource)
		       :columns (columns resource)
		       :value (format nil "~d"
				      (resource-value resource info))))
  (setf (activate-callback (resource-widget resource info))
	(make-callback #'set-numeric-resource-value resource info))
  (setf (losing-focus-callback (resource-widget resource info))
	(make-callback #'set-value-after-focus resource info))
  (setf (gina::modify-verify-callback (resource-widget resource info))
    (make-callback #'verify-numeric resource)))

(defmethod set-numeric-resource-value ((resource numeric-resource) info value)
  (let ((new-value (read-from-string value)))
     (when (and (integerp new-value)
                (not (= new-value (resource-value resource info))))
         (set-new-resource-value resource info new-value))))

(defmethod set-value-after-focus ((resource numeric-resource) info &rest rest)
  (declare (ignore rest))
  (let ((new-value (read-from-string (value (resource-widget resource info)))))
     (when (and (integerp new-value)
                (not (= new-value (resource-value resource info))))
         (set-new-resource-value resource info new-value))))

(defmethod verify-numeric ((resource numeric-resource) &rest call-data
			   &aux str length)
  (setf str (fifth call-data)
	length (sixth call-data))
  (when (and (zerop length) (equal 1 (first call-data)) 
	     (equal 1 (second call-data)) (zerop (third call-data))
	     (equal 1 (fourth call-data)))
    (return-from verify-numeric nil))
  (dotimes (i length)
    (unless (digit-char-p (schar str i))
      (return-from verify-numeric nil)))
  t)

(defmethod display-resource-value ((resource numeric-resource)
				   (info widget-info) value)
  "Display changed resource value in dialog"
  (setf (value (resource-widget resource info)) (format nil "~d" value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'limited-numeric-resource
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass limited-numeric-resource (motif-resource)
  ((minimum :accessor minimum :initform 0 :initarg :minimum)
   (maximum :accessor maximum :initform 100 :initarg :maximum))
  (:documentation "A limited numeric resource"))

(defun make-limited-numeric-resource (resource-name default-value
				      min max
				      &key (access-mode :set-motif-resources))
  (make-motif-resource resource-name default-value access-mode
		       :class 'limited-numeric-resource
		       :initargs `(:minimum ,min :maximum ,max )))

(defmethod create-resource-widget ((resource limited-numeric-resource) 
				   parent info)
  (setf (resource-widget resource info)
	(make-scale parent 
		    :orientation :horizontal
		    :minimum (minimum resource)
		    :maximum (maximum resource)
		    :title-string (print-name resource)
		    :value (resource-value resource info)))
  (setf (value-changed-callback (resource-widget resource info))
	(make-callback #'set-new-resource-value resource info)))

(defmethod display-resource-value ((resource limited-numeric-resource)
				   (info widget-info) value)
  "Display changed resource value in dialog"
  (setf (value (resource-widget resource info)) value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'enumerative-resource
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass enumerative-resource (motif-resource)
  ((possible-values :accessor possible-values :initarg :possible-values)
   (cplus-prefix    :accessor cplus-prefix  :initarg :cplus-prefix))
  (:documentation "An enumerative resource"))

(defun make-enumerative-resource (resource-name default-value items
				  &key (access-mode :set-motif-resources)
                                       (cplus-prefix "Xm"))
  (make-motif-resource resource-name default-value access-mode
		       :class 'enumerative-resource
		       :initargs `(:possible-values ,items
                                   :cplus-prefix ,cplus-prefix)))

(defmethod create-resource-widget ((resource enumerative-resource) parent info)
  (setf (resource-widget resource info)
	(make-option-menu parent 
			  (mapcar #'(lambda (item)
					    (list (symbol-name item) item))
				  (possible-values resource))
			  :initial-value (resource-value resource info)
			  :label-string (print-name resource)))
  (setf (value-changed-callback (resource-widget resource info))
	(make-callback #'set-new-resource-value resource info)))

(defmethod display-resource-value ((resource enumerative-resource)
				   (info widget-info) value)
  "Display changed resource value in dialog"
  (setf (value (resource-widget resource info)) value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Definitions for class 'callback-resource
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass callback-resource (motif-resource)
  ((default-params :accessor default-params :initarg :default-params))
  (:documentation "A callback"))

(defun make-callback-resource (resource-name default-params
				&key (access-mode :ignore))
  (make-motif-resource resource-name '("<proc>" "<obj>") access-mode
		       :class 'callback-resource
                       :initargs (list :default-params default-params)))

(defmethod create-resource-widget ((resource callback-resource) parent info
                                   &aux form list1 list2 button1 button2
                                        button0 label1 text)
  ;; hack to prevent modification of the default value by destructive ops
  (setf (resource-value resource info)
        (copy-list (resource-value resource info)))

  (setq form (make-form parent))
  (setq label1 (make-label form (print-name resource)))
  (setq button0 (make-push-button form "Clear"))
  (setq list1 (make-selection-list form 
                   (append-default-params 
                       (resource-value resource info) resource)
                   :visible-item-count 6))
  (setf (resource-widget resource info) list1)
  (setq list2 (make-scrollable-selection-list form nil
                  :motif-resources '(:list-size-policy :variable)))
  (setq text (make-text form))
  (setf (value-changed-callback list1)
        (make-callback #'show-selectable-items resource info list2 text))
  (setf (value-changed-callback list2)
        (make-callback #'select-object-param resource info list1))
  (setf (value-changed-callback text)
        (make-callback #'edit-object-param resource info list1))
  (setq button1 (make-push-button form "Add Arg"))
  (setf (activate-callback button1)
        (make-callback #'insert-param resource info))
  (setq button2 (make-push-button form "Delete"))
  (setf (activate-callback button2)
        (make-callback #'delete-param resource info))
  (setf (activate-callback button0)
        (make-callback #'clear-callback resource info list1 list2 text))
  (define-form-constraint label1
      :top-attachment :form :left-attachment :form)
  (define-form-constraint button0 :top-attachment :form :left-attachment :none
      :right-attachment :form)
  (define-form-constraint list1
      :left-attachment :form 
      :right-attachment :position :right-position 45
      :top-attachment :widget :top-widget button0 :top-offset 2)
  (define-form-constraint list2
      :top-attachment :widget :top-widget button0 :top-offset 2
      :left-attachment :widget :left-widget list1 :left-offset 6
      :right-attachment :form
      :bottom-attachment :opposite-widget :bottom-widget list1)
  (define-form-constraint button1
      :left-attachment :form
      :top-attachment :widget :top-widget list1 :top-offset 4)
  (define-form-constraint button2
      :left-attachment :widget :left-widget button1 :left-offset 4
      :top-attachment :widget :top-widget list1 :top-offset 4)
  (define-form-constraint text
      :top-attachment :widget :top-widget list1 :top-offset 2
      :left-attachment :widget :left-widget list1 :left-offset 6
      :right-attachment :form))

(defun clear-callback (resource info list1 list2 text)
  (declare (ignore list1))
  (set-new-resource-value resource info '("<proc>" "<obj>"))
  (update-callback-list resource info 1)
  (set-item-list list2 nil)
  (setf (value text) ""))

;(defun insert-param (resource info)
;  (let ((sel (value (resource-widget resource info)))
;        (current (resource-value resource info)))
;    (unless (or (not sel) (> sel 1)) (return-from insert-param))
;    (set-new-resource-value resource info
;          (if sel
;              (append (subseq current 0 (1- sel))
;                      '("<arg>")
;                      (subseq current (1- sel)))
;              (append current
;                      '("<arg>"))))
;    (update-callback-list resource info)
;    (setf (value (resource-widget resource info))
;          (if sel (1+ sel) (1+ (length current))))))

(defun insert-param (resource info)
  (let ((current (resource-value resource info))
        (sel (value (resource-widget resource info))))
     (set-new-resource-value resource info (append current '("<arg>")))
     (update-callback-list resource info (1+ (length current)))
     (execute (value-changed-callback (resource-widget resource info))
              (list (value (resource-widget resource info)) sel))
  ))

(defun delete-param (resource info)
  (let ((current (resource-value resource info))
        (sel (value (resource-widget resource info))))
    (when (and (> (length current) 2)
               sel
               (> sel 2))
        (set-new-resource-value resource info
              (append (subseq current 0 (1- sel))
                      (subseq current sel)))
        (update-callback-list resource info
              (if (< sel (length current)) sel (1- (length current))))
        (execute (value-changed-callback (resource-widget resource info))
                 (list (value (resource-widget resource info)) sel))
  )))

(defun show-selectable-items (resource info other-list text value old-value)
  (unless value
    (set-item-list other-list nil)
    (setf (value text) ""))
  (when value
    (if (> value 1)
        (progn
          (when (or (not old-value) (= old-value 1))
            (set-item-list other-list
              (append 
                  '("<Constructor>" "<Document>")
                  (list (widget-name (main-info (document info))))
                  (loop for one-info in (find-all-widget-infos 
                                         (main-info (document info)))
                        collect (widget-name one-info)))))
            (setf (value text) ""))
        (progn
          (let ((second-info (find-widget-info (main-info (document info))
                                   (second (resource-value resource info)))))
             (set-item-list other-list
                 (when (and second-info
                            (eql (widget-class second-info) 'view)
                            (equal (dialog-class second-info) "ib-demo-view"))
                     '("set-wavelength" "set-phase" "set-amplitude"
                       "set-style" "set-thickness" "set-background"))))
          (setf (value text) 
                (let ((procname (first (resource-value resource info))))
                   (if (string= procname "<proc>") "" procname)))))))

(defun select-object-param (resource info other-list value old-value)
  (declare (ignore old-value))
  (let ((sel (value other-list)))
    (unless sel (return-from select-object-param))
    (set-new-resource-value resource info
          (my-replace (resource-value resource info) (list value)
                   :start1 (1- sel) :end1 sel))
    (update-callback-list resource info sel)))

(defun edit-object-param (resource info other-list value)
  (let ((sel (value other-list)))
    (when (or (not sel)
              (equal value "")
              (equal value (nth (1- sel) (resource-value resource info))))
        (return-from edit-object-param))
      (set-new-resource-value resource info
            (my-replace (resource-value resource info) (list value)
                     :start1 (1- sel) :end1 sel))
    (update-callback-list resource info sel)))

(defun my-replace (list replace-list &key (start1 0) (end1 (length list)))
  (append (subseq list 0 start1)
          replace-list
          (subseq list end1)))

(defun append-default-params (list resource)
  (append
      (loop for item in list
            for i from 1
            collect (list item i))
      (loop for item in (default-params resource)
            collect (list item nil))))

(defmethod display-resource-value ((resource callback-resource)
				   (info widget-info) value)
  "Display changed resource value in dialog"
  (set-item-list (resource-widget resource info)
        (append
            (loop for item in value
                  for i from 1
                  collect (list item i))
            (loop for item in (default-params resource)
                  collect (list item nil))))
    (execute (value-changed-callback (resource-widget resource info))
             (list 1 2)))

(defun update-callback-list (resource info new-value)
  (set-item-list (resource-widget resource info)
        (append
            (loop for item in (resource-value resource info)
                  for i from 1
                  collect (list item i))
            (loop for item in (default-params resource)
                  collect (list item nil)))
        :selected-value new-value))
