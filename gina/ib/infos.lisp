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

(setq *sccs-id* "@(#)infos.lisp	1.14	11/8/93")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class widget-info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Contains all information about a defined widget that must be stored
;;; in the document.

'(defclass widget-info ()
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Constructor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-widget-info (parent class
                         &key (document (when parent (document parent)))
                              (auto-name t)
			 &aux new-widget-info)
  "Make a widget-info without a widget"
  (setf new-widget-info
        (make-instance class :parent-info parent :document document))
  (unless (all-resources new-widget-info)
    (initialize-resources new-widget-info)
    (initialize-positional-resources new-widget-info)
    (initialize-key-resources new-widget-info)
    (initialize-motif-resources new-widget-info))
  ;(set-initial-values new-widget-info initial-values)
  (when (and document auto-name)
      (initialize-widget-name new-widget-info document))
  (when parent (add-child parent new-widget-info))
  new-widget-info)

(defmethod find-resource ((info widget-info) name)
  (gethash name (all-resources info)))

(defmethod initialize-positional-resources ((info widget-info))
  "Get a sorted list of the positional resources"
  (setf (positional-resources info) (loop for name in (positional-names info)
				      collect (find-resource info name))))

(defmethod initialize-key-resources ((info widget-info))
  "Get an unsorted list of keyword resources"
  (setf (key-resources info)
	(loop for name in (key-names info)
	      collect (find-resource info name))))

(defmethod initialize-motif-resources ((info widget-info) 
				       &aux non-motif-resources)
  "Get a list of the motif-resources"
  (setf non-motif-resources 
        (append '(:name :export) 
            (positional-names info) (key-names info) (callback-names info)))
  (setf (motif-resources info) nil)
  (maphash #'(lambda (key value)
	       (unless (member key non-motif-resources)
		 (push value (motif-resources info))))
	   (all-resources info)))

(defmethod initialize-widget-name ((info widget-info) document)
  "Create a new unique widget name for this info"
  (setf (resource-value (find-resource info :name) info)
    (string-downcase
      (format nil "~a-~a" (widget-class info) 
                          (new-instance-count document
                                        (widget-class info))))))

;;;;;;;;;;;;;;;;;;;; to be overridden by subclasses ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-resources ((info widget-info))
  (setf (all-resources info) (make-hash-table :test 'equal))
  (add-resources info 
                 (list (make-immediate-text-resource :name "" 
                            :access-mode :ignore)
                       (make-enumerative-resource :export :local
                            '(:local :widget :abstract :value)
                            :access-mode :ignore))))

(defmethod all-widgets ((info widget-info) gina-widget)
  "return list of all additional widget components"
  (declare (ignore gina-widget))
  nil)

(defmethod special-params-for-install ((info widget-info))
  "return list of key parameters to be used when creating the widget"
  nil)

(defmethod is-row-column-or-subclass ((info widget-info))
  "to test for resize hack for row-columns"
  nil)

(defmethod is-constraint-widget ((info widget-info))
  "if this info is a constraint manager"
  nil)
;; now also works for children of dialogs!

(defmethod make-template ((info widget-info) parent)
  "create a template widget, disconnecting all translations"
  (make-label parent "Label Template"))

(defmethod prefers-variable-size ((info widget-info) horizontal)
  "whether widget is usually variable in this direction in a form"
  (declare (ignore horizontal))
  nil)


;;;;;; to be used by info-classes to define available resources ;;;;;;;;;;;;;;

(defmethod add-resources ((info widget-info) resource-list)
  (loop for resource in resource-list do
    (setf (table-index resource) (num-resources info))
    (incf (num-resources info) 1)
    (setf (gethash (resource-name resource) (all-resources info)) resource)))

(defmethod override-default ((info widget-info) name new-default)
  (override-default-value (find-resource info name) new-default))

;;;;;;;;;;;;;;;;;;;;; collecting lists of current values ;;;;;;;;;;;;;;;;;;;;;;

(defmethod find-positional-values ((info widget-info))
  (loop for resource in (positional-resources info)
	collect (resource-value resource info)))

(defmethod find-key-values ((info widget-info)
                            &key (resource-list (key-resources info)))
  (loop for resource in resource-list
        when (or (resource-modified resource info)
		 (member (resource-name resource)
			 (required-key-names info)))
        collect (resource-name resource) and
        collect (resource-value resource info)))

(defmethod find-motif-values ((info widget-info))
  (loop for resource in (motif-resources info)
	when (or (resource-modified resource info)
		 (member (resource-name resource)
			 (required-key-names info)))
	collect (resource-name resource) and
        collect (resource-value resource info)))

(defmethod named-resource-value ((info widget-info) resource-name)
  (resource-value (find-resource info resource-name) info))

;;;;;;;;;;;;;;;;;;;;;;; Check for special resources ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod widget-name ((info widget-info))
  (resource-value (find-resource info :name) info))

(defmethod important-resources ((info widget-info))
  (loop for resource in (motif-resources info)
        when (member (resource-name resource) (important-names info))
        collect resource))

(defmethod callback-resources ((info widget-info))
  (loop for name in (callback-names info)
        collect (find-resource info name)))

(defmethod dialog-class ((info widget-info) &aux resource)
  (setq resource (find-resource info :class))
  (if resource
      (resource-value resource info)
      "undefined-class"))

(defmethod needs-doc ((info widget-info) &aux resource)
  (setq resource (find-resource info :needs-doc))
  (if resource
      (resource-value resource info)
      t))

;;;;;;;;;;;;;;;;;;;;;;; Manage Hierarchy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reparent ((widget-info widget-info) new-parent)
  (remove-child (parent-info widget-info) widget-info)
  (add-child new-parent widget-info))

(defmethod find-toplevel-object ((child-widget widget-info) &aux top)
  (setf top child-widget)
  (loop while (parent-info (parent-info top))
	do (setf top (parent-info top)))
  top)

(defmethod add-child ((parent widget-info) child)
  (setf (parent-info child) parent)
  (setf (children parent) (append (children parent) (list child))))

(defmethod remove-child ((parent widget-info) child)
  (setf (children parent) (remove child (children parent)))
  (setf (parent-info child) nil)
  (remove-dialogs child))

(defmethod remove-dialogs ((info widget-info))
  (when (res-dialog info)
      (pop-down (res-dialog info)))
  (when (is-symbolic (plate info))
      (switch-symbolic-state (plate info) nil))
  (loop for child in (children info) do
      (remove-dialogs child)))

(defmethod is-constraint-child ((info widget-info))
  "if this info is child of a constraint widget"
  (is-constraint-widget (parent-info info)))

(defmethod find-widget-info ((info widget-info) name)
  (if (equal (widget-name info) name)
      info
      (first (loop for child in (children info)
                   for found = (find-widget-info child name)
                   when found
                   collect found)))) 

(defmethod find-all-widget-infos ((info widget-info))
  "Find all widget-infos that belong to the dialog-box"
  (apply 'append (cons (children info)
		       (loop for child in (children info)
			collect (find-all-widget-infos child)))))

(defmethod find-all-of-a-class ((info widget-info) class)
  (append
      (when (eql (widget-class info) class)
          (list info))
      (loop for child in (children info)
            append (find-all-of-a-class child class))))

;;;;;;;;;;;;;;;;;; saving and reading info information ;;;;;;;;;;;;;;;;;;;;;;;

(defmethod info-to-stream ((info widget-info) stream &aux motif-values)
  "write information about this info and children, starting with class"
  (format stream "ib::~a-info ~d ~d ~d ~d ~s " 
                 (symbol-name (widget-class info))
                 (x-pos info) (y-pos info)
                 (width info) (height info)
                 (widget-name info))

  (setq motif-values
        (loop for resource in (append (motif-resources info) 
                                      (callback-resources info))
              when (resource-modified resource info)
              collect (resource-name resource) and
              collect (resource-value resource info)))
  (format stream "~d~%" (+ (length (positional-resources info))
                          (length (key-resources info))
                          (/ (length motif-values) 2)))
  (loop for resource in (positional-resources info)
        do (format stream "~s ~s " 
                          (resource-name resource)
                          (resource-value resource info)))
  (loop for resource in (key-resources info)
        do (format stream "~s ~s " 
                          (resource-name resource)
                          (resource-value resource info)))
  (format stream "~{~s ~}~%" motif-values)

  (if (constraints info)
      (constraints-to-stream (constraints info) stream)
      (format stream ":no-constraints~%"))

  (format stream "~d~%" (length (children info)))
  (loop for child in (children info)
        do (info-to-stream child stream)))

(defun convert-old-class (read-symbol)
  (let* ((string (symbol-name read-symbol))
         (part1 (subseq string 0 (- (length string) 5)))
         (part2 (subseq string (- (length string) 5))))
    (if (equal part2 "PLATE")
        (read-from-string (concatenate 'simple-string "ib::" part1 "info"))
        read-symbol)))

;; CHANGES: does not install plate nor check constraints
(defun info-from-stream (class parent-info document stream 
                         &aux new-info no-children constraint-class no-inits)
  "read generic info information and create info"
  (setq new-info 
        (make-widget-info parent-info (convert-old-class class)
                         :document document :auto-name nil))

  (setf (x-pos new-info) (read stream))
  (setf (y-pos new-info) (read stream))
  (setf (width new-info) (read stream))
  (setf (height new-info) (read stream))
  (setf (resource-value (find-resource new-info :name) new-info) (read stream))

  (setq no-inits (read stream))
  (loop for i from 1 to no-inits do
        (setf (resource-value (find-resource new-info (read stream)) new-info) 
              (read stream)))

  (setq constraint-class (read stream))
  (unless (equal constraint-class :no-constraints)
     (setf (constraints new-info) (make-instance constraint-class))
     (constraints-from-stream (constraints new-info) stream))

  (setq no-children (read stream))

  (loop for i from 1 to no-children
        as class = (read stream)
        do (info-from-stream class new-info document stream))

  (loop for child in (children new-info)
        when (constraints child)
        do (check-constraints (constraints child) child (children new-info)))
           ;(update-constraints (constraints child) child)
  new-info)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; resource dialogs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod resource-dialog ((info widget-info))
  "Create resource dialog if necessary and pop it up"
  (with-clock-cursor
   (unless (res-dialog info)
           (create-resource-dialog info))
   (when (res-dialog info)
         (when (plate info)
             (setf (relative-widget (res-dialog info))
                   (gina-widget (plate info))))
	 (pop-up (res-dialog info)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bounding box ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bounding-box (list-of-infos)
  (let ((min-x 1000) (max-x 0) (min-y 1000) (max-y 0))
     (loop for info in list-of-infos
           do (setq min-x (min min-x (x-pos info)))
              (setq max-x (max max-x (+ (x-pos info) (width info))))
              (setq min-y (min min-y (y-pos info)))
              (setq max-y (max max-y (+ (y-pos info) (height info)))))
     (list min-x min-y (- max-x min-x) (- max-y min-y))))
