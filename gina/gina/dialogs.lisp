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

(setq *sccs-id* "@(#)dialogs.lisp	1.22  11/8/93")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;            special predifined dialogs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; predefined standard dialogs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ginachapter "Predefined Standard Dialogs")

(defginafun warning-dialog (message
			     &key 
			     (document (first (document-list *application*)))
			     (modal t)
			     (dialog-title "Warning")
			     (icon-pixmap "default_xm_error")
			     "icon used (error warning information question working)"
			     &aux dialog-box label button icon)
  (description "pop a a warning message as a modal dialog")
  (example (with-application-stopped
	     (warning-dialog "Error: CPU not found!"))
	   (with-application-stopped
	     (warning-dialog "Be very very careful !!"
			     :modal nil
			     :icon-pixmap "default_xm_information")))
  
  (setq dialog-box
	(if modal (make-modal-dialog-box dialog-title :document document)
	       (make-modeless-dialog-box dialog-title :document document)))
  
  (setq icon (make-label dialog-box icon-pixmap :label-type :pixmap))
  (define-form-constraint icon 
			  :top-attachment :form :top-offset 10
			  :left-attachment :form :left-offset 10)
  
  (setq label (make-label dialog-box message :alignment :beginning))
  (define-form-constraint label
			  :top-attachment  :form :top-offset 15 
			  :left-attachment :widget :left-widget icon
			  :left-offset 10
			  :right-attachment :form :right-offset 10)
  
  (setq button (make-dialog-dismiss-button dialog-box "  OK  " 
					   :default-button t :result t))
  (define-form-constraint button
			  :top-attachment :widget
			  :top-widget label
			  :top-offset 20
			  :bottom-attachment :form :bottom-offset 10
			  :left-attachment   :form :left-offset   10)
  (pop-up dialog-box) ;; blocks when modal
  (when modal (destroy (dialog-shell dialog-box))) ;; wg. motif-bug
  t)

(defginafun question-dialog (prompt
			      &key (document (first (document-list *application*)))
			      (dialog-title "Question")
			      (default "")
			      (icon-pixmap "default_xm_question")
			      "icon used (error warning information question working)"
			      &aux dialog-box label text icon
			           button-row (answer nil))
  (description "request an input string from the user"
	       :result "the string entered by the user or NIL if cancelled")
  (example (with-application-stopped
	     (question-dialog "Filename:" :default "Untitled")))
  
  (setq dialog-box (make-modal-dialog-box dialog-title :document document))
  (setq icon (make-label dialog-box icon-pixmap :label-type :pixmap))
  (define-form-constraint icon 
			  :top-attachment :form :top-offset 10
			  :left-attachment :form :left-offset 10)
  (setq label (make-label dialog-box prompt :alignment :beginning))
  (define-form-constraint label
			  :top-attachment  :form :top-offset 15 
			  :left-attachment :widget :left-widget icon
			  :left-offset 10
			  :right-attachment :form :right-offset 10)
  (setq text (make-text dialog-box :value default
			:columns 30))
  (define-form-constraint text
			  :top-attachment  :widget :top-widget label :top-offset  10 
			  :left-attachment :form :left-offset 10
			  :right-attachment :form :right-offset 10)
  (setq button-row (make-dialog-dismiss-button-row
		     dialog-box :item-list '(("  OK  " :ok) (" Cancel " :cancel))))
  (define-form-constraint button-row
			  :top-attachment  :widget :top-widget text :top-offset  10 
			  :left-attachment :form :left-offset 10
			  :right-attachment :form :right-offset 10
			  :bottom-attachment :form :bottom-offset 10)
  
  ;; bring up dialog box and wait
  (pop-up dialog-box)
 
  ;; get the modified text before destroying the dialog-box
  (when (eq (result dialog-box) :ok)
    (setq answer (value text)))

  (destroy (dialog-shell dialog-box)) ;; wg. motif-bug

  ;; return the modified text
  answer)

(defginafun confirm-dialog (prompt
			     &key (document (first (document-list *application*)))
		             (dialog-title "Confirm")
			     (icon-pixmap "default_xm_question")
			     "icon used (error warning information question working)"
			     &aux dialog-box label button-row icon)
  (description "ask the user to confirm or reject"
	       :result "T or NIL")
  (example (with-application-stopped
	     (confirm-dialog "Reformat disk?")))
  
  (setq dialog-box (make-modal-dialog-box dialog-title :document document))
  (setq icon (make-label dialog-box icon-pixmap :label-type :pixmap))
  (define-form-constraint icon 
			  :top-attachment :form :top-offset 10
			  :left-attachment :form :left-offset 10)
  (setq label (make-label dialog-box prompt :alignment :beginning))
  (define-form-constraint label
			  :top-attachment  :form :top-offset  15 
			  :left-attachment :widget :left-widget icon
			  :left-offset 10
			  :right-attachment :form :right-offset 10)
  (setq button-row (make-dialog-dismiss-button-row
		     dialog-box :item-list '(("  OK  " :ok) ("Cancel" :cancel))))
  (define-form-constraint button-row
			  :top-attachment  :widget :top-widget label :top-offset  10 
			  :left-attachment :form :left-offset 10
			  :right-attachment :form :right-offset 10
			  :bottom-attachment :form :bottom-offset 10)
  
  ;; bring up dialog box and wait
  (pop-up dialog-box)
  
  (destroy (dialog-shell dialog-box)) ;; wg. motif-bug
  
  ;; return whether confirmed
  (eq (result dialog-box) :ok))

(defginafun alternative-dialog (prompt
			&key (document (first (document-list *application*)))
		             (dialog-title "Alternative")
			     (icon-pixmap "default_xm_question")
			     "icon used (error warning information question working)"
			&aux dialog-box label button-row icon)
  (description "ask the user yes , no or cancel"
	       :result "one of :yes :no and :cancel")
  (example (with-application-stopped
	     (alternative-dialog "Save Document ?")))
  
  (setq dialog-box (make-modal-dialog-box dialog-title :document document))
  (setq icon (make-label dialog-box icon-pixmap :label-type :pixmap))
  (define-form-constraint icon 
			  :top-attachment :form :top-offset 10
			  :left-attachment :form :left-offset 10)
  (setq label (make-label dialog-box prompt :alignment :beginning))
  (define-form-constraint label
			  :top-attachment  :form :top-offset  15 
			  :left-attachment :widget :left-widget icon
			  :left-offset 10
			  :right-attachment :form :right-offset 10)
  (setq button-row (make-dialog-dismiss-button-row
		     dialog-box :item-list '(("  Yes   " :yes)
					     ("   No   " :no)
					     (" Cancel " :cancel))))
  ;; default if box is closed
  (setf (result dialog-box) :cancel)
  
  (define-form-constraint button-row
			  :top-attachment  :widget :top-widget label :top-offset  10
			  :bottom-attachment :form :bottom-offset 10
			  :left-attachment  :form :left-offset  10
			  :right-attachment :form :right-offset 10)
  
  ;; bring up dialog box and wait
  (pop-up dialog-box)

  (destroy (dialog-shell dialog-box)) ;; wg. motif-bug

  ;; return answer
  (result dialog-box))

(defginafun select-dialog (prompt item-list
			&key (document (first (document-list *application*)))
		             (dialog-title "Select")
			&aux dialog-box label selection-list button-row)
  (description "let the user select from a list of strings"
	       :result "the selected value or NIL if cancelled")
  (example (with-application-stopped
	     (select-dialog "Open which file?"
			    '("File 1" "Brief" "Untitled" "Ordner"
			      "Ordner1" "Ordner2" "Ordner3" "Ordner4"
			      "Ordner5" "Ordner6" "Ordner7" "Ordner8"))))
  
  (setq dialog-box (make-modal-dialog-box dialog-title :document document :resize t
					  ;;:motif-resources (list :width 300 :height 250)
					  :dialog-shell-resources (list :min-width 150
									:min-height 150)))
  (setq label (make-label dialog-box prompt))
  (setq selection-list (make-scrollable-selection-list
			 dialog-box item-list
			 :visible-item-count (min 10 (length item-list))
			 :width 250
			 ))
  (setq button-row (make-dialog-dismiss-button-row
		     dialog-box :item-list '(("  OK  " :ok) (" Cancel " :cancel))))
  (setf (default-action-button selection-list) (first (buttons button-row)))
  
  ;; layout definition:
  (define-form-constraint label
			  :top-attachment  :form :top-offset  10 
			  :left-attachment :form :left-offset 10)
  (define-form-constraint selection-list
			  :top-attachment  :widget :top-widget label :top-offset  10 
			  :left-attachment :form :left-offset 10
			  :right-attachment :form :right-offset 10
			  :bottom-attachment :widget :bottom-widget button-row
			  :bottom-offset 10)
  (define-form-constraint button-row
			  :top-attachment :none
			  :bottom-attachment :form :bottom-offset 10  
			  :left-attachment   :form :left-offset   10
			  ;; :right-attachment => button verschwindet bei resize!!??
			  ;;:right-attachment  :form :right-offset  10
			  ) 
  
  ;; bring up dialog box and wait
  (pop-up dialog-box)

  (destroy (dialog-shell dialog-box)) ;; wg. motif-bug

  ;; return the selected text if OK was pressed
  (when (eq (result dialog-box) :ok)
    (value selection-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  class open-dialog-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass open-dialog-box (modal-dialog-box)
   (;; instance parameters
    (file-type :accessor file-type :initarg :file-type)
    ;; instance variables
    (directory-menu :accessor directory-menu)
    (file-list      :accessor file-list))
   (:documentation "a modal dialog box to select an existing file"))

(defginafun make-open-dialog-box (wildcard
			     &key (file-type :wild)
			          (document (first (document-list *application*)))
			          (class 'open-dialog-box)
			     &aux box button-row open-button)
  (description "create a modal dialog box to select an existing file"
	       ;;:constructor-for-class open-dialog-box
	       :called-by-application :sometimes)
  (example (with-application-stopped
	     (pop-up (make-open-dialog-box "W1:>GINA>documents>*.*.newest"))))
  (setq box
	(make-modal-dialog-box "Open which file?"
			       :document document
			       :class class
			       :initargs (list :file-type file-type)
			       :resize t))
  ;;(resize box 300 300) ;; bug in form-widget !!

  (setf (directory-menu box)
	(make-option-menu box '(" ")
			  :value-changed-callback
			  (make-callback 'change-directory box)))
  
  (setf (file-list box)
	(make-scrollable-selection-list box '(" ") :width 300))
  ;; set file-list and directory-menu according to initial directory
  (change-directory box wildcard)
		   
  (setq button-row (make-row-column box
				    :orientation :horizontal :spacing 10))
  (setq open-button	
	(make-push-button button-row "Open"
			  :show-as-default 1
			  :activate-callback
			  `(lambda () (file-selected ',box
						     (value ',(file-list box))))
			  ))

  (setf (default-action-button (file-list box)) open-button)
  (setf (default-button box) open-button)


  (make-dialog-dismiss-button button-row "Cancel"
			      :dialog-box box
			      :result nil)
  
  (define-form-constraint (directory-menu box)
			  :top-attachment  :form :top-offset  10 
			  :left-attachment :form :left-offset 10)
  (define-form-constraint (file-list box)
			  :top-attachment  :widget
			  :top-widget (directory-menu box) :top-offset  10 
			  :left-attachment  :form :left-offset  10
			  :right-attachment :form :right-offset 10
			  :bottom-attachment :widget
			  :bottom-widget button-row
			  :bottom-offset 10)
  
  (define-form-constraint button-row
			  :top-attachment :none 
			  :bottom-attachment :form :bottom-offset 10   
			  :left-attachment   :form :left-offset 10)
  box)

(defmethod change-directory ((box open-dialog-box) directory-wildcard 
			     &optional old-value
			     &aux files
			          (directory-items nil) 
			          (document-items nil))
  "update file-list and directory-menu"
  (declare (ignore old-value))
  (with-clock-cursor
    (set-item-list
      (directory-menu box)
      (loop for directory in (parent-directory-wildcards directory-wildcard)
	    collect (list (directory-namestring directory) directory)))

    ;; lookup the directory contents (time consuming!)
    (setq files (sort (directory directory-wildcard) #'pathname-name<))

    ;; collect directories 
    (loop for file in files
     when (directoryp file)
     do (push (list (concatenate 'string "-> " (name-and-type file))
		    ;;(directory-namestring (subdirectory-wildcard file))
		    (subdirectory-wildcard file))
	      directory-items))

    ;; collect documents of desired file-type
    (loop for file in files
      when (or (eq (file-type box) :wild)
               (string-equal (file-type box) (pathname-type file)))
     do (push (list (if (eq (file-type box) :wild)
			(name-and-type file)
		      (pathname-name file))
		    file)
              document-items))

    ;; display directories and documents in the selection-list
    (set-item-list
      (file-list box)
      (reverse (append document-items directory-items)))))

(defmethod file-selected ((box open-dialog-box) pathname)
  "return pathname or open as directory"
  (setq pathname (pathname pathname))
  (if (wildp (pathname-name pathname))
      (change-directory box pathname)
      ;; else
      (progn
	(setf (result box) pathname)
	(pop-down box))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  class save-dialog-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass save-dialog-box (modal-dialog-box)
   (;; instance parameters
    (file-type :accessor file-type :initarg :file-type)
    ;; instance variables
    (directory-menu :accessor directory-menu)
    (file-list      :accessor file-list)
    (text-field     :accessor text-field))
   (:documentation "a modal dialog box to select a new file"))


(defginafun make-save-dialog-box (wildcard default-name
			     &key (file-type :wild)
			          (document (first (document-list *application*)))
			          (class 'save-dialog-box)
			     &aux box button-row save-button open-button)
  (description "create a modal dialog box to select a new file"
	       ;;:constructor-for-class save-dialog-box
	       :called-by-application :sometimes)
  (example (with-application-stopped
	     (pop-up (make-save-dialog-box "W1:>GINA>documents>*.*.newest" "Untitled"))))
  (setq box
	(make-modal-dialog-box "Save to which file?"
			       :document document
			       :class class
			       :initargs (list :file-type file-type)
			       :resize t))
  ;;(resize box 300 300) ;; bug in form-widget !!

  (setf (directory-menu box)
	(make-option-menu box '(" ")
			  :value-changed-callback
			  (make-callback #'change-directory box)))
  
  (setf (file-list box)
	(make-scrollable-selection-list box '(" ") :width 300))
  ;; set file-list and directory-menu according to initial directory
  (change-directory box wildcard)

  (setf (text-field box)
	(make-text box :value default-name))
  	   
  (setq button-row (make-row-column box
				    :orientation :horizontal :spacing 10))
  (setq save-button	
	(make-push-button button-row "Save"
			  :show-as-default 1
			  :activate-callback (make-callback #'save-pressed box)))
  (setf (default-button box) save-button) ;; return-key
  (setq open-button
	(make-push-button button-row "Open"
			  :activate-callback (make-callback #'open-pressed box)))

  (setf (default-action-button (file-list box)) open-button) ;; double-click

  (make-dialog-dismiss-button button-row "Cancel"
			      :dialog-box box
			      :result nil)
  
  (define-form-constraint (directory-menu box)
			  :top-attachment  :form :top-offset  10 
			  :left-attachment :form :left-offset 10)
  (define-form-constraint (file-list box)
			  :top-attachment  :widget
			  :top-widget (directory-menu box) :top-offset  10 
			  :left-attachment  :form :left-offset  10
			  :right-attachment :form :right-offset 10
			  :bottom-attachment :widget
			  :bottom-widget (text-field box)
			  :bottom-offset 10)
  (define-form-constraint (text-field box)
			  :top-attachment :none
			  :bottom-attachment :widget
			  :bottom-widget button-row
			  :bottom-offset 10
			  :left-attachment :form :left-offset 10
			  :right-attachment :form :right-offset 10)
  (define-form-constraint button-row
			  :top-attachment :none 
			  :bottom-attachment :form :bottom-offset 10   
			  :left-attachment   :form :left-offset 10)
  box)

(defmethod change-directory ((box save-dialog-box) directory-wildcard 
			     &optional old-value
			     &aux files
			          (directory-items nil) 
			          (document-items nil))
  "update file-list and directory-menu"
  (declare (ignore old-value))
  (with-clock-cursor
    (set-item-list
      (directory-menu box)
      (loop for directory in (parent-directory-wildcards directory-wildcard)
	    collect (list (directory-namestring directory) directory)))

    ;; lookup the directory contents (time consuming!)
    (setq files (sort (directory directory-wildcard) #'pathname-name<))

    ;; collect directories
    (loop for file in files
     when (directoryp file)
     do (push (list (concatenate 'string "-> " (name-and-type file))
		    ;;(directory-namestring (subdirectory-wildcard file))
		    (subdirectory-wildcard file))
	      directory-items))

    ;; collect documents of desired file-type
    (loop for file in files
      when (or (eq (file-type box) :wild)
               (string-equal (file-type box) (pathname-type file)))
     do (push (list (pathname-name file) file)
              document-items))

    ;; display directories and documents in the selection-list
    (set-item-list
     (file-list box)
     (reverse (append document-items directory-items)))))

(defmethod open-pressed ((box save-dialog-box) &aux pathname)
  "open subdirectory or copy file into the text field"
  (setq pathname (value (file-list box)))
  (if (wildp (pathname-name pathname))
      (change-directory box pathname)
      ;; else
      (progn
	(setf (value (text-field box)) (pathname-name pathname)))))

(defmethod save-pressed ((box save-dialog-box) &aux directory pathname)
  "return selected pathname"
  (setq directory (value (directory-menu box)))
  (setq pathname
	(ignore-errors 
	  (make-pathname :name (value (text-field box))
			 :type (file-type box)
			 :defaults directory)))
  (when (or (not pathname)
	    (not (file-exists pathname))
	    (confirm-dialog (format nil "Overwrite file ~s?" 
				    (pathname-name pathname))
			    :document (document box)))
    (if (not pathname)
	;; erroneous name component specified
	(progn
	  (xlib:bell *display*)
	  (warning-dialog "Illegal file name" :document (document box))
	  )
	;; else: okay
	(progn
	  (setf (result box) pathname)
	  (pop-down box)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; inspector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defginavar *inspected-object*
	    (description "the last object inspected"
			 :default nil)
	    (comment "It is modified each time any object is inspected."))
(setq *inspected-object* nil)

'(defclass inspector-box (modeless-dialog-box)
  (;; instance variables
    (path-text :accessor path-text)
    (class-label :accessor class-label)
    (object-list :accessor object-list)
    (slot-list :accessor slot-list)
    (button-row     :accessor button-row)
    (inspect-button :accessor inspect-button)
    (inspect-button2 :accessor inspect-button2)
    (browser-button :accessor browser-button))
   (:documentation "a dialog box showing object contents"))

(defun make-inspector-box (document &aux new-box path-label)
  "create dialog box to inspect a clos object"
  (setq new-box 
        (make-modeless-dialog-box "Inspector"
				  :resize t
				  :document document
				  :class 'inspector-box))

  (setf path-label (make-label new-box "Path:"))
  (setf (path-text new-box)
    (make-text new-box :value "document" :columns 40 :editable nil
	       :motif-resources '(:shadow-thickness 0)))
  (setf (class-label new-box)
    (make-label new-box "document" :alignment :alignment-beginning))
  
  (setf (object-list new-box)
    (make-option-menu new-box
		      (loop for obj in (list *application*
					     document
					     `(0 (undo-commands ,document))
					     `(main-view ,document))
			    for path in (list "*application*"
					   "*document*"
					   "(first (undo-commands *document*))"
					   "(main-view *document*)")
			    do (unless (eq obj document)
				 (setf (gethash obj
						(inspector-hash-table document))
				       path))
			    collect (list (format nil "~s" obj) obj))
		      :initial-value document
		      :value-changed-callback (make-callback 
					       #'change-inspection
					       new-box)))
  
  (setf (slot-list new-box)
    (make-scrollable-selection-list new-box '("a" "b" "c") :width 150))
  
  (setf (button-row new-box)
    (make-row-column new-box :orientation :horizontal :spacing 10))
	
  (setf (inspect-button new-box)
    (make-push-button (button-row new-box) "Inspect Obj"
		      :show-as-default 2
		      :activate-callback (make-callback #'inspect-selection 
							new-box t)))
  (setf (default-action-button (slot-list new-box)) (inspect-button new-box))

  (setf (default-button new-box) (inspect-button new-box))

  (setf (inspect-button2 new-box)
    (make-push-button (button-row new-box) "Inspect Path"
		      :activate-callback (make-callback #'inspect-selection 
							new-box nil)))
  (make-push-button (button-row new-box) "Pop"
		    :activate-callback (make-callback #'pop-entry new-box))
  
  (setf (browser-button new-box)
    (make-push-button (button-row new-box) "Browser"
		      ;;:sensitive (clos-object-p object)
		      :activate-callback
		      (make-callback #'focus-gina-browser new-box)))
    
  (make-push-button (button-row new-box) "Dismiss"
		    :activate-callback (make-callback #'closed-by-wm 
						      (dialog-shell new-box)))

  (define-form-constraint (object-list new-box)
      :top-attachment   :form :top-offset 10
      :left-attachment  :form :left-offset  10
      :right-attachment :none ;;:form :right-offset 10
      :bottom-attachment :none)
  (define-form-constraint path-label
      :top-attachment :widget :top-widget (object-list new-box) :top-offset 13
      :left-attachment :form :left-offset 10)
  (define-form-constraint (path-text new-box)
      :top-attachment :widget :top-widget (object-list new-box) :top-offset 10
      :left-attachment :widget :left-widget path-label :left-offset 2
      :right-attachment :form :right-offset 10)
  (define-form-constraint (class-label new-box)
      :top-attachment :widget :top-widget path-label :top-offset 5
      :left-attachment :form :left-offset 10)
  (define-form-constraint (slot-list new-box)
      :top-attachment   :widget :top-offset   2 
      :top-widget (class-label new-box)
      :left-attachment  :form :left-offset  10
      :right-attachment :form :right-offset 10
      :bottom-attachment :widget :bottom-widget (button-row new-box)
      :bottom-offset 10)
  (define-form-constraint (button-row new-box)
      :top-attachment :none 
      :bottom-attachment :form :bottom-offset 10   
      :left-attachment   :form :left-offset 10
      ;; :right-attachment => button verschwindet !!??
      ;;:right-attachment  :form :right-offset  10
      )

  (make-tap new-box nil #'refresh-contents)
  ;;(pop-up new-box)
  
  (refresh-selection-list new-box document)
  new-box)

(defmethod refresh-selection-list ((box inspector-box) object-description
				   &aux inspected-object is-clos-object
				   item-list new-text (next 0))
  "refresh slots/elements shown"
  (setq inspected-object (evaluate-object-description object-description))
  
  #+(or allegro-v4.0 (and excl (version>= 4 1)) )
  (setq is-clos-object (or (clos-object-p inspected-object)
			   (typep inspected-object 'structure)))
  #-(or allegro-v4.0 (and excl (version>= 4 1)) )
  (setq is-clos-object (clos-object-p inspected-object))
  
  (set-sensitivity (browser-button box) (clos-object-p inspected-object))
  
  (when is-clos-object
    ;; one line for each slot
    (let ((*print-level* 2) (*print-length* 3) (*print-pretty* nil))
      (setq item-list
	(loop for (slot-name slot-value) in (slots inspected-object)
	    collect (list (format nil "~s:  ~s"  slot-name slot-value)
			  (list slot-name slot-value))))))

  (when (listp inspected-object)
    (let ((*print-level* 2) (*print-length* 3) (*print-pretty* nil))
      (setq item-list
	(loop for element in inspected-object
	      for i from 0
	    collect (list (format nil "~s"  element)
			  (list i element))))))

  (when (or is-clos-object (listp inspected-object))
    ;; modify item-list of widget
    (set-item-list (slot-list box) item-list)
    ;; sensitivity no longer appropriate in Motif 1.2
    ;;(xtk:set-sensitive (widget-id (slot-list box)))
    (xtk:set-sensitive (widget-id (inspect-button box)))
    (xtk:set-sensitive (widget-id (inspect-button2 box)))
    )
  
  (when (not (or is-clos-object (listp inspected-object)))
    (setq new-text
      (with-output-to-string (stream)
	(let ((*standard-output* stream))
	  (describe inspected-object))))
    (setq item-list
      (loop for end = (position #\newline new-text :start next)
            while end
            collect (subseq new-text next end)
            do (setq next (1+ end))))
    (when (< next (length new-text)) 
      (setq item-list 
	    (append item-list (list (subseq new-text next)))))
    (set-item-list (slot-list box) item-list :selected-value nil)
    ;; sensitivity no longer appropriate in Motif 1.2
    ;; (xtk:set-insensitive (widget-id (slot-list box)))
    (xtk:set-insensitive (widget-id (inspect-button box)))
    (xtk:set-insensitive (widget-id (inspect-button2 box)))
    )

  (let ((hash-value (get-pathname box object-description)))
    (setf (value (path-text box)) (if (stringp hash-value) hash-value "")))
  
  (setf (label-string (class-label box))
    (cond ((not inspected-object) "nil")
          ((listp inspected-object) "list")
	  (is-clos-object
	        (format nil "~(~s~)" (clos-class-of inspected-object)))
	  ((arrayp inspected-object) "array")
	  (t (format nil "~(~s~)" (type-of inspected-object)))))
  
  (setq *inspected-object* inspected-object))

(defmethod change-inspection ((box inspector-box) new-value old-value)
  (declare (ignore old-value))
  (refresh-selection-list box new-value))

(defun evaluate-object-description (object)
  (when object
    (if (listp object)
        (let* ((slotname (first object))
	      (apply-object (if (eq slotname 'quote)
				(second object)
			        (evaluate-object-description (second object)))))
	  (if (symbolp slotname)
	      (if (eq slotname 'quote)
		  apply-object
		(when apply-object 
		  (if (slot-boundp apply-object slotname)
		      (ignore-errors (slot-value apply-object slotname))
		    :unbound)))
	      (when (numberp slotname) 
	        (nth slotname apply-object))))
      object)))

(defmethod inspect-selection ((box inspector-box) use-object &aux new-selection)
  "call inspector for the currently selected item"
  (setq new-selection (value (slot-list box)))
  (when (listp new-selection) ;; otherwise it is a text description, no object
    (let* ((selected-object (second new-selection))
	   (new-object (if (and use-object (clos-object-p selected-object))
			   selected-object
		         (list (first new-selection)
			       (value (object-list box))))))
      (select-new-object box new-object
			 (get-new-path box (first new-selection))))))

(defmethod get-new-path (box slot
			 &aux (object (value (object-list box)))
			      (old-path (get-pathname box object)))
  (cond 
   ((symbolp slot)
    (format nil "(~(~a~) ~a)" slot old-path))
   ((not (numberp slot))
    (error "Should never occur (oops)!"))
   ((zerop slot)
    (format nil "(first ~a)" old-path))
   ((= slot (length (slot-list box)))
    (format nil "(last ~a)" old-path))
   (t
    (format nil "(nth ~d ~a)" slot old-path))))
  
(defmethod select-new-object ((box inspector-box) new-object path)
  (if (loop for obj in (value-list (object-list box))
	    when (equal new-object obj)
	    do (setf (value (object-list box)) obj)
	       (return t))
      (set-pathname box new-object path)
      (add-new-object box new-object path))
  (change-inspection box new-object nil))

(defmethod add-new-object ((box inspector-box) new-object path)
  (set-pathname box new-object path)
  (set-item-list (object-list box)
		 (append (loop for obj in (value-list (object-list box))
			       collect (list (format nil "~s" obj) obj))
			 (list (list (format nil "~s" new-object) new-object)))
		 :selected-value new-object))

(defmethod get-pathname ((box inspector-box) object)
  (if (eq object (document box))
      "*document*"
    (gethash object (inspector-hash-table (document box)))))

(defmethod set-pathname ((box inspector-box) object path)
  (unless (eq object (document box))
    (setf (gethash object (inspector-hash-table (document box))) path)))

(defginafun inspect-dialog (object
		       &key (document (first (document-list *application*)))
			    (use-old-one t)
		       &aux known-box)
  (description "create a dialog box displaying the object's slots")
  (example (with-application-stopped
	     (inspect-dialog *application*))
	   "inspect the current application object")
  (example (with-application-stopped
	     (inspect-dialog '(1 2 3 4 5 6)))
	   "inspect a any lisp object")
  
  (when (and (listp object) (= (length object) 1) 
	     (clos-object-p (first object)))
    ;; directly inspect the only element
    (setq object (first object)))
  (when (listp object)
    (setq object (list 'quote object)))
  (with-clock-cursor
    (cond ((and document use-old-one
		(setq known-box (gethash document 
					 (inspector-hash-table document))))
	   (select-new-object known-box object "*")
	   (pop-up known-box)
	   (expose (dialog-shell known-box)))

	  ((and object document)  ;; not nil
	   (setf (inspector-hash-table document) 
	         (make-hash-table :test #'equal))
	   (setq known-box (make-inspector-box document))
	   (setf (gethash document (inspector-hash-table document)) known-box)
  
	   (select-new-object known-box object "*")
	   (pop-up known-box))

	  (t (xlib:bell *display*)
	     (xlib:display-force-output *display*)))))

(defmethod refresh-contents ((box inspector-box) cmd)
  (declare (ignore cmd))
  (change-inspection box (value (object-list box)) nil))

(defmethod focus-gina-browser ((box inspector-box))
  (bring-up-gina-browser :class (gina-class-of *inspected-object*)))

(defmethod pop-entry ((box inspector-box) 
		      &aux current-pathname old-pathname old-object from to)
  (setq current-pathname (get-pathname box (value (object-list box))))
  ;;(format t "current-pathname: ~a~%" current-pathname)
  (setq from (position #\( current-pathname :start 1))
  (unless from 
    (setq from (position #\* current-pathname :start 1)))
  (setq to (position #\) current-pathname :from-end t
		     :end (1- (length current-pathname))))
  (unless to
    (setq to (position #\* current-pathname :from-end t
		       :end (1- (length current-pathname)))))
  (if (and from to (<= from to))
      (setq old-pathname (subseq current-pathname from (1+ to)))
    (setq old-pathname "*document*"))
  ;;(format t "from: ~d :to ~d old-pathname: ~a~%" from to old-pathname)
  (setq old-object (document box)) ;; just to be sure
  (loop for obj in (value-list (object-list box))
      when (string-equal old-pathname (get-pathname box obj))
      do (setq old-object obj)
	 (return))
  ;;(format t "~s~%" old-object)
  (unless (eq (value (object-list box)) (document box))
    (set-item-list (object-list box)
		   (loop for obj in (value-list (object-list box))
		       unless (eq obj (value (object-list box)))
		       collect (list (format nil "~s" obj) obj))
		   :selected-value (document box))
    ;; use document as dummy selection else label may not be updated
    (setf (value (object-list box)) old-object)
    (refresh-selection-list box old-object)))
