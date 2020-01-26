;;; -*- mode:LISP;Syntax: Common-Lisp;Package: gina ;Base:10-*-
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

(setq *sccs-id* "@(#)browser.lisp	1.18  11/8/93")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class GINA-Browser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass GINA-Browser (application)
  (;; overrides
   (name          :initform "GINA Browser"                        :allocation :class)
   (document-type :initform 'browser-document                     :allocation :class)
   (signature     :initform "browser"                             :allocation :class)
   (file-type     :initform nil                                   :allocation :class)
   (subclass-tools :accessor subclass-tools :initform nil))
  (:documentation "tool for browsing available GINA classes, methods and functions"))

(defun make-GINA-browser ()
  "start the GINA Browser"
  (make-application :class 'GINA-Browser))

(defun bring-up-gina-browser (&key (class nil) &aux browser-appl)
  "start or expose browser from another application"
  (with-clock-cursor
     (setq browser-appl 
       (start-file nil ;; no pathname given
		   :signature "browser"))
     (when (and browser-appl class)
       ;; make sure application is started up
       (wait-until-running browser-appl)
       ;; tell browser to display desired class
       (send-message 
	browser-appl 
	(make-callback #'select-class 
		       (first (document-list browser-appl)) class))))

  ;; return application if successful
  browser-appl)
	      

'(with-application-stopped (bring-up-gina-browser
			     :class (find-gina-class 'callback)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class browser-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass browser-document (document)
  (;; instance-variables
    (basic-class-set :accessor basic-class-set :initform nil)
    (previous-classes :accessor previous-classes :initform nil)
    
    (basic-method-set :accessor basic-method-set :initform nil)
    (previous-methods :accessor previous-methods :initform nil)

    (basic-function-set :accessor basic-function-set :initform nil)
   ))

(defmethod initialize-instance :after ((doc browser-document) &rest initargs)
  "the name of a browser document is constructed in a nonstandard way"
  (declare (ignore initargs))
  (setf (name doc) (format nil "~d" (next-number doc))))


(defmethod create-windows ((doc browser-document))
  "create a GINA browser box"
  (with-slots (main-shell main-view) doc
    (setq main-shell
      (make-browser-document-shell doc :class 'browser-document-shell
				   :initargs nil :motif-resources '(:icon-name "Browser")))

    ;; remove unnecessary menu entries
    (remove-menu-entry (main-menu main-shell) "File" "Open..")
    (remove-menu-entry (main-menu main-shell) "File" "Save")
    (remove-menu-entry (main-menu main-shell) "File" "Save as..")
    (remove-menu-entry (main-menu main-shell) "File" "Revert")

    (remove-menu-entry (main-menu main-shell) "Edit" "Undo")
    (remove-menu-entry (main-menu main-shell) "Edit" "Redo")
    (remove-menu-entry (main-menu main-shell) "Edit" "Replay History")
    (remove-menu-entry (main-menu main-shell) "Edit" "History Scroller")

    ;; names of menues appear also to make them (in)sensitive. 
    (add-menu-command (main-menu main-shell) "Classes" "Basic classes"
		      (make-callback #'refresh-class-list doc :new-mode :basic-classes))
    (add-menu-command (main-menu main-shell) "Classes" "All classes"
		      (make-callback #'refresh-class-list doc :new-mode :all-classes))
    (add-menu-command (main-menu main-shell) "Classes" "Superclasses"
		      (make-callback #'refresh-class-list doc :new-mode :superclasses))
    (add-menu-command (main-menu main-shell) "Classes" "Direct superclasses"
		      (make-callback #'refresh-class-list doc :new-mode :direct-superclasses))
    (add-menu-command (main-menu main-shell) "Classes" "Subclasses"
		      (make-callback #'refresh-class-list doc :new-mode :subclasses))
    (add-menu-command (main-menu main-shell) "Classes" "Direct subclasses"
		      (make-callback #'refresh-class-list doc :new-mode :direct-subclasses))
    (add-menu-command (main-menu main-shell) "Classes" "Defining selected method"
		      (make-callback #'refresh-class-list doc :new-mode :defining))
    (add-menu-command (main-menu main-shell) "Classes" "Inheriting selected method"
		      (make-callback #'refresh-class-list doc :new-mode :inheriting))
    (add-menu-command (main-menu main-shell) "Classes" "Defining any listed method"
		      (make-callback #'refresh-class-list doc :new-mode :defining-any))
    (add-menu-command (main-menu main-shell) "Classes" "Inheriting any listed method"
		      (make-callback #'refresh-class-list doc :new-mode :inheriting-any))
    (add-menu-command (main-menu main-shell) "Classes" "Class of selected constructor"
		      (make-callback #'refresh-class-list doc
				     :new-mode :of-selected-constructor))
    (add-menu-command (main-menu main-shell) "Classes" "Previous classes"
		      (make-callback #'refresh-class-list doc :new-mode :previous-classes))

    (add-menu-command (main-menu main-shell) "Methods" "All methods"
		      (make-callback #'refresh-method-list doc :new-mode :all-methods))
    (add-menu-command (main-menu main-shell) "Methods" "Methods of selected class"
		      (make-callback #'refresh-method-list doc :new-mode :methods-of-class))
    (add-menu-command (main-menu main-shell) "Methods" "Methods inherited by selected class"
		      (make-callback #'refresh-method-list doc :new-mode :inherited-methods))
    (add-menu-command (main-menu main-shell) "Methods" "Named as selected method"
		      (make-callback #'refresh-method-list doc :new-mode :same-name))
    (add-menu-command (main-menu main-shell) "Methods" "Previous methods"
		      (make-callback #'refresh-method-list doc :new-mode :previous-methods))

    (add-menu-command (main-menu main-shell) "Other" "Functions, macros and vars"
		      (make-callback #'refresh-function-list doc :new-mode :all))
    (add-menu-command (main-menu main-shell) "Other" "Functions"
		      (make-callback #'refresh-function-list doc :new-mode :functions))
    (add-menu-command (main-menu main-shell) "Other" "Macros"
		      (make-callback #'refresh-function-list doc :new-mode :macros))
    (add-menu-command (main-menu main-shell) "Other" "Variables"
		      (make-callback #'refresh-function-list doc :new-mode :variables))
    (add-menu-command (main-menu main-shell) "Other" "Constructor of selected class"
		      (make-callback #'refresh-function-list doc :new-mode :constructor))

    (add-menu-command (main-menu main-shell)
		      "Tool" "Generate Code to Override Selected Class..."
		      (make-callback #'make-subclass-tool doc))
    (add-menu-command (main-menu main-shell)
		      "Tool" "Generate Code to Override Selected Method..."
		      (make-callback #'make-subclass-tool doc))

    ;; set callbacks for filters (other callbacks are set within the interface-builder)
    (setf (activate-callback (class-search-text (main-shell doc)))
	    `(lambda (ignore)
	       (xtk:with-immediate-update-enabled
		 (setf (value ',(class-filter-button (main-shell doc))) t))
	       (refresh-class-list ',doc :new-mode nil)))
    (setf (value-changed-callback (class-search-text (main-shell doc)))
	    `(lambda (ignore)
	       (xtk:with-immediate-update-enabled
		 (when (value ',(class-filter-button (main-shell doc)))
		   (setf (value ',(class-filter-button (main-shell doc))) nil)
		   (refresh-class-list ',doc :new-mode nil)))))
    (setf (value-changed-callback (class-filter-button (main-shell doc)))
      `(lambda (new-value) (refresh-class-list ',doc :new-mode nil)))

    (setf (value-changed-callback (method-filter-button (main-shell doc)))
	    `(lambda (new-value) (refresh-method-list ',doc :new-mode nil)))
    (setf (activate-callback (method-search-text (main-shell doc)))
	    `(lambda (ignore)
	       (xtk:with-immediate-update-enabled
		 (setf (value ',(method-filter-button (main-shell doc))) t))
	       (refresh-method-list ',doc :new-mode nil)))
    (setf (value-changed-callback (method-search-text (main-shell doc)))
	    `(lambda (ignore)
	       (xtk:with-immediate-update-enabled
		 (when (value ',(method-filter-button (main-shell doc)))
		   (setf (value ',(method-filter-button (main-shell doc))) nil)
		   (refresh-method-list ',doc :new-mode nil)))))

    (setf (value-changed-callback (override-group (main-shell doc)))
            `(lambda (set &optional ignore1 ignore2)
	       (declare (ignore set ignore1 ignore2))
	       (refresh-method-list ',doc :new-mode nil)))
    (setf (value (override-group (main-shell doc))) :sometimes)

    (setf (value-changed-callback (function-filter-button (main-shell doc)))
	    `(lambda (new-value) (refresh-function-list ', doc :new-mode nil)))
    (setf (activate-callback (function-search-text (main-shell doc)))
	    `(lambda (ignore)
	       (xtk:with-immediate-update-enabled
		 (setf (value ',(function-filter-button (main-shell doc))) t))
	       (refresh-function-list ',doc :new-mode nil)))
    (setf (value-changed-callback (function-search-text (main-shell doc)))
	    `(lambda (ignore)
	       (xtk:with-immediate-update-enabled
		 (when (value ',(function-filter-button (main-shell doc)))
		   (setf (value ',(function-filter-button (main-shell doc))) nil)
		   (refresh-function-list ',doc :new-mode nil)))))

    (class-selected doc (find-gina-class 'application) nil)
    (refresh-class-list doc :new-mode :basic-classes)
    (setf (value (class-list (main-shell doc))) (find-gina-class 'application))
    (refresh-method-list doc :new-mode :methods-of-class)
    (refresh-function-list doc :new-mode :all)
    (setup-menus (main-shell doc))))


(defmethod setup-menus ((main-shell browser-document-shell)
                         &aux sel-cl sel-m sel-f)
  (with-slots (main-menu class-list method-list function-list) main-shell
      (setq sel-cl (not (not (value class-list))))    ; T iff class selected
      (setq sel-m  (not (not (value method-list))))   ; T iff method selected
      (setq sel-f  (not (not (value function-list)))) ; T iff function selected
      (setf (sensitive (find-menu-entry main-menu "Classes" "Superclasses")) sel-cl)
      (setf (sensitive (find-menu-entry main-menu "Classes" "Direct superclasses")) sel-cl)
      (setf (sensitive (find-menu-entry main-menu "Classes" "Subclasses")) sel-cl)
      (setf (sensitive (find-menu-entry main-menu "Classes" "Direct subclasses")) sel-cl)
      (setf (sensitive (find-menu-entry main-menu "Classes" "Defining selected method")) sel-m)
      (setf (sensitive (find-menu-entry main-menu "Classes" "Inheriting selected method")) sel-m)
      (setf (sensitive (find-menu-entry main-menu "Classes" "Class of selected constructor")) sel-f)
      (setf (sensitive (find-menu-entry main-menu "Methods" "Methods of selected class")) sel-cl)
      (setf (sensitive (find-menu-entry main-menu "Methods" "Methods inherited by selected class")) sel-cl)
      (setf (sensitive (find-menu-entry main-menu "Methods" "Named as selected method")) sel-m)
      (setf (sensitive (find-menu-entry main-menu "Other" "Constructor of selected class")) sel-cl)
      (setf (sensitive
	     (find-menu-entry main-menu "Tool" "Generate Code to Override Selected Class..."))
	sel-cl)
	;; do not define a subclass of class callback:
	;; (and sel-cl (not (equal 'callback (name (value class-list)))))
      (setf (sensitive
	     (find-menu-entry main-menu "Tool" "Generate Code to Override Selected Method..."))
	sel-m)
	;; do not overrid a method of class callback:
	;; (and sel-m (not (equal 'callback (name (value class-list)))))
      ))

(defmethod class-selected ((doc browser-document) class old-class)
  "a new class has been selected"
  (declare (ignore old-class))
  ;; show description of the new class
  (setf (label-string (description-label (main-shell doc)))
	(format nil "Class ~a" (short-name class)))
  (setf (value (description-text (main-shell doc)))
	(description-text class))

  ;; deselect other objects
  (setf (value (method-list (main-shell doc))) nil)
  (setf (value (function-list (main-shell doc))) nil)
  (setup-menus (main-shell doc)))


(defmethod method-selected ((doc browser-document) method old-method)
  "a new method has been selected"
  (declare (ignore old-method))
  ;; show description of the new method
  (setf (label-string (description-label (main-shell doc)))
	(format nil "Method ~a" (short-name method)))
  (setf (value (description-text (main-shell doc)))
	(description-text method))

  ;; deselect other objects
  (setf (value (class-list (main-shell doc))) nil)
  (setf (value (function-list (main-shell doc))) nil)
  (setup-menus (main-shell doc)))

(defmethod function-selected ((doc browser-document) function old-function)
  "a new function, variable or macro has been selected"
  (declare (ignore old-function))
  ;; show description of the new function
  (setf (label-string (description-label (main-shell doc)))
	(case (clos-class-of function)
	  (gina-variable (format nil "Variable ~a" (short-name function)))
	  (gina-macro    (format nil "Macro ~a"    (short-name function)))
	  (gina-function (format nil "Function ~a" (short-name function)))))
  (setf (value (description-text (main-shell doc)))
	(description-text function))

  ;; deselect other objects
  (setf (value (class-list (main-shell doc))) nil)
  (setf (value (method-list (main-shell doc))) nil)
  (setup-menus (main-shell doc)))

(defmethod set-functions-filter ((doc browser-document) &rest args)
  (format t "(set-functions-filter <doc> ~a)~%" args))

(defmethod refresh-class-list ((doc browser-document) &key (new-mode nil)
			       &aux (classes-to-show nil)
			       selected-class selected-method item-list
			       old-label-string)
  "recompute the classes to show in the scrollable list"
  (with-clock-cursor
    (setq selected-class  (value (class-list  (main-shell doc))))
    (setq selected-method (value (method-list (main-shell doc))))
    (setq old-label-string (label-string (class-header (main-shell doc))))

    (when new-mode
      (setq classes-to-show
	    (case new-mode
	      (:all-classes
		(setf (label-string (class-header (main-shell doc))) "All classes")
		(all-gina-classes))
	      (:basic-classes
		(setf (label-string (class-header (main-shell doc))) "Basic classes")
		(sort (cons (find-gina-class 'view)
			    (loop for class in (all-gina-classes)
				  when (null (superclasses class)) collect class))
		      #'class<)) 
	      (:superclasses
		(when (not selected-class)
		  (xlib:bell *display*)
		  (return-from refresh-class-list))
		(setf (label-string (class-header (main-shell doc)))
		      (format nil "Superclasses of ~a" (short-name selected-class)))
		(delete selected-class (all-superclasses selected-class)))
	      (:direct-superclasses
		(when (not selected-class)
		  (xlib:bell *display*)
		  (return-from refresh-class-list))
		(setf (label-string (class-header (main-shell doc)))
		      (format nil "Direct superclasses of ~a" (short-name selected-class)))
		(superclasses selected-class))
	      (:subclasses
		(when (not selected-class)
		  (xlib:bell *display*)
		  (return-from refresh-class-list))
		(setf (label-string (class-header (main-shell doc)))
		      (format nil "Sublasses of ~a" (short-name selected-class)))
		(delete selected-class (sort (all-subclasses selected-class) #'class<)))
	      (:direct-subclasses
		(when (not selected-class)
		  (xlib:bell *display*)
		  (return-from refresh-class-list))
		(setf (label-string (class-header (main-shell doc)))
		      (format nil "Direct sublasses of ~a" (short-name selected-class)))
		(subclasses selected-class))
	      (:defining
		(when (not selected-method)
		  (xlib:bell *display*)
		  (return-from refresh-class-list))
		(setf (label-string (class-header (main-shell doc)))
		      (format nil "Defining ~a" (short-name selected-method)))
		(relevant-classes selected-method))
	      (:inheriting
		(when (not selected-method)
		  (xlib:bell *display*)
		  (return-from refresh-class-list))
		(setf (label-string (class-header (main-shell doc)))
		      (format nil "Inheriting ~a" (short-name selected-method)))
		(loop for class in (relevant-classes selected-method)
		      append (all-subclasses class)))
	      (:defining-any
		(setf (label-string (class-header (main-shell doc)))
		      (format nil "Defining any listed method"))
		(let ((classes nil))
		  (loop for method in (value-list (method-list (main-shell doc)))
			do (loop for class in (relevant-classes method)
				 when (not (member class classes))
				   do (push class classes)))
		  (sort classes #'class<)))
	      (:inheriting-any
		(setf (label-string (class-header (main-shell doc)))
		      (format nil "Inheriting any listed method"))
		(let ((relevant-classes nil) (subclasses nil))
		  (loop for method in (value-list (method-list (main-shell doc)))
			do (loop for class in (relevant-classes method)
				 when (not (member class relevant-classes))
				   do (push class relevant-classes)))
		  (loop for class in relevant-classes
			do (loop for subclass in (all-subclasses class)
				 when (not (member subclass subclasses))
				   do (push subclass subclasses)))
		  (sort subclasses #'class<)))
	      (:of-selected-constructor
		(when (not (and (value (function-list (main-shell doc)))
				(typep (value (function-list (main-shell doc))) 'gina-function)
				(constructor-for-class (value (function-list (main-shell doc))))))
		  (xlib:bell *display*)
		  (return-from refresh-class-list))
		(list (find-gina-class
			(constructor-for-class (value (function-list (main-shell doc)))))))
		
	      (:previous-classes
		(when (previous-classes doc)
		  (setf (label-string (class-header (main-shell doc)))
			(first (previous-classes doc)))
		  (rest (previous-classes doc))))
	      ))
    
      ;; store class list for possible undo
      (when (not (eq :previous-classes new-mode))
	(setf (previous-classes doc)
	      (cons old-label-string (basic-class-set doc))))

      ;; turn off filter button because class list changes
      (setf (value (class-filter-button (main-shell doc))) nil)
      
      (setf (basic-class-set doc) classes-to-show))
    
    (when (not new-mode) (setq classes-to-show (basic-class-set doc)))
    
    ;; apply selection criteria
    (when (and (not (equal "" (value (class-search-text (main-shell doc)))))
	       (value (class-filter-button (main-shell doc))))
      (setq classes-to-show
	    (loop for class in classes-to-show
		  when (search (value (class-search-text (main-shell doc)))
			       (short-name class) )
		    collect class)))
  
    (setq item-list
	  (loop for class in classes-to-show
		collect (list (short-name class) class)))

    (set-item-list (class-list (main-shell doc)) item-list :selected-value nil)))

(defmethod refresh-method-list ((doc browser-document) &key (new-mode nil)
				&aux methods-to-show selected-class
				selected-method old-label-string item-list)
  "recompute the methods to show in the scrollable list"
  (with-clock-cursor
    (setq selected-class  (value (class-list  (main-shell doc))))
    (setq selected-method (value (method-list (main-shell doc))))
    (setq old-label-string (label-string (method-header (main-shell doc))))

    (when new-mode
      (setq methods-to-show
	    (case new-mode
	      (:all-methods
		(setf (label-string (method-header (main-shell doc))) "All methods")
		(all-gina-methods))
	      (:methods-of-class
		(when (not selected-class)
		  (xlib:bell *display*)
		  (return-from refresh-method-list))
		(setf (label-string (method-header (main-shell doc)))
		      (format nil "Methods of class ~a" (short-name selected-class)))
		(sort-methods selected-class))
	      (:inherited-methods
		(when (not selected-class)
		  (xlib:bell *display*)
		  (return-from refresh-method-list))
		(setf (label-string (method-header (main-shell doc)))
		      (format nil "Methods inherited by class ~a" (short-name selected-class)))
		(sort (loop for class in (all-superclasses selected-class)
			    append (methods class))
		      #'method<))
	      (:same-name
		(when (not selected-method)
		  (xlib:bell *display*)
		  (return-from refresh-method-list))
		(setf (label-string (method-header (main-shell doc)))
		      (format nil "Methods named ~a"
			      (string-downcase (symbol-name (basic-name selected-method)))))
		(related-methods selected-method))
	      (:previous-methods
		(when (previous-methods doc)
		  (setf (label-string (method-header (main-shell doc)))
			(first (previous-methods doc)))
		  (rest (previous-methods doc))))
	      ))

      (when (not (eq :previous-methods new-mode))
	(setf (previous-methods doc)
	      (cons old-label-string (basic-method-set doc))))

      ;; turn off filter button because method list changes
      (setf (value (method-filter-button (main-shell doc))) nil)
      
      (setf (basic-method-set doc) methods-to-show))

    (when (not new-mode) (setq methods-to-show (basic-method-set doc)))

    ;; apply selcetion criteria
    (when (not (eq :all (value (override-group (main-shell doc)))))
      (setq methods-to-show
	(loop for method in methods-to-show
	  when (or (and (eq :sometimes (value (override-group (main-shell doc))))
			(not (and (eq :rarely (override method))
				  (eq :rarely (called-by-application method)))))
		   (and (eq :called (value (override-group (main-shell doc))))
			(not (eq :rarely (called-by-application method))))
		   (and (eq :overriden (value (override-group (main-shell doc))))
			(not (eq :rarely (override method)))))
	 collect method)))

    (when (and (not (equal "" (value (method-search-text (main-shell doc)))))
	       (value (method-filter-button (main-shell doc))))
      (setq methods-to-show
	    (loop for method in methods-to-show
		  when (search (value (method-search-text (main-shell doc)))
			       (short-name method))
		    collect method)))
  
    (setq item-list
	  (loop for method in methods-to-show
		collect (list (name-with-indicator method) method)))

    (set-item-list (method-list (main-shell doc)) item-list :selected-value nil)))

(defmethod refresh-function-list ((doc browser-document) &key (new-mode nil)
				  &aux (functions-to-show nil)
				  selected-class item-list)
  "recompute the functions, macros and variables to show in the scrollable list"
  (with-clock-cursor
    (setq selected-class  (value (class-list  (main-shell doc))))

    (when new-mode
      (setq functions-to-show 
	    (case new-mode
	      (:all
		(setf (label-string (function-header (main-shell doc))) "Functions, macros and vars")
		(sort (append (all-gina-functions) (all-gina-macros) (all-gina-variables))
		      #'function<))
	      (:functions
		(setf (label-string (function-header (main-shell doc))) "Functions")
		(all-gina-functions))
	      (:macros
		(setf (label-string (function-header (main-shell doc))) "Macros")
		(all-gina-macros))
	      (:variables
		(setf (label-string (function-header (main-shell doc))) "Variables")
		(all-gina-variables))
	     
	      (:constructor
		(when (not selected-class)
		  (xlib:bell *display*)
		  (return-from refresh-function-list))
		(setf (label-string (function-header (main-shell doc)))
		      (format nil "Constructor of ~a" (short-name selected-class)))
		(when (constructor selected-class) (list (constructor selected-class))))))

      ;; turn off filter button because function list changes
      (setf (value (function-filter-button (main-shell doc))) nil)
      
      (setf (basic-function-set doc) functions-to-show))
    
    (when (not new-mode) (setq functions-to-show (basic-function-set doc)))
    
    ;; apply selection criteria
    (when (and (not (equal "" (value (function-search-text (main-shell doc)))))
	       (value (function-filter-button (main-shell doc))))
      (setq functions-to-show
	    (loop for function in functions-to-show
		  when (search (value (function-search-text (main-shell doc)))
			       (short-name function))
		    collect function)))
  
    (setq item-list
	  (loop for function in functions-to-show
		collect (list (short-name function) function)))

    (set-item-list (function-list (main-shell doc)) item-list :selected-value nil)))

(defmethod select-class ((doc browser-document) class)
  "show info for desired class in browser"
  ;; show all classes in class list
  (refresh-class-list doc :new-mode :all-classes)
  ;; select desired-class
  (setf (value (class-list (main-shell doc))) class)
  (show-item (class-list (main-shell doc)) class)
  ;; show description of the new class
  (class-selected doc class nil)
  ;; show methods of class 
  (refresh-method-list doc :new-mode :methods-of-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; subclass-dialog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass subclass-tool ()
          ((the-superclass :initarg :super-class :accessor the-superclass)
	   (the-document :initarg :document :accessor the-document)
	   (the-dialog-window :accessor the-dialog-window)
	   (its-slots :accessor its-slots :initform nil)
	   (its-methods :accessor its-methods :initform nil))
          (:documentation "Tool for generating subclass code"))


(defmethod make-subclass-tool ((doc browser-document)
	       &aux (the-class  (value (class-list  (main-shell doc))))
	            (the-method (value (method-list (main-shell doc))))
	            the-tool the-dialog)
  "Show dialogbox for generating code to write a subclass of given class"
  
  ;; beep if no class and no method  selected
  ;; (internal error: command should be insensitive)
  (when (and (not the-class) (not the-method))
    (xlib:bell *display*) (return-from make-subclass-tool))

  (when (not the-class)
    (setq the-class (first (relevant-classes the-method))))

  ;; search whether there is already a subclass-tool for this gina class
  (setq the-tool
    (loop for dialog-tool in (subclass-tools *application*)
     do (when (eq the-class (the-superclass dialog-tool)) (return dialog-tool))))

  ;; otherwise generate a subclass-tool and show the-dialog-window
  (when (not the-tool)
    (with-clock-cursor
     (setq the-tool (make-object 'subclass-tool `(:super-class ,the-class :document ,doc)))
     (setq the-dialog (make-subclass-dialog doc))
     (setf (the-dialog-window the-tool) the-dialog)
     (setf (subclass-tools *application*)
       (cons the-tool (subclass-tools *application*)))

     ;; fix error of ib (ib generates :scroll-bar-display-policy :static as default)
     (set-motif-resources (class-slots-selection the-dialog) :scroll-bar-display-policy :as-needed)
     (set-motif-resources (instance-slots-selection the-dialog) :scroll-bar-display-policy :as-needed)
     (set-motif-resources (methods-selection the-dialog) :scroll-bar-display-policy :as-needed)
     (set-motif-resources (key-selection the-dialog) :scroll-bar-display-policy :as-needed)

     ;; set callbacks for buttons (can not be set from ib because class subclass-tool unknown)
     (setf (activate-callback (show-button the-dialog)) (make-callback 'show-code the-tool))
     (setf (activate-callback (save-button the-dialog)) (make-callback 'save-code the-tool))

     ;; set initial value for toggle-button-group 
     (setf (value (generate-buttons the-dialog)) (list :class :method :constructor))

     ;; set header and default-name for new subclass:
     (setf (label-string (subclass-header the-dialog))
	   (format nil "Define Subclass of ~a" (name the-class)))
     (setf (value (name-of-subclass-text the-dialog))
       (format nil "my-~(~a~)" (name the-class)))      

     ;; enter entries into selection-lists:
     (set-item-list (class-slots-selection the-dialog)
        (loop for class in (all-superclasses the-class)
	 append (loop for slot in (class-variables class)
		 when (and (listp slot) (member :documentation slot))
		 collect (list (if (member :initform slot)
				   (format nil "~(~a~) (default: ~s)" (first slot)
					   (nth (1+ (position :initform slot)) slot))
				 (format nil "~(~a~)" (first slot)))
			       slot))))

     (set-item-list (instance-slots-selection the-dialog)
	  (loop for class in (all-superclasses the-class)
	   append (loop for slot in (instance-variables class)
		   when (and (listp slot) (member :documentation slot))
		   collect (list (if (member :initform slot)
				     (format nil "~(~a~) (default: ~s)" (first slot)
					     (nth (1+ (position :initform slot)) slot))
				     (format nil "~(~a~)" (first slot)))
				 slot))))
     (set-item-list (methods-selection the-dialog)
		    (loop for class in (all-superclasses the-class)
			with all-methods = nil
			append (loop for method in (methods class)
				   when (not (member (list (name method)
							   (qualifiers method))
						     all-methods :test #'equal))
				   do (setq all-methods (cons 
							 (list (name method)
							       (qualifiers method))
							 all-methods))
				    and collect 
					(list (if (qualifiers method)
						  (format nil "~(~a ~s~)" 
							 (name method) 
							 (first (qualifiers method)))
					          (format nil "~(~a~)" (name method)))
					      method)))
		    :selected-value
		    (if the-method
			(list the-method)
		      (loop for class in (all-superclasses the-class)
			  with all-methods = nil		 
			  append (loop for method in (methods class)
				     when (not (member method all-methods))
				       do (setq all-methods (cons method all-methods))
				      and when (not (eq :rarely (override method)))
				            collect method)))) 

     (when (constructor the-class)
       (set-item-list
	   (key-selection the-dialog)
	   (loop for element in (lambda-list (constructor the-class))
	      with key-found = nil
	      when key-found collect (list (format nil "~(~a~)" element) element)
	      else do (setq key-found (eq '&key element)))))))    

   ;; select and show selected method of browser within dialogbox:
   (when the-method
     (setf (value (generate-buttons  (the-dialog-window the-tool))) (list :method))
     (setf (value (methods-selection (the-dialog-window the-tool))) (list the-method))
     (show-item (methods-selection (the-dialog-window the-tool)) the-method))

   ;; popup the dialog:
   (pop-up (the-dialog-window the-tool))
   (expose (dialog-shell (the-dialog-window the-tool))))


(defmethod show-code ((tool subclass-tool)
		      &aux (doc (the-document tool))
		      (dialog-window (the-dialog-window tool)))
  (with-slots (name-of-subclass-text) dialog-window
    (setf (label-string (description-label (main-shell doc)))
      (format nil "Code for Subclass ~a of ~a"
	      (value name-of-subclass-text) (name (the-superclass tool))))
    (setf (value (description-text (main-shell doc)))
      (with-output-to-string (text)
	(generate-code tool text)))))

(defmethod save-code ((tool subclass-tool) &aux file-name no-error condition)
  (setq file-name (value (file-name-text (the-dialog-window tool))))
  (if (and (file-exists file-name)
           (not (confirm-dialog "File exists. Do you want to replace it?")))
      (return-from save-code))
  ;; now save code onto file
  (multiple-value-setq (no-error condition)
      (ignore-errors
       (with-open-file (output-stream file-name :direction :output
			                        :if-exists :new-version)
	 (generate-code tool output-stream))
       :okay))
  (when (not no-error)
    (warning-dialog (format nil "~a" condition))))

(defmethod generate-code ((tool subclass-tool) to-stream
			  &aux (open-paren "(") gen-class (pass-class nil))
  (with-slots (the-superclass the-dialog-window) tool
   (with-slots (name-of-subclass-text generate-buttons
		class-slots-selection instance-slots-selection
		methods-selection key-selection) the-dialog-window

    ;; generate class code:
    (when (member :class (value generate-buttons))
      (format to-stream "(defclass ~a (~(~a~))~%" 
	      (value name-of-subclass-text)
	      (name the-superclass))
      (when (value class-slots-selection)
	(format to-stream "   (;; overriden class slots:~%")
	(setq open-paren " ")
	(loop for slot in (value class-slots-selection)
	 do (format to-stream "    (~(~20a~)" (first slot))
	    (if (member :initform slot)
		(format to-stream " :initform ~(~15s~)"
			(nth (1+ (position :initform slot)) slot))
	        (format to-stream " :accessor ~(~15a~)"
			(first slot)))
	    (format to-stream " :allocation :class)~%")
	  ))
      (when (value instance-slots-selection)
	(format to-stream "   ~a;; overriden instance slots:~%" open-paren)
	(setq open-paren " ")
	(loop for slot in (value instance-slots-selection)
	 do (format to-stream "    (~(~20a~)" (first slot))
	    (if (member :initform slot)
		(format to-stream " :initform ~(~15s~)"
			(nth (1+ (position :initform slot)) slot))
	        (format to-stream " :accessor ~(~15a~)"
			(first slot)))
	    (format to-stream " :allocation :instance)~%")
	  ))
      (format to-stream "   ~a;; your new slots:~%   )~%" open-paren)
      (format to-stream "   (:documentation \"yourDocumentation\")~%)~%~%"))

    ;; generate constructor:
    (when (and (member :constructor (value generate-buttons))
	       (constructor the-superclass))
	  (format to-stream "(defun make-~a (" (value name-of-subclass-text))
	  (loop for s-expr in (lambda-list (constructor the-superclass))
	    do (cond ((equal s-expr '&key) (return t))
		     (t (format to-stream "~(~s~) " s-expr))))
	  (when (value key-selection) (format to-stream "&key")
		(loop for parm in (value key-selection)
		 as some-blanks = (make-string (+ 14 (length (value name-of-subclass-text)))
					       :initial-element #\space)
		 do (if (eq (first parm) 'class)
			(progn ;; special case passing class (insert name of subclass):
			  (format to-stream "~%~a(class '~a)" some-blanks
				  (value name-of-subclass-text))
			  (setq pass-class t))
		      ;; normal case passing any keyword paramter
		      (format to-stream "~%~a~(~a~)" some-blanks parm))))
	  (format to-stream ")~%")

	  (setq gen-class (and (not pass-class)
			       (loop for parm in (lambda-list (constructor the-superclass))
				do (when (and (listp parm) (eq (first parm) 'class))
				     (return t)))))
	  (format to-stream "   (~(~a~) " (name (constructor the-superclass)))
	  (loop for s-expr in (lambda-list (constructor the-superclass))
	    do (cond ((equal s-expr '&key) (return t))
		     (t (format to-stream "~(~s~) " s-expr))))
	  (when (or gen-class (value key-selection))
	    (when gen-class (format to-stream ":class '~a" (value name-of-subclass-text)))
	    (loop for parm in (value key-selection)
	     with first-line = (not gen-class)
	     as some-blanks =
	      (make-string (+ 5 (length (format nil "~a"
						(name (constructor the-superclass)))))
			   :initial-element #\space)
	     do (if first-line
		    (setq first-line nil)
		    (format to-stream "~%~a" some-blanks))
	        (format to-stream "~(:~a ~a~)" (first parm) (first parm))))
	  (format to-stream ")~%  )~%"))
    
    ;; generate method code:
    (when (member :method (value generate-buttons))
      (loop for method in (value methods-selection)
	  do (format to-stream "(defmethod ~(~a~) "
		     (name method))
	     (when (qualifiers method)
	       (loop for qualifier in (qualifiers method)
		 do (format to-stream "~(~s~) " qualifier)))
	     (format to-stream "(")
	     (loop for parm in (lambda-list method)
		 with super-class-names = 
		   (loop for class in (all-superclasses the-superclass) 
		       collect (name class))
	      do (if (and (listp parm)
			  (member (second parm) super-class-names :test #'equal))
		     (format to-stream "(~(~a~) ~a) "
			     (first parm)
			     (value name-of-subclass-text))
		     (format to-stream"~(~a~) " parm)))
	     (format to-stream ")~%")
	     (format to-stream "    \"yourDocumentation\"~%")
	     ;;(format to-stream "    ;; your code:~%")
	     (format to-stream ")~%~%")))
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; main program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(register-application "browser" 'GINA-browser nil)
'(make-gina-browser)
