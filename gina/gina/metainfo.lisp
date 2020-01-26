;;; -*- Mode:LISP;Syntax: Common-Lisp;Package: gina; Base:10-*-
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;   macros to extract meta information about GINA itself
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :gina)

(defvar *sccs-id*)
(eval-when (eval load) 
  (export '*sccs-id*))
(setq *sccs-id* "@(#)metainfo.lisp	1.19  11/8/93")

;; Throughout the GINA source-code the macros defginaclass, defginamethod, ... are
;; used instead of the almost equivalent macros defclass, defmethod, ...
;; The macros allow for additional semiformal comments and descriptions.
;; When the macros are expanded, information about the classes, methods, functions,
;; ... of GINA is collected. A special browser can be used to examine
;; the structure of GINA itself

(defvar *generate-metainfo* t)
;; set this variable to NIL before compiling GINA to suppress collection of metainfo
(defvar  *insert-debug-output* nil)
;; set this variable to NIL before compiling GINA to suppress extra debug code

(defmacro ginachapter (&rest comments)
  "comments may be placed anywhere on toplevel"
  (declare (ignore comments)))

(defmacro ginasection (&rest comments)
  "comments may be placed anywhere on toplevel"
  (declare (ignore comments)))
  
(defmacro ginacomment (&rest comments)
  "comments may be placed anywhere on toplevel"
  (declare (ignore comments)))

(defvar *gina-variable-table*)
(unless (boundp '*gina-variable-table*)
  (setq *gina-variable-table* (make-hash-table)))

(defvar *gina-macro-table*)
(unless (boundp '*gina-macro-table*)
  (setq *gina-macro-table* (make-hash-table)))

(defvar *gina-function-table*)
(unless (boundp '*gina-function-table*)
  (setq *gina-function-table* (make-hash-table)))

(defvar *gina-class-table*)
(unless (boundp '*gina-class-table*)
  (setq *gina-class-table* (make-hash-table)))

(defvar *gina-method-table*)
(unless (boundp '*gina-method-table*)
  (setq *gina-method-table* (make-hash-table :test #'equal)))

(defvar *index-counter* 0)
;;(setq *index-counter* 0)

(defclass gina-variable ()
  ((name          :accessor name        :initarg :name)
   (description   :accessor description :initarg :description)
   (modify        :accessor modify)
   (default       :accessor default)
   (comments      :accessor comments :initarg :comments)
   (index-number :accessor index-number :initform 1 :initarg :index-number)))

(defclass gina-macro ()
  ((name          :accessor name        :initarg :name)
   (lambda-list   :accessor lambda-list :initarg :lambda-list)
   (description   :accessor description :initarg :description)
   (commented-parms :accessor commented-parms :initarg :commented-parms)
   (comments      :accessor comments :initarg :comments)
   (examples      :accessor examples   :initarg :examples)
   (called-by-gina        :accessor called-by-gina)
   (called-by-application :accessor called-by-application)
   (bugs                  :accessor bugs)
   (topics                :accessor topics)
   (index-number :accessor index-number :initform 1 :initarg :index-number)))

(defclass gina-function ()
  ((name          :accessor name        :initarg :name)
   (lambda-list   :accessor lambda-list :initarg :lambda-list)
   (description   :accessor description :initarg :description)
   (commented-parms :accessor commented-parms :initarg :commented-parms)
   (comments      :accessor comments :initarg :comments)
   (examples      :accessor examples   :initarg :examples)
   (called-by-gina        :accessor called-by-gina)
   (called-by-application :accessor called-by-application)
   (constructor-for-class :accessor constructor-for-class)
   (result                :accessor result)
   (bugs                  :accessor bugs)
   (topics                :accessor topics)
   (index-number :accessor index-number :initform 1 :initarg :index-number)))

(defclass gina-class ()
  ((name                :accessor name                :initarg :name)
   (superclass-names    :accessor superclass-names    :initarg :superclass-names)
   (class-variables     :accessor class-variables     :initarg :class-variables)
   (instance-variables  :accessor instance-variables  :initarg :instance-variables)
   (description         :accessor description         :initarg :description)
   ;;(comments            :accessor comments            :initarg :comments)
   (methods             :accessor methods             :initform nil)
   (constructor         :accessor constructor         :initform nil)
   (callbacks           :accessor callbacks           :initform nil)
   (resource-slots      :accessor resource-slots      :initform nil)
   (known-subclasses    :accessor known-subclasses    :initform nil)
   (gina-subclasses     :accessor gina-subclasses)
   (application-subclasses :accessor application-subclasses)
   (instantiate         :accessor instantiate)
   (index-number :accessor index-number :initform 1 :initarg :index-number)))

(defclass gina-method ()
  ((name            :accessor name            :initarg :name)
   (lambda-list     :accessor lambda-list     :initarg :lambda-list)
   (qualifiers      :accessor qualifiers      :initarg :qualifiers)
   (type-specifiers :accessor type-specifiers :initarg :type-specifiers)
   (description     :accessor description     :initarg :description)
   (commented-parms :accessor commented-parms :initarg :commented-parms)
   (comments      :accessor comments :initarg :comments)
   (examples      :accessor examples   :initarg :examples)
   (called-by-gina        :accessor called-by-gina)
   (called-by-application :accessor called-by-application)
   (override              :accessor override)
   (default-version       :accessor default-version)
   (overridden-by-gina    :accessor overridden-by-gina)
   (result                :accessor result)
   (known-short-name      :accessor known-short-name :initform nil)
   (known-name-with-indicator :accessor known-name-with-indicator :initform nil)
   (index-number :accessor index-number :initform 1 :initarg :index-number)))

;; test dialogs
'(with-application-stopped
   (inspect-dialog (all-gina-classes)))
'(with-application-stopped
   (inspect-dialog
     (loop for class in (all-gina-classes)
	   when (null (superclass-names class)) collect class)))

'(with-application-stopped
   (inspect-dialog (relevant-classes (find-gina-method
				      'set-item-list nil '(selection-list t)))))
'(print (all-gina-methods))
'(with-application-stopped
   (inspect-dialog (all-gina-methods)))
'(with-application-stopped
   (inspect-dialog (methods (find-gina-class 'application))))
'(with-application-stopped
   (inspect-dialog (related-methods (find-gina-method
				      'set-item-list nil '(selection-list t)))))

'(with-application-stopped
   (inspect-dialog (all-gina-variables)))

'(with-application-stopped
   (inspect-dialog (all-gina-functions)))
'(with-application-stopped
   (inspect-dialog (find-gina-function 'register-application)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;    meta information about global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((variable gina-variable) stream)
  "specialized printed representation"
  (let ((*print-case* :downcase)) 
    (format stream "#<gina-variable ~a>"
	    (if (slot-boundp variable 'name) (name variable) "???"))))

(defmethod short-name ((variable gina-variable))
  "short string representation"
  (let ((*print-case* :downcase))
    (format nil "~a" (name variable))))

(defun make-gina-variable (name description comments &aux new old)
  "create a new gina-variable object and store it in the hash-table"
  (when (not (and (listp description) (eq 'description (first description))))
    (error "variable description must be a a string or a list (description \"...\" ...)"))
  
  (setq old (find-gina-variable name))
  (if old
      ;; just change some slots
      (progn
	(setq new old)
	(setf (comments new) comments)
	(setf (description new)  (second description)))
      ;; else: create a new variable-object
      (setq new (setq new (make-instance 'gina-variable
			   :name name
			   :description (second description)
			   :comments comments
			   :index-number (incf *index-counter*)))))
  
  ;; analyze (description "...." ...)
  (apply #'set-variable-description (cons new (rest description)))

  (when (not old)
    (setf (gethash name *gina-variable-table*) new))
  new)

(defun set-variable-description (variable string
				 &key (export t)
				      (default :none)
				      (modify :rarely)
				      ;; :rarely :always :sometimes <string>
				      )
  "store details of description in the gina-variable object"
  ;; this method documents the keywords allowed in the description of a variable
  (declare (ignore string))
  (when export (export (name variable)))

  ;; store details of description
  (setf (default variable) default)
  (setf (modify variable) modify)
  )

(eval-when (eval load) 
  (export 'defginavar))
(defmacro defginavar (name description &rest comments)
  "GINA version of defvar"
  
  ;; the description may be just a string
  (when (stringp description)
    (setq description `(description ,description)))

  ;; filter out comments before the body
  (setq comments
	(loop while (and comments (first comments) (listp (first comments))
			 (eq 'comment (first (first comments))))
	      append (cdr (first comments)) ;; the comment
	      do (pop comments)))
 

  `(progn
     ,(when *generate-metainfo* 
	`(make-gina-variable ',name ',description ',comments))
     (defvar ,name)))

'(defginavar *mike* "just another test")

(defun variable< (v1 v2)
  "determine if v1 < v2"
  (string< (symbol-name (name v1)) (symbol-name (name v2))))

(defun all-gina-variables (&aux the-list)
  "collect all hash-table entries and sort them"
  (maphash #'(lambda (key value) (declare (ignore key)) (push value the-list))
	   *gina-variable-table*)
  (setq the-list
	(sort the-list #'variable<))
  the-list)

'(print (all-gina-variables))

(defun find-gina-variable (name-symbol)
  "lookup name-symbol in hash-table"
  (gethash name-symbol *gina-variable-table*))

'(describe (find-gina-variable '*mike*))
'(describe (find-gina-variable '*display*))

(defmethod description-text ((variable gina-variable))
  "print description into a string"
  (replace-returns
    (with-output-to-string (text)
      (print-description variable text))))

(defmethod print-description ((variable gina-variable) stream &key (for-manual nil))
  "print description of the variable to stream"
  (declare (ignore for-manual))
  (let ((*print-case* :downcase)
	(*print-pretty* nil))
    (format stream "- ~a~%~%" (description variable))
    (format stream "Default: ~a~%" (default variable))

    (case (modify variable)
      (:rarely    (format stream "Normally, you do NOT modify this variable.~%"))
      (:always    (format stream "You modify this variable in EACH application.~%"))
      (:sometimes (format stream "You CAN modify this variable.~%"))
      (otherwise  (format stream "You CAN modify this variable ~a.~%"
			  (modify variable))))
	
    (when (slot-boundp variable 'comments)
      (when (comments variable)
	;;(format stream "~%")
	(loop for comment in (comments variable)
	      do (format stream "~a~%" comment))))
    ))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;    meta information about macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((macro gina-macro) stream)
  "specialized printed representation"
  (let ((*print-case* :downcase))
    (format stream "#<gina-macro ~a>"
	    (if (slot-boundp macro 'name) (name macro) "???"))))

(defmethod short-name ((macro gina-macro))
  "short string representation"
  (let ((*print-case* :downcase))
    (format nil "~a" (name macro))))

(defun make-gina-macro (name lambda-list description
			commented-parms comments examples
			&aux new old)
  "create a new gina-macro object and store it in the hash-table"
  (when (not (and (listp description) (eq 'description (first description))))
    (error "macro description must be a a string or a list (description \"...\" ...)"))
  
  ;; check if gina-macro already exists
  (setq old (find-gina-macro name))
  (if old
      ;; just change some slots
      (progn
	(setq new old)
	(setf (lambda-list new)     lambda-list)
	(setf (description new)     (second description))
	(setf (commented-parms new) commented-parms)
	(setf (comments new) comments)
	(setf (examples new) examples))
      ;; else: create a new macro-object
      (setq new (make-instance 'gina-macro
			   :name name
			   :lambda-list lambda-list
			   :description (second description)
			   :commented-parms commented-parms
			   :comments comments
			   :examples examples
			   :index-number (incf *index-counter*))))
  
  ;; analyze (description "...." ...)
  (apply #'set-macro-description (cons new (rest description)))

  (when (not old)
    (setf (gethash name *gina-macro-table*) new))
  new)

(defun set-macro-description (macro string
			      &key (export t)
				   (called-by-gina t)
				    ;; t nil <string>
				   (called-by-application :sometimes)
				   ;; :rarely :always :sometimes <string>
				   (bugs nil)
				   (topics nil))
  "store details of description in the gina-macro object"
  ;; this method documents the keywords allowed in the description of a macro
  (declare (ignore string))
  (when export
    (export (name macro)))
  (setf (called-by-gina macro) called-by-gina)
  (setf (called-by-application macro) called-by-application)
  (setf (bugs macro) bugs)
  (setf (topics macro) topics))

(eval-when (eval load) 
  (export 'defginamacro))
(defmacro defginamacro (name lambda-list description &body body
			&aux uncommented-lambda-list lambda-list-without-aux
			(last-parm nil) (commented-parms nil)
			description-string comments examples)
  "GINA version of defmacro"
  
  ;; the description may be just a string
  (when (stringp description)
    (setq description `(description ,description)))
  (setq description-string (second description))
  
  ;; additional strings are allowed in the lambda-list as comments
  (setq uncommented-lambda-list
	(loop for parm in lambda-list
	      when (not (stringp parm)) collect parm
	      else do (push (list last-parm parm) commented-parms)
	      do (setq last-parm parm)))
  (setq commented-parms (reverse commented-parms))

  ;; throw out &aux-variables
  (setq lambda-list-without-aux
	(loop for parm in uncommented-lambda-list
	      while (not (eq '&aux parm))
	      collect parm))

  ;; filter out comments before the body
  (setq comments
	(loop while (and body (first body) (listp (first body))
			 (eq 'comment (first (first body))))
	      append (cdr (first body)) ;; the comment
	      do (pop body)))
  
  ;; filter out examples before the body
  (setq examples
	(loop while (and body (first body) (listp (first body))
			 (eq 'example (first (first body))))
	      append (cdr (first body)) ;; the example
	      do (pop body)))


  `(progn
     ,(when *generate-metainfo* 
	`(make-gina-macro ',name ',lambda-list-without-aux ',description
			  ',commented-parms ',comments ',examples))

     ,(append `(defmacro ,name ,uncommented-lambda-list ,description-string)
	     body)))

(defun macro< (m1 m2)
  "determine if m1 < m2"
  (string< (symbol-name (name m1)) (symbol-name (name m2))))

(defun all-gina-macros (&aux the-list)
  "collect all hash-table entries and sort them"
  (maphash #'(lambda (key value) (declare (ignore key)) (push value the-list))
	   *gina-macro-table*)
  (setq the-list
	(sort the-list #'macro<))
  the-list)

'(print (all-gina-macros))

(defun find-gina-macro (name-symbol)
  "lookup name-symbol in hash-table"
  (gethash name-symbol *gina-macro-table*))

(defmethod description-text ((macro gina-macro))
  "print description into a string"
  (replace-returns
    (with-output-to-string (text)
      (print-description macro text))))

(defmethod print-description ((macro gina-macro) stream &key (for-manual nil))
  "print description of the macro to stream"
  (declare (ignore for-manual))
  (let ((*print-case* :downcase)
	(*print-pretty* nil))
    (format stream "- ~a~%~%" (description macro))
    (format stream "(defmacro ~a ~s~%   ...)~%"
	    (name macro)
	    (lambda-list macro))

    (when (slot-boundp macro 'commented-parms)
      (loop for (parm string) in (commented-parms macro)
	    collect (format stream "  ~a: ~a~%"
			    (if (listp parm) (first parm) parm)
			    string)))
    (format stream "~%")

    (case (called-by-application macro)
      (:rarely    (format stream "Normally, you do NOT call this macro.~%"))
      (:always    (format stream "You call this macro in EACH application.~%"))
      (:sometimes (format stream "You CAN call this macro.~%"))
      (otherwise  (format stream "You CAN call this macro ~a.~%"
			  (called-by-application macro))))
	
    (case (called-by-gina macro)
      ((t)       (format stream "It is sometimes called by GINA.~%"))
      ((nil)     (format stream "It is never called by GINA.~%"))
      (otherwise (format stream "It is called by GINA ~a.~%"
			 (called-by-gina macro))))

    (when (slot-boundp macro 'comments)
      (when (comments macro)
	;;(format stream "~%")
	(loop for comment in (comments macro)
	      do (format stream "~a~%" comment))))

    (when (slot-boundp macro 'examples)
      (let ((*print-pretty* t))
	(when (examples macro)
	  (format stream "Example:~%")
	  (loop for example in (examples macro)
		when (not (stringp example))
		  do ;;(format stream "  * ")
		    (format stream "~s~%" example)
		else
		  do ;;(format stream "    ")
		    (format stream "~a~%" example)))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;    meta information about functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((function gina-function) stream)
  "specialized printed representation"
  (let ((*print-case* :downcase))
    (format stream "#<gina-function ~a>"
	    (if (slot-boundp function 'name) (name function) "???"))))

(defmethod short-name ((function gina-function))
  "short string representation"
  (let ((*print-case* :downcase))
    (format nil "~a" (name function))))

(defun make-gina-function (name lambda-list description 
			   commented-parms comments examples
			   &aux new old)
  "create a new gina-function object and store it in the hash-table"
  (when (not (and (listp description) (eq 'description (first description))))
    (error "function description must be a a string or a list (description \"...\" ...)"))
  
  ;; check if gina-function already exists
  (setq old (find-gina-function name))
  (if old
      ;; just change some slots
      (progn
	(setq new old)
	(setf (lambda-list new)     lambda-list)
	(setf (description new)     (second description))
	(setf (commented-parms new) commented-parms)
	(setf (comments new) comments)
	(setf (examples new) examples))
      ;; else: create a new function-object
      (setq new (make-instance 'gina-function
			   :name name
			   :lambda-list lambda-list
			   :description (second description)
			   :commented-parms commented-parms
			   :comments comments
			   :examples examples
			   :index-number (incf *index-counter*))))
  
  ;; analyze (description "...." ...)
  (apply #'set-function-description (cons new (rest description)))
  
  (when (not old)
    (setf (gethash name *gina-function-table*) new))
  new)

(defun set-function-description (function string
				 &key (export t)
				 (called-by-gina t)
				 ;; t nil <string>
				 (called-by-application :sometimes)
				 ;; :rarely :always :sometimes <string>
				 (constructor-for-class nil)
				 (result nil)
				 (bugs nil)
				 (topics nil)
				 &aux class)
  "store details of description in the gina-function object"
  ;; this method documents the keywords allowed in the description of a function
  (declare (ignore string))
  (when export
    (export (name function)))
  (when constructor-for-class
    (setq class (find-gina-class constructor-for-class))
    (when class (setf (constructor class) function)))
  (setf (called-by-gina function) called-by-gina)
  (setf (called-by-application function) called-by-application)
  (setf (constructor-for-class function) constructor-for-class)
  
  (when (and (not result) constructor-for-class)
    (setq result
	  (let ((*print-case* :downcase))
	    (format nil "a newly created object of class ~a" constructor-for-class))))
  (setf (result function) result)
  
  (setf (bugs function) bugs)
  (setf (topics function) topics))

(eval-when (eval load)
  (export 'defginafun))
(defmacro defginafun (name lambda-list description &body body
		      &aux uncommented-lambda-list lambda-list-without-aux
			 (last-parm nil) (commented-parms nil)
			 description-string comments examples declarations)
  "GINA version of defun"
  
  ;; the description may be just a string
  (when (stringp description)
    (setq description `(description ,description)))
  (setq description-string (second description))

  ;; additional strings are allowed in the lambda-list as comments
  (setq uncommented-lambda-list
	(loop for parm in lambda-list
	      when (not (stringp parm)) collect parm
	      else do (push (list last-parm parm) commented-parms)
	      do (setq last-parm parm)))
  (setq commented-parms (reverse commented-parms))

  ;; throw out &aux-variables
  (setq lambda-list-without-aux
	(loop for parm in uncommented-lambda-list
	      while (not (eq '&aux parm))
	      collect parm))

  ;; filter out comments before the body
  (setq comments
	(loop while (and body (first body) (listp (first body))
			 (eq 'comment (first (first body))))
	      append (cdr (first body)) ;; the comment
	      do (pop body)))
  
  ;; filter out examples before the body
  (setq examples
	(loop while (and body (first body) (listp (first body))
			 (eq 'example (first (first body))))
	      append (cdr (first body)) ;; the example
	      do (pop body)))
  
  (setq declarations
	(loop while (and body (first body) (listp (first body))
			 (eq 'declare (first (first body))))
	      collect (first body) ;; the declaration 
	      do (pop body)))

  `(progn
     ,(when *generate-metainfo* 
	`(make-gina-function ',name ',lambda-list-without-aux ',description
			     ',commented-parms ',comments ',examples))

     ,(append `(defun ,name ,uncommented-lambda-list ,description-string)
	      declarations
	      (when *insert-debug-output*
		`((when (and (boundp '*application*) (debug-out *application*))
		    (format t "~a " ',name))))
	      body)))

(defun function< (f1 f2)
  "determine if f1 < f2"
  (string< (symbol-name (name f1)) (symbol-name (name f2))))

(defun all-gina-functions (&aux the-list)
  "collect all hash-table entries and sort them"
  (maphash #'(lambda (key value) (declare (ignore key)) (push value the-list))
	   *gina-function-table*)
  (setq the-list
	(sort the-list #'function<))
  the-list)

'(print (all-gina-functions))

(defun find-gina-function (name-symbol)
  "lookup name-symbol in hash-table"
  (gethash name-symbol *gina-function-table*))

'(describe (find-gina-function 'anfang))

(defmethod description-text ((function gina-function))
  "print description into a string"
  (replace-returns
    (with-output-to-string (text)
      (print-description function text))))

(defmethod print-description ((function gina-function) stream &key (for-manual nil))
  "print decsription of the function to stream"
  (declare (ignore for-manual))
  (let ((*print-case* :downcase)
	;;(*print-escape* nil)
	(*print-pretty* t))
    (format stream "- ~a~%~%" (description function))
    (format stream "(defun ~a ~s~%   ...)~%"
	    (name function)
	    (lambda-list function))

    (loop for (parm string) in (commented-parms function)
	  collect (format stream "  ~a: ~a~%"
			  (if (listp parm) (first parm) parm)
			  string))
    (format stream "~%")

    (when (constructor-for-class function)
      (format stream "This is the constructor function for class ~a.~%"
	      (constructor-for-class function)))

    (when (result function)
      (format stream "The returned value is ~a.~%" (result function)))

    (case (called-by-application function)
      (:rarely    (format stream "Normally, you do NOT call this function.~%"))
      (:always    (format stream "You call this function in EACH application.~%"))
      (:sometimes (format stream "You CAN call this function.~%"))
      (otherwise  (format stream "You CAN call this function ~a.~%"
			  (called-by-application function))))
	
    (case (called-by-gina function)
      ((t)       (format stream "It is sometimes called by GINA.~%"))
      ((nil)     (format stream "It is never called by GINA.~%"))
      (otherwise (format stream "It is called by GINA ~a.~%"
			 (called-by-gina function))))
	
    (when (comments function)
      ;;(format stream "~%")
      (loop for comment in (comments function)
	    do (format stream "~a~%" comment)))

    (let ((*print-pretty* t))
      (when (examples function)
	(format stream "Example:~%")
	(loop for example in (examples function)
	      when (not (stringp example))
		do ;;(format stream "  * ")
		  (format stream "~s~%" example)
	      else
		do ;;(format stream "    ")
		  (format stream "~a~%" example))))

    ))

'(print-description (find-gina-function 'select-dialog) t :for-manual t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;    meta information about classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((class gina-class) stream)
  "specialized printed representation"
  (let ((*print-case* :downcase))
    (format stream "#<gina-class ~a>"
	    (if (slot-boundp class 'name) (name class) "???"))))

(defmethod short-name ((class gina-class))
  "short string representation"
  (let ((*print-case* :downcase))
    (format nil "~a" (name class))))

(defun make-gina-class (name superclass-names class-variables
			instance-variables description
			&aux old new)
  "create a new gina-class object and store it in the hash-table"
  (when (not (and (listp description) (eq 'description (first description))))
    (error "class description must be a a string or a list (description \"...\" ...)"))
  
  ;; check if gina-class already exists
  (setq old (find-gina-class name))
  (if old
      ;; change some slots
      (progn
	(setq new old)
	(setf (superclass-names new)   superclass-names)
	(setf (class-variables new)    class-variables)
	(setf (instance-variables new) instance-variables)
	(setf (description new)        (second description)))
      ;;else create new instance
      (setq new (make-instance 'gina-class
			       :name name
			       :superclass-names superclass-names
			       :class-variables class-variables
			       :instance-variables instance-variables
			       :description (second description)
			       :index-number (incf *index-counter*))))
  
  ;; analyze (description "...." ...)
  (apply #'set-class-description (cons new (rest description)))

  (when (not old)
    (setf (gethash name *gina-class-table*) new))

  new)

(defun set-class-description (class string
			      &key (export t)
				   (export-slots t)
				   (application-subclasses :sometimes)
				   ;; :rarely :always :sometimes <string>
				   (gina-subclasses nil)  ;; t nil <string>
				   (instantiate t)  ;; t nil <string>
				   )
  "store details of description in the gina-class object"
  ;; this method documents the keywords allowed in the description of a class
  (declare (ignore string))
  (when export
    (export (name class)))
  (when export-slots
    (if (eq t export-slots)
	;; export all slots
	(loop for slot in (append (class-variables class) (instance-variables class))
	      do (if (atom slot) (export slot) (export (first slot))))
	;; else export listed slots
	(loop for slot in export-slots
	      do (export slot))))
  (setf (application-subclasses class) application-subclasses)
  (setf (gina-subclasses class) gina-subclasses)
  (setf (instantiate class) instantiate)
  )

(eval-when (eval load)
  (export 'defginaclass))
(defmacro defginaclass (name includes description slots &rest options
			&aux (class-variables nil)
			(instance-variables nil)
			description-string)
  "GINA version of defclass"
  
  ;; analyze allocation  information of slots
  (loop for slot in slots
	when (and (listp slot)
		  (member :allocation slot)
		  (member :class slot))
	  do (push slot class-variables)
	else
	  do (push slot instance-variables))
  (setq class-variables     (reverse class-variables))
  (setq instance-variables  (reverse instance-variables))

  ;; the description may be just a string
  (when (stringp description)
    (setq description `(description ,description)))
  (setq description-string (second description))

  ;; build documentation option from description
  (push `(:documentation ,description-string) options)

  `(progn
     ,(when *generate-metainfo* 
	`(make-gina-class ',name ',includes ',class-variables
			  ',instance-variables ',description))
	  
     ,(append `(defclass ,name ,includes ,slots)
	      options)))

'(defginaclass toggle-button (label)
   "the Motif ToggleButton widget"
   ((value :accessor value :initarg :value)
    (value-changed-callback :accessor value-changed-callback :initarg :value-changed-callback)
    (arm-callback    :accessor arm-callback :initform nil)
    (disarm-callback :accessor disarm-callback :initform nil)
    )
   (:documentation "the Motif ToggleButton widget"))

(defmacro defcallback (widget-class callback-slot motif-callback-name
		       &key (lambda-list "(Parameters not yet documented)")
		       (hidden nil))
  "macro to define :after-methods for callback slot accessors"
  `(progn
     ,(when *generate-metainfo* 
	`(when (find-gina-class ',widget-class)
	   (setf (callbacks (find-gina-class ',widget-class))
	     (loop for triple in (callbacks (find-gina-class ',widget-class))
	      when (not (eq (first triple) ',callback-slot))
	      collect triple))
	   ,(when (not hidden)
	      `(push ',(list callback-slot motif-callback-name lambda-list)
		     (callbacks (find-gina-class ',widget-class))))))
     ,(when motif-callback-name
	`(defmethod (setf ,callback-slot) :after (new-callback (x ,widget-class))
		    "the callback is set or changed => inform toolkit server"
		    (set-callback x ,motif-callback-name new-callback)))))
'(print (callbacks (find-gina-class 'radio-button-group)))

'(defcallback shell popup-callback :popup)
'(describe (find-gina-class 'shell))

(defmacro defresourceslot (widget-class slot motif-resource &key (mode '(:read :write)))
  "macro to define :before- and :after-methods for slots corresponding to a resource"
  ;;called-by-gina for each resource which is cached on the Lisp side
  `(progn
     ,(when *generate-metainfo* 
	`(progn
	   (setf (resource-slots (find-gina-class ',widget-class))
	     (loop for resource-slot 
	               in (resource-slots (find-gina-class ',widget-class))
	      when (not (eq ',slot (first resource-slot)))
	      collect resource-slot))
	   (push ',(list slot motif-resource)
		 (resource-slots (find-gina-class ',widget-class)))))
     ,(when (member :read mode)
	`(defmethod  ,slot :before ((x ,widget-class))
		     "ask toolkit server before returning the slot"
		     (with-slots (,slot) x
		       (setq ,slot (first (xtk:get-values (widget-id x) 
							  ,motif-resource))))))
     ,(when (member :write mode)
	`(defmethod (setf ,slot) :after (new-value (x ,widget-class))
		    "set resource in the toolkit"
		    (xtk:set-values (widget-id x) ,motif-resource new-value)))))

'(defresourceslot scale value :value :mode (:read))

(defmethod class-object ((class gina-class))
  "find the underlying class object of PCL"
  (find-class (name class)))

(defmethod sort-methods ((class gina-class))
  "sort list of all methods of this class"
  (setf (methods class) (sort (methods class) #'method<)))

'(describe (class-object (find-gina-class 'toggle-button)))
'(inspect-dialog (find-class 'document)) ;; => error
'(describe (find-class 'document))

;(pcl:class-class-precedence-list (find-class 'application))
;(pcl:class-local-supers (find-class 'application))
;(pcl:class-direct-subclasses (find-class 'application))

(defun class< (c1 c2)
  "determine if c1 < c2"
  (string< (symbol-name (name c1)) (symbol-name (name c2))))

(defun all-gina-classes (&aux the-list)
  "collect all hash-table entries and sort them"
  (maphash #'(lambda (key value) (declare (ignore key)) (push value the-list))
	   *gina-class-table*)
  (setq the-list
	(sort the-list #'class<))
  the-list)

'(print (all-gina-classes))

(defun find-gina-class (name-symbol)
  "lookup name-symbol in hash-table"
  (gethash name-symbol *gina-class-table*))

'(describe (find-gina-class 'toggle-button))

(defmethod subclasses ((gina-class gina-class))
  "find all direct subclasses of a given class"
  (or (known-subclasses gina-class)
      (setf (known-subclasses gina-class)
	    (loop for class in (all-gina-classes)
		  when (member (name gina-class) (superclass-names class)) collect class))))

'(print (subclasses (find-gina-class 'widget)))

(defmethod all-subclasses ((gina-class gina-class))
  "find all direct or indirect subclasses of a given class"
  (cons gina-class
	(loop for direct-subclass in (subclasses gina-class)
	      append (all-subclasses direct-subclass))))

'(print (all-subclasses (find-gina-class 'widget)))
'(print (all-subclasses (find-gina-class 'form)))

(defmethod superclasses ((gina-class gina-class))
  "find all direct superclasses of a given class"
  (loop for class-name in (superclass-names gina-class)
	collect (find-gina-class class-name)))

(defmethod all-superclasses ((gina-class gina-class))
  "find all direct or indirect superclasses of a given class"
  (cons gina-class
	(loop for direct-superclass in (superclasses gina-class)
	      append (all-superclasses direct-superclass))))

'(print (all-superclasses (find-gina-class 'form)))

(defmethod description-text ((gina-class gina-class))
  "print description into a string"
  (replace-returns
    (with-output-to-string (text)
      (print-description gina-class text))))

(defmethod print-description ((gina-class gina-class) stream &key (for-manual nil)
			     &aux all-superclasses all-callbacks
			          any-slot-found slot-found-for-class
				  own-methods all-methods)
  "print decsription of the class to stream"
  (let ((*print-case* :downcase))
    (format stream "- ~a~%~%" (description gina-class))

    (setq all-superclasses (all-superclasses gina-class))
    (when (superclass-names gina-class)
      (format stream "It is a subclass of ")
      (loop for superclass in (cdr all-superclasses)
	    do (format stream "~a~a"
		       (short-name superclass)
		       (cond ((eq superclass (car (last (cdr all-superclasses))))
			      ".")
			     ((eq superclass (car (last (butlast (cdr all-superclasses)))))
			      " and ")
			     (t ", "))))
      (format stream "~%"))

    (case (application-subclasses gina-class)
      (:rarely    (format stream "You RARELY define subclasses of this class.~%"))
      (:always    (format stream "You MUST define a subclass of this class.~%"))
      (:sometimes (format stream "You CAN define a subclass of this class.~%")) 
      (otherwise  (format stream "You define a subclass of this class ~a.~%"
			  (application-subclasses gina-class))))

    (case (gina-subclasses gina-class)
      ((t)       (format stream "GINA defines subclasses of this class.~%"))
      ((nil)     (format stream "GINA does NOT define subclasses of this class.~%"))
      (otherwise (format stream "GINA defines subclasses of this class: ~a.~%"
			 (gina-subclasses gina-class))))

    (case (instantiate gina-class)
      ((t)       (format stream "You can create an instance of this class.~%"))
      ((nil)     (format stream "You never create an instance of this class.~%"))
      (otherwise (format stream "You create an instance of this class ~a.~%"
			 (instantiate gina-class))))

    ;; own and inherited callbacks
    (setq all-callbacks
	  (loop for class in all-superclasses
		append (reverse (callbacks class))))
    (when all-callbacks
      (format stream "Callbacks: ~%")
      (loop for (callback-slot motif-callback-name lambda-list) in all-callbacks
	    do (when motif-callback-name nil) ;; to make compiler happy
	       (format stream "  ~a ~a~%" callback-slot lambda-list)))
	
    ;; documented slots
    (setq any-slot-found nil)
    (loop for class in all-superclasses
	  do
      (setq slot-found-for-class nil)
      (loop for slot in (class-variables class)
	    when (and (listp slot) (member :documentation slot))
	      do (when (not any-slot-found)
		   (format stream "Class variables:~%")
		   (setq any-slot-found t))
		 (when (not slot-found-for-class)
		   (when (> (length all-superclasses) 1)
		     (format stream "  ~a:~%" (name class)))
		   (setq slot-found-for-class t))
		 (format stream "    ~20a ~a~%"
			 (first slot)
			 (nth (1+ (position :documentation slot)) slot))
		 (when (member :initform slot)
		   (format stream "         Default:  ~s~%"
			   (nth (1+ (position :initform slot)) slot)))
;		    else do (format stream "   ~20a~%"
;				 (if (listp slot) (first slot) slot))
		 ))

    (setq any-slot-found nil)
    (loop for class in all-superclasses
	  do
      (setq slot-found-for-class nil)
      (loop for slot in (instance-variables class)
	    when (and (listp slot) (member :documentation slot))
	      do (when (not any-slot-found)
		   (format stream "Instance variables:~%")
		   (setq any-slot-found t))
		 (when (not slot-found-for-class)
		   (when (> (length all-superclasses) 1)
		     (format stream "  ~a:~%" (name class)))
		   (setq slot-found-for-class t))
		 (format stream "    ~20a ~a~%"
			 (first slot)
			 (nth (1+ (position :documentation slot)) slot))
		 (when (member :initform slot)
		   (format stream "        Default:  ~s~%"
			   (nth (1+ (position :initform slot)) slot)))
		 
;		    else do (format stream "   ~20a~%"
;				 (if (listp slot) (first slot) slot))
		 ))
    

    (when (resource-slots gina-class) 
      (format stream "~%Resource slots: ")
      (loop for (slot motif-resource) in (resource-slots gina-class)
	    when (equal (symbol-name slot) (symbol-name motif-resource))
	      do (format stream "~a " slot)
	    else
	      do (format stream "~a (corresponds to Motif-resource ~s) "
			 slot motif-resource))
      (format stream "~%"))

    (when for-manual
      (setq own-methods (sort-methods gina-class))
      (format stream "~%Methods of class ~a:~%" (name gina-class))
      (loop for method in own-methods
	    do (format stream "  ~a          (Page m~a)~%          ~a~%"
		       (name-with-indicator method :full-lambda-list t)
		       (index-number method)
		       (description method)))

      ;; do NOT show methods of class widget all the time
      (setq all-superclasses (delete (find-gina-class 'widget) all-superclasses))
      (setq all-methods (sort (loop for class in (cdr all-superclasses)
				    append (methods class))
			      #'method<))
      (when all-methods
	(format stream "~%Methods of Superclasses:~%")
	(loop for method in all-methods
	      do (format stream "  ~a          (Page m~a)~%          ~a~%"
			 (name-with-indicator method :full-lambda-list t)
			 (index-number method)
			 (description method)))))
    ))

'(print-description (find-gina-class 'document) t :for-manual t)
'(print-description (find-gina-class 'view-object) t :for-manual t)
'(print-description (find-gina-class 'direct-manipulation-object) t :for-manual t)
'(print-description (find-gina-class 'scrollbar) t :for-manual t)
'(print-description (find-gina-class 'view) t :for-manual t)
'(print-description (find-gina-class 'object-mover) t :for-manual t)

(defmethod print-subtree ((gina-class gina-class) stream indentation
			  &key (for-manual nil))
  "print class and all its subclasses as an indented tree"
  (format stream "~a~a         (Page c~a)~%~a              - ~a ~%"
	  (make-string (* indentation 2) :initial-element #\space)
	  (short-name gina-class)
	  (index-number gina-class)
	  (make-string (* indentation 2) :initial-element #\space)
	  (description gina-class))
  (loop for subclass in (subclasses gina-class)
	do (print-subtree subclass stream (1+ indentation) :for-manual for-manual)))


'(print-subtree (find-gina-class 'widget) t 1)
'(loop for basic-class in '(application document view-object command callback widget)
      do (print-subtree (find-gina-class basic-class) t 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;    meta information about methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((method gina-method) stream)
  "specialized printed representation"
  (let ((*print-case* :downcase))
    (format stream "#<gina-method ~a ~a ~a>"
	    (if (slot-boundp method 'name) (name method) "???")
	    (if (slot-boundp method 'qualifiers)
		(if (qualifiers method) (first (qualifiers method)) "")
		"???")
	    (if (slot-boundp method 'type-specifiers) (type-specifiers method) "???"))))

'(describe (first (all-gina-methods)))

(defmethod short-name ((method gina-method))
  "short string representation"
  (or (known-short-name method)
      (setf (known-short-name method)
	    (let ((*print-case* :downcase))
	      (format nil "~a ~a ~a"
		      (name method)
		      (if (qualifiers method) (first (qualifiers method)) "")
		      (type-specifiers method))))))

(defmethod name-with-indicator ((method gina-method)
				&key (full-lambda-list nil)
				&aux (new-name nil))
  "short string representation"
  (when (or (not (known-name-with-indicator method))
	    full-lambda-list)
    ;; recompute
    (setq new-name
	  (let ((*print-case* :downcase)
		(*print-pretty* nil))
	    (format nil "~a~a~a ~a~s"
		    (case (override method)
		      (:rarely " ") (:always "O") (otherwise "o"))
		    (case (called-by-application method)
		      (:rarely "  ") (:always "C ") (otherwise "c "))
		    (name method)
		    (if (qualifiers method)
			(format nil "~s " (first (qualifiers method)))
			"")
		    (if full-lambda-list
			(lambda-list method)
			(type-specifiers method)))))
    (when (not full-lambda-list)
      (setf (known-name-with-indicator method) new-name)))

  ;; return:
  (or new-name
      (known-name-with-indicator method)))

(defun make-gina-method (name qualifiers lambda-list description
			 commented-parms comments examples
			 &aux new old type-specifiers)
  "create a new gina-method object and store it in the hash-table"
  (when (not (and (listp description) (eq 'description (first description))))
    (error "method description must be a a string or a list (description \"...\" ...)"))
  
  ;; determine the type-specifiers from the lambda list
  (setq type-specifiers nil)
  (loop for parm in lambda-list
	when (member parm '(&optional &key &rest &allow-other-keys &aux))
	  do (return nil)
	do (push (if (listp parm) (second parm) t)
		 type-specifiers))
  (setq type-specifiers (reverse type-specifiers))

  ;; check if gina-method already exists
  (setq old (find-gina-method name qualifiers type-specifiers))
  (if old
      ;; just change some slots
      (progn
	(setq new old)
	(setf (lambda-list new)     lambda-list)
	(setf (qualifiers new)      qualifiers)
	(setf (type-specifiers new) type-specifiers)
	(setf (description new)     (second description))
	(setf (commented-parms new) commented-parms)
	(setf (known-short-name new) nil)
	(setf (known-name-with-indicator new) nil) 
	(setf (comments new) comments)
	(setf (examples new) examples))
      ;; else: create a new method-object
      (setq new (make-instance 'gina-method
			       :name name
			       :lambda-list lambda-list
			       :qualifiers qualifiers
			       :type-specifiers type-specifiers
			       :description (second description)
			       :commented-parms commented-parms
			       :comments comments
			       :examples examples
			       :index-number (incf *index-counter*))))
  
  ;; analyze (description "...." ...)
  (apply #'set-method-description (cons new (rest description)))

  (when (not old)
    (setf (gethash (list name qualifiers type-specifiers) *gina-method-table*) new)

    ;; store method in all relevant classes
    (loop for class in (relevant-classes new)
	  when class ;; maybe still unknown
	    do (push new (methods class))))
  
  new)

(defun set-method-description (method string
			       &key (export t)
			       (called-by-gina t) ;; t nil <string>
			       (called-by-application :rarely)
			       ;; :rarely :always :sometimes <string>
			       (override :rarely)
			       ;; :rarely :always :sometimes <string>
			       (default-version :simple)
			       ;; :simple :do-nothing <string>
			       (overridden-by-gina nil)  ;; t nil <string>
			       (result nil)) ;; nil <string>
  "store details of description in the gina-methdod object"
  ;; this method documents the keywords allowed in the description of a method
  (declare (ignore string))
  (when export (export (name method)))
  (setf (called-by-gina method) called-by-gina)
  (setf (called-by-application method) called-by-application)
  (setf (override method) override)
  (setf (default-version method) default-version)
  (setf (overridden-by-gina method) overridden-by-gina)
  (setf (result method) result)
)

(eval-when (eval load)
  (export 'defginamethod))
(defmacro defginamethod (name specialized-lambda-list description
			 &body body
			 &aux (qualifiers nil)
			 uncommented-lambda-list
			 (last-parm nil) (commented-parms nil)
			 description-string comments examples declarations)
  "GINA version of defmethod"

  ;; check for optional qualifier
  (when (atom specialized-lambda-list)
    ;; it's really a qualifier: move parms
    (setq qualifiers (list specialized-lambda-list))
    (setq specialized-lambda-list description)
    (setq description (first body))
    (pop body))
  
  ;; the description may be just a string
  (when (stringp description)
    (setq description `(description ,description)))

  (setq description-string (second description))

  ;; additional strings are allowed in the lambda-list as comments
  (setq uncommented-lambda-list
	(loop for parm in specialized-lambda-list
	      when (not (stringp parm))
		collect parm
	      else do (push (list last-parm parm) commented-parms)
	      do (setq last-parm parm)))
  (setq commented-parms (reverse commented-parms))

  ;; throw out &aux-variables
  (setq specialized-lambda-list
	(loop for parm in uncommented-lambda-list
	      while (not (eq '&aux parm))
	      collect parm))

  ;; filter out comments before the body
  (setq comments
	(loop while (and body (first body) (listp (first body))
			 (eq 'comment (first (first body))))
	      append (cdr (first body)) ;; the comment
	      do (pop body)))
  
  ;; filter out examples before the body
  (setq examples
	(loop while (and body (first body) (listp (first body))
			 (eq 'example (first (first body))))
	      append (cdr (first body)) ;; the example
	      do (pop body)))

  (setq declarations
	(loop while (and body (first body) (listp (first body))
			 (eq 'declare (first (first body))))
	      collect (first body) ;; the declaration 
	      do (pop body)))

  `(progn
     ,(when *generate-metainfo*
	`(make-gina-method ',name ',qualifiers ',specialized-lambda-list
		       ',description ',commented-parms ',comments ',examples))

     ,(append `(defmethod ,name)
	      qualifiers 
	      `(,uncommented-lambda-list ,description-string)
	      declarations
	      (when *insert-debug-output*
		`((when (and (boundp '*application*) (debug-out *application*))
		    (format t "~a " ',name))))
	      body)))

(eval-when (eval load)
  (export 'undefginamethod))
(defmacro undefginamethod (name specialized-lambda-list description
			   &body body
			   &aux (qualifiers nil) (type-specifiers nil)
			        gina-method clos-method)
  "GINA version of undefmethod"

  ;; check for optional qualifier
  (when (atom specialized-lambda-list)
    ;; it's really a qualifier: move parms
    (setq qualifiers (list specialized-lambda-list))
    (setq specialized-lambda-list description)
    (setq description (first body))
    (pop body))

  ;; determine the type-specifiers from the lambda list
  (loop for parm in specialized-lambda-list
	when (member parm '(&optional &key &rest &allow-other-keys &aux))
	  do (return nil)
	when (not (stringp parm))
	  do (push (if (listp parm) (second parm) t)
		   type-specifiers))
  (setq type-specifiers (reverse type-specifiers))
  
  
  ;; additional strings are allowed in the lambda-list as comments
  (setq specialized-lambda-list
	(loop for parm in specialized-lambda-list
	      when (not (stringp parm)) collect parm))

  (when *generate-metainfo*
    (setq gina-method (find-gina-method name qualifiers type-specifiers))
    (when (not gina-method)
      (error "method ~a ~a ~a not found!"
	     name
	     (if qualifiers (first qualifiers) "")
	     type-specifiers))
    ;; remove from table of all methods
    (remhash (list name qualifiers type-specifiers) *gina-method-table*)

    ;; remove from classes having a list of their methods
    (loop for class in (relevant-classes gina-method)
     do (setf (methods class) (delete gina-method (methods class)))))
  
  ;; remove from PCL
  (setq clos-method (method-object gina-method))
  (remove-method (symbol-function name) clos-method)
  (format t " ~S erased ~%" clos-method)
  t)

(eval-when (eval load)
  (export 'undefmethod))
(defmacro undefmethod (name specialized-lambda-list
		       &body body
		       &aux (qualifiers nil) (type-specifiers nil)
		            generic-function method)
  "an undefmethod with identical syntax as defmethod"

  ;; check for optional qualifier
  (when (atom specialized-lambda-list)
    ;; it's really a qualifier: move parms
    (setq qualifiers (list specialized-lambda-list))
    (setq specialized-lambda-list (first body))
    (pop body))

  ;; determine the type-specifiers from the lambda list
  (loop for parm in specialized-lambda-list
	when (member parm '(&optional &key &rest &allow-other-keys &aux))
	  do (return nil)
	do (push (if (listp parm) (second parm) t)
		 type-specifiers))
  (setq type-specifiers (reverse type-specifiers))

  (setq generic-function (symbol-function name))
  (setq method (find-method generic-function
			    qualifiers
			    (loop for specifier in type-specifiers
				  collect (if (listp specifier)
					      specifier ;; (eql form)
					      (find-class specifier)))))
  (remove-method generic-function method)
  (format t " ~S erased ~%" method)
  t)

(defmethod basic-name ((method gina-method))
  "return the name without a possible (setf ..."
  (if (symbolp (name method))
      (name method)
      (second (name method))))

(defun method< (m1 m2)
  "determine if m1 < m2"
  (string< (symbol-name (basic-name m1)) (symbol-name (basic-name m2))))

(defun all-gina-methods (&aux the-list)
  "collect all hash-table entries and sort them"
  (maphash #'(lambda (key value) (declare (ignore key)) (push value the-list))
	   *gina-method-table*)
  (setq the-list
	(sort the-list #'method<))
  the-list)

'(print (all-gina-methods))

(defun find-gina-method (name-symbol qualifiers type-specifiers)
  "lookup name-symbol in hash-table"
  (gethash (list name-symbol qualifiers type-specifiers) *gina-method-table*))

'(describe (find-gina-method 'quit-app nil '(application)))

(defun find-gina-methods-by-name (name-symbol &aux the-list)
  "find all method with a given name"
  (maphash #'(lambda (key method) (declare (ignore key))
		     (when (equal name-symbol (name method)) (push method the-list)))
	   *gina-method-table*)
  the-list)
'(print (find-gina-methods-by-name 'set-item-list))

(defmethod method-object ((method gina-method))
  "find the underlying method object of PCL"
  (find-method (symbol-function (name method))
	       (qualifiers method)
	       (loop for specifier in (type-specifiers method)
		     collect (if (listp specifier)
				 specifier ;; (eql form)
				 (find-class specifier)))))

'(describe (method-object (find-gina-method 'quit-command nil '(application))))

(defmethod relevant-classes ((method gina-method))
  "find all gina-class objects mentioned in the type-specifiers"
  (loop for specifier in (type-specifiers method)
	when (and (symbolp specifier) (not (eq t specifier)))
	  collect (find-gina-class specifier)))

'(relevant-classes (find-gina-method 'quit-app nil '(application)))

(defmethod related-methods ((method gina-method) &aux the-list)
  "find all gina-method objects with the same name"
  (maphash #'(lambda (key value) (declare (ignore key))
		     (when (equal (name value) (name method))
		       (push value the-list)))
	   *gina-method-table*)
  the-list)

'(print (related-methods (find-gina-method 'set-item-list nil '(selection-list t))))

(defmethod description-text ((method gina-method))
  "print description into a string"
  (replace-returns
    (with-output-to-string (text)
      (print-description method text))))

(defmethod print-description ((method gina-method) stream &key (for-manual nil)
			      &aux related-methods  relevant-classes)
  "print decsription of the method to stream"
  (let ((*print-case* :downcase)
	(*print-pretty* nil))
    (format stream "- ~a~%~%" (description method))
    (format stream "(defmethod ~a ~a~s~%   ...)~%"
	    (name method)
	    (if (qualifiers method)
		(format nil "~s " (first (qualifiers method)))
		"")
	    (lambda-list method))

    (loop for (parm string) in (commented-parms method)
	  collect (format stream "  ~a: ~a~%"
			  (if (listp parm) (first parm) parm)
			  string))
    (format stream "~%")

    (when (result method)
      (format stream "The returned value is ~a.~%" (result method)))

		 
    (case (override method)
      (:rarely    (format stream "You RARELY override this method or add deamons.~%"))
      (:always    (format stream "You MUST override this method.~%"))
      (:sometimes (format stream "You CAN override this method.~%"))
      (otherwise  (format stream "You override this method ~a.~%" (override method))))
		 
    (format stream "The default version ~a.~%"
	    (case (default-version method)
	      (:do-nothing "does not do anything")
	      (:simple     "is sufficient for most cases")
	      (otherwise   (default-version method))))
		 
    (case (called-by-application method)
      (:rarely    (format stream "Normally, you do NOT call this method.~%"))
      (:always    (format stream "You call this method in EACH application.~%"))
      (:sometimes (format stream "You CAN call this method.~%"))
      (otherwise  (format stream "You CAN call this method ~a.~%"
			  (called-by-application method))))
	
    (case (called-by-gina method)
      ((t)       (format stream "It is called by GINA when appropriate.~%"))
      ((nil)     (format stream "It is never called by GINA.~%"))
      (otherwise (format stream "It is called by GINA ~a.~%"
			 (called-by-gina method))))
		   

    (case (overridden-by-gina method)
      ((t)       (format stream "It is overridden by GINA for some subclasses.~%"))
      ((nil)     (format stream "It is NEVER overridden by GINA.~%"))
      (otherwise (format stream "It is overridden by GINA ~a.~%"
			 (overridden-by-gina method))))

    (when (comments method)
      ;;(format stream "~%")
      (loop for comment in (comments method)
	    do (format stream "~a~%" comment)))

    (let ((*print-pretty* t))
      (when (examples method)
	(format stream "Example:~%")
	(loop for example in (examples method)
	      when (not (stringp example))
		do ;;(format stream "  * ")
		  (format stream "~s~%" example)
	      else
		do ;;(format stream "    ")
		  (format stream "~a~%" example)
		  )))

    (when for-manual
      ;; classes where method can be called
      (setq relevant-classes
	    (loop for class in (relevant-classes method)
		  append (all-subclasses class)))
      (setq relevant-classes (set-difference relevant-classes (relevant-classes method)))
      (when (> (length relevant-classes) 0)
	(format stream "~%This method is inherited by class ")
	(loop for class in relevant-classes
	      do (format stream "~a~a" (short-name class)
			 (cond ((eq class (car (last relevant-classes)))
				".")
			       ((eq class (car (last (butlast relevant-classes))))
				   " and ")
			       (t ", "))))
	(format stream "~%"))

      ;; print methods with-the same name
      (setq related-methods (delete method (related-methods method)))
      (when related-methods
	(format stream "~%Related methods:~%")
	(loop for method in related-methods
	      do (format stream "    ~a~%"
			 (name-with-indicator method :full-lambda-list t)))))

    ))

'(print-description (find-gina-method 'set-item-list nil '(selection-list t))
		    t :for-manual t)
'(print-description (find-gina-method 'read-from-stream nil '(document t))
		    t :for-manual t)
'(print-description (find-gina-method 'doit nil '(command))
		    t :for-manual t)
'(print-description (find-gina-method 'draw-rectangle nil '(view-object t t t t))
		    t :for-manual t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;    automatic generation of the reference manual
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defmethod all-subclasses (object)
  (declare (ignore object))
  nil)



(defun extract-manual (output-file &rest input-files
		       &aux name type-specifiers specialized-lambda-list qualifiers)
  "scan input-files for (defgina... and write the description text into output-file"
  (let ((*print-case* :downcase))
  (with-open-file (ostream output-file :direction :output)
    ;; print class tree
    (format ostream "The Hierarchy of GINA Classes~%~%")
    (loop for basic-class in '(application document view-object command callback widget)
      do (print-subtree (find-gina-class basic-class) ostream 0 :for-manual t))

    ;; print classes, methods ... in the order of source files
    (loop for input-file in input-files
	  ;;do (format ostream "~%~|~a~%~%" input-file)
	  do (with-open-file (istream input-file :direction :input)
	       (loop as form = (read istream nil :eof)
		     while (not (eq form :eof))

		     ;; something like '(defginaclass application ....)
		     when (and (listp form)
			       (eq 'quote (first form))
			       (listp (second form))
			       (eq 'defginaclass (first (second form))))
		       do (setq form (second form))
			  (setq name (second form))
			  (format ostream "~%~|")
			  (format ostream "~%~%----------------------------------------------------------------------~%")
			  (format ostream "Class ~a~%~%" name)
			  (print-description (find-gina-class name) ostream :for-manual t)

		     ;; something like '(defginamacro pipapo ....)
		     when (and (listp form)
			       (eq 'quote (first form))
			       (listp (second form))
			       (eq 'defginamacro (first (second form))))
		       do (setq form (second form))
			  (setq name (second form))
			  (format ostream "~%~%----------------------------------------------------------------------~%")
			  (format ostream "Macro ~a~%~%" name)
			  (print-description (find-gina-macro name) ostream :for-manual t)
			     
		     ;; something like (defginamethod doit ....)
		     when (and (listp form)
			       (eq 'defginamethod (first form)))
		       do (setq name (second form))
			  ;; analyze specializers ...
			  (if (atom (third form))
			      ;; it's a qualifier
			      (progn
				(setq qualifiers (list (third form)))
				(setq specialized-lambda-list (fourth form)))
			      ;; else: no qualifier
			      (progn
				(setq qualifiers nil)
				(setq specialized-lambda-list (third form))))		       

			  ;; determine the type-specifiers from the lambda list
			  (setq type-specifiers nil)
			  (loop for parm in specialized-lambda-list
				when (member parm
					     '(&optional &key &rest &allow-other-keys &aux))
				  do (return nil)
				when (not (stringp parm))
				  do (push (if (listp parm) (second parm) t)
					   type-specifiers))
			  (setq type-specifiers (reverse type-specifiers))

			  (format ostream "~%~%----------------------------------------------------------------------~%")
			  (format ostream "Method ~a~%~%" name)
			  (when (not (find-gina-method name qualifiers type-specifiers))
			    (format t "~a ~a ~a" name qualifiers type-specifiers))
			  (print-description
			    (find-gina-method name qualifiers type-specifiers) ostream
			    :for-manual t)

		     ;; something like (defginafun parent-directory ....)
		     when (and (listp form)
			       (eq 'defginafun (first form)))
		       do (setq name (second form))
			  (format ostream "~%~%----------------------------------------------------------------------~%")
			  (format ostream "Function ~a~%~%" name)
			  (print-description (find-gina-function name) ostream :for-manual t)

		     ;; something like (defginavar *application* ....)
		     when (and (listp form)
			       (eq 'defginavar (first form)))
		       do (setq name (second form))
			  (format ostream "~%~%----------------------------------------------------------------------~%")
			  (format ostream "Variable ~a~%~%" name)
			  (print-description (find-gina-variable name) ostream :for-manual t)
		     ;; something like (ginacomment " .... ")
		     when (and (listp form)
			       (eq 'ginacomment (first form)))
		       do (format ostream "~%~|")
			  (loop for comment in (cdr form)
				do (format ostream "~a~%" comment))
		       
			 )))
    )))

'(extract-manual "/home/spenke/gina/reference-manual.text"
		 "/home/spenke/gina/globals.lisp"
		 "/home/spenke/gina/framework.lisp"
		 "/home/spenke/gina/views.lisp"
		 "/home/spenke/gina/commands.lisp"
		 "/home/spenke/gina/motif-widgets.lisp"
		 "/home/spenke/gina/dialogs.lisp"
                 "/home/spenke/gina/background.lisp"
		 "/home/spenke/gina/OS-dependent.lisp"
		 )

