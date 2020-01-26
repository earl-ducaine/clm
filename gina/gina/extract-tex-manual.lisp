;;; -*- Mode:LISP;Syntax: Common-Lisp;Package: GINA; Base:10-*-
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;   extract manual for latex-input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :GINA)

(defvar *section-for-routine*) (setq *section-for-routine* t)

(defun section-or-subsection ()
  (if *section-for-routine*
      "section"
      "subsection"))


(defun print-tex-header (stream name type &optional (with-asterisk nil))
  ;; header with lines
  (format stream "\\vspace{~acm}\\begin{flushleft}~%"
	  (if *section-for-routine* "0.15" "0.35"))
  (format stream "\\~a~a~a~a \\hfill ~a~a\\\\*~%"
	  (section-or-subsection)
	  (if with-asterisk
	      "*{\\arabic{chapter}\\hspace{6mm}"
	      (format nil "[~a ~a]{" type
		      (escape-special (format nil "~a" name))))
	  (if *section-for-routine* "" "{\\Large {\\bf ")
	  (escape-special (format nil "~a" name))
	  type
	  (if *section-for-routine* "" "}}"))
  (format stream "\\vspace{~acm}\\protect\\rule{\\textwidth}{1mm}\\\\*~%"
	  (if *section-for-routine* "-1.3" "-1.2"))
  (format stream "\\vspace{~acm}\\protect\\rule{\\textwidth}{0.5mm}}~%"
	  (if *section-for-routine* "0.5" "0.7"))
  (format stream "\\end{flushleft}~%~%"))


'(progn (print-tex-header t "Draw" "Function" t)
	(print-tex-header t "Draw" "Function"))

(defun begin-tex-table (stream &optional (number-of-chars 23))
  (format stream "\\begin{tabbing}~%")
  ;; set one tabulator after number-of-chars:
  (format stream "{\\tt ~a}\\= \\kill~%" (make-string number-of-chars :initial-element #\m)))

(defun first-tex-word (stream word &optional (max-number-of-chars 23))
  (when (not (stringp word))
    (setq word (format nil "~a" word)))
  (format stream "{\\tt ~a} " word)
  (if (> (length word) max-number-of-chars) ;; does word exceed width of column?
      (format stream "\\\\~% \\> ")         ;; follow text on next line after tab
      (format stream "\\> "))               ;; follow text same line after tab
)

(defun end-tex-table (stream)
  (format stream "\\end{tabbing}~%~%"))

'(format t "~a" (escape-special "hallo & <hali> & #hash und #has und &key <date> <datum>"))

(defun escape-special (string)
  (escape-with-dollar #\<
    (escape-with-dollar #\>
      (escape-with-backslash #\#
        (escape-with-backslash #\& string)))))
  
(defun escape-with-backslash (char string &aux position)
  (setq position (position char string :test #'char-equal))
  (if position
	  (format nil "~a~a~a" (subseq string 0 position) (format nil "\\~a" char)
		  (escape-with-backslash char (subseq string (1+ position))))
	  string))

(defun escape-with-dollar (char string &aux position)
  (setq position (position char string :test #'char-equal))
  (if position
	  (format nil "~a~a~a" (subseq string 0 position) (format nil "$~a$" char)
		  (escape-with-dollar char (subseq string (1+ position))))
	  string))
  
(defun escape-hash (string &aux position)
  (setq position (position #\& string))
  (if position
	  (format nil "~a~a~a" (subseq string 0 position) "\\#"
		  (escape-hash (subseq string (1+ position))))
	  string))

(defmethod print-tex-description ((variable gina-variable) stream)
  "print description of the variable to stream"
  (let ((*print-case* :downcase)
	(*print-pretty* nil))

    ;; header with lines
    (print-tex-header stream (name variable) "Variable")

    (format stream  "\\index{~a (Variable)}~%" (name variable))

    (format stream "{\\bf Value: ~a.} \\\\~%~%" (description variable))
    
    (format stream "Default: ~a\\\\~%" (default variable))

    (format stream "\\begin{flushleft}~%")
    (case (modify variable)
      (:rarely    (format stream "Normally, you do {\\bf not} modify this variable."))
      (:always    (format stream "You modify this variable in {\\bf each} application."))
      (:sometimes (format stream "You {\\bf can} modify this variable."))
      (otherwise  (format stream "You {\\bf can} modify this variable ~a."
			  (modify variable))))
    (format stream " \\\\~%" (default variable))
    (when (slot-boundp variable 'comments)
      (when (comments variable)
	(loop for comment in (comments variable)
	      do (format stream "~a \\\\~%" comment))))
    (format stream "\\end{flushleft}~%~%")
    ))

'(print-tex-description (find-gina-variable '*display*) t)

(defmethod print-tex-description ((macro gina-macro) stream)
  "print description of the macro to stream"
  (let ((*print-case* :downcase)
	(*print-pretty* nil))

    ;; header with lines
    (print-tex-header stream (name macro) "Macro")

    (format stream "\\index{~a (Macro)}~%"
	    (escape-special (format nil "~a" (name macro))))
    (format stream "Purpose: {\\bf ~a} \\\\~%~%" (description macro))

    ;; give header:
    (format stream "\\begin{flushleft}~%")
    (format stream "{\\tt~%(defmacro ~a ~a)} \\\\~%"
	    (name macro)   
	    (escape-special (format nil "~s" (lambda-list macro))))
    (format stream "\\end{flushleft}~%~%")

    (when (commented-parms macro)
      (begin-tex-table stream)
      (loop for (parm string) in (commented-parms macro)
	  do (first-tex-word stream
			     (format nil "~a:" (if (listp parm) (first parm) parm)))
	     (format stream "~a \\\\~%" string))
      (end-tex-table stream))

    (format stream "\\begin{flushleft}~%")
    (case (called-by-application macro)
      (:rarely	  (format stream "Normally, you do {\\bf not} call this macro.~%"))
      (:always    (format stream "You call this macro in {\\bf each} application.~%"))
      (:sometimes (format stream "You {\\bf can} call this macro.~%"))
      (otherwise  (format stream "You {\\bf can} call this macro ~a.~%"
			  (called-by-application macro))))
    (format stream " \\\\~%")
	
    (case (called-by-gina macro)
      ((t)       (format stream "It is called by GINA when appropriate.~%"))
      ((nil)     (format stream "It is {\\bf never} called by GINA.~%"))
      (otherwise (format stream "It is called by GINA ~a.~%"
			 (called-by-gina macro))))
    (format stream " \\\\~%")

    (when (comments macro)
      (loop for comment in (comments macro)
	    do (format stream "~a \\\\~%" comment)))
    (format stream "\\end{flushleft}~%~%")

    (let ((*print-pretty* t))
      (when (examples macro)
	(format stream "\\subsection*{Example}~%")
	(loop for example in (examples macro)
	      when (not (stringp example))
		do (format
		     stream
		     "\\footnotesize\\begin{verbatim}~%~s~%\\end{verbatim}\\normalsize~%"
		     example)
	      else
		do (format stream "~a \\\\~%" example)
		  )))
    ))

'(print-tex-description (find-gina-function 'make-application) t)
'(examples (find-gina-function 'make-application))

'(let ((*print-case* :downcase)
	(*print-pretty* t)
	(function1 (find-gina-function 'make-open-dialog-box))
	(function2 (find-gina-function 'question-dialog)))
   (loop for function in (all-gina-functions)
	 do(format t "~s~%"
	    (list 'defun (name function) (lambda-list function)))
	   (format t "(defun ~a ~s~%   ...)~%"
	    (name function)
	    (lambda-list function))))

(defmethod print-tex-description ((function gina-function) stream)
  "print decsription of the function to stream"
  
  (let ((*print-case* :downcase)
	(*print-pretty* t))
    
    ;; header with lines
    (print-tex-header stream (name function) "Function")

    (format stream  "\\index{~a (Function)}~%" (name function))
    (format stream "{\\bf Purpose: ~a.}\\\\~%~%" (description function))

    ;; give header:
    (format stream "\\footnotesize\\begin{verbatim}~%")
    ;; pretty-print einer liste mit (defun .. liefert leider quote statt '.
    (format stream "~s~%\\end{verbatim}\\normalsize~%~%"
	    (list 'defun (name function) (lambda-list function)))

    (when (commented-parms function)
      (begin-tex-table stream)
      (loop for (parm string) in (commented-parms function)
	    do (first-tex-word stream
			       (format nil "~a:" (if (listp parm) (first parm) parm)))
	       (format stream "~a \\\\~%" string))
      (end-tex-table stream))

    (format stream "\\begin{flushleft}~%")
    (when (constructor-for-class function)
      (format stream "This is the constructor function for class ~a. \\\\~%"
	      (constructor-for-class function)))
    (when (result function)
      (format stream "The returned value is ~a. \\\\~%" (result function)))
    (case (called-by-application function)
      (:rarely	  (format stream "Normally, you do {\\bf not} call this function."))
      (:always    (format stream "You call this function in {\\bf each} application."))
      (:sometimes (format stream "You {\\bf can} call this function."))
      (otherwise  (format stream "You {\\bf can} call this function ~a."
			  (called-by-application function))))
    (format stream " \\\\~%")	
    (case (called-by-gina function)
      ((t)       (format stream "It is sometimes called by GINA."))
      ((nil)     (format stream "It is never called by GINA."))
      (otherwise (format stream "It is called by GINA ~a."
			 (called-by-gina function))))
    (format stream " \\\\~%")
    (when (comments function)
      (loop for comment in (comments function)
	    do (format stream "~a \\\\~%" comment)))
    (format stream "\\end{flushleft}~%~%")

    (let ((*print-pretty* t))
      (when (examples function)
	(format stream "\\subsection*{Example}~%")
	(loop for example in (examples function)
	      when (not (stringp example))
		do (format
		     stream
		     "\\footnotesize\\begin{verbatim}~%~s~%\\end{verbatim}\\normalsize~%"
		     example)
	      else
		do (if (equal #\( (aref example 0))
		       ;; special case when code must be included in quotes:
		       (format
			 stream
			 "\\footnotesize\\begin{verbatim}~%~a~%\\end{verbatim}\\normalsize~%"
			 example)
		  (format stream "~a \\\\~%" (escape-special example)))
		   )))
    ))

'(format t "~a~%" (second (examples (find-gina-function 'directory-wildcard))))

'(print-tex-description (find-gina-class 'application) t)
'(print-tex-description (find-gina-class 'view) t)
'(print-tex-description (find-gina-class 'document) t)
'(print-tex-description (find-gina-class 'toggle-entry) t)
'(name (find-gina-class 'view))
'(all-superclasses (find-gina-class 'radio-button-group))

(defmethod print-tex-description ((gina-class gina-class) stream
			     &aux all-superclasses all-callbacks
			          any-slot-found slot-found-for-class
				  own-methods all-methods)
  "print decsription of the class to stream"

  ;; New chapter for one of the seven main classes:
  (let ((*print-case* :upcase))
    (when (member  (name gina-class)
		   '(application document view view-object command callback widget))
      (if (member  (name gina-class) '(view-object command widget))
	  (format stream "\\chapter{The Class ~a and its Subclasses}~%" (name gina-class))
	  (format stream "\\chapter{The Class ~a}~%" (name gina-class))
	  )))

  ;; header with lines
  (setq *section-for-routine* t)
  (print-tex-header stream (name gina-class) "Class"
		    ;; do not produce a special number and toc-entry for these classes:
		    (member (name gina-class) '(application document view callback)))

  ;; set the global variable *section-for-method*
  (setq *section-for-routine*
	(member (name gina-class) '(application document view callback)))

  (let ((*print-case* :downcase))
    (format stream  "\\label{c~a}  " (index-number gina-class))
    (format stream  "\\index{~a (Class)}~%" (name gina-class))

    (if (char-equal #\t (aref (description gina-class) 0))
	;; The class .. is the class of ...
	(format stream "The class {\\em ~a} is ~a.~%"
		(name gina-class) (description gina-class))
	;; An instance of class ... is an object where ...
	(format stream "An instance of class {\\em ~a} is ~a.~%"
		(name gina-class) (description gina-class)))

    (format stream "\\begin{flushleft}~%")
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
      (format stream " \\\\ ~%"))

    (case (application-subclasses gina-class)
      (:rarely    (format stream "You {\\bf rarely} define subclasses of this class. \\\\~%"))
      (:always    (format stream "You {\\bf must} define a subclass of this class. \\\\~%"))
      (:sometimes (format stream "You {\\bf can} define a subclass of this class. \\\\~%")) 
      (otherwise  (format stream "You define a subclass of this class ~a. \\\\~%"
			  (application-subclasses gina-class))))

    (case (gina-subclasses gina-class)
      ((t)       (format stream "GINA defines subclasses of this class. \\\\~%"))
      ((nil)     (format stream
			 "GINA does {\\bf not} define subclasses of this class. \\\\~%"))
      (otherwise (format stream "GINA defines subclasses of this class: ~a. \\\\~%"
			 (gina-subclasses gina-class))))

    (case (instantiate gina-class)
      ((t)       (format stream "You can create an instance of this class. \\\\~%"))
      ((nil)     (format stream "You never create an instance of this class. \\\\~%"))
      (otherwise (format stream "You create an instance of this class ~a. \\\\~%"
			 (instantiate gina-class))))
    (format stream "\\end{flushleft}~%~%")

    ;; own and inherited callbacks
    (setq all-callbacks
	  (loop for class in all-superclasses
		append (reverse (callbacks class))))
    (setq all-callbacks
	  (remove-duplicates all-callbacks
			     :from-end t
			     :key #'first))
    (when all-callbacks
      (format stream "\\subsection*{Callbacks:}~%")

      (begin-tex-table stream)
      (loop for (callback-slot motif-callback-name lambda-list) in all-callbacks
	    do (first-tex-word stream callback-slot)
	       (format stream "~a \\\\~%" lambda-list))
      (end-tex-table stream))
	
    ;; documented slots
    (setq any-slot-found nil)
    (loop for class in all-superclasses
	  do
      (setq slot-found-for-class nil)
      (loop for slot in (class-variables class)
	    when (and (listp slot) (member :documentation slot))
	      do (when (not any-slot-found)
		   (format stream "\\subsection*{Shared Slots:}~%~%")
		   (begin-tex-table stream)
		   (setq any-slot-found t))
		 (when (not slot-found-for-class)
		   (when (> (length all-superclasses) 1)
		     (format stream "  {\\bf \\underline{~a:}} \\\\*~%" (name class)))
		   (setq slot-found-for-class t))
		 (first-tex-word stream (first slot))
		 (format stream "\\parbox[t]{11cm}{~a} \\\\~a~%"
			 (nth (1+ (position :documentation slot)) slot)
			 (if (member :initform slot) "*" ""))
		 (when (member :initform slot)
		   (format stream "     \\> Default:  {\\tt ~s} \\\\~%"
			   (nth (1+ (position :initform slot)) slot)))
;		    else do (format stream "    \\> ~a \\\\~%"
;				 (if (listp slot) (first slot) slot))
		 ))
    (when any-slot-found
      (end-tex-table stream))

    (setq any-slot-found nil)
    (loop for class in all-superclasses
	  do
      (setq slot-found-for-class nil)
      (loop for slot in (instance-variables class)
	    when (and (listp slot) (member :documentation slot))
	      do (when (not any-slot-found)
		   (format stream "\\subsection*{Local Slots:}~%~%")
		   (begin-tex-table stream)
		   (setq any-slot-found t))
		 (when (not slot-found-for-class)
		   (when (> (length all-superclasses) 1)
		     (format stream "  {\\bf \\underline{~a:}} \\\\*~%" (name class)))
		   (setq slot-found-for-class t))
		 (first-tex-word stream (first slot))
		 (format stream "\\parbox[t]{11cm}{~a} \\\\~a~%"
			 (escape-special (nth (1+ (position :documentation slot)) slot))
			 (if (member :initform slot) "*" ""))
		 (when (member :initform slot)
		   (format stream "     \\> Default:  {\\tt ~s} \\\\~%"
			   (nth (1+ (position :initform slot)) slot)))
		 ))
    (when any-slot-found
      (end-tex-table stream))


    (when (resource-slots gina-class) ;; +++ kommt nicht vor?
      (format stream "\\subsection*{Resource slots:}~%~%")
      (loop for (slot motif-resource) in (resource-slots gina-class)
	    when (equal (symbol-name slot) (symbol-name motif-resource))
	      do (format stream "~a " slot)
	    else
	      do (format stream "~a (corresponds to Motif-resource ~s) "
			 slot motif-resource))
      (format stream " \\\\~%~%"))

    (setq own-methods (sort-methods gina-class))
    (when own-methods ;; new for version 2.x of manual!
      (format stream "\\subsection*{Methods of class ~a:}~%~%" (name gina-class))

      (format stream "\\begin{tabbing}~%")
      (format stream "{\\tt mmmmm}\\= \\kill~%")
      (loop for method in own-methods
	    do (print-tex-method stream (name-with-indicator method :full-lambda-list t))
	       (format stream "\\`(Page \\pageref{m~a})\\\\*~%"
		       (index-number method))
	       (format stream "\\> ~a \\\\~%" (description method)))
      (format stream "\\end{tabbing}~%~%"))

    ;; do NOT show methods of class widget all the time
    (setq all-superclasses (delete (find-gina-class 'widget) all-superclasses))
    (setq all-methods (sort (loop for class in (cdr all-superclasses)
				  append (methods class))
			    #'method<))
    (when all-methods
      (format stream "\\subsection*{Methods of superclasses:}~%~%")
      (format stream "\\begin{tabbing}~%")
      (format stream "{\\tt mmmmm}\\= \\kill~%")
      (loop for method in all-methods
	    do (print-tex-method stream (name-with-indicator method :full-lambda-list t))
	       (format stream "\\`(Page \\pageref{m~a})\\\\*~%"
		       (index-number method))
	       (format stream "\\> ~a \\\\~%" (description method))
	       )
      (format stream "\\end{tabbing}~%~%"))
    ))

'(print-tex-method t 
		  (name-with-indicator
		      (find-gina-method 'inside-rectangle 'nil '(view-object t t t t))
		      :full-lambda-list t))

(defun print-tex-method (stream method-description
			 &aux first-indicator scnd-indicator method-descriptor)
  (setq first-indicator (subseq method-description 0 1))
  (setq scnd-indicator  (subseq method-description 1 2))
  (setq method-descriptor (escape-special (subseq method-description 3)))
  (format stream "\\verb;[~a~a]; \\> \\parbox[t]{12.5cm}{~%" first-indicator scnd-indicator)
  (format stream "{\\tt ~a}}~%" method-descriptor))
  

(defun start-tex-subtree (stream)
  (format stream "\\chapter{The Hierarchy of GINA Classes}~%")
  (setq *section-for-routine* t)
  (format stream "\\begin{tabbing}~%")
  (format stream "mm\\=mm\\=mm\\=mm\\=mm\\=mm\\=mm\\=mm\\=mm\\=mm\\=\\kill~%")
  )

(defun end-tex-subtree (stream)
  (format stream "\\end{tabbing}~%~%"))

(defmethod print-tex-subtree ((gina-class gina-class) stream indentation)
  "print class and all its subclasses as an indented tree"

  (loop repeat indentation
	do (format stream "$\\triangleright$\\>"))
  (format stream "{\\large {\\bf ~a}} \\`(Page \\pageref{c~a}) \\\\*~%"
	  (short-name gina-class)
	  (index-number gina-class))

  (loop repeat 5
	do (format stream "\\>"))
  (format stream "~a \\\\~%" (description gina-class))
  
  (loop for subclass in (subclasses gina-class)
	do (print-tex-subtree subclass stream (1+ indentation))))

(defmethod print-tex-subtree (object stream indentation)
  "print class and all its subclasses as an indented tree"
  (declare (ignore indentation))
  (format stream "~%~a not yet compiled~%" object)
  )

'(print-tex-subtree (find-gina-class 'widget) t 1)
'(loop for basic-class in '(application document view-object command callback widget)
      do (print-tex-subtree (find-gina-class basic-class) t 0))

'(print-tex-description
   (find-gina-method '(setf modified) '(:before) '(t document)) t)
'(print-tex-description
   (find-gina-method 'initialize-instance '(:after) '(document)) t)
'(print-tex-description
   (find-gina-method 'create-windows nil '(document)) t)


(defmethod print-tex-description ((method gina-method) stream
			      &aux related-methods  ;;relevant-classes
			      (qualifier-string ""))
  "print description of the method to stream"

  (let ((*print-case* :downcase)
	(*print-pretty* nil))
    (when (qualifiers method)
    (setq qualifier-string (format nil " ~s" (first (qualifiers method)))))
    
    ;; header with lines
    (print-tex-header stream
		      (concatenate 'string
			(format nil "~a" (name method))
			qualifier-string) "Method")
    
    (format stream  "\\label{m~a}  " (index-number method))

    (if (listp (name method))
	;; methods (setf xxx) should appear under s within the index:
	(format stream  "\\index{setf ~a~a (Method)}~%"
		(escape-special (format nil "~a" (second (name method))))
		qualifier-string)
	(format stream  "\\index{~a~a (Method)}~%"
		(escape-special (format nil "~a" (name method)))
		qualifier-string))

    (format stream "{\\bf Purpose: ~a.} \\\\~%~%" (description method))

    ;; give header:
    (format stream "\\begin{flushleft}~%")
    (format stream "{\\tt~%(defmethod ~a~a ~a)} \\\\~%~%"
	    (name method)
	    qualifier-string
	    (escape-special (format nil "~s" (lambda-list method))))
    (format stream "\\end{flushleft}~%~%")

    (when (commented-parms method)
      (begin-tex-table stream)
      (loop for (parm string) in (commented-parms method)
	    do (first-tex-word stream
			       (format nil "~a:" (if (listp parm) (first parm) parm)))
	       (format stream "\\parbox[t]{11cm}{~a} \\\\~%" string))
      (end-tex-table stream))

    (format stream "\\begin{flushleft}~%")
    (when (result method)
      (format stream "The returned value is ~a. ~%~%" (result method)))

    (if (qualifiers method)
	(format stream "Deamon installed by GINA. \\\\~%")
	;; else (no qualifiers):
	(progn
	  (case (override method)
	    (:rarely
	      (format stream"You {\\bf rarely} override this method or add deamons. \\\\~%"))
	    (:always
	      (format stream "You {\\bf must} override this method. \\\\~%"))
	    (:sometimes
	      (format stream "You {\\bf can} override this method. \\\\~%"))
	    (otherwise
	      (format stream "You override this method ~a. \\\\~%" (override method))))
		 
	  (format stream "The default version ~a. \\\\~%"
		  (case (default-version method)
		    (:do-nothing "does not do anything")
		    (:simple     "is sufficient for most cases")
		    (otherwise   (default-version method))))
		 
	  (case (called-by-application method)
	    (:rarely
	      (format stream "Normally, you do {\\bf not} call this method. \\\\~%"))
	    (:always
	      (format stream "You call this method in {\\bf each} application. \\\\~%"))
	    (:sometimes
	      (format stream "You {\\bf can} call this method. \\\\~%"))
	    (otherwise  (format stream "You {\\bf can} call this method ~a. \\\\~%"
				(called-by-application method))))
	
	  (case (called-by-gina method)
	    ((t)       (format stream "It is called by GINA when appropriate. \\\\~%"))
	    ((nil)     (format stream "It is never called by GINA. \\\\~%"))
	    (otherwise (format stream "It is called by GINA ~a. \\\\~%"
			       (called-by-gina method))))
		   

	  (case (overridden-by-gina method)
	    ((t)  (format stream "It is overridden by GINA for some subclasses. \\\\~%"))
	    ((nil)     (format stream "It is {\\bf never} overridden by GINA. \\\\~%"))
	    (otherwise (format stream "It is overridden by GINA ~a. \\\\~%"
			       (overridden-by-gina method))))))

    (when (comments method)
      (loop for comment in (comments method)
	    do (format stream "~a \\\\~%" comment)))
    (format stream "\\end{flushleft}~%~%")

    (let ((*print-pretty* t))
      (when (examples method)
	(format stream "\\subsection*{Example}~%")
	(loop for example in (examples method)
	      when (not (stringp example))
		do (format
		     stream
		     "\\footnotesize\\begin{verbatim}~%~s~%\\end{verbatim}\\normalsize~%"
		     example)
	      else
		do (format stream "~a \\\\~%" example)
		  )))

    ;; classes where method can be called
;    (setq relevant-classes
;	  (loop for class in (relevant-classes method)
;		append (all-subclasses class)))
;    (setq relevant-classes (set-difference relevant-classes (relevant-classes method)))
;    (when (> (length relevant-classes) 0)
;      (format stream "~%This method is inherited by class ")
;      (loop for class in relevant-classes
;	    do (format stream "~a~a" (short-name class)
;		       (cond ((eq class (car (last relevant-classes)))
;			      ".")
;			     ((eq class (car (last (butlast relevant-classes))))
;			      " and ")
;			     (t ", "))))
;      (format stream "~%"))

    ;; print methods with-the same name
    (setq related-methods (delete method (related-methods method)))
    (when related-methods
      (format stream "~%\\subsection*{Related methods:}~%~%")
      (format stream "\\begin{tabbing}~%")
      (format stream "{\\tt mmmmm}\\= \\kill~%")
      (loop for method in related-methods
	    do (print-tex-method stream (name-with-indicator method :full-lambda-list t))
	       (format stream "\\`(Page \\pageref{m~a})\\\\*~%"
		       (index-number method))
	       (format stream "\\> ~a \\\\~%" (description method)))
      (format stream "\\end{tabbing}~%~%"))

    ))

(defmethod print-tex-description (object stream)
  "just print nothing"

  (format stream "~%~a not yet compiled~%" object)
  )


(defun extract-tex-manual (output-file &rest input-files
			   &aux name type-specifiers specialized-lambda-list qualifiers)
  "scan input-files for (defgina... and write the description text into output-file"
  (let ((*print-case* :downcase))
  (with-open-file (ostream output-file :direction :output)
    ;; print class tree
    (start-tex-subtree ostream)
    (loop for basic-class in '(application document view-object command callback widget)
      do (print-tex-subtree (find-gina-class basic-class) ostream 0))
    (end-tex-subtree ostream)

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
			  (print-tex-description (find-gina-class name) ostream)

			  
		     ;; something like '(defginamacro pipapo ....)
		     else
		       when (and (listp form)
				 (eq 'quote (first form))
				 (listp (second form))
				 (eq 'defginamacro (first (second form))))
		       do (setq form (second form))
			  (setq name (second form))
			  (print-tex-description (find-gina-macro name) ostream)

		     ;; something like (defginamacro pipapo ....)
		     else
		     when (and (listp form)
			       (eq 'defginamacro (first form)))
		       do (setq name (second form))
			  (print-tex-description (find-gina-macro name) ostream)
			     
		     ;; something like (defginamethod doit ....)
		     else
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
				  return nil
				when (not (stringp parm))
				  do (push (if (listp parm) (second parm) t)
					   type-specifiers))
			  (setq type-specifiers (reverse type-specifiers))

			  (when (not (find-gina-method name qualifiers type-specifiers))
			    (format t "~a ~a ~a" name qualifiers type-specifiers))
			  (print-tex-description
			    (find-gina-method name qualifiers type-specifiers) ostream)

		     ;; something like (defginafun parent-directory ....)
		     else
		     when (and (listp form)
			       (eq 'defginafun (first form)))
		       do (setq name (second form))
			  (print-tex-description (find-gina-function name) ostream)

		     ;; something like (defginavar *application* ....)
		     else
		     when (and (listp form)
			       (eq 'defginavar (first form)))
		       do (setq name (second form))
			  (print-tex-description (find-gina-variable name) ostream)

		     ;; something like (ginachapter "title of chapter" "...." ...)
		     else
		     when (and (listp form)
			       (eq 'ginachapter (first form)))
		       do (format ostream "\\chapter{~a}~%" (second form))
			   (setq *section-for-routine* t)
			  (loop for comment in (cddr form)
				do (format ostream "~a ~%" comment))
			  (when (cddr form)
			    (format ostream "\\\\~%"))

		     ;; something like (ginasection "title of section" "...." ...)
		     else
		     when (and (listp form)
			       (eq 'ginasection (first form)))
		       do (format ostream "\\section{~a}~%" (second form))
			   (setq *section-for-routine* nil) ;; need subsection
			  (loop for comment in (cddr form)
				do (format ostream "~a ~%" comment))
			  (when (cddr form)
			    (format ostream "\\\\~%"))

		     ;; something like (ginacomment "..." "..." ...)
		     else
		     when (and (listp form)
			       (eq 'ginacomment (first form)))
		       do (loop for comment in (cdr form)
				do (format ostream "~a ~%" comment))
			  (format ostream "\\\\~%")
		       
			 )))
    )))

'(extract-tex-manual "/home/cici/Manual/manual.tex"
		       
		     "/home/spenke/gina/globals.lisp"
		     "/home/spenke/gina/framework.lisp"
		     "/home/spenke/gina/views.lisp"
		     "/home/spenke/gina/commands.lisp"
		     "/home/spenke/gina/motif-widgets.lisp"
		     "/home/spenke/gina/dialogs.lisp"
                     "/home/spenke/gina/background.lisp"
		     "/home/spenke/gina/OS-dependent.lisp"
		     )
