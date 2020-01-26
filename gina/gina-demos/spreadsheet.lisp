;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ss ;Base: 10 -*-
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
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS WHETHER IN AN ACTION
;;; OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
;;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;;
;;; Authors: Project GINA (spenke@gmd.de)
;;;          P.O. Box 1316
;;;          D-5205 Sankt Augustin 1
;;;

(in-package :GINA)
(defginapackage :ss)
(in-package :ss)

;; Facilities of this simple spreadsheet application:
;;
;; Interface:
;;
;; - enter strings into cells (ENTER-button or RETURN-Key)
;; - cancel input editing
;; - resize column width using the mouse
;; - subsequently resizing two columns to the same width 
;;   => resize all columns is suggested
;; - change global font 
;; - show/hide grid lines
;; - select cells using the mouse
;; - insert cell name into input on any special click
;; - draw borders around selected cells
;; - clear selected cells
;; - copy selected cells to clipboard
;; - paste clipboard into selected cells
;; - full undo/redo capability
;;
;; Computation:
;; - the Lisp evaluator is used for formula evaluation
;; - cell contents entered are regarded as numbers, lists or strings
;; - formulas start with an equal sign (=)
;; - a formula is any Lisp expresssion containing cell references like A1
;; - example: =(* A1 (+ A2 A3)) or =(list A1 A2 A3 A4)
;; - syntax errors in formulas lead to a beep
;; - SHOW ERROR MESSAGE displays the last error diagnostic
;; - on errors during formula evaluation <error> is displayed in the cell
;; - normally the values of formulas are shown
;; - with SHOW FORMULAS formulas can be displayed in the sheet
;; - formulas are evaluated after every modification of a cell
;; - formulas can be replicated by COPY/PASTE
;; - during pasting cell references in formula are automatically transformed
;; - references into the copied selection are regarded as relative
;; - all others as absolute
;; - new functions can be defined "by example"
;;   entering (define foo (A1 A2 A3) A4) defines a new function foo
;;   with three parameters. The formulas connecting A1,A2 and A3 to A4
;;   are used as the body of the function (which is printed on standard-output)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class spreadsheet-application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass spreadsheet-application (application)
  (;; overrides
   (name          :initform "Spreadsheet" :allocation :class)
   (document-type :initform 'sheet        :allocation :class)
   (signature     :initform "Spreadsheet" :allocation :class)
   (file-type     :initform "sheet"       :allocation :class)
   (clipboard     :accessor clipboard :initform nil
		  :documentation "a list of cell objects"))
  (:documentation "a typical spreadsheet application"))

(defun make-spreadsheet-application ()
  (make-application :class 'spreadsheet-application))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this forward declaration is necessary because of PCL only !!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cell ()
  ((document :accessor document :initarg :document)
   (row      :accessor row      :initarg :row)
   (column   :accessor column   :initarg :column)
   (name     :accessor name     :initarg :name)
   (borders  :accessor borders  :initform nil)
   (content  :accessor content  :initform "")
   ;; the following slots are derived from content
   (formula  :accessor formula  :initform nil)
   (value    :accessor value    :initform 0)
   (value-string :accessor value-string :initform "0")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class sheet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sheet (document)
  ((shell-width   :initform 463)
   (shell-height  :initform 330)
   (nr-of-rows         :accessor nr-of-rows         :initform 50)
   (nr-of-columns      :accessor nr-of-columns      :initform 26)
   (row-height         :accessor row-height         :initform 20)
   (column-width-list  :accessor column-width-list)
   (show-grid          :accessor show-grid :initform t)
   (show-grid-entry    :accessor show-grid-entry)
   (show-formulas      :accessor show-formulas :initform nil)
   (show-formulas-entry :accessor show-formulas-entry)
   (cell-array         :accessor cell-array)
   (current-cell       :accessor current-cell)
   (last-selected-cell :accessor last-selected-cell :initform nil)
   (columns-view       :accessor columns-view)
   (rows-view          :accessor rows-view)
   (scroller           :accessor scroller)
   (edit-field         :accessor edit-field)
   (cell-label         :accessor cell-label)
   (last-error         :accessor last-error :initform nil)
   (suggestion-box     :accessor suggestion-box)
   (suggestion-counter :accessor suggestion-counter :initform 0)))

(defmethod initialize-instance :after ((doc sheet) &rest initargs)
   (declare (ignore initargs))
   (setf (cell-array doc) 
     (make-array (list (1+ (nr-of-rows doc)) (1+ (nr-of-columns doc)))
		 ;; initial-element is undefined by default!!
		 :initial-element nil)) 
   (setf (column-width-list doc)
     (make-list (nr-of-columns doc) :initial-element 80))
   (setf (current-cell doc) (get-cell doc 1 1))
   (setf (last-selected-cell doc) (current-cell doc)))

(defmethod compute-table-width ((doc sheet))
  "compute total size from individual column widths"
  (1+ (apply '+  (column-width-list doc))))

(defmethod compute-table-height ((doc sheet))
  "compute total height of table"
  (1+ (* (nr-of-rows doc) (row-height doc))))

(defmethod write-to-stream ((doc sheet) stream &aux cell (nr-ofcells 0))
  "write the sheet to the specified stream"
  
  (format stream "~s~%" (name-of-font (main-view doc)))
  (format stream "~s~%" (column-width-list doc))
  (format stream "~s ;; show-grid~%" (show-grid doc))
  
  ;; count filled cells
  (loop for row from 1 to (nr-of-rows doc)
   do (loop for column from 1 to (nr-of-columns doc)
       when (aref (cell-array doc) row column)    
       do (incf nr-ofcells)))

  (format stream "~d ;; nr of cells~%" nr-ofcells) 
	  
  ;; write out filled cells
  (loop for row from 1 to (nr-of-rows doc)
     do (loop for column from 1 to (nr-of-columns doc)
	 when (setq cell (aref (cell-array doc) row column))     
	 do (format stream "~d ~d ~s ~a~%" 
		    (row cell) (column cell) (content cell) (borders cell))))
  )

(defmethod read-from-stream ((doc sheet) stream 
			     &aux font nr-of-cells row col cell) 
  "read the sheet from the specified stream"
  
  ;; read and set font in all three views
  (setq font (read stream))
  (setf (name-of-font (main-view doc)) font)
  (when (gcontext (main-view doc)) ;;  ... already exists
    ;; it is a REVERT command
    (set-new-font (main-view doc)    font :redraw nil)
    (set-new-font (columns-view doc) font :redraw nil)
    (set-new-font (rows-view doc)    font :redraw nil))

  (setf (column-width-list doc) (read stream))

  ;; read whether grid wanted and indicate state in the menu bar
  (setf (show-grid doc)         (read stream))
  (setf (value (show-grid-entry doc)) (show-grid doc))
  
  ;; read all cells and their contents
  (setq nr-of-cells (read stream))
  (loop repeat nr-of-cells
    do (setq row (read stream))
       (setq col (read stream))
       (setq cell (make-cell doc row col))
       (setf (aref (cell-array doc) row col) cell)
       (setf (content cell) (read stream))
       (setf (borders cell) (read stream)))
  
  (recompute-all-formulas doc)
  )

(defmethod create-windows ((doc sheet) 
			   &aux form row-scroller 
			        cancel-button enter-button
			        columns-scroller separator)
  "create the windows belonging to this document"
  (with-slots (main-shell main-view edit-field cell-label scroller
               columns-view rows-view) doc
    (setq main-shell (make-document-shell doc))
    (setq form (make-form main-shell))
    (setq cell-label (make-label form " A1 " :recompute-size nil))
    (setq edit-field (make-text form))
    (setf (activate-callback edit-field) (make-callback 'return-pressed doc))

    (setq cancel-button 
      (make-push-button form "Cancel"
               :activate-callback (make-callback 'cancel-input doc)))
    (setq enter-button 
      (make-push-button form "Enter"
	       :show-as-default 1
               :activate-callback (make-callback 'return-pressed doc)))

    (setq separator (make-separator form))

    (setq columns-scroller 
      (make-scroller form :motif-resources '(:height 30)))
    (setq columns-view (make-columns-view columns-scroller doc))
    (unmanage (horizontal-scrollbar columns-scroller))
    (unmanage (vertical-scrollbar columns-scroller))

    (setq row-scroller 
      (make-scroller form :motif-resources '(:width 30)))
    (setq rows-view    (make-rows-view row-scroller doc))
    (unmanage (horizontal-scrollbar row-scroller))
    (unmanage (vertical-scrollbar row-scroller))

    (setq scroller   (make-scroller form))
      
    (setq main-view  (make-cell-view scroller doc))

    (let ((sb (horizontal-scrollbar scroller))
	  (cb (make-callback 'scroll columns-view)))
      (setf (drag-callback sb) cb)
      (setf (increment-callback sb) cb)
      (setf (decrement-callback sb) cb)
      (setf (page-increment-callback sb) cb)
      (setf (page-decrement-callback sb) cb)
      (setf (to-top-callback sb) cb)
      (setf (to-bottom-callback sb) cb)
      (setf (value-changed-callback sb) cb))

    (let ((sb (vertical-scrollbar scroller))
	  (cb (make-callback 'scroll rows-view)))
      (setf (drag-callback sb) cb)
      (setf (increment-callback sb) cb)
      (setf (decrement-callback sb) cb)
      (setf (page-increment-callback sb) cb)
      (setf (page-decrement-callback sb) cb)
      (setf (to-top-callback sb) cb)
      (setf (to-bottom-callback sb) cb)
      (setf (value-changed-callback sb) cb))

    (define-form-constraint cell-label 
			    :top-attachment :form :top-offset 11
			    :left-attachment :form
			    :right-attachment :none)
    (define-form-constraint edit-field 
			    :top-attachment :form :top-offset 5
			    :left-attachment :widget :left-widget cell-label
			    :right-attachment :widget
			    :right-widget enter-button
			    :right-offset 5)
    (define-form-constraint enter-button 
			    :top-attachment :form :top-offset 2
			    :right-attachment :widget 
			    :right-widget cancel-button  :right-offset 5)
    (define-form-constraint cancel-button 
			    :top-attachment :form :top-offset 8
			    :right-attachment :form  :right-offset 5)
    (define-form-constraint separator
			    :top-attachment :widget :top-widget edit-field
			    :top-offset 5
			    :left-attachment :form
			    :right-attachment :form)
    (define-form-constraint columns-scroller
			    :top-attachment :widget :top-widget separator
			    :top-offset 5
			    :left-attachment :form :left-offset 34
			    :right-attachment :form :right-offset 21)
    (define-form-constraint row-scroller
			    :top-attachment :widget :top-widget separator
			    :top-offset 37
			    :left-attachment :form :left-offset 2
			    :bottom-attachment :form :bottom-offset 21)
    (define-form-constraint scroller 
			    :top-attachment :widget 
			    :top-widget columns-scroller
			    :left-attachment :widget 
			    :left-widget row-scroller
			    :right-attachment :form
                            :bottom-attachment :form)

    ;; create the modeless dialog box used to present suggestions
    (setf (suggestion-box doc) (make-suggestion-box doc))
    (setf (relative-y (suggestion-box doc)) :bottom)
    (setf (relative-widget (suggestion-box doc)) (main-shell doc))

    ;; add an application specific command
    (add-menu-command (main-menu main-shell)
		      "Edit" "Clear" 
		      (make-callback 'make-clear-command doc))
    (add-menu-command (main-menu main-shell)
		      "Edit" "Copy" 
		      (make-callback 'make-copy-command doc))
    (add-menu-command (main-menu main-shell)
		      "Edit" "Paste" 
		      (make-callback 'make-paste-command doc))
    (add-menu-command (main-menu main-shell)
		      "Edit" "Paste Formulas" 
		      (make-callback 'make-paste-command doc :formulas-only t))
    (add-menu-command (main-menu main-shell)
		      "Edit" "Display Selection" 
		      (make-callback 'display-selection doc))
    (add-menu-command (main-menu main-shell)
		      "Options" "Font..." 
		      (make-callback 'select-font doc))
    (add-menu-command (main-menu main-shell)
		      "Options" "Borders On" 
		      (make-callback 'make-borders-command doc t))
    (add-menu-command (main-menu main-shell)
		      "Options" "Borders Off" 
		      (make-callback 'make-borders-command doc nil))
    (setf (show-grid-entry doc)
      (make-toggle-entry "Show Grid" 
			 (make-callback 'toggle-grid doc)
			 :value t))
    (insert-menu-entry (main-menu main-shell) "Options" "Show Grid"
		       (show-grid-entry doc))
    
    (setf (show-formulas-entry doc)
      (make-toggle-entry "Show Formulas" 
			 (make-callback 'toggle-show-formulas doc)
			 :value nil))
    (insert-menu-entry (main-menu main-shell) "Options" "Show Formulas"
		       (show-formulas-entry doc))
       
    (add-menu-command (main-menu main-shell)
		      "Commands" "Reevaluate" 
		      (make-callback 'recompute-all-formulas  doc))
    (add-menu-command (main-menu main-shell)
                      "Commands" "Show Error Message"
                      (make-callback 'show-error-message doc))
    ))

(defmethod toggle-grid ((doc sheet) flag)
  "turn grid on/off"
  (setf (show-grid doc) flag)
  (setf (modified doc) t)
  (force-redraw (main-view doc)))

(defmethod toggle-show-formulas ((doc sheet) flag)
  "turn show-formulas on/off"
  (setf (show-formulas doc) flag)
  (force-redraw (main-view doc)))

(defmethod show-error-message ((doc sheet))
  "display the last error message in a dialog box"
  (warning-dialog (if (last-error doc)
		      (format nil "~a" (last-error doc))
		      "There has been no error yet!")
                  :document doc
                  :dialog-title "Last Error Message"))
                   

(defmethod return-pressed ((doc sheet) &optional input-string)
  "editing finished"
  (declare (ignore input-string))
  ;; scroll modified cell into visible part
  (display-selection doc)
  
  ;; select cell below current cell
  (select-new-cell doc
		   (min (1+ (row (current-cell doc))) (nr-of-rows doc))  
		   (column (current-cell doc)))

  ;; make sure cell is shown
  (display-selection doc))

(defmethod display-selection ((doc sheet))
  "do minimal scrolling to show selection"
  (show-point (scroller doc) 
	      (1+ (right  (last-selected-cell doc) doc))
	      (1+ (bottom (last-selected-cell doc) doc)))
  (show-point (scroller doc) 
	      (left (current-cell doc) doc)
	      (top  (current-cell doc) doc)))

(defmethod cancel-input ((doc sheet))
  (setf (value (edit-field doc)) (content (current-cell doc))))

(defmethod input-field-to-cell ((doc sheet) &aux cell)
  (setf cell (current-cell doc))
  (when (not (equal (content cell) (value (edit-field doc))))
    (make-enter-command doc cell (content cell) (value (edit-field doc))))
  )

(defmethod cell-to-input-field ((doc sheet))
  "display the cell contents in the input field"
  (setf (value (edit-field doc)) (content (current-cell doc)))
  ;; move cursor to end of text
  (xtk:append-text (widget-id (edit-field doc)) "" :move-cursor t)
  ;; focus on edit field 
  (xtk:process-traversal (widget-id (edit-field doc)) :current))

(defmethod select-new-cell ((doc sheet) row column 
			&aux old-cell (view (main-view doc)))
  "move current cell to another cell"
  (setq old-cell (current-cell doc))

  ;; get back the currently edited string
  (input-field-to-cell doc)

  ;; clear dashed selection rectangle
  (clear-selection view)

  ;; make the new cell both current and last selected cell
  (setf (current-cell doc) (get-cell doc row column))
  (setf (last-selected-cell doc) (current-cell doc))
  (recompute-selected-area view)
 
  ;; redraw new and old current cell
  (redraw-cell view old-cell)
  (redraw-cell view (current-cell doc))

  ;; dispaly contents of new current cell
  (cell-to-input-field doc)
  (setf (label-string (cell-label doc)) (name (current-cell doc))))

(defmethod collect-selected-cells ((doc sheet) &aux current last)
  (setq current (current-cell doc))
  (setq last (last-selected-cell doc))
  (loop for row from (row current) to (row last)
    append
   (loop for col from (column current) to (column last)
     collect (get-cell doc row col))))

(defmethod command-executed ((doc sheet))
  "override GINA method called after each command"

  ;; suggestion boxes stay up for the duration one command only
  (when (= 1 (suggestion-counter doc))
    (pop-down (suggestion-box doc)))

  (when (> (suggestion-counter doc) 0)
    (decf (suggestion-counter doc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class suggestion-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass suggestion-box (modeless-dialog-box)
   ((label :accessor label)
    (ok-button :accessor ok-button)
    (suggested-callback :accessor suggested-callback :initform nil))
   (:documentation "a dialog box to present suggestions"))

(defun make-suggestion-box (document &aux dialog-box button-row)
  (setq dialog-box 
    (make-modeless-dialog-box "Suggestion:" 
			      :document document
			      :class 'suggestion-box))
  (with-slots (label ok-button) dialog-box
     (setq label 
       (make-label dialog-box "No suggestion yet."))
     (setq button-row 
       (make-row-column dialog-box :orientation :horizontal :spacing 20))
     (setq ok-button 
       (make-push-button button-row "   OK   "
			 :activate-callback
			 `(lambda () 
			    (execute (suggested-callback ',dialog-box) nil)
			    (pop-down ',dialog-box))))
     (make-push-button button-row " Cancel "
		       :activate-callback
		       `(lambda () (pop-down ',dialog-box)))
     
     (define-form-constraint label
	    :top-attachment   :form :top-offset   10 
	    :left-attachment  :form :left-offset  10
	    :right-attachment :form :right-offset 10)
     (define-form-constraint button-row
	    :top-attachment  :widget :top-widget label :top-offset  10
	    :bottom-attachment :form :bottom-offset 10
	    :left-attachment  :form :left-offset  10
	    :right-attachment :form :right-offset 10))
  dialog-box)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class cell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; see forward declaration above
'(defclass cell ()
  ((document :accessor document :initarg :document)
   (row      :accessor row      :initarg :row)
   (column   :accessor column   :initarg :column)
   (name     :accessor name     :initarg :name)
   (borders  :accessor borders  :initform nil)
   (content  :accessor content  :initform "")
   ;; the following slots are derived from content
   (formula  :accessor formula  :initform nil)
   (value    :accessor value    :initform 0)
   (value-string :accessor value-string :initform "0")))

(defun make-cell (doc row column)
  (make-object 'cell nil :row row :column column 
	       :name (format nil "~A~d" (column-name column) row)
	       :document doc))

(defmethod copy ((cell cell) &aux new)
  "create an identical clone"
  (with-slots (document row column borders content) cell
  (setq new (make-cell document row column))
  (setf (content new) content)
  (setf (borders new) borders))
  new)
  

(defun column-name (column-nr)
  (code-char (+ (char-code #\A) (1- column-nr))))

(defun top (cell doc)
  (* (row-height doc) (1- (row cell))))

(defun bottom (cell doc)
  (* (row-height doc) (row cell)))

(defun left (cell doc)
  (apply #'+ (subseq (column-width-list doc) 0 (1- (column cell)))))

(defun right (cell doc)
  (apply #'+ (subseq (column-width-list doc) 0 (column cell))))

(defmethod find-cell ((doc sheet) x y &aux row column (col-right 0))
  "find the hit cell to give pixel coordinates"
  (setq row (min (nr-of-rows doc) 
		 (ceiling y (row-height doc))))
  (setq column (loop for col-width in (column-width-list doc)
		     for col from 1
		do (incf col-right col-width)
		 (when (>= col-right x) 
		   (return col))))
  (when (not column) (setq column (nr-of-columns doc)))
  (get-cell doc row column))
  
(defmethod get-cell ((doc sheet) row column &aux cell)
  "find a cell-object and create it if necessary"
  (setq cell (aref (cell-array doc) row column))
  (when (not cell)
    (setq cell (make-cell doc row column))
    (setf (aref (cell-array doc) row column) cell))
  cell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; parser and evaluator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; computations are triggered when a cell is modified

(defmethod (setf content) :after (new-content (cell cell))
  "analyze user input for lisp-values and formulas"
  (declare (ignore new-content))
  (parse-formula cell)
  (when (not (formula cell))
    (parse-value cell)))

(defmethod (setf value) :after (new-value (cell cell))
  "always recompute value-string to be displayed"
  (setf (value-string cell) 
    (format nil (if (stringp new-value) "~a" "~s") new-value)))

(defmethod parse-formula ((cell cell))
  "try to read cell content as code and replace cell references"
  (setf (formula cell) nil) ;; default: no formula
  (when (and (> (length (content cell)) 0)
	     (equal #\= (aref (content cell) 0)))
    ;; content begins with =, it is a formula
    (multiple-value-bind (expression error)
      (gina::ignore-errors 
       (multiple-value-bind (expression nr-of-chars)
	   (read-from-string (subseq (content cell) 1))
	 (when (not (= nr-of-chars (1- (length (content cell)))))
	   (error "Superflous input!"))
	 expression))
      (if (typep error 'error)
	  (progn 
	    (xlib:bell *display*)
	    (setf (last-error (document cell)) error))
	  (setf (formula cell) 
	    (replace-cell-references expression (document cell))))
      )))

(defmethod parse-value ((cell cell) &aux row+col function
			                 function-name result-cell 
					 parms parm-cells
			                 (doc (document cell)))
  "try to read a number or a list from the user input"
  (multiple-value-bind (value error)
      (gina::ignore-errors (read-from-string (content cell)))
    (if (and (not (typep error 'error)) 
	     (or (listp value) (numberp value)))
	;; use the parsed lisp expression
	(setf (value cell) value)
	;; use user input as a string
	(setf (value cell) (content cell)))
  
  ;; experimental: (define <function> (<cell> ... <cell>) <result-cell>)
  (when (and (listp value)
	     (symbolp (first value))
             (string-equal (symbol-name (first value)) "DEFINE")
	     (setq function-name (second value))
	     (symbolp function-name)
	     (setq parms (third value))
	     (listp parms)
	     (setq parm-cells 
	       (loop for parm in parms
		as cell = (multiple-value-bind (row col)
			      (parse-cell-reference parm doc)
			    (when row (get-cell doc col row)))
		when cell collect cell
		when (not cell) do (return nil)))
             (setq row+col
	       (multiple-value-list
		   (parse-cell-reference (fourth value) doc)))
             (not (null (first row+col)))
	     (setq result-cell 
	       (get-cell doc (second row+col) (first row+col))))
	     
    (setq function 
      (list 'defun 
	    function-name
	    parms
	    (build-complete-formula result-cell parm-cells)))
    (play "computer")
    (format t "~s~%" function) 
    (eval function))))

(defmethod compute-value ((cell cell))
  "evaluate formula if any"
  (if (formula cell)
      (eval-formula cell)
    ;; else just return contant value
    (value cell)))

(defmethod eval-formula ((cell cell))
  "recompute the value, use <error> in case of an error"
  ;;(format t "Eval:  ~a~%" (content cell))
  (setf (value cell)
    (multiple-value-bind (result error)
      (gina::ignore-errors  (eval (formula cell)))
      (if (typep error 'error)
	  (progn 
	    (setf (last-error (document cell)) error)
	    "<error>")
        result))))
  
(defmethod recompute-all-formulas ((doc sheet) &aux cell)
  "recompute all formulas of the sheet"
  (with-clock-cursor
   (loop for row from 1 to (nr-of-rows doc)
    do (loop for column from 1 to (nr-of-columns doc)
	when (and (setq cell (aref (cell-array doc) row column))
		  (formula cell))
	do (compute-value cell)
	   (redraw-cell (main-view doc) cell)
	   ))
  ))
                   
(defun parse-cell-reference (x doc &aux symbol-name length column)
  "check if x is a cell reference"
  (when (symbolp x)
    (setq symbol-name (symbol-name x))
    (setq length (length symbol-name))
    (when (and (>= length 2) (<= length 3))
      (setf column (- (char-code (aref symbol-name 0)) (1- (char-code #\A))))
      (when (and (>= column 1) (<= column (nr-of-columns doc)))
	(multiple-value-bind (row endpos)
	    (parse-integer (symbol-name x) :start 1 :junk-allowed t)
	  (when (and row (>= row 1) (<= row (nr-of-rows doc))
		     (= endpos length))
	    (values column row)))))))

(defun replace-cell-references (formula doc)
  "recursivly replace all cell references in a formula"
  (when (listp formula)
    (return-from replace-cell-references
      (loop for subformula in formula
       collect (replace-cell-references subformula doc))))
  
  (multiple-value-bind (column row) (parse-cell-reference formula doc)
    (if (null column)
	formula
        `(compute-value (get-cell ',doc ,row ,column)))))

(defun unreplace-cell-references (formula doc cell-map &aux cell row col)
  "recursivly replace all (compute-value (get-cell <x> <y>)) by Symbols"
  (when (and (listp formula) (not (eq 'compute-value (first formula))))
    (return-from unreplace-cell-references
     (loop for subformula in formula
      collect (unreplace-cell-references subformula doc cell-map))))

  (when (not (listp formula))
    (return-from unreplace-cell-references formula))

  (when (and (listp formula) (eq 'compute-value (first formula)))
    (setq row (third (second formula)) 
	  col (fourth (second formula)))
    ; check if reference points into clipboard, else do not map reference
    ;;(format t "(~d,~d) -> ~a~%" row col (gethash (list row col) cell-map))
    (setq cell (or (gethash (list row col) cell-map) 
		   (get-cell doc row col)))
    (make-symbol (name cell))))

(defun transform-formula (cell cell-map)
  "modify cell references in formula according to cell mapping table"
  (setf (content cell) 
    (format nil "=~a" 
	    (unreplace-cell-references 
	     (formula cell) (document cell) cell-map))))

(defun build-complete-formula (cell parameter-cells)
  "recursively collect the whole formula tree"
  (if (not (formula cell))
      (value cell)
    (expand-formula (formula cell) (document cell) parameter-cells)))

(defun expand-formula (formula document parameter-cells &aux cell)
  "recursively replace cell references by their formulas"
   (when (not (listp formula))
    (return-from expand-formula formula))

   (when (and (listp formula) (not (eq 'compute-value (first formula))))
    (return-from expand-formula
     (loop for subformula in formula
      collect (expand-formula subformula document parameter-cells))))

   (when (and (listp formula) (eq 'compute-value (first formula)))
    (setq cell (get-cell document 
			 (third (second formula))
			 (fourth (second formula))))
    (if (member cell parameter-cells)
      ;; just use name of parameter cell
      (intern (name cell))
      ;; insert formula for cell
      (build-complete-formula cell  parameter-cells))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class rows-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rows-view (view)
  ())

(defun make-rows-view (parent doc)
  (make-view parent :document doc :class 'rows-view))

(defmethod determine-window-id :after ((view rows-view))
  "inits when x-window exists"
  (set-new-font view (name-of-font (main-view (document view))) :redraw nil))
 
(defmethod set-new-font ((view rows-view) font-name 
			 &key (redraw t)
			 &aux (doc (document view)))
  (setf (xlib:gcontext-font (gcontext view)) font-name)
  (setf (name-of-font view) font-name)
  (setf (row-height (document view))
    (+ 6 (xlib:font-ascent (xlib:gcontext-font (gcontext view)))))
  (resize view 30 (1+ (* (nr-of-rows doc) (row-height doc))))
  (when redraw (force-redraw view)))  

(defmethod draw ((view rows-view) count area-x area-y area-width area-height
		 &aux row-height (y 0) 
		      (doc (document view)))
  "draw row names"
  (declare (ignore count area-x area-width))
  (setf row-height (row-height doc))
  (loop for row from 1 to (nr-of-rows doc)
   do (incf y row-height)
      (when (and (>= y area-y) (<= (- y row-height) (+ area-y area-height)))
	(draw-glyphs view 2 (- y 3) (format nil "~d" row))
	)))

(defmethod button-press ((view rows-view) code repetition x y
			 &aux row-hit (doc (document view)))
  "select a complete row"
  (declare (ignore code repetition x))
  (setq row-hit (ceiling y (row-height doc)))
  (select-new-cell doc row-hit 1)
  (setf (last-selected-cell doc) (get-cell doc row-hit (nr-of-columns doc)))
  (recompute-selected-area (main-view doc))
  (draw-selection (main-view doc)))

(defmethod scroll ((view rows-view) new-offset)
  (xtk::move-widget (widget-id view) 0 (- 0 new-offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class columns-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass columns-view (view)
  ())

(defun make-columns-view (parent doc)
  (make-view parent :document doc :class 'columns-view))

(defmethod determine-window-id :after ((view columns-view))
  "inits when x-window exists"
  (set-new-font view (name-of-font (main-view (document view))) :redraw nil))
 
(defmethod set-new-font ((view columns-view) font-name 
			 &key (redraw t)
			 &aux (doc (document view)))
  (setf (xlib:gcontext-font (gcontext view)) font-name)
  (setf (name-of-font view) font-name)
  (setf (row-height (document view))
    (+ 6 (xlib:font-ascent (xlib:gcontext-font (gcontext view)))))
  (resize view (compute-table-width doc) 30)
  (when redraw (force-redraw view)))  

(defmethod draw ((view columns-view) count area-x area-y area-width area-height
		 &aux (x 0) (doc (document view)))
  "draw column names"
  (declare (ignore count area-y area-height))
  (loop for col-width in (column-width-list doc)
        for col from 1
   do (when (and (>= (+ x col-width) area-x) (<= x (+ area-x area-width)))
	(draw-glyphs view 
		     (+ x (floor col-width 2) -5) 
		     18  
		     (format nil "~A" (column-name col))))
    (incf x col-width)))

(defmethod button-press ((view columns-view) code repetition x y
			 &aux column-hit (doc (document view)))
  "select a complete column"
  (declare (ignore code repetition y))
  (setq column-hit (column (find-cell doc x 1)))
  (select-new-cell doc 1 column-hit)
  (setf (last-selected-cell doc) (get-cell doc (nr-of-rows doc) column-hit))
  (recompute-selected-area (main-view doc))
  (draw-selection (main-view doc)))

(defmethod scroll ((view columns-view) new-offset)
  (xtk::move-widget (widget-id view) (- 0 new-offset) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class cell-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cell-view (view)
  ((name-of-font :initform "*adobe-helvetica-bold-r-normal--14*")
   (selection-x        :accessor selection-x)
   (selection-y        :accessor selection-y)
   (selection-width    :accessor selection-width)
   (selection-height   :accessor selection-height)
   (show-selection :accessor show-selection :initform nil)))

(defun make-cell-view (parent doc)
  (make-view parent :document doc :class 'cell-view))

(defmethod determine-window-id :after ((view cell-view))
  "inits when x-window exists"
  (set-new-font view (name-of-font view) :redraw nil))
 
(defmethod set-new-font ((view cell-view) font-name 
			 &key (redraw t)
			 &aux (doc (document view)))
  (setf (xlib:gcontext-font (gcontext view)) font-name)
  (setf (name-of-font view) font-name)
  (setf (row-height (document view))
    (+ 6 (xlib:font-ascent (xlib:gcontext-font (gcontext view)))))
  (recompute-selected-area view)
  (resize view (compute-table-width doc) (compute-table-height doc))
  (when redraw (force-redraw view))) 

(defmethod recompute-selected-area ((view cell-view) 
				    &aux (doc (document view))
				         current-cell last-selected-cell)
  "recompute x,y,width and height of selection rectangle"

  (setq current-cell       (current-cell       doc))
  (setq last-selected-cell (last-selected-cell doc))

  (setf (selection-x view) (left current-cell doc))
  (setf (selection-y view) (top  current-cell doc))
  (setf (selection-width view)
    (- (right  last-selected-cell doc) (selection-x view)))
  (setf (selection-height view)
    (- (bottom last-selected-cell doc) (selection-y view)))) 

(defmethod draw ((view cell-view) count area-x area-y area-width area-height
		 &aux (x 0) (y 0) cell 
		      (doc (document view))
		      (row-height (row-height doc)))
  "draw grid and cell contents"
  (declare (ignore count))
  ;;(format t "draw ~d ~d ~d ~d~%" area-x area-y area-width area-height)
  
  (when (show-grid doc)
  (xlib:with-gcontext ((gcontext view) :line-style :dash :dashes 1)
    ;; draw horizontal lines
    (loop for row from 0 to (nr-of-rows doc)
     do (when (and (>= y area-y) (<= y (+ area-y area-height)))
	  (draw-line view 0 y (width view) y))
        (incf y row-height)) 
    
    ;; draw vertical lines
    (loop for col-width in (cons 0 (column-width-list doc))
     do (incf x col-width)
        (when (and (>= x area-x) (<= x (+ area-x area-width)))
	  (draw-line view x 0 x (height view))))
    (xlib:display-force-output *display*)
  ))
  ;; draw-cells
  (xlib:with-gcontext ((gcontext view) :line-width 2)
    (setq y 0)
    (loop for row from 1 to (nr-of-rows doc)
     do 
      (incf y row-height)
      (when (and (>= y area-y) (<= (- y row-height) (+ area-y area-height)))
	(setq x 0)
	(loop for column from 1
	 for col-width in (column-width-list doc)
	 do 
	  (incf x col-width)
	  (when (setq cell (aref (cell-array doc) row column)) 
	    ;; draw cell contents
	    (draw-glyphs view (- x col-width -2) (- y 3) 
			 (if (and (formula cell) (not (show-formulas doc))) 
			     (value-string cell) 
			     (content cell)))
	    (when (borders cell)
	      (draw-rectangle view (- x col-width) (- y row-height) 
			      col-width row-height))
	    (when (eq cell (current-cell doc))
	      ;; draw selected
	      (draw-rectangle view (- x col-width -1) (- y row-height -1) 
			      (- col-width 2) (- row-height 2)))
	    )))))
  
  ;; draw selection
  (when (and (show-selection view)
	     (not (eq (last-selected-cell doc) (current-cell doc))))
    (xlib:with-gcontext ((gcontext view) :line-width 3 :line-style :dash)
	(draw-rectangle view
			(selection-x view)
			(selection-y view)
			(selection-width  view)
			(selection-height view)
              ))))

(defmethod redraw-cell ((view cell-view) cell &aux (doc (document view)))
  "redraw just the line containing the cell"
  (invalidate-rectangle view 
			(left cell doc) (top cell doc)
			(width view) (row-height doc)))

(defmethod redraw-selection ((view cell-view) 
			     &aux (doc (document view))
			     x y 
			     width height)
  "redraw the area containing the selection"
  (when (not (eq (last-selected-cell doc) (current-cell doc)))
    ;; compute area to be redrawn
    (setq x (1- (selection-x view)) 
	  y (1- (selection-y view))
	  width (+ (selection-width view) 3)
	  height (+ (selection-height view) 3))

    ;; clear edges of selection rectangle only
    (clear-area view x y 3 height)
    (clear-area view x y width 3)
    (clear-area view (+ x width -3) y 3 height)
    (clear-area view x (+ y height -3) width 3)
		
    ;; draw whole area
    (draw view 0 x y width height)
    ))

(defmethod clear-selection ((view cell-view))
  (setf (show-selection view) nil)
  (redraw-selection view))

(defmethod draw-selection ((view cell-view))
  (setf (show-selection view) t)
  (redraw-selection view))

(defmethod button-press ((view cell-view) code repetition x y
			 &aux row column 
			      (col-right 0)
			      (col-border nil)
			      found-width
			      (doc (document view)))
  (declare (ignore repetition))
  (when (eq code :select)
    (setq row (ceiling y (row-height doc)))
    (setq column (loop for col-width in (column-width-list doc)
		  for col from 1
		  do (incf col-right col-width)
		  when (and (>= x (- col-right 2)) (<= x (+ col-right 3)))
		  do (setq col-border col)
		   (setq found-width col-width)
		  do (when (>= col-right x) 
		       (return col))))
    
    (if (and (show-grid doc) col-border)
	;; column border hit
	(make-column-resizer view x y col-border found-width)
      ;; cell hit
      (progn 
	(select-new-cell doc row column)
	(make-cell-selector view x y))))

  ;; shift/control/middle ... insert a cell reference
  (when (not (eq code :select))
    (xtk:insert-text 
     (widget-id (edit-field doc))
     (first (get-motif-resources (edit-field doc) :cursor-position))
     (format nil " ~a" (name (find-cell doc x y))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class cell-selector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cell-selector (mouse-down-command)
  (;; overrides
   (name          :initform "Select Cells"   :allocation :class)
   (undoable      :initform nil              :allocation :class)
   (causes-change :initform nil              :allocation :class)
   (hysteresis    :initform 5                :allocation :class)
   ))

(defun make-cell-selector (view x y)
  (make-mouse-down-command (document view) view x y
			   :cursor :dot
			   :class 'cell-selector))

(defmethod constrain-mouse ((cmd cell-selector) x y)
  "prevent upward dragging "
  (setq x (max x (start-x cmd)))
  (setq y (max y (start-y cmd)))
  (values x y))

(defmethod track-mouse ((cmd cell-selector) x y 
			&key (started nil) (finished nil)
			&aux cell 
			     (doc (document cmd))
			     (view (view cmd)))
  (declare (ignore started finished))
  (setq cell (find-cell doc x y))
  (when (not (eq cell (last-selected-cell doc)))
    (clear-selection view)
    (setf (last-selected-cell doc) cell)
    (recompute-selected-area view)
    (draw-selection view)))
    
(defmethod draw-feedback ((cmd cell-selector) x y &key clear)
  "no feedback"
  (declare (ignore x y clear))
  nil)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class column-resizer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass column-resizer (mouse-down-command)
  (;; overrides
   (name :initform "Resize Column" :allocation :class)
   (hysteresis :initform 5         :allocation :class)
   ;; instance-variables
   (column    :accessor column :initarg :column)
   (old-width :accessor old-width :initarg :old-width)
   (new-width :accessor new-width)))

(defun make-column-resizer (view x y column old-width)
  (make-mouse-down-command (document view) view x y
			   :cursor :right-side
			   :class 'column-resizer
			   :initargs (list :column column
					   :old-width old-width)))

(defmethod constrain-mouse ((cmd column-resizer) x y)
  "guarantee min and max column width"
  (setq x (max x (+ 10 (- (start-x cmd) (old-width cmd)))))
  (setq x (min x (1- (width (view cmd)))))
  (values x y))

(defmethod draw-feedback ((cmd column-resizer) x y &key clear)
  "draw rectangular feedback"
  (declare (ignore clear))
  (with-slots (view) cmd
    (xlib:with-gcontext ((gcontext view) :line-style :dash)
      ;; vertical  line
      (draw-line view x 0 x (height view)) ;
      ;; display current column width
      (draw-glyphs 
       view (+ x 5) (+ y (round (- (row-height (document cmd)) 6) 2)) 
       (format nil "~d" (+ (old-width cmd) (- x (start-x cmd))))))))

(defmethod set-column-width ((doc sheet) column-nr new-width)
  "update column width list, resize views, and redisplay"

  ;; modify column width in the list
  (setf (column-width-list doc)
    (loop for col-width in (column-width-list doc)
          for col from 1
     collect (if (= col column-nr) new-width col-width)))
  
  (redraw-after-column-resize doc))

(defmethod redraw-after-column-resize ((doc sheet))
  "recompute view sizes and redisplay according to new column-width-list"

  ;; resize cell-view and recompute selection area
  (resize (main-view doc) 
	  (compute-table-width doc)
	  (height (main-view doc))) ;; constant height
  (recompute-selected-area (main-view doc))
  (force-redraw (main-view doc))

  ;; resize and redisplay column headers
  (resize (columns-view doc) 
	  (width (main-view doc))      ;; same width as cell-view 
	  (height (columns-view doc))) ;; constant height 
  (force-redraw (columns-view doc)))

(defmethod doit ((cmd column-resizer) &aux last-cmd (doc (document cmd)))
  "modify column width and check for repetetive resize commands"
  (setf (new-width cmd) 
    (+ (old-width cmd) (- (last-x cmd) (start-x cmd))))
  (redoit cmd)

  (setq last-cmd (first (undo-commands doc)))
  (when (and last-cmd 
	     (typep last-cmd 'column-resizer)
	     (= (new-width cmd) (new-width last-cmd)))
    ;; suggest to set width for all columns
    (setf (label-string (label (suggestion-box doc)))
      (format nil "Resize all columns to ~d pixels?" (new-width cmd)))
    (setf (suggested-callback (suggestion-box doc))
      (make-callback 'make-all-columns-resizer doc (new-width cmd)))
    (pop-up (suggestion-box doc))
    (setf (suggestion-counter doc) 2) ;; pop down after the next command 
  ))

(defmethod redoit ((cmd column-resizer))
  "modify column width"
  (set-column-width (document cmd) (column cmd) (new-width cmd)))

(defmethod undoit ((cmd column-resizer))
  "set old column width again"
  (set-column-width (document cmd) (column cmd) (old-width cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class all-columns-resizer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass all-columns-resizer (command)
  (;; overrides
   (name :initform "Resize All Columns" :allocation :class)
   ;; instance-variables
   (old-column-width-list :accessor old-column-width-list 
			  :initarg :old-column-width-list) 
   (new-width :accessor new-width :initarg :new-width)))

(defun make-all-columns-resizer (document new-width)
  (make-command document 
		:class 'all-columns-resizer
		:initargs 
		(list :new-width new-width
		      :old-column-width-list (column-width-list document))))

(defmethod doit ((cmd all-columns-resizer) &aux (doc (document cmd)))
  "set all column sizes to new-width"
  (setf (column-width-list doc)
    (make-list (nr-of-columns doc) 
                :initial-element (new-width cmd)))
  (redraw-after-column-resize doc))

(defmethod undoit ((cmd all-columns-resizer) &aux (doc (document cmd)))
  "reinstall old column-width-list"
  (setf (column-width-list doc) (old-column-width-list cmd))
  (redraw-after-column-resize doc))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class enter-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass enter-command (command)
  (;; overrides
   (name :initform "Enter Value" :allocation :class)
   ;; instance-parameters
   (cell        :accessor cell        :initarg :cell)
   (old-content :accessor old-content :initarg :old-content)
   (new-content :accessor new-content :initarg :new-content)))

(defun make-enter-command (document cell old-content new-content)
  (make-command document
		:class 'enter-command
		:initargs (list :cell cell 
				:old-content old-content 
				:new-content new-content)))

(defmethod doit ((cmd enter-command) &aux (cell (cell cmd)))
  "enter new value into cell"
  (setf (content cell) (new-content cmd))
  (recompute-all-formulas (document cmd))
  (redraw-cell (main-view (document cmd)) cell))

(defmethod undoit ((cmd enter-command) &aux (cell (cell cmd)))
  "enter old value into cell"
  (setf (content cell) (old-content cmd))
  (recompute-all-formulas (document cmd))
  (redraw-cell (main-view (document cmd)) cell))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class clear-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clear-command (command)
  (;; overrides
   (name :initform "Clear" :allocation :class)
   ;; instance-parameters
   (cells :accessor cells :initarg :cells
	  :documentation "cells being cleared")
   (old-contents :accessor old-contents :initarg :old-contents))
  (:documentation "a command to clear the selected cells"))

(defun make-clear-command (document &aux cells)
  (setq cells (collect-selected-cells document))
  (make-command 
   document
   :class 'clear-command
   :initargs (list :cells cells
		   :old-contents (loop for cell in cells 
				  collect (content cell)))))

(defmethod doit ((cmd clear-command) 
		 &aux (doc (document cmd)))
  "set cell content to empty string"
  (loop for cell in (cells cmd)
   do (setf (content cell) ""))

  (recompute-all-formulas (document cmd))
  (force-redraw (main-view doc))
  (cell-to-input-field doc))

(defmethod undoit ((cmd clear-command)
		   &aux (doc (document cmd)))
  "put old-contents into cells again"
  (loop for cell in (cells cmd)
        for old-content in (old-contents cmd)
	do (setf (content cell) old-content))
  
  (recompute-all-formulas (document cmd))
  (force-redraw (main-view doc))
  (cell-to-input-field doc))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class copy-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass copy-command (command)
  (;; overrides
   (name :initform "Copy" :allocation :class)
   ;; instance-parameters
   (old-clipboard :accessor old-clipboard :initarg :old-clipboard)
   (cells :accessor cells :initarg :cells
	  :documentation "cells being copied"))
  (:documentation "a command to copy the selected cells"))

(defun make-copy-command (document)
  (make-command 
   document
   :class 'copy-command
   :initargs (list :cells 
		   (loop for cell in (collect-selected-cells document)
		     collect (copy cell))
		    :old-clipboard (clipboard *application*))))

(defmethod doit ((cmd copy-command))
  "copy selected cells into applications clipboard"
  (setf (clipboard *application*) (cells cmd)))

(defmethod undoit ((cmd copy-command))
  "restore old clipboard contents"
  (setf (clipboard *application*) (old-clipboard cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class paste-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass paste-command (command)
  (;; overrides
   (name :allocation :instance :initarg :name)
   ;; instance-parameters
   (formulas-only :accessor formulas-only :initarg :formulas-only)
   (old-contents :accessor old-contents :initarg :old-contents)
   (new-contents :accessor new-contents)
   (cells :accessor cells :initarg :cells
	  :documentation "cells being affected by paste"))
  (:documentation "a command to paste into the selected cells"))

(defun make-paste-command (document &key (formulas-only nil)
			   &aux cells)
  (if (not (clipboard *application*))
      (xlib:bell *display*)
    (progn
      (setq cells (collect-selected-cells document))
      (make-command 
       document
       :class 'paste-command
       :initargs (list :name (if formulas-only "Paste Formulas" "Paste")
		       :cells cells
		       :formulas-only formulas-only
		       :old-contents
		       (loop for cell in cells collect (content cell)))))))

(defmethod doit ((cmd paste-command) 
		 &aux (doc (document cmd)) 
		 cell-map length1 length2)
  "copy from applications clipboard into selected cells"

  (setq cell-map (make-hash-table :test #'equal))
  (setq length1 (length (clipboard *application*)))
  (setq length2 (length (cells cmd)))
  (loop for start from 0 to (- length2 length1) by length1
   as target-cells = (subseq (cells cmd) start (+ start length1))
   do
    ;; build cell mapping table
    (clrhash cell-map)
    (loop for cell1 in (clipboard *application*)
          for cell2 in target-cells
     do (setf (gethash (list (row cell1) (column cell1)) cell-map) cell2)
      ;;(format t "~a -> ~a~%" (list (row cell1) (column cell1)) (name cell2))
      )

    ;; assign new contents to target cells
    (loop for cell1 in (clipboard *application*)
          for cell2 in target-cells
     do (when (or (not (formulas-only cmd)) (formula cell1))
	  (setf (content cell2) (content cell1))
	  (when (formula cell2)
	    (transform-formula cell2 cell-map)))))

  ;; store transformed contents in cmd
  (setf (new-contents cmd)
    (loop for cell in (cells cmd) collect (content cell)))

  (cell-to-input-field doc)
  (recompute-all-formulas (document cmd))
  (force-redraw (main-view (document cmd))))

(defmethod redoit ((cmd paste-command) &aux (doc (document cmd)))
  "copy from applications clipboard into selected cells"
  (loop for content in (new-contents cmd)
        for cell in (cells cmd)
    do (setf (content cell) content))
  (cell-to-input-field doc)
  (recompute-all-formulas (document cmd))
  (force-redraw (main-view (document cmd))))

(defmethod undoit ((cmd paste-command))
  "restore old values of selected cells"
  (loop for cell in (cells cmd)
        for content in (old-contents cmd)
    do (setf (content cell) content))
  (cell-to-input-field (document cmd))
  (recompute-all-formulas (document cmd))
  (force-redraw (main-view (document cmd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class borders-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass borders-command (command)
  (;; overrides
   (name :initarg :name)
   ;; instance-parameters
   (flag :accessor flag :initarg :flag)
   (cells :accessor cells :initarg :cells)
   (old-borders :accessor old-borders :initarg :old-borders)))

(defun make-borders-command (document flag &aux cells)
  (setq cells (collect-selected-cells document))
  (make-command 
   document 
   :class 'borders-command
   :initargs (list :cells cells
		   :flag flag
		   :old-borders (loop for cell in cells collect (borders cell))
		   :name (format nil "Borders ~a" (if flag "On" "Off")))))

(defmethod doit ((cmd borders-command))
  (loop for cell in (cells cmd)
	do (setf (borders cell) (flag cmd)))
  (force-redraw (main-view (document cmd))))

(defmethod undoit ((cmd borders-command))
  (loop for cell in (cells cmd)
        for flag in (old-borders cmd)
	do (setf (borders cell) flag))
  (force-redraw (main-view (document cmd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class font-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod select-font ((doc sheet) &aux new-font)
  "ask user for a font"
  (setq new-font
    (select-dialog "Select a font"
		   (xlib:list-font-names *display* "*adobe*")
		   :document doc))
  (when new-font 
    (make-font-command doc (name-of-font (main-view doc)) new-font)))
 
(defclass font-command (command)
  (;; overrides
   (name :initform "Change Font" :allocation :class)
   ;; instance-parameters
   (old-font :accessor old-font :initarg :old-font)
   (new-font :accessor new-font :initarg :new-font)))

(defun make-font-command (document old-font new-font)
  (make-command document
		:class 'font-command
		:initargs (list  :old-font old-font :new-font new-font)))

(defmethod doit ((cmd font-command) &aux (doc (document cmd)))
  (set-new-font (main-view doc)    (new-font cmd))
  (set-new-font (columns-view doc) (new-font cmd))
  (set-new-font (rows-view doc)    (new-font cmd)))

(defmethod undoit ((cmd font-command) &aux (doc (document cmd)))
  (set-new-font (main-view doc)    (old-font cmd))
  (set-new-font (columns-view doc) (old-font cmd))
  (set-new-font (rows-view doc)    (old-font cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; sound
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun play (sound-name &key (volume 40))
   "submit a play command to unix"
   ;; 1<=volume<=100
   (shell-command 
    (format nil "/usr/demo/SOUND/play -v~d /usr/demo/SOUND/sounds/~a.au &"
	    volume sound-name)))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; main program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(register-application "spreadsheet" 'spreadsheet-application "sheet")
'(make-spreadsheet-application)




