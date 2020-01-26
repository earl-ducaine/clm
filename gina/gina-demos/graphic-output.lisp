;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: output;Base: 10 -*-
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

(in-package :GINA)
(defginapackage :output)
(in-package :output)
(setq *sccs-id* "@(#)graphic-output.lisp	1.8  11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class graphic-output-application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass graphic-output-application (application)
  (;; overrides
   (name          :initform "Graphic Output"          :allocation :class)
   (document-type :initform 'graphic-output-document  :allocation :class)
   (signature     :initform "output"                  :allocation :class)
   (file-type     :initform "output"                  :allocation :class)))

(defun make-graphic-output-application ()
  "start the graphic-output-application"
  (make-application :class 'graphic-output-application))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class graphic-output-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass graphic-output-document (document)
  (;; instance-variables
   (line-dialog :accessor line-dialog :initform nil)
   (fill-dialog :accessor fill-dialog :initform nil)
   (font-dialog :accessor font-dialog :initform nil)
   ;; overrides
   (shell-width  :initform 500)
   (shell-height :initform 500)))

(defmethod create-windows ((doc graphic-output-document))
  "create the windows belonging to this document"
  (with-slots (main-shell main-view) doc
    ;; create a simple document-shell
    (setq main-shell (make-document-shell-with-scroller doc))
    ;; create the view
    (setq main-view (make-graphic-output-view  main-shell doc))
    
    ;; add some menu commands
    (add-menu-command (main-menu main-shell)
		      "GContext" "Line ..."
		      (make-callback #'display-line-dialog doc))
    (add-menu-command (main-menu main-shell)
		      "GContext" "Fill ..."
		      (make-callback #'display-fill-dialog doc))
    (add-menu-command (main-menu main-shell)
		      "GContext" "Font ..."
		      (make-callback #'display-font-dialog doc))

    ;; remove unnecessary menu entries
    (remove-menu-entry (main-menu main-shell) "File" "Open..")
    (remove-menu-entry (main-menu main-shell) "File" "Save")
    (remove-menu-entry (main-menu main-shell) "File" "Save as..")
    (remove-menu-entry (main-menu main-shell) "File" "Revert")

    (remove-menu-entry (main-menu main-shell) "Edit" "Undo")
    (remove-menu-entry (main-menu main-shell) "Edit" "Redo")
    (remove-menu-entry (main-menu main-shell) "Edit" "Replay History")
    (remove-menu-entry (main-menu main-shell) "Edit" "History Scroller")

    ))

(defmethod display-line-dialog ((doc graphic-output-document) &aux column)
  "pop-up gcontext dialog, create if necessary"
  (with-slots (line-dialog main-view) doc
    (when (not line-dialog)
      (setq line-dialog (make-modeless-dialog-box "Line Attributes" :document doc))
      (setq column (make-row-column line-dialog))
      ;; a scale to adjust the line width
      (make-scale column
		  :orientation :horizontal
		  :title-string "line-width"
		  :minimum 0 :maximum 10 :value 0
		  :value-changed-callback
		  `(lambda (new)
		     (setf (xlib:gcontext-line-width ',(gcontext main-view)) new)
		     (force-redraw ',main-view)))

      ;; radio button group for line style
      (make-radio-button-group
	column '((":solid" :solid)
		 (":dash" :dash)
		 (":double-dash" :double-dash))
	:label-string "line-style"
	:value-changed-callback
	`(lambda (new old)
	   (setf (xlib:gcontext-line-style ',(gcontext main-view)) new)
	   (force-redraw ',main-view)))

      
      ;; radio button group for join-style
      (make-radio-button-group
	column '((":miter" :miter)
		 (":round" :round)
		 (":bevel" :bevel))
	:label-string "join-style"
	:value-changed-callback
	`(lambda (new old)
	   (setf (xlib:gcontext-join-style ',(gcontext main-view)) new)
	   (force-redraw ',main-view)))

      ;; radio button group for cap-style
      (make-radio-button-group
	column '((":not-last" :not-last)
		 (":butt" :butt)
		 (":round" :round)
		 (":projecting" :projecting))
	:label-string "cap-style"
	:value-changed-callback
	`(lambda (new old)
	   (setf (xlib:gcontext-cap-style ',(gcontext main-view)) new)
	   (force-redraw ',main-view)))
      )
    (pop-up line-dialog)))

(defmethod display-fill-dialog ((doc graphic-output-document) &aux column)
  "create modeless dialog box"
  (with-slots (fill-dialog main-view) doc
    (when (not fill-dialog)
      (setq fill-dialog (make-modeless-dialog-box "Fill Attributes" :document doc))
      (setq column (make-row-column fill-dialog))
 
      ;; radio button group for fill style
      (make-radio-button-group
	column '((":solid" :solid)
		 (":tiled" :tiled)
		 (":opaque-stippled" :opaque-stippled)
		 (":stippled" :stippled))
	:label-string "fill-style"
	:value-changed-callback
	`(lambda (new old)
	   (setf (xlib:gcontext-fill-style (gcontext (main-view ',doc))) new)
	   (force-redraw (main-view ',doc))))

      ;; radio button group for fill rule
      (make-radio-button-group 
	column '((":even-odd" :even-odd)
		 (":even-odd" :winding))
	:label-string "fill-rule"
	:value-changed-callback
	`(lambda (new old)
	   (setf (xlib:gcontext-fill-rule (gcontext (main-view ',doc))) new)
	   (force-redraw (main-view ',doc))))


      ;; radio button group for arc-mode
      (make-radio-button-group
	column '((":chord" :chord)
		 (":pie-slice" :pie-slice))
	:label-string "arc-mode"
	:value-changed-callback
	`(lambda (new old)
	   (setf (xlib:gcontext-arc-mode (gcontext (main-view ',doc))) new)
	   (force-redraw (main-view ',doc))))

      ;; option-menu for drawing-function
      (make-option-menu
	column `(("boole-clr" ,boole-clr)
		 ("boole-set" ,boole-set)
		 ("boole-1" ,boole-1)
		 ("boole-2" ,boole-2)
		 ("boole-c1" ,boole-c1)
		 ("boole-c2" ,boole-c2)
		 ("boole-and" ,boole-and)
		 ("boole-ior" ,boole-ior)
		 ("boole-xor" ,boole-xor)
		 ("boole-eqv" ,boole-eqv)
		 ("boole-nand" ,boole-nand)
		 ("boole-nor" ,boole-nor)
		 ("boole-andc1" ,boole-andc1)
		 ("boole-andc2" ,boole-andc2)
		 ("boole-orc1" ,boole-orc1)
		 ("boole-orc2" ,boole-orc2))
	:label-string "drawing-function"
	:initial-value boole-1
	:value-changed-callback
	`(lambda (new old)
	   (setf (xlib:gcontext-function (gcontext (main-view ',doc))) new)
	   (force-redraw (main-view ',doc))))
    
      )
    (pop-up fill-dialog)))

(defmethod display-font-dialog ((doc graphic-output-document) &aux column font-list)
  "create modeless dialog box"
  (with-slots (font-dialog main-view) doc
    (when (not font-dialog)
      (setq font-dialog (make-modeless-dialog-box "Fonts" :document doc))
      (setq column (make-row-column font-dialog))
 
      ;; font menu
      (setq font-list
	    (make-scrollable-selection-list column (xlib:list-font-names *display* "*")))
      (setf (default-action-button font-list)
	    (make-push-button column "Change Font"
			      :activate-callback
			      `(lambda ()
				 (setf (xlib:gcontext-font (gcontext (main-view ',doc)))
				       (value ',font-list))
				 (force-redraw (main-view ',doc)))))
      )
    (pop-up font-dialog)))

'(with-application-stopped
   (print (xlib:list-font-names *display* "*x*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class graphic-output-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass graphic-output-view (view)
  ()
  (:documentation "a view with special draw method"))

(defun make-graphic-output-view (parent document)
  (make-view parent
	     :width 600 :height 800 :document document
	     :backing-store t
	     :class 'graphic-output-view))

(defmethod draw ((view graphic-output-view) count x y width height)
  "draw window contents"
  (declare (ignore x y width height))
  (with-slots (gcontext document) view
    (when (zerop count) ;; Ignore all but the last exposure event
	
      ;; draw some rectangles
      ;;(draw-rectangle view x y width height &optional fill-p)
      (draw-rectangle view 5  5 40 40 t)
      (xlib:with-gcontext (gcontext :line-width 4)
	(draw-rectangle view 50 5 40 40 nil))

      ;; draw some arcs
      ;;(draw-arc view x y width height angle1 angle2 &optional fill-p)
      (draw-arc view 5  100 40 40 0 (* 2 pi) t)
      (draw-arc view 50 100 40 40 0 (* 2 pi) nil)

      (draw-arc view 100 100 40 40 0 (* 0.5 pi) t)
      (draw-arc view 150 100 40 40 0 (* 0.5 pi) nil)

      ;; draw some lines and points
      (draw-line view 5 150 100 150)
      (draw-lines view '(220 150 260 170 240 120 220 170) :fill-p t)
      (draw-points view '(1 160 4 160 9 160 16 160 25 160 36 160 49 160 64 160 81 160 100 160))

      ;; draw some text in different fonts
      (xlib:with-gcontext (gcontext :font "6x10")
	(draw-glyphs view 5 200 "This is a text in font 6x10"))
      (draw-glyphs view 5 230 "This is a text in a variable font")
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; main program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(register-application "output" 'graphic-output-application "output")
'(make-graphic-output-application)
