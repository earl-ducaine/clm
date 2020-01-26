;;;-*-Mode:LISP;Syntax: Common-Lisp;Package:lisp-widgets;Base:10-*-
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
(defginapackage :lisp-widgets)
(in-package :lisp-widgets)
(setq *sccs-id* "@(#)lisp-widgets.lisp	1.7  11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class lisp-widget-demo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass lisp-widget-demo (application)
  (;; overrides
   (name          :initform "Lisp Widgets"          :allocation :class)
   (document-type :initform 'lisp-widget-document   :allocation :class)
   (signature     :initform "lisp-widgets"          :allocation :class)
   (file-type     :initform nil                     :allocation :class))
  (:documentation "demo how new widgets can be implemented in Lisp"))

(defun make-lisp-widget-demo ()
  "start the lisp-widget-demo"
  (make-application :class 'lisp-widget-demo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class lisp-widget-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass lisp-widget-document (document)
  ()
  (:documentation "application dependent document type"))

(defmethod create-windows ((doc lisp-widget-document) &aux form scrollbar)
  "create the windows belonging to this document"
  (with-slots (main-shell main-view) doc
    ;; create and install the view in a tool-window
    (setq main-shell (make-document-shell doc))
    (setq form (make-form main-shell :motif-resources '(:height 200)))
    (setq scrollbar (make-selfmade-scrollbar form
					     :document doc
					     :value-changed-callback
					     '(lambda (new-val) nil)))
    (define-form-constraint scrollbar
			    :top-attachment  :form 
			    :left-attachment :form
			    :bottom-attachment :form)
    (setq main-view scrollbar)

    ;; remove unnecessary menu entries
    (remove-menu-entry (main-menu main-shell) "File" "Open..")
    (remove-menu-entry (main-menu main-shell) "File" "Save")
    (remove-menu-entry (main-menu main-shell) "File" "Save as..")
    (remove-menu-entry (main-menu main-shell) "File" "Revert")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class selfmade-scrollbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass selfmade-scrollbar (view)
  (;; instance-parameters
   (max-value     :accessor max-value     :initarg :max-value
	          :documentation "current value is between 0 and max-value")
   (value-changed-callback
     :accessor value-changed-callback
     :initarg :value-changed-callback
     :documentation "Function called with new value when user has scrolled")
   
   ;; instance-variables
   (current-value :accessor current-value :initform 0)))

(defun make-selfmade-scrollbar (parent &key (width 20)
				            (height 200)
					    (max-value 100)
					    (value-changed-callback nil)
					    (document nil)
				       &aux frame new-sb)
  "make a new selfmade-scrollbar widget"
  (setq frame (make-frame parent))
  (setq new-sb
	(make-view frame :width width :height height
		   :document document
		   :resize-policy :any
		   :initargs (list :max-value max-value
				   :value-changed-callback value-changed-callback)
		   :class 'selfmade-scrollbar))
  (setf (group-widget-id new-sb) (widget-id frame))
  new-sb)

(defmethod (setf current-value) :after (new-value (sb selfmade-scrollbar))
  "when the text is changed a redisplay is necessary"
  (declare (ignore new-value))
  (force-redraw sb))

(defmethod resized :after ((view selfmade-scrollbar) ignore)
   "the whole scrollbar must be redrawn when resized"
   (declare (ignore ignore))
   (force-redraw view))

(defmethod draw ((view selfmade-scrollbar) count x y width height &aux current-y)
  "draw window contents"
  (declare (ignore x y width height))
  (with-slots (gcontext width height current-value max-value) view
    (when (zerop count) ;; Ignore all but the last exposure event
      (setq current-y (round (* (- height 10) (/ (float current-value) max-value))))
      (draw-rectangle view 0 current-y width 10 t)   ;; filled
      (xlib:with-gcontext (gcontext :function boole-xor :font "6x10")
	(draw-glyphs view 2 (+ 8 current-y) (format nil "~d" current-value)))
      )))

(defmethod button-press ((sb selfmade-scrollbar) code repetition x y)
  "react to button-press event in the window"
  (declare (ignore code repetition))
  (make-scroll-command sb x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class scroll-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass scroll-command (mouse-down-command)
  (;; overrides
   (name          :initform "Scrolling" :allocation :class)
   (undoable      :initform t           :allocation :class)
   (causes-change :initform nil         :allocation :class)

   ;; instance-parameters
   (old-value :accessor old-value :initarg :old-value)
   ;; instance-variables
   (new-value :accessor new-value))
  (:documentation "a mouse-down-command for scrolling"))

(defun make-scroll-command (sb mouse-x mouse-y &key (class 'scroll-command))
  (make-mouse-down-command (document sb) sb mouse-x mouse-y
			   :class class
			   :initargs `(:old-value ,(current-value sb))))

(defmethod draw-feedback ((cmd scroll-command) x y &key clear)
  "no feedback"
  (declare (ignore clear x y)))

(defmethod doit ((cmd scroll-command))
  "nothing to do"
  )

(defmethod track-mouse ((cmd scroll-command) x y &key (started nil) (finished nil))
  "move the slider"
  (declare (ignore x started))
  (with-slots ((scrollbar view) new-value) cmd
    (with-slots (current-value max-value height value-changed-callback) scrollbar
      (setq current-value (round (* max-value (/ y (float height)))))
      (force-redraw scrollbar)
      (execute value-changed-callback (list current-value))
      (when finished
	(setq new-value current-value)))))

(defmethod undoit ((cmd scroll-command))
  "move the elevator and call callback"
  (with-slots ((scrollbar view) old-value) cmd
    (with-slots (current-value value-changed-callback) scrollbar
      (setq current-value old-value)
      (force-redraw scrollbar)
      (execute value-changed-callback (list current-value)))))

(defmethod redoit ((cmd scroll-command))
  "move the elevator and call callback"
  (with-slots ((scrollbar view) new-value) cmd
    (with-slots (current-value value-changed-callback) scrollbar
      (setq current-value new-value)
      (force-redraw scrollbar)
      (execute value-changed-callback (list current-value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; main program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(register-application "lisp-widgets" 'lisp-widget-demo nil)
'(make-lisp-widget-demo)
