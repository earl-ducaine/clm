;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: hello; Base: 10 -*-
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
(defginapackage :hello)
(in-package :hello)
(setq *sccs-id* "@(#)hello.lisp	1.8  11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class hello-world-application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass hello-world-application (application)
  (;; overrides
   (name          :initform "Hello World"         :allocation :class)
   (document-type :initform 'hello-world-document :allocation :class)
   (signature     :initform "hello"               :allocation :class)
   (file-type     :initform "hello"               :allocation :class))
  (:documentation "a simple demo application"))

(defun make-hello-world-application ()
  "start the hello-world-application"
  (make-application :class 'hello-world-application))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class hello-world-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass hello-world-document (document)
  (;; instance-variables
   (hi-list       :initform nil :accessor hi-list
		  :documentation "List of coordinates where HIs are displayed"))
  (:documentation "application dependent document type"))

(defmethod write-to-stream ((doc hello-world-document) stream)
  "write the document to the specified stream"
  (prin1 (hi-list doc) stream))

(defmethod read-from-stream ((doc hello-world-document) stream) 
  "read the document from the specified stream"
  (setf (hi-list doc) (read stream)))

(defmethod create-windows ((doc hello-world-document) &aux scroller)
  "create the windows belonging to this document"
  (with-slots (main-shell main-view) doc
    (setq main-shell (make-document-shell doc))
    (setq scroller   (make-scroller main-shell))
    (setq main-view  (make-hello-world-view scroller doc))

    ;; add an application specific command
    (add-menu-command (main-menu main-shell)
		      "Hello" "Clear all" (make-callback #'make-clear-all-command doc))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class hello-world-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass hello-world-view (view)
  ()
  (:documentation "a view with special draw method and reaction to clicks"))

(defun make-hello-world-view (parent doc)
  "create a new hello-world-view"
  (make-view parent :document doc :class 'hello-world-view))

(defmethod draw ((view hello-world-view) count x y width height)
  "draw window contents"
  (declare (ignore x y width height))
  (when (zerop count) ;; Ignore all but the last exposure event
    (loop for (x y) in (hi-list (document view))
	  do (draw-glyphs view x y "Hi!"))))

(defmethod button-press ((view hello-world-view) code repetition x y)
  "react to button-press event in the window"
  (declare (ignore code repetition))
  (make-add-hi-command (document view) x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; undoable clear-all-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clear-all-command (command)
  ((name :initform "Clear All Command" :allocation :class)
   (old-hi-list :accessor old-hi-list :initarg :old-hi-list)))

(defun make-clear-all-command (document)
  "create command object storing the current hi-list"
  (make-command document
		:class 'clear-all-command
		:initargs (list :old-hi-list (hi-list document))))

(defmethod doit ((cmd clear-all-command))
  "clear hi-list"
  (with-slots (document) cmd
    (setf (hi-list document) nil)
    (force-redraw (main-view document))))

(defmethod undoit ((cmd clear-all-command))
  "restore hi-list"
  (with-slots (document old-hi-list) cmd
    (setf (hi-list document) old-hi-list)
    (force-redraw (main-view document))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; undoable add-hi-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass add-hi-command (command)
  ((name :initform "Add Hi Command" :allocation :class)
   (hi :accessor hi :initarg :hi)))

(defun make-add-hi-command (document x y)
  "create command object storing the x/y coordinates of the mouse-click"
  (make-command document
		:class 'add-hi-command
		:initargs (list :hi (list x y))))

(defmethod doit ((cmd add-hi-command))
  "add a new pair to hi-list"
  (with-slots (document hi) cmd
    (push hi (hi-list document))
    (force-redraw (main-view document))))

(defmethod undoit ((cmd add-hi-command))
  "pop hi-list"
  (with-slots (document) cmd
    (pop (hi-list document))
    (force-redraw (main-view document))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; main program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(register-application "hello" 'hello-world-application "hello")
'(make-hello-world-application)
