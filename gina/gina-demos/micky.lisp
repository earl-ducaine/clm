;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: micky;Base: 10-*-
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
(defginapackage :micky)
(in-package :micky)
(setq *sccs-id* "@(#)micky.lisp	1.7  11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class micky-application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass micky-application (application)
  (;; overrides
   (name          :initform "Micky"         :allocation :class)
   (document-type :initform 'micky-document :allocation :class)
   (signature     :initform "micky"         :allocation :class)
   (file-type     :initform "micky"         :allocation :class))
  (:documentation "a simple demo application"))

(defun make-micky-application ()
  "start the micky-application"
  (make-application :class 'micky-application))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class micky-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass micky-document (document)
  (;; instance-variables
   (circles      :accessor circles :initform t)
   (micky-width  :initform 100 :accessor micky-width
		 :documentation "Width of scalable Micky-Mouse")
   (micky-height :initform 100 :accessor micky-height
		 :documentation "Height of scalable Micky-Mouse")
   (h-scale      :accessor h-scale)
   (v-scale      :accessor v-scale)
   
   ;; overrides
   (shell-width  :initform 300)
   (shell-height :initform 400))
  (:documentation "application dependent document type"))

(defmethod create-windows ((doc micky-document) &aux form scroller)
  "create the windows belonging to this document"
  (with-slots (main-shell main-view h-scale v-scale) doc
    ;; create document-shell
    (setq main-shell (make-document-shell doc))
    ;; the main-part of the window is organized as a form
    (setq form (make-form main-shell))
    ;; horizontal scale to change the width of Micky
    (setq h-scale (make-scale form
			      :orientation :horizontal
			      :value (micky-width  doc)
			      :maximum 500
			      :value-changed-callback
			      (make-callback #'new-micky-width doc)
			      ))
    (setf (drag-callback h-scale) (make-callback #'new-micky-width doc))
    
    ;; vertical scale to change the height of Micky
    (setq v-scale (make-scale form
			      :orientation :vertical
			      :processing-direction :max-on-bottom
			      :value (micky-height doc)
			      :maximum 500
			      :value-changed-callback
			      (make-callback #'new-micky-height doc)
			      ))
    (setf (drag-callback v-scale) (make-callback #'new-micky-height doc))
    
    (setq scroller (make-scroller form :motif-resources (list :width 250 :height 300)))
    
    ;; create the view
    (setq main-view (make-view scroller
			       :width 500 :height 500 :document doc
			       :class 'micky-view))
    ;; define the layout of the form
    (define-form-constraint h-scale
			    :top-attachment   :form 
			    :left-attachment  :widget :left-widget v-scale
			    :right-attachment :form)
    (define-form-constraint v-scale
			    :top-attachment    :form :top-offset 30
			    :left-attachment   :form
			    :bottom-attachment :form)
    (define-form-constraint scroller
			    :top-attachment  :widget :top-widget  h-scale 
			    :left-attachment :widget :left-widget v-scale 
			    :right-attachment  :form 
			    :bottom-attachment :form)

    ;; add a radio group within the main menu to set the desired shape
    (insert-menu-entry (main-menu main-shell) "Shape" "Group"
		       (make-radio-group-entry "Group"
						'("Circle" "Rectangle")
						(make-callback #'toggle-shape doc)
						:is-submenu nil))
    
    ))

(defmethod new-micky-width ((doc micky-document) new-width)
  "user has changed width of Micky-Mouse"
  (with-slots (main-view micky-width) doc
    (setq micky-width new-width)
    (force-redraw main-view)
    (setf (modified doc) t)))

(defmethod new-micky-height ((doc micky-document) new-height)
  "user has changed height of Micky-Mouse"
  (with-slots (main-view micky-height) doc
    (setq micky-height new-height)
    (force-redraw main-view)
    (setf (modified doc) t)))

(defmethod toggle-shape ((doc micky-document) new-shape old-shape)
  "toggle between circle and rectangle presentation"
  (declare (ignore old-shape))
  (with-slots (circles main-view v-scale h-scale) doc
    (setq circles (equal new-shape "Circle"))
    (force-redraw main-view)))

(defmethod write-to-stream ((doc micky-document) stream)
  "write the document to the specified stream"
  (format stream "~d ~d~%" (micky-width  doc) (micky-height doc)))

(defmethod read-from-stream ((doc micky-document) stream) 
  "read the document from the specified stream"
  (with-slots (h-scale v-scale micky-width micky-height) doc
    (setq micky-width  (read stream))
    (setq micky-height (read stream))
    (setf (value h-scale) micky-width)
    (setf (value v-scale) micky-height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class micky-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass micky-view (view)
  ()
  (:documentation "a view with special draw method and reaction to clicks"))

(defmethod draw ((view micky-view) count x y width height)
  "draw window contents"
  (declare (ignore x y width height))
  (with-slots (x-window gcontext document) view
    (with-slots (micky-width micky-height circles) document 
      (when (zerop count) ;; Ignore all but the last exposure event
	(when circles 
	  ;; outline of the head
	  (draw-arc view
		    (round (* 0.35 micky-width)) (round (* 0.43 micky-height))
		    (round (* 0.5 micky-width)) (round (* 0.5 micky-height))
		    0 (* 2 pi) nil)
	  ;; ears
	  (draw-arc view 
		    (round (* 0.3 micky-width)) (round (* 0.3 micky-height))
		    (round (* 0.2 micky-width)) (round (* 0.2 micky-height))
		    0 (* 2 pi) t)
	  (draw-arc view
		    (round (* 0.7 micky-width)) (round (* 0.3 micky-height))
		    (round (* 0.2 micky-width)) (round (* 0.2 micky-height))
		    0 (* 2 pi) t)
	  ;; nose
	  (draw-arc view
		    (round (* 0.58 micky-width)) (round (* 0.64 micky-height))
		    (round (* 0.05 micky-width)) (round (* 0.05 micky-height))
		    0 (* 2 pi) t)
	  ;; eyes
	  (draw-arc view
		    (round (* 0.5 micky-width)) (round (* 0.55 micky-height))
		    (round (* 0.05 micky-width)) (round (* 0.05 micky-height))
		    0 (* 2 pi) nil)
	  (draw-arc view
		    (round (* 0.65 micky-width)) (round (* 0.55 micky-height))
		    (round (* 0.05 micky-width)) (round (* 0.05 micky-height))
		    0 (* 2 pi) nil)
	  ;; mouth
	  (draw-arc view
		    (round (* 0.45 micky-width)) (round (* 0.5 micky-height))
		    (round (* 0.3 micky-width)) (round (* 0.3 micky-height))
		    (* 1.3 pi) (* 0.4 pi) nil))

	(when (not circles) 
	  ;; outline of the head
	  (draw-rectangle view
			  (round (* 0.35 micky-width)) (round (* 0.43 micky-height))
			  (round (* 0.5 micky-width)) (round (* 0.5 micky-height)) nil)
	  ;; ears
	  (draw-rectangle view
			  (round (* 0.3 micky-width)) (round (* 0.3 micky-height))
			  (round (* 0.2 micky-width)) (round (* 0.2 micky-height))  t)
	  (draw-rectangle view
			  (round (* 0.7 micky-width)) (round (* 0.3 micky-height))
			  (round (* 0.2 micky-width)) (round (* 0.2 micky-height))  t)
	  ;; nose
	  (draw-rectangle view
			  (round (* 0.58 micky-width)) (round (* 0.64 micky-height))
			  (round (* 0.05 micky-width)) (round (* 0.05 micky-height)) t)
	  ;; eyes
	  (draw-rectangle view
			  (round (* 0.5 micky-width)) (round (* 0.55 micky-height))
			  (round (* 0.05 micky-width)) (round (* 0.05 micky-height)) nil)
	  (draw-rectangle view
			  (round (* 0.65 micky-width)) (round (* 0.55 micky-height))
			  (round (* 0.05 micky-width)) (round (* 0.05 micky-height)) nil)
	  ;; mouth
	  (draw-line view
		     (round (* 0.5 micky-width)) (round (* 0.8 micky-height))
		     (round (* 0.7 micky-width)) (round (* 0.8 micky-height))))
	))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; main program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(register-application "micky" 'micky-application "micky")
'(make-micky-application)
