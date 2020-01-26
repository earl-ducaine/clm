;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: dd;Base: 10 -*-
;;;
;;; Copyright 1991 GMD (German National Research Center for Computer Science)
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
(defginapackage :dd)
(in-package :dd)
(setq *sccs-id* "@(#)drag-and-drop.lisp	1.7  11/10/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class drag-application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass drag-application (application)
  (;; overrides
   (name          :initform "Drag"         :allocation :class)
   (document-type :initform 'drag-document :allocation :class)
   (signature     :initform "drag"               :allocation :class)
   (file-type     :initform "drag"               :allocation :class))
  (:documentation "a simple drag demo application"))

(defun make-drag-application ()
  "start the drag-application"
  (make-application :class 'drag-application))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class drag-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass drag-document (document)
  (;; instance-variables
   (track-label    :accessor track-label
         :documentation "a label where tracking information is displayed")
   (drag-template  :accessor drag-template
         :documentation "a drag label as triangle template")
   (drag-protocols :initform '(:copy_triangle) :allocation :class))
  (:documentation "application dependent document type"))

(defmethod write-to-stream ((doc drag-document) stream)
  "write the document to the specified stream"
  (format stream "~d~%" (length (view-objects (main-view doc))))
  (loop for obj in (view-objects (main-view doc))
        do (format stream "~d ~d ~d~%" (x-pos obj) (y-pos obj) 
		   (id-number obj))))

(defmethod read-from-stream ((doc drag-document) stream &aux objects) 
  "read the document from the specified stream"
  ;; delete all present objects in case of revert
  (loop for obj in (view-objects (main-view doc))
        do (deinstall obj))

  (setq objects (read stream))
  (loop repeat objects
   do (let* ((x-pos (read stream))
	     (y-pos (read stream))
	     (view (main-view doc))
	     (new-tri (make-triangle (obj-width view) (obj-height view) 
				     (read stream))))
	(install new-tri view x-pos y-pos))))

(defmethod create-windows ((doc drag-document) 
			   &aux scroller form label)
  "create the windows belonging to this document"
  (with-slots (main-shell main-view) doc
    (setq main-shell (make-document-shell doc))
    (setq scroller   (make-scroller main-shell))
    (setq main-view  (make-drag-view scroller doc))
    (setq form (make-form main-shell))
    (set-motif-resources (main-window main-shell) 
			 :command-window (widget-id form))

    (setq label (make-drag-label form doc "magnet"
                                 :cursor-mask "magnet-mask"))
    (setf (activate-callback label)
          (make-callback 'make-magnet-dragger doc label))
    (define-form-constraint label :left-attachment :form :top-attachment :form
                            :bottom-attachment :form)

    (setf (drag-template doc) 
          (make-drag-label form doc "triangle"))
    (setf (activate-callback (drag-template doc))
          (make-callback 'make-triangle-dragger doc (drag-template doc)))
    (define-form-constraint (drag-template doc) :left-attachment :none 
			    :top-attachment :form :right-attachment :form)

    (setf (track-label doc)
          (make-label form ""))
    (define-form-constraint (track-label doc) :left-attachment :widget
			    :left-widget label :top-attachment :form
			    :bottom-attachment :form
			    :right-attachment :widget
			    :right-widget (drag-template doc))
    ))

(defmethod create-drop-command ((doc drag-document) shell x y protocol-id
				transfer-value received-from)
  "create a duplicate command when dropping from another document"
  (case protocol-id
    ((:copy_triangle)
        (make-triangle-copier doc shell x y 
                              :transfer-value transfer-value
                              :received-from received-from))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class drag-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass drag-view (view)
  ((bitmap-name   :accessor bitmap-name  :initform "triangle")
   (image         :accessor image)
   (pixmap        :accessor pixmap)
   (obj-width     :accessor obj-width)
   (obj-height    :accessor obj-height))
  (:documentation "a view with special draw method and reaction to clicks"))

(defun make-drag-view (parent doc &aux new-view)
  "create a new drag-view"
  (setq new-view (make-view parent :document doc :class 'drag-view))
  (setf (image new-view)
        (xlib:read-bitmap-file (find-bitmap (bitmap-name new-view))))
  (setf (obj-width new-view) (xlib:image-width (image new-view)))
  (setf (obj-height new-view) (xlib:image-height (image new-view)))
  new-view)

(defmethod determine-window-id :after ((view drag-view) &aux gc)
  "create a pixmap for triangle drawing"
  (setf (pixmap view)
        (xlib:create-pixmap :width (obj-width view) :height (obj-height view) 
                            :depth 1 :drawable (x-window view)))
  ;; must have a separate GC for the pixmap because of different depth
  (setq gc (xlib:create-gcontext :drawable (pixmap view)))
  (xlib:put-image (pixmap view) gc (image view) :x 0 :y 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class triangle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass triangle (view-object)
  ((id-number :accessor id-number :initarg :id-number))
  (:documentation "a triangle in a view"))

(defun make-triangle (width height id-number)
  (make-view-object width height :class 'triangle 
		    :initargs (list :id-number id-number)))

(defmethod draw ((obj triangle) count x y width height)
  (declare (ignore count x y width height))
  (xlib:copy-plane (pixmap (parent-view obj)) (gcontext (parent-view obj)) 1
                   0 0 
		   (obj-width (parent-view obj)) (obj-height (parent-view obj))
                   (x-window (parent-view obj)) (x-pos obj) (y-pos obj))
  (draw-glyphs obj (- (round (/ (width obj) 2)) 5) (- (height obj) 6) 
	       (write-to-string (id-number obj))))

(defmethod button-press ((obj triangle) code repetition x y)
  "create a copy-drag command for this triangle"
  (when (= repetition 1)
    (case code
      ((:select)
            (make-triangle-copier (document (parent-view obj))
				  (parent-view obj) x y
				  :transfer-value (id-number obj)
				  :x-off (x-pos obj) :y-off (y-pos obj))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class magnet-dragger
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass magnet-dragger (drag-command)
  ((name      :initform "Delete Triangle"   :allocation :class)
   (deleted-object :accessor deleted-object)))

(defun make-magnet-dragger (doc source x y)
  (make-drag-command doc source x y (list (list (main-view doc) :view))
		     :cursor (cursor source)
		     :class 'magnet-dragger))

(defmethod draw-target-feedback ((cmd magnet-dragger) view x y value
				 &key (clear nil))
  "find view object under cursor and highlight it"
  (declare (ignore clear))
  (when (eql value :view)
    (loop for obj in (view-objects view)
        when (point-inside obj x y)
        do (xlib:with-gcontext ((gcontext view) :function boole-xor
				:foreground (xor-foreground view))
             (draw-rectangle obj 0 0 (1- (width obj)) (1- (height obj))))
           (return nil))))

(defmethod executable ((cmd magnet-dragger))
  "check whether successful in final position"
  (setf (deleted-object cmd)
        (loop for obj in (view-objects (current-target cmd))
          do (when (point-inside obj (target-x cmd) (target-y cmd))
                (return obj))))
  (deleted-object cmd))

(defmethod doit ((cmd magnet-dragger))
  "delete the object"
  (deinstall (deleted-object cmd)))

(defmethod undoit ((cmd magnet-dragger))
  "reinstall the deleted object"
  (install (deleted-object cmd) (current-target cmd)
           (x-pos (deleted-object cmd)) (y-pos (deleted-object cmd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class triangle-dragger
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass triangle-dragger (drag-command)
  ((name         :initform "Make Triangle"   :allocation :class)
   (new-triangle :accessor new-triangle   :initform nil)))

(defun make-triangle-dragger (doc source x y)
  (make-drag-command doc source x y (list (list (main-view doc) nil))
		     :cursor :left-ptr
		     :shell-to-move (drag-shell source)
		     :class 'triangle-dragger))

(defmethod track-target ((cmd triangle-dragger) widget x y value)
  "display current position as feedback"
  (declare (ignore value))
  (setf (label-string (track-label (document cmd)))
        (if widget
            (format nil "(~d, ~d)" x y)
	    "")))

(defmethod doit ((cmd triangle-dragger))
  "create a new triangle at target position"
  (unless (new-triangle cmd)
    (let ((view (main-view (document cmd))))
      (setf (label-string (track-label (document cmd))) "")
      (setf (new-triangle cmd) 
	    (make-triangle (obj-width view) (obj-height view) 
			   (length (view-objects view))))))
  (install (new-triangle cmd) (main-view (document cmd))
           (target-x cmd) (target-y cmd)))

(defmethod undoit ((cmd triangle-dragger))
  "remove the installed triangle"
  (deinstall (new-triangle cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class triangle-copier
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass triangle-copier (drag-command)
  ((name    :initform "Copy Triangle"    :allocation :class)
   ;; triangle must be dragged away
   (hysteresis    :initform 5            :allocation :class)
   ;; protocol-id needed to identify for foreign apps
   (protocol-id   :initform :copy_triangle  :allocation :class)
   ;; new slot
   (new-triangle  :accessor new-triangle :initform nil)))

(defun make-triangle-copier (doc source x y
			     &key (x-off 0) (y-off 0)
			          (transfer-value nil)
			          (received-from nil))
  (make-drag-command doc source 
		     (+ x x-off) (+ y y-off)
		     (list (list (main-view doc) nil))
		     :shell-to-move (drag-shell (drag-template doc))  ;; abused
		     :cursor :left-ptr
		     :x-off x-off
		     :y-off y-off
		     :transfer-value transfer-value  ;; triangle number will
		                                     ;; be transferred
		     :received-from received-from
		     :class 'triangle-copier))

(defmethod track-target ((cmd triangle-copier) widget x y value)
  "display current position as feedback, only in own document"
  (declare (ignore value))
  (setf (label-string (track-label (document cmd)))
        (if widget
            (format nil "(~d, ~d)" x y)
	    "")))

(defmethod doit ((cmd triangle-copier))
  "install a new triangle copy"
  (unless (new-triangle cmd)
    (setf (label-string (track-label (document cmd))) "")
    (setf (new-triangle cmd)
          (make-triangle (obj-width (main-view (document cmd)))
                         (obj-height (main-view (document cmd)))
                         (transfer-value cmd))))
  (install (new-triangle cmd) (main-view (document cmd))
           (target-x cmd) (target-y cmd)))

(defmethod undoit ((cmd triangle-copier))
  "remove the triangle"
  (deinstall (new-triangle cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; main program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; start drag application

(register-application "drag" 'drag-application "drag")
'(make-drag-application)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class drop-application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass drop-application (application)
  (;; overrides
   (name          :initform "Drop"         :allocation :class)
   (document-type :initform 'drop-document :allocation :class)
   (signature     :initform "drop"               :allocation :class)
   (file-type     :initform "drop"               :allocation :class))
  (:documentation "a simple drop demo application"))

(defun make-drop-application ()
  "start the drop-application"
  (make-application :class 'drop-application))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class drop-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass drop-document (document)
  (;; instance-variables
   (fax            :accessor fax)
   (phone          :accessor phone)
   (panel          :accessor panel)
   ;; signal the acceptance of a triangle-copier
   (drag-protocols :initform '(:copy_triangle) :allocation :class))
  (:documentation "application dependent document type"))

(defmethod create-windows ((doc drop-document) 
			   &aux rc)
  "create the windows belonging to this document"
  (with-slots (main-shell) doc
    (setq main-shell (make-document-shell doc))
    (setq rc (make-row-column main-shell :orientation :vertical
             :entry-alignment :center))
    (setf (panel doc)
          (make-label rc " "))
    (setf (phone doc)
          (make-push-button rc "PHONE" 
		      :motif-resources '(:margin-height 20 :margin-width 15)))
    (setf (fax doc)
          (make-push-button rc "FAX"
                      :motif-resources '(:margin-height 20 :margin-width 15)))
    ))

(defmethod create-drop-command ((doc drop-document) shell x y protocol-id
				transfer-value received-from)
  "when accepting a triangle copier, a command of a different class is created"
  (case protocol-id
    ((:copy_triangle)
        (make-triangle-receiver doc shell x y 
                                :transfer-value transfer-value
                                :received-from received-from))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class triangle-receiver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass triangle-receiver (drag-command)
  ((name          :allocation :class)
   (causes-change :initform nil             :allocation :class)
   (protocol-id   :initform :copy_triangle  :allocation :class)
   (do-tracking   :initform nil             :allocation :class)
   ;; new slot
   (old-label     :accessor old-label       :initform nil)))

(defun make-triangle-receiver (doc source x y
			       &key (transfer-value nil)
			            (received-from nil))
  (make-drag-command doc source x y
		     (list (list (phone doc) :phone)
			   (list (fax doc)   :fax))
		     :transfer-value transfer-value
		     :received-from received-from
		     :class 'triangle-receiver))

(defmethod doit ((cmd triangle-receiver))
  "display action that would have been taken, name depends on target"
  (unless (old-label cmd)
    (setf (old-label cmd) (label-string (panel (document cmd))))
    (setf (name cmd) 
          (format nil "~a #~d" (current-value cmd) (transfer-value cmd))))
  (setf (label-string (panel (document cmd))) (name cmd)))

(defmethod undoit ((cmd triangle-receiver))
  "restore old panel contents"
  (setf (label-string (panel (document cmd))) (old-label cmd)))

;;; start drop application
(register-application "drop" 'drop-application "drop")
'(make-drop-application)

