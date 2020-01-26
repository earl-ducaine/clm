;;;-*-Mode:LISP;Syntax: Common-Lisp;Package:ib;Base:10-*-
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
;;;          Germany

(in-package :ib)

(setq *sccs-id* "@(#)widget-drag.lisp	1.14	11/8/93")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class drag-template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass drag-template ()
  ((drag-widget  :accessor drag-widget)
   (drag-shell   :accessor drag-shell)
   (drag-copy    :accessor drag-copy)
   (target-view  :accessor target-view :initarg :target-view)
   (document     :accessor document :initarg :document)
   (info         :accessor info        :initarg :info)
   (info-constructor   :accessor info-constructor 
                       :initarg :info-constructor))
  (:documentation "a widget as template"))

;; widget-constructor must disconnect conflicting translations for btn1
'(make-drag-template parent target-view 'make-label-template 'make-label-info)

(defun make-drag-template (parent target-view info info-constructor
                           &key (class 'drag-template)
                           &aux new-template)
  (setq new-template 
        (make-instance class :target-view (list target-view nil)
                             :document (document target-view)
                             :info info
                             :info-constructor info-constructor))
  (setf (drag-widget new-template)
        (make-template info parent))
  (loop for widget in (cons (drag-widget new-template)
                            (all-widgets info (drag-widget new-template))) 
        when widget do
            (add-event-handler widget :button-press-mask
                       (make-callback #'handle-button-press new-template))
            (add-event-handler widget :button-release-mask
		       (make-callback #'gina::handle-button-release 
		      		      (document target-view)))
            (add-event-handler widget :button-motion-mask
		       (make-callback #'gina::handle-button-motion 
				      (document target-view))))
  (setf (drag-shell new-template) 
        (make-shell parent nil :motif-widget-class 'override-shell
                          :motif-resources '(:border-width 0)))
  (setf (drag-copy new-template)
        (make-label (drag-shell new-template) "" :label-type :pixmap
		    :motif-resources '(:margin-width 0 :margin-height 0)))
  (update-slots (drag-shell new-template))
  new-template)

(defmethod handle-button-press ((temp drag-template) &rest parm-list)
  (when (and (= (seventh parm-list) 1) (zerop (sixth parm-list)))
         ;; btn1 w/o any modifiers
    (let ((x (second parm-list))
          (y (third parm-list))
          (widget (drag-widget temp)))
      (update-slots widget)
      (when (string= (label-string (drag-copy temp)) "")
        (let* ((win (get-clx-window widget))
	       (gc (xlib:create-gcontext :drawable win
				         :subwindow-mode :include-inferiors))
	       (pixmap (xlib:create-pixmap :width (width widget) 
					   :height (height widget)
					   :depth (xlib:drawable-depth win) 
					   :drawable win)))
	  (xlib:copy-area win gc 0 0 (width widget) (height widget)
			  pixmap 0 0)
	  (xlib:display-finish-output *display*) ;; make sure pixmap is known
	  (setf (label-string (drag-copy temp))
	        (format nil "%~d" (xlib:drawable-id pixmap)))))
      (resize (drag-shell temp) (width widget) (height widget))
      (make-widget-dragger temp x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class widget-dragger
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass widget-dragger (drag-command)
  (;; overrides
   (name :initform "Create Widget"  :allocation :class)
   (hysteresis :initform 0          :allocation :class)
   (do-tracking :allocation :class :initform nil)
   (protocol-id :allocation :class :initform :widget_drag)
   ;; instance variables
   (installed-object :accessor installed-object :initform nil))
  (:documentation "a mouse-down-command to move a template"))

(defun make-widget-dragger (temp x y)
  (make-drag-command (document temp) (drag-widget temp) x y 
                     (list (target-view temp))
                           :x-off 2 :y-off 2
                           :cursor :left-ptr
                           :shell-to-move (drag-shell temp)
                           :transfer-value temp
			   :class 'widget-dragger))

;(defmethod point-visible ((view view) x y
;			  &key (ask-for-work-area-pos t)
;		               (ask-for-clipper-size  t))
; (when ask-for-work-area-pos
;      (update-slots view))
;  (if (scroller view)
;      (point-visible (scroller view) x y
;                     :ask-for-work-area-pos nil
;                     :ask-for-clipper-size ask-for-clipper-size)
;      (and (>= x 0)
;           (< x (width view))
;           (>= y 0)
;           (< y (height view)))))

(defmethod doit ((cmd widget-dragger))
  "create a new widget info in the destination view"
  (let ((temp (transfer-value cmd)))
    ;;(multiple-value-bind (root-x root-y) (root-coordinates (view cmd)))
    (setf (installed-object cmd)
          (funcall (info-constructor temp)
                   (main-info (document cmd))))
    (loop for resource in (append (positional-resources (info temp))
                                  (key-resources (info temp))) do
        (setf (resource-value resource (installed-object cmd))
              (resource-value resource (info temp))))
    (setf (x-pos (installed-object cmd))
          ;(+ (global-x-off temp) (last-x cmd) (- root-x))
          (target-x cmd))
    (setf (y-pos (installed-object cmd))
          ;(+ (global-y-off temp) (last-y cmd) (- root-y))
          (target-y cmd))
    (setf (width (installed-object cmd))
          (width (drag-widget temp)))
    (setf (height (installed-object cmd))
          (height (drag-widget temp)))
    (deselect-all (view cmd))
    (install-plate (installed-object cmd) (view cmd))
    ;(adapt-to-plate-size (plate (installed-object cmd))) ;; wg. RC
  ))

(defmethod undoit ((cmd widget-dragger))
  "remove the newly created widget info"
  (destroy (gina-widget (plate (installed-object cmd))))
  (deinstall (plate (installed-object cmd)))
  (remove-child (parent-info (installed-object cmd)) (installed-object cmd)))

(defmethod redoit ((cmd widget-dragger))
  (let ((info (installed-object cmd)))
    (add-child (main-info (document cmd)) info)
    (install (plate info) (view cmd)
             (x-pos (plate info)) (y-pos (plate info)))
    (install-widget (plate info))
    (adapt-to-plate-pos (plate info))
    (adapt-to-plate-size (plate info))
    (manage (gina-widget (plate info)))
    ;(adapt-to-plate-size (plate info)) ;; wg. RC
  ))






