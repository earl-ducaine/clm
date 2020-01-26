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

(setq *sccs-id* "@(#)plates.lisp	1.13	11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class widget-plate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A widget-plate is a view object that maintains the border around a
;;; widget. There is exactly one plate for each info. Plates for objects
;;; contained in managers are deinstalled.

'(defclass widget-plate (view-object)
  (;; overrides in the future
   ;(facilities    :initform :resizable    :allocation :class)
   ;(outlines      :initform nil           :allocation :class)
   ;; new slots
   (info          :accessor info          :initarg :info)
   (gina-widget   :accessor gina-widget   :initform nil)
   (symbolic-view :accessor symbolic-view :initform nil)
   (is-symbolic   :accessor is-symbolic   :initform nil)
   (click-time    :accessor click-time    :initform 0))
  (:documentation ""))

(defun make-widget-plate (info &aux new-plate)
  "create a new plate corresponding to an info"
  (setq new-plate
        (make-view-object (if (width info) (+ (width info) 9) 10)
                          (if (height info) (+ (height info) 9) 10)
                          :class 'widget-plate
                          :initargs (list :info info)))
  (when (x-pos info) (setf (x-pos new-plate) (+ (x-pos info) 5)))
  (when (y-pos info) (setf (y-pos new-plate) (+ (y-pos info) 5)))
  (setf (plate info) new-plate)
  new-plate)  

;;;;;;;;;;;;;;;; create and sync widget belonging to info ;;;;;;;;;;;;;;;;;;;;

(defmethod create-tree ((plate widget-plate))
  "reinstall the widget of a plate and all its children after changes"
  (install-widget plate)
  (loop for child in (children (info plate)) do
        (create-tree (if (plate child) 
                         (plate child) 
                         (make-widget-plate child))))
  (set-constraints plate)
  (when (parent-view plate)
      (adapt-to-plate-pos plate))
  ;(adapt-to-info-size plate)
  (manage (gina-widget plate))
  (update-slots (gina-widget plate))
  (when (and (parent-view plate)
             (width (info plate)) (height (info plate))
             (or (/= (width (gina-widget plate)) (width (info plate)))
                 (/= (height (gina-widget plate)) (height (info plate)))))
    (adapt-to-info-size plate))
  )

(defmethod set-constraints ((plate widget-plate))
  "set constraints when all the children have been created"
  (when (is-constraint-widget (info plate))
      (loop for child in (children (info plate))
            when (constraints child)
            do (update-constraints (constraints child) (plate child)))))

(defmethod install-plate ((info widget-info) view 
                          &key (x-pos (- (x-pos info) 5))
                               (y-pos (- (y-pos info) 5))
                          &aux new-plate)
  "create a new widget-plate and install it directly"
  (setq new-plate (make-widget-plate info))
  (install new-plate view x-pos y-pos)
  (create-tree new-plate)
  new-plate)
            
(defmethod handle-reparent ((plate widget-plate) &aux old-gina-widget)
  "destroy widget hierarchy and recreate with new parent"
  (setf old-gina-widget (gina-widget plate))
  (install-widget plate)
  (loop for child in (children (info plate)) do
	(handle-reparent (plate child)))
  (when (is-constraint-widget (info plate))
      (loop for child in (children (info plate))
            when (constraints child)
            do (update-constraints (constraints child) (plate child))))
  (adapt-to-info-size plate)
  (manage (gina-widget plate))
  (destroy old-gina-widget)) ;; If bug in destroy-widget -> unmanage

(defmethod install-widget ((plate widget-plate))
  "create the widget belonging to the plate"
  (setf (gina-widget plate)
	(apply (create-function (info plate)) 
	       (append (list (install-parent plate))
		       (find-positional-values (info plate))
		       (find-key-values (info plate))
		       (list :motif-resources
			     (find-motif-values (info plate)))
                       (list :managed nil)
                       (special-params-for-install (info plate)))))
  (loop for widget in (cons (gina-widget plate)
                            (all-widgets (info plate) (gina-widget plate)))
        when widget
        do (gina::add-event-handler widget :button-press-mask
                     (make-callback #'resource-edit-click plate)))
  )

(defmethod install-parent ((plate widget-plate))
  "the parent widget to install under"
  (let ((p-info (parent-info (info plate))))
    (if (eql p-info (main-info (document p-info)))
        (parent-view plate)
        (gina-widget (plate p-info)))))

(defmethod adapt-to-widget-size ((plate widget-plate))
  "change plate size so that widget is surrounded"
  (with-slots (gina-widget) plate
    (update-slots gina-widget) ;; get up-to-date size
    ;; adapt size of plate
    (resize plate (+ (width gina-widget) 9) (+ (height gina-widget) 9))
    (setf (width (info plate)) (width gina-widget))
    (setf (height (info plate)) (height gina-widget)))
  (resize-symbolic-views plate))

(defmethod adapt-to-info-size ((plate widget-plate))
  "change widget size to correspond to new info size"
  (let* ((info (info plate))
         (new-width (width info))
         (new-height (height info)))
    (when (and new-width new-height) ;; if initialized
      (resize plate (+ new-width 9) (+ new-height 9))
      (if (is-row-column-or-subclass info)
          ;; hard way for row-column
         (xtk::resize-widget (group-widget-id (gina-widget plate)) 
                              new-width new-height 0)
         (resize (gina-widget plate) new-width new-height)))))

(defmethod adapt-to-plate-size ((plate widget-plate))
  "change widget size to correspond to new plate size"
  (let* ((info (info plate)))
    (setf (width info) (- (width plate) 9))
    (setf (height info) (- (height plate) 9))
    (if (is-row-column-or-subclass info)
        ;; hard way for row-column
       (xtk::resize-widget (group-widget-id (gina-widget plate)) 
                            (width info) (height info) 0)
       (resize (gina-widget plate) (width info) (height info)))
    (resize-symbolic-views plate)))

(defmethod resize-symbolic-views ((plate widget-plate))
  (when (is-symbolic plate)
     (update-slots (gina-widget plate))
     (resize (symbolic-view plate) 
	     (width (gina-widget plate)) (height (gina-widget plate))) 
     (update-geometry (symbolic-view plate)))
  (loop for child in (children (info plate))
        do (resize-symbolic-views (plate child))))

;(defmethod adapt-to-widget-pos ((plate widget-plate))
;  "change plate pos to correspond to new widget pos"
;  (with-slots (gina-widget) plate
;    (update-slots gina-widget)
;    (move plate (- (x-pos gina-widget) 5) (- (y-pos gina-widget) 5))
;    (setf (x-pos (info plate)) (x-pos gina-widget))
;    (setf (y-pos (info plate)) (y-pos gina-widget))))

;(defmethod adapt-to-info-pos ((plate widget-plate))
;  "change widget pos to correspond to new plate pos"
;  (move plate (- (x-pos (info plate)) 5) (- (y-pos (info plate)) 5))
;  (with-slots (gina-widget x-pos y-pos) plate
;    (move gina-widget (+ x-pos 5) (+ y-pos 5))))

(defmethod adapt-to-plate-pos ((plate widget-plate))
  "change widget pos to correspond to new plate pos"
  (setf (x-pos (info plate)) (+ (x-pos plate) 5))
  (setf (y-pos (info plate)) (+ (y-pos plate) 5))
  (with-slots (gina-widget x-pos y-pos) plate
    (move gina-widget (+ x-pos 5) (+ y-pos 5)))
  (move-symbolic-views plate 0 0))

(defmethod move-symbolic-views ((plate widget-plate) parent-x parent-y)
  (update-slots (gina-widget plate))
  (incf parent-x (x-pos (gina-widget plate)))
  (incf parent-y (y-pos (gina-widget plate)))
  (when (is-symbolic plate)
      (move (symbolic-view plate) parent-x parent-y)) 
  (loop for child in (children (info plate))
        do (move-symbolic-views (plate child) parent-x parent-y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; interactive behaviour ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod draw ((widget-plate widget-plate) count x y width height)
  "draw widget-plate into the view"
  (declare (ignore x y width height)) ;; draw completly
  (with-slots 
   (x-pos y-pos width height parent-view mouse-sensitive) widget-plate
   (when (zerop count) ;; Ignore all but the last exposure event
     (when mouse-sensitive
       (progn
	 (xlib:with-gcontext ((gcontext parent-view) :line-style :dash)
	    (draw-rectangle parent-view x-pos y-pos width height))
         (draw-rectangle parent-view (+ x-pos 4) (+ y-pos 4)
                           (- width 8) (- height 8))
	 (draw-rectangle parent-view 
			   (+ x-pos width -5) (+ y-pos height -5) 6 6 t))
       ;(draw-rectangle parent-view (+ x-pos 5) (+ y-pos 5)
       ;                (- width 10) (- height 10))
  ))))

(defmethod select-plate ((plate widget-plate) code)
  "handle selection action depending on button code"
  (case code
    ((:select) (unless (mouse-sensitive plate)
                 (loop for child in (view-objects (parent-view plate))
                   do (setf (mouse-sensitive child) (eql child plate)))
                 (force-redraw (parent-view plate))))
    ((:extend) (setf (mouse-sensitive plate) t)
               (force-redraw plate))
    ((:toggle) (setf (mouse-sensitive plate) (not (mouse-sensitive plate)))
               (force-redraw plate))))

(defmethod button-press ((plate widget-plate) code repetition x y)
  "react to button-press event in the widget-plate"
  (if (> repetition 1)
      (resource-dialog (info plate))
      (with-slots (width height mouse-sensitive) plate
        (select-plate plate code)
        (when (eql code :select)
            (if (and (> x (- width 5)) (> y (- height 5)))
	        (make-widget-resizer plate x y) 
	        (make-widget-mover plate x y))))))

(defmethod resource-edit-click ((plate widget-plate) &rest parm-list)
  ;; parm-list: type x y root-x root-y state button
  ;;(format t "~s~%" parm-list)
  ;;(format t "State: ~s Button: ~s~%" (nth 5 parm-list) (nth 6 parm-list))

  (let ((visible-plate (plate (find-toplevel-object (info plate)))))
    (when (eql (seventh parm-list) 1)
      (when (= (sixth parm-list) 0) (select-plate visible-plate :select))
      (when (= (sixth parm-list) 1) (select-plate visible-plate :extend))
      (when (= (sixth parm-list) 4) (select-plate visible-plate :toggle)))
    ;
    (when (and (= (seventh parm-list) 2)
             (= (sixth parm-list) 0))  (select-plate visible-plate :extend))
    ;
    ;(when (and (= 4 (sixth parm-list)) (= 3 (seventh parm-list))) 
	;; Control-right button
    ;  (resource-dialog (info plate)))
   ))

;; now testing:
;(defmethod reconfigured-by-user ((plate widget-plate) moved resized)
;  (when moved (adapt-to-plate-pos plate))
;  (when resized (adapt-to-plate-size plate)))

;(defmethod double-clicked ((plate widget-plate) code repetition x y)
;  (declare (ignore code x y))
;  (when (= repetition 2)
;    (resource-dialog (info plate))))

;;;;;;;;;;;;;;;;;;;;;;;;;; symbolic view ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; currently experimental: every plate may have a symbolic view that
;;; can be switched over the normal widget appearance

(defmethod create-symbolic-view ((plate widget-plate))
  "create the form view"
  (setf (symbolic-view plate) 
        (make-form-view (main-view (document (info plate))) (info plate))))

(defmethod fill-symbolic-view ((plate widget-plate))
  "update the symbolic view"
  (fill-form-view (symbolic-view plate)))

(defmethod switch-symbolic-state ((plate widget-plate) new-state)
  "switch between symbolic and widget view of a plate"
  (unless (eql new-state (is-symbolic plate))
    (unless (symbolic-view plate)
        (create-symbolic-view plate)
        (xtk:unmap-widgets (widget-id (symbolic-view plate))))
    (setf (is-symbolic plate) new-state)
    (if new-state
        (progn
          (xtk:unmap-widgets (widget-id (gina-widget plate)))
          (update-slots (gina-widget plate))
          (move (symbolic-view plate)
                (x-pos-in-view (gina-widget plate) 
			       (main-view (document (info plate))))
                (y-pos-in-view (gina-widget plate)
			       (main-view (document (info plate)))))
          (resize (symbolic-view plate) 
                  (width (gina-widget plate)) (height (gina-widget plate)))
          (fill-symbolic-view plate)
          (xtk:map-widgets (widget-id (symbolic-view plate)))
          (unless (mouse-sensitive plate)
            (setf (mouse-sensitive plate) t)
            (force-redraw plate)))
        (progn
          (xtk:unmap-widgets (widget-id (symbolic-view plate)))
          (xtk:map-widgets (widget-id (gina-widget plate)))))))
