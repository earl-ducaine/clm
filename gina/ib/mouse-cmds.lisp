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

(setq *sccs-id* "@(#)mouse-cmds.lisp	1.12	11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class widget-selector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass widget-selector (mouse-down-command)
  (;; overrides
   (name       :initform "Select Widget" :allocation :class)
   (undoable      :accessor undoable       :initform nil :allocation :class)
   (causes-change :accessor causes-change  :initform nil :allocation :class)
   ;; new
   (extend        :accessor extend         :initarg :extend))
  (:documentation "a mouse-down-command to select one or more widgets"))

(defun make-widget-selector (view x y extend)
  (make-mouse-down-command (document view) view x y
			   :cursor :arrow     
			   :class 'widget-selector
                           :initargs `(:extend ,extend)))

(defmethod constrain-mouse ((cmd widget-selector) x y)
  "modify x and y position of mouse pointer"
  (with-slots (start-x start-y) cmd
    (setq x (max x (1+ start-x)))
    (setq y (max y (1+ start-y))))
  (values x y))

(defmethod draw-feedback ((cmd widget-selector) x y &key clear)
  "draw rectangular feedback"
  (declare (ignore clear))
  (with-slots (start-x start-y view) cmd
    (draw-rectangle view start-x start-y (- x start-x) (- y start-y))))

(defmethod doit ((cmd widget-selector))
  "create the specified widget on top of a widget-plate"
  (with-slots (start-x start-y last-x last-y view extend) cmd
    ;; look for widget-plates inside the selection rectangle
    (loop for plate in (view-objects view)
	  do (if (gina::inside-rectangle plate
		     start-x start-y (- last-x start-x) (- last-y start-y))
                 (setf (mouse-sensitive plate)
                       (not (and (eql extend :toggle) (mouse-sensitive plate))))
                 (when (eql extend :select)
                       (setf (mouse-sensitive plate) nil)))
	     (force-redraw plate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class widget-resizer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass widget-resizer (mouse-down-command)
  (;; overrides
   (name       :initform "Resize Widget"  :allocation :class)
   (hysteresis :initform 5                :allocation :class)
   ;; instance-parameters
   (widget-plate :accessor widget-plate :initarg :widget-plate)
   (old-width  :accessor old-width  :initarg :old-width)
   (old-height :accessor old-height :initarg :old-height)
   ;; instance-variables
   (new-width :accessor new-width)
   (new-height :accessor new-height))
  (:documentation "a mouse-down-command to resize a widget-plate"))

(defun make-widget-resizer (widget-plate x y)
  "create a new mouse-down-command object to resize the specified widget-plate"
  (with-slots 
   (parent-view x-pos y-pos width height mouse-sensitive) widget-plate
   (setq mouse-sensitive nil) (force-redraw widget-plate) 
   (make-mouse-down-command
    (document parent-view) parent-view (+ x-pos x) (+ y-pos y)
    :class 'widget-resizer
    :initargs (list :widget-plate widget-plate :old-width width 
		    :old-height height))))

(defmethod constrain-mouse ((cmd widget-resizer) x y)
  "modify x and y position of mouse pointer"
  (with-slots (widget-plate) cmd
    (setq x (max x (+ 13 (x-pos widget-plate)))) ;; minimum size 4x4
    (setq y (max y (+ 13 (y-pos widget-plate)))))
  (values x y))

(defmethod draw-feedback ((cmd widget-resizer) x y &key clear)
  "no feedback"
  (declare (ignore clear x y)))

(defmethod track-mouse ((cmd widget-resizer) x y 
                        &key (started nil) (finished nil))
  "process the possibly modified coordinates"
  (declare (ignore started finished))
  (with-slots (widget-plate old-width old-height new-width new-height 
              start-x start-y) cmd
    (setq new-width  (+ old-width  (- x start-x))
	  new-height (+ old-height (- y start-y)))
    ;(resize (gina-widget widget-plate) (- new-width 9) (- new-height 9))
    ;; does not work for row-column
    ;; hack: set plate-size first without invalidate
    (setf (width widget-plate) new-width)
    (setf (height widget-plate) new-height)
    (adapt-to-plate-size widget-plate)
    ))

(defmethod doit ((cmd widget-resizer))
  "resize widget and widget-plate"
  (with-slots (widget-plate new-width new-height) cmd
    (setf (mouse-sensitive widget-plate) t)
    (resize widget-plate new-width new-height)
    (adapt-to-plate-size widget-plate)
    ))

(defmethod undoit ((cmd widget-resizer))
  "restore old size of widget and widget-plate"
  (with-slots (widget-plate old-width old-height) cmd
    (setf (mouse-sensitive widget-plate) t)
    (resize widget-plate old-width old-height)
    (adapt-to-plate-size widget-plate)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class widget-mover
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass widget-mover (mouse-down-command)
  (;; overrides
   (name       :initform "Move Widgets" :allocation :class)
   (hysteresis :initform 5                :allocation :class)
   ;; instance-parameters
   (widget-plates :accessor widget-plates :initarg :widget-plates)
  )
  (:documentation "a mouse-down-command to move a widget-plate around"))

(defun make-widget-mover (widget-plate x y)
  "create a new mouse-down-command object to move the specified widget-plate"
  (with-slots (parent-view x-pos y-pos mouse-sensitive) widget-plate
    (make-mouse-down-command
      (document parent-view) parent-view (+ x-pos x) (+ y-pos y)
      :class 'widget-mover
      :initargs (list :widget-plates (selected-objects parent-view) 
  ))))

(defmethod draw-feedback ((cmd widget-mover) x y &key clear)
  "draw feedback while moving widget around"
  (declare (ignore clear))
  (with-slots (widget-plates start-x start-y view) cmd
    (loop for child in widget-plates do
      (draw-rectangle view (+ (x-pos child) x (- start-x) 5)
                             (+ (y-pos child) y (- start-y) 5)
                             (- (width child) 10) (- (height child) 10)))
  ))

;(defmethod track-mouse ((cmd widget-mover) x y 
;                        &key (started nil) (finished nil))
;  "process the possibly modified coordinates"
;  (declare (ignore started finished))
;  (with-slots (widget-plate old-x old-y new-x new-y start-x start-y) cmd
;    (loop for child in widget-plate do
;      (incf (x-pos child) (- x (last-x cmd)))
;      (incf (y-pos child) (- y (last-y cmd)))
;      (adapt-to-plate-pos child))
;    ))

(defmethod doit ((cmd widget-mover))
  "move widget and widget-plate to new pos"
  (with-slots (widget-plates new-x new-y) cmd
    (loop for child in widget-plates do
      (move child (+ (x-pos child) (last-x cmd) (- (start-x cmd)))
                  (+ (y-pos child) (last-y cmd) (- (start-y cmd))))
      (adapt-to-plate-pos child))
    ))

(defmethod undoit ((cmd widget-mover))
  "move back widget and widget-plate"
  (with-slots (widget-plates old-x old-y) cmd
    (loop for child in widget-plates do
      (move child (- (x-pos child) (last-x cmd) (- (start-x cmd)))
                  (- (y-pos child) (last-y cmd) (- (start-y cmd))))
      (adapt-to-plate-pos child))
    ))

