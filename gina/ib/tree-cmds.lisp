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

(setq *sccs-id* "@(#)tree-cmds.lisp	1.12	11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class one-element-group-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass one-element-group-command (command)
  ((selected-plates :accessor selected-plates :initarg :selected-plates)
   (group-plates    :accessor group-plates    :initform nil))
  (:documentation "a command to group with one-element managers"))

(defmethod executable ((cmd one-element-group-command))
  (selected-plates cmd))

(defmethod create-info ((cmd one-element-group-command) parent-info)
  (declare (ignore parent-info))
  )

(defmethod destroy ((plate widget-plate))
  (when (parent-view plate) (deinstall plate))
  (destroy (gina-widget plate)))

(defmethod reparent ((plate widget-plate) new-info)
  (destroy plate)
  (remove-child (parent-info (info plate)) (info plate))
  (add-child new-info (info plate)))

(defmethod build-tree ((cmd one-element-group-command) new-info x-pos y-pos
                       &aux new-plate)
  (setq new-plate 
        (install-plate new-info (view cmd) :x-pos x-pos :y-pos y-pos))
  (adapt-to-widget-size new-plate)
  new-plate)

(defmethod rebuild-tree ((cmd one-element-group-command) new-plate)
  (install new-plate (view cmd) (x-pos new-plate) (y-pos new-plate))
  (create-tree new-plate))

(defmethod doit ((cmd one-element-group-command) &aux new-info)
  (setf (group-plates cmd)
        (loop for plate in (selected-plates cmd)
              do (setq new-info (create-info cmd (main-info (document cmd))))
               ;  (setf (width new-info) 10 (height new-info) 10)
                 (reparent plate new-info)
              collect (build-tree cmd new-info (x-pos plate) (y-pos plate)))))

(defmethod undoit ((cmd one-element-group-command))
  (loop for old-plate in (selected-plates cmd)
        for new-plate in (group-plates cmd)
        do (reparent old-plate (main-info (document cmd)))
           (remove-child (main-info (document cmd)) (info new-plate))
           (destroy new-plate)
           (rebuild-tree cmd old-plate)))

(defmethod redoit ((cmd one-element-group-command))
  (loop for old-plate in (selected-plates cmd)
        for new-plate in (group-plates cmd)
        do (add-child (main-info (document cmd)) (info new-plate))
           (reparent old-plate (info new-plate))
           (rebuild-tree cmd new-plate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class frame-group-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass frame-group-command (one-element-group-command)
  (;;overrides
   (name         :initform "Make Frame"      :allocation :class))
  (:documentation "a command to group with a frame"))

(defun make-frame-group-command (view)
  (let ((selected-plates (selected-objects view)))
    (make-command (document view) :view view :class 'frame-group-command
                  :initargs (list :selected-plates selected-plates))))

(defmethod create-info ((cmd frame-group-command) parent-info)
  (make-frame-info parent-info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class scroller-group-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass scroller-group-command (one-element-group-command)
  (;;overrides
   (name         :initform "Make Scroller"      :allocation :class))
  (:documentation "a command to group with a frame"))

(defun make-scroller-group-command (view)
  (let ((selected-plates (selected-objects view)))
    (make-command (document view) :view view :class 'scroller-group-command
                  :initargs (list :selected-plates selected-plates))))

(defmethod create-info ((cmd scroller-group-command) parent-info)
  (make-scroller-info parent-info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class n-element-group-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass n-element-group-command (one-element-group-command)
  ()
  (:documentation "a command to group with n-element managers"))

(defmethod min-pos ((cmd n-element-group-command))
  (values
      (loop for plate in (selected-plates cmd)
            minimize (x-pos plate))
      (loop for plate in (selected-plates cmd)
            minimize (y-pos plate))))

(defmethod determine-constraints ((cmd n-element-group-command) plate)
  (declare (ignore plate))
  )

(defmethod determine-initial-size ((cmd n-element-group-command) new-info)
  (declare (ignore new-info))
  ;(setf (width new-info) 10 (height new-info) 10)
  )

(defmethod doit ((cmd n-element-group-command) &aux new-info min-x min-y)
  (multiple-value-setq (min-x min-y) (min-pos cmd))
  (setq new-info (create-info cmd (main-info (document cmd))))
  (determine-initial-size cmd new-info)
  (loop for plate in (selected-plates cmd)
        do (reparent plate new-info)
           (determine-constraints cmd plate))
  (setf (group-plates cmd) (build-tree cmd new-info min-x min-y)))

(defmethod undoit ((cmd n-element-group-command))
  (loop for old-plate in (selected-plates cmd)
        do (reparent old-plate (main-info (document cmd)))
           (rebuild-tree cmd old-plate))
  (remove-child (main-info (document cmd)) (info (group-plates cmd)))
  (destroy (group-plates cmd)))

(defmethod redoit ((cmd n-element-group-command))
  (add-child (main-info (document cmd)) (info (group-plates cmd)))
  (loop for old-plate in (selected-plates cmd)
        do (reparent old-plate (info (group-plates cmd))))
  (rebuild-tree cmd (group-plates cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class row-column-group-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass row-column-group-command (n-element-group-command)
  (;;overrides
   (name         :initform "Make Row-Column"      :allocation :class)
   ;;new
   (orientation  :accessor orientation            :initarg :orientation))
  (:documentation "a command to group with a row-column"))

(defun get-orientation (plates)
  (when plates
    (let* ((min-x (x-pos (first plates)))
           (max-x min-x)
           (min-y (y-pos (first plates)))
           (max-y min-y))
        (loop for plate in plates
	      when (< (y-pos plate) min-y) do (setf min-y (y-pos plate))
	      when (> (y-pos plate) max-y) do (setf max-y (y-pos plate))
	      when (< (x-pos plate) min-x) do (setf min-x (x-pos plate))
	      when (> (x-pos plate) max-x) do (setf max-x (x-pos plate)))
        (if (< (- max-x min-x) (- max-y min-y)) :vertical :horizontal))))

(defun make-row-column-group-command (view)
  (let* ((orientation (get-orientation (selected-objects view)))
         (selected-plates (sort (selected-objects view)
		                (if (eql orientation :vertical)
		      #'(lambda (p-1 p-2) (< (y-pos p-1) (y-pos p-2)))
		      #'(lambda (p-1 p-2) (< (x-pos p-1) (x-pos p-2)))))))
    (make-command (document view) :view view :class 'row-column-group-command
                  :initargs (list :selected-plates selected-plates
                                  :orientation orientation))))

(defmethod create-info ((cmd row-column-group-command) parent-info 
                        &aux new-info)
  (setq new-info (make-row-column-info parent-info))
  ;;set orientation
  (setf (resource-value (find-resource new-info :orientation) new-info)
        (orientation cmd))
  new-info)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class form-group-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass form-group-command (n-element-group-command)
  (;;overrides
   (name         :initform "Make Form"      :allocation :class)
   ;;new slots
   (child-rows    :accessor child-rows)
   (child-columns :accessor child-columns))
  (:documentation "a command to group with a form"))

(defun make-form-group-command (view)
  (let ((selected-plates (selected-objects view)))
    (make-command (document view) :view view :class 'form-group-command
                  :initargs (list :selected-plates selected-plates))))

(defmethod create-info ((cmd form-group-command) parent-info)
  (make-form-info parent-info))

(defmethod determine-initial-size ((cmd form-group-command) new-info)
  (declare (ignore new-info))
  (multiple-value-bind (rows columns)
       (calculate-rows-and-columns 
             (loop for plate in (selected-plates cmd) collect (info plate)))
     (multiple-value-bind (new-rows new-columns)
               (determine-row-column-layout rows columns)
         (setf (child-rows cmd) new-rows)
         (setf (child-columns cmd) new-columns)))
  ;(let ((min-x 1000) (min-y 1000) (max-x 0) (max-y 0))
  ;  (loop for plate in (selected-plates cmd)
  ;      do (setq min-x (min (x-pos (info plate)) min-x))
  ;         (setq min-y (min (y-pos (info plate)) min-y))
  ;         (setq max-x (max (+ (x-pos (info plate)) (width (info plate))) 
  ;                          max-x))
  ;         (setq max-y (max (+ (y-pos (info plate)) (height (info plate)))
  ;                          max-y)))
  ;  (setf (width new-info) (- max-x min-x))
  ;  (setf (height new-info) (- max-y min-y)))
  )

(defmethod determine-constraints ((cmd form-group-command) plate)
  (let ((all-infos (loop for plate in (selected-plates cmd) 
                         collect (info plate))))
    (multiple-value-bind (min-x min-y) (min-pos cmd)
      (new-constraints (info plate)
       (calculate-matrix-form-constraints (info plate) (child-rows cmd)
                   (child-columns cmd) min-x min-y)
;       (calculate-initial-form-constraints (info plate) all-infos)
       all-infos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class paned-window-group-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass paned-window-group-command (n-element-group-command)
  (;;overrides
   (name         :initform "Make Paned-Window"      :allocation :class))
  (:documentation "a command to group with a paned-window"))

(defun make-paned-window-group-command (view)
  (let* ((selected-plates (sort (selected-objects view)
		            #'(lambda (p-1 p-2) (< (y-pos p-1) (y-pos p-2))))))
    (make-command (document view) :view view :class 'paned-window-group-command
                  :initargs (list :selected-plates selected-plates))))

(defmethod create-info ((cmd paned-window-group-command) parent-info)
  (make-paned-window-info parent-info))

(defmethod determine-constraints ((cmd paned-window-group-command) plate)
  (new-constraints (info plate) (make-pane-constraints) 
      (loop for plate in (selected-plates cmd) collect (info plate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class ungroup-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass ungroup-command (one-element-group-command)
  (;; overrides
   (name :initform "Ungroup Widgets" :allocation :class))
  (:documentation "a command to ungroup a compound widget"))

(defun make-ungroup-command (view)
  (let ((objs (loop for view-object in (view-objects view)
	            when (and (mouse-sensitive view-object)
                              (member (widget-class (info view-object))
		                '(row-column frame scroller paned-window form)))
                    collect view-object)))
    (when objs
        (make-command (document view) :view view
		:class 'ungroup-command
		:initargs (list :selected-plates objs)))))

(defmethod doit ((cmd ungroup-command))
  (setf (group-plates cmd)
        (loop for plate in (selected-plates cmd)
              collect 
                  (loop for info in (children (info plate))
                        do (reparent (plate info) (main-info (document cmd)))
                           (rebuild-tree cmd (plate info))
                        collect (plate info))
              do (remove-child (main-info (document cmd)) (info plate))
                 (destroy plate))))

(defmethod undoit ((cmd ungroup-command))
  (loop for group in (group-plates cmd)
        for plate in (selected-plates cmd)
        do (add-child (main-info (document cmd)) (info plate))
           (loop for old-plate in group
                 do (destroy old-plate)
                    (reparent old-plate (info plate)))
           (rebuild-tree cmd plate)))

(defmethod redoit ((cmd ungroup-command))
  (loop for group in (group-plates cmd)
        for plate in (selected-plates cmd)
        do (loop for old-plate in group
                 do (reparent old-plate (main-info (document cmd)))
                    (rebuild-tree cmd old-plate))
           (remove-child (main-info (document cmd)) (info plate))
           (destroy plate)))


