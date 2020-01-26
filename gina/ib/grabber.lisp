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

(setq *sccs-id* "@(#)grabber.lisp	1.14	11/8/93")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class grab-info-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass grab-info-command (drag-command)
 (;;overrides
  (name    :initform "Grab Info"  :allocation :class)
  (call-doit :allocation :class :initform nil)
  ;; new slots
  (animate :accessor animate :initform t :allocation :class)
  (label   :accessor label)
  (info    :accessor info   :initform nil)
  ))

(defun make-grab-info-command (document widget start-x start-y
                               &key (class 'grab-info-command)
                                    (initargs nil)
                               &aux cmd)
  (setq cmd 
        (make-drag-command document widget start-x start-y 
                           (list (list (main-view document) nil))
                                 :cursor (cursor widget)
                                 :class class :initargs initargs))
  (setf (label cmd) widget)
  cmd)

(defmethod track-target ((cmd grab-info-command) view view-x view-y value)
  "determine info that is hit"
  (declare (ignore value))
  (if view
      (progn
        (setf (info cmd) 
              (find-info-for-point (main-info (document cmd))
                                    view-x view-y))
        (when (eql (info cmd) (main-info (document cmd)))
             (setf (info cmd) nil)))
      (setf (info cmd) nil)))

(defmethod draw-target-feedback ((cmd grab-info-command) view view-x view-y
                                 value &key (clear nil))
  (declare (ignore view view-x view-y value clear))
  (when (info cmd)
      (let* ((w (gina-widget (plate (info cmd))))
             (parent-view (main-view (document cmd)))
             (wx (x-pos-in-view w parent-view))
             (wy (y-pos-in-view w parent-view)))
        (xlib:with-gcontext ((gcontext parent-view) 
                              :function boole-xor
			      :foreground (xor-foreground parent-view)
                              :line-style :dash
                              :subwindow-mode :include-inferiors)
          (draw-rectangle parent-view wx wy (width w) (height w))
          (draw-rectangle parent-view (1+ wx) (1+ wy) 
			  (- (width w) 2) (- (height w) 2))))))

(defun x-pos-in-view (w view)
  (let ((p (parent w)))
     (if (eql p view)
         (x-pos w)
         (+ (x-pos w) (x-pos-in-view p view)))))

(defun y-pos-in-view (w view)
  (let ((p (parent w)))
     (if (eql p view)
         (y-pos w)
         (+ (y-pos w) (y-pos-in-view p view)))))
         
(defmethod find-info-for-point ((info widget-info) x y 
                                &aux child-found its-widget)
  (loop for child in (children info)
        for widget = (gina-widget (plate child))
        when (point-inside widget x y)
        do (setq child-found child 
                 its-widget widget))
  (if child-found
    (find-info-for-point child-found 
                         (- x (x-pos its-widget)) (- y (y-pos its-widget)))
    info))

(defmethod executable ((cmd grab-info-command))
  (info cmd))

(defmethod not-submitted ((cmd grab-info-command))
  (declare (special *display*))
  (when (and (executable cmd) (mouse-already-moved cmd) (current-target cmd))
    (xlib:display-force-output *display*)
    (if (and (animate cmd) (info cmd))
        (make-animator cmd)
        (submit cmd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class animator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass animator ()
   ((cmd            :accessor cmd      :initarg :cmd)
    (ashell  :accessor ashell  :initform nil)   ;; the shell to animate
    (timer   :accessor timer   :initform nil)   ;; timer for animation
    (bcount  :accessor bcount  :initform nil)
    (x-inc   :accessor x-inc   :initform nil)
    (y-inc   :accessor y-inc   :initform nil)
    (x-root  :accessor x-root)
    (y-root  :accessor y-root)))

(defun make-animator (cmd)
  (let ((animator (make-instance 'animator :cmd cmd)))
    (setf (bcount animator) 10)
    (setf (x-root animator) (- (+ (global-x-off cmd) (source-x cmd))  8))
    (setf (y-root animator) (- (+ (global-y-off cmd) (source-y cmd)) 8))
    (multiple-value-bind (tx ty) (root-coordinates (current-target cmd)
                                          :x (target-x cmd) :y (target-y cmd))
      (setf (x-inc animator) (round (/ (- (global-x-off cmd) tx -8) 10)))
      (setf (y-inc animator) (round (/ (- (global-y-off cmd) ty -8) 10))))
    (setf (ashell animator) (drag-shell (label cmd)))
    (move (ashell animator) (- (x-root animator) (* (x-inc animator) 10))
	                    (- (y-root animator) (* (y-inc animator) 10)))
    (pop-up (ashell animator))
    (setf (timer animator)
          (xtk:create-timer 25 'redraw-grabber animator :enabled t
                               :active-in-handler t))
    animator))

(defun redraw-grabber (timer animator)
  (decf (bcount animator))
  (when (< (bcount animator) 0)
    (xtk:stop-timer timer)
    (pop-down (ashell animator))
    (xtk:destroy-timer timer)
    (submit (cmd animator)))
  (when (>= (bcount animator) 0)
    (xtk::move-widget (widget-id (ashell animator))
          (- (x-root animator) (* (x-inc animator) (bcount animator)))
          (- (y-root animator) (* (y-inc animator) (bcount animator))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class grab-resources-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass grab-resources-command (grab-info-command)
 (;;overrides
  (name    :initform "Grab Resources"  :allocation :class)
  (causes-change  :initform nil :allocation :class)
  (undoable       :initform nil :allocation :class)
  (animate        :initform nil :allocation :class)
  ))

(defun make-grab-resources-command (document widget start-x start-y
                               &key (class 'grab-resources-command)
                                    (initargs nil)
                               &aux cmd)
  (setq cmd (make-grab-info-command document widget start-x start-y
                               :class class :initargs initargs))
  cmd)

(defmethod doit ((cmd grab-resources-command))
  (resource-dialog (info cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; clipboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *clipboard-type* nil)
(defvar *clipboard-text* "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class delete-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass delete-command (grab-info-command)
 (;;overrides
  (name    :initform "Cut Widget"  :allocation :class)
  ;;new slots
  (old-parent   :accessor old-parent)
  (old-position :accessor old-position)
  ))

(defun make-delete-command (document widget start-x start-y
                               &key (class 'delete-command)
                                    (initargs nil)
                               &aux cmd)
  (setq cmd (make-grab-info-command document widget start-x start-y
                               :class class :initargs initargs))
  cmd)

(defmethod track-target :after ((cmd delete-command) view
                               target-x target-y value)
  (declare (ignore view target-x target-y value))
  (declare (special *display*))
  (when (info cmd)
    (loop while (and (parent-info (parent-info (info cmd)))
                     (= 1 (length (children (parent-info (info cmd))))))
          do (setf (info cmd) (parent-info (info cmd))))
    (when (and (eql (widget-class (parent-info (info cmd))) 'form)
               (loop for child in (children (parent-info (info cmd)))
                     for constraints = (constraints child)
                     when (or (eql (widget (left constraints)) (info cmd))
                              (eql (widget (right constraints)) (info cmd))
                              (eql (widget (top constraints)) (info cmd))
                              (eql (widget (bottom constraints)) (info cmd)))
                       do (return t)))
      ;(xlib:bell *display*)
      (setf (info cmd) nil))))

(defmethod doit ((cmd delete-command))
  (setf *clipboard-type* :widget)
  (setf *clipboard-text* 
        (with-output-to-string (ss)
            (info-to-stream (info cmd) ss)))
  (setf (old-parent cmd) (parent-info (info cmd)))
  (setf (old-position cmd) 
        (position (info cmd) (children (parent-info (info cmd)))))
  (let ((plate (plate (info cmd))))
    (when (parent-view plate)
        (deinstall plate))
    (unmanage (gina-widget plate))
    (remove-child (old-parent cmd) (info cmd))
    (update-child-selection (old-parent cmd))
    (destroy (gina-widget plate))
    (when (parent-info (old-parent cmd))
      (adapt-to-widget-size (plate (find-toplevel-object (old-parent cmd)))))))

(defmethod undoit ((cmd delete-command))
  (if (eql (old-parent cmd) (main-info (document cmd)))
      (progn
         (add-child (old-parent cmd) (info cmd))
         (install (plate (info cmd)) (main-view (document cmd))
                  (x-pos (plate (info cmd))) (y-pos (plate (info cmd))))
         (create-tree (plate (info cmd)))
         (adapt-to-widget-size (plate (info cmd))))
      (progn                  
        (loop for child in (children (old-parent cmd))
              do (unmanage (gina-widget (plate child)))
                 (destroy (gina-widget (plate child))))
        (add-child (old-parent cmd) (info cmd))
        (setf (children (old-parent cmd))
              (append
                 (subseq (children (old-parent cmd)) 0 (old-position cmd))
                 (list (info cmd))
                 (subseq (children (old-parent cmd)) (old-position cmd)
                                   (1- (length (children (old-parent cmd)))))))
        (loop for child in (children (old-parent cmd))
              do (create-tree (plate child)))
        (set-constraints (plate (old-parent cmd)))
        (update-child-selection (old-parent cmd))
        (adapt-to-widget-size 
                    (plate (find-toplevel-object (old-parent cmd)))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class copy-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass copy-command (grab-info-command)
 (;;overrides
  (name    :initform "Copy Widget"  :allocation :class)
  ))

(defun make-copy-command (document widget start-x start-y
                               &key (class 'copy-command)
                                    (initargs nil)
                               &aux cmd)
  (setq cmd (make-grab-info-command document widget start-x start-y
                               :class class :initargs initargs))
  cmd)

(defmethod doit ((cmd copy-command))
  (setf *clipboard-type* :widget)
  (setf *clipboard-text* 
        (with-output-to-string (ss)
            (info-to-stream (info cmd) ss))))

;; undo in the next version will focus on the previous clipboard item

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class paste-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass paste-command (grab-info-command)
 (;;overrides
  (name    :initform "Paste Widget"  :allocation :class)
  (animate        :initform nil :allocation :class)
  ;;new slots
  (new-info    :accessor new-info     :initform nil)
  (new-parent  :accessor new-parent   :initform nil)
  (new-pos     :accessor new-pos      :initform nil)   
  ))

(defun make-paste-command (document widget start-x start-y
                               &key (class 'paste-command)
                                    (initargs nil)
                               &aux cmd)
  (setq cmd (make-grab-info-command document widget start-x start-y
                               :class class :initargs initargs))
  cmd)


(defmethod executable ((cmd paste-command))
  (eql *clipboard-type* :widget))

(defmethod doit ((cmd paste-command))
  ;; doit is only called if target-view hit
  ;; if no info, means main-info
  (unless (info cmd)
    (setf (info cmd) (main-info (document cmd))))
  (if (member (widget-class (info cmd)) '(form row-column paned-window))
      (progn
        (setf (new-parent cmd) (info cmd))
        (setf (new-pos cmd) (length (children (info cmd)))))
      (loop while (parent-info (info cmd))
            when (member (widget-class (parent-info (info cmd))) 
                         '(form row-column paned-window))
            do (setf (new-pos cmd) 
                     (position (info cmd) (children (parent-info (info cmd)))))
               (setf (new-parent cmd) (parent-info (info cmd)))
               (return nil)
            do (setf (info cmd) (parent-info (info cmd)))))
  (if (new-parent cmd)
      (progn
        (loop for child in (children (new-parent cmd))
              do (unmanage (gina-widget (plate child)))
                 (destroy (gina-widget (plate child))))
        (setf (new-info cmd)
              (with-input-from-string (ss *clipboard-text*)
                  (info-from-stream (read ss) (new-parent cmd)
                                    (document cmd) ss)))
        (setf (x-pos (new-info cmd)) (target-x cmd))
        (setf (y-pos (new-info cmd)) (target-y cmd))
        (update-widget-name (new-info cmd) (document cmd))
        (initialize-inserted-info (new-parent cmd) (new-info cmd))
        (make-widget-plate (new-info cmd))
        (setf (children (new-parent cmd))
              (append
                 (subseq (children (new-parent cmd)) 0 (new-pos cmd))
                 (list (new-info cmd))
                 (subseq (children (new-parent cmd)) (new-pos cmd)
                                   (1- (length (children (new-parent cmd)))))))
        (loop for child in (children (new-parent cmd))
              do (create-tree (plate child)))
        (update-child-selection (new-parent cmd))
        (set-constraints (plate (new-parent cmd)))
        (adapt-to-widget-size 
                    (plate (find-toplevel-object (new-parent cmd)))))
      (progn
         (setf (new-info cmd)
               (with-input-from-string (ss *clipboard-text*)
                   (info-from-stream (read ss) (main-info (document cmd))
                                     (document cmd) ss)))
         (update-widget-name (new-info cmd) (document cmd))
         (deselect-all (main-view (document cmd)))
         (install-plate (new-info cmd) (main-view (document cmd)) 
                        :x-pos (target-x cmd) :y-pos (target-y cmd))
         (adapt-to-plate-size (plate (new-info cmd))) ;; only for row-column
  )))

(defmethod undoit ((cmd paste-command))
  (unmanage (gina-widget (plate (new-info cmd))))
  (destroy (gina-widget (plate (new-info cmd))))
  (when (parent-view (plate (new-info cmd)))
      (deinstall (plate (new-info cmd))))
  (remove-child (parent-info (new-info cmd)) (new-info cmd))
  (when (new-parent cmd)
    (update-child-selection (new-parent cmd))
    (adapt-to-widget-size (plate (find-toplevel-object (new-parent cmd))))))

(defmethod redoit ((cmd paste-command))
  (if (new-parent cmd)
      (progn
        (loop for child in (children (new-parent cmd))
              do (unmanage (gina-widget (plate child)))
                 (destroy (gina-widget (plate child))))
        (setf (children (new-parent cmd))
              (append
                 (subseq (children (new-parent cmd)) 0 (new-pos cmd))
                 (list (new-info cmd))
                 (subseq (children (new-parent cmd)) (new-pos cmd))))
        (setf (parent-info (new-info cmd)) (new-parent cmd))
        (loop for child in (children (new-parent cmd))
              do (create-tree (plate child)))
        (set-constraints (plate (new-parent cmd)))
        (update-child-selection (new-parent cmd))
        (adapt-to-widget-size 
                    (plate (find-toplevel-object (new-parent cmd)))))
      (let ((info (new-info cmd)))
        (add-child (main-info (document cmd)) info)
        (install (plate info) (main-view (document cmd))
                 (x-pos (plate info)) (y-pos (plate info)))
        (create-tree (plate info))
        (adapt-to-plate-size (plate info)))))

(defmethod initialize-inserted-info ((parent-info widget-info) info)
  (declare (ignore info)))

(defmethod initialize-inserted-info ((parent-info paned-window-info) info)
  (new-constraints info (make-pane-constraints) (children parent-info)))

(defmethod update-widget-name ((info widget-info) document
                               &optional (new-names nil))
  (when (or (loop for a-name in new-names
                  when (string= a-name (widget-name info)) do (return t))
            (existing-widget-name info (main-info document) (widget-name info)))
      (initialize-widget-name info document)
      (push (widget-name info) new-names))
  (loop for child in (children info) do
      (update-widget-name child document new-names)))

(defun existing-widget-name (new-info start-info name)
  (if (string= name (widget-name start-info))
      (not (eq start-info new-info))
      (loop for child in (children start-info)
            when (existing-widget-name new-info child name)
              do (return t))))

(defmethod initialize-inserted-info ((parent-info form-info) info)
  (let ((constraints (make-form-constraints)))
    (setf (atype (left constraints)) :form)
    (setf (offset (left constraints)) 
          (- (x-pos info) (x-pos-in-info parent-info)))
    (setf (atype (top constraints)) :form)
    (setf (offset (top constraints)) 
          (- (y-pos info) (y-pos-in-info parent-info)))
    ;; always override old constraints !!
    (setf (constraints info) constraints)))

(defun x-pos-in-info (info)
  (let ((p (parent-info info)))
     (if (eql p (main-info (document info)))
         (x-pos info)
         (+ (x-pos info) (x-pos-in-info p)))))

(defun y-pos-in-info (info)
  (let ((p (parent-info info)))
     (if (eql p (main-info (document info)))
         (y-pos info)
         (+ (y-pos info) (y-pos-in-info p)))))
         


