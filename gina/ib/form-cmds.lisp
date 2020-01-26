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

(setq *sccs-id* "@(#)form-cmds.lisp	1.13	11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class side-attach-cmd
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass side-attach-command (mouse-down-command)
  (;; overrides
   (name       :initform "Reattach Side" :allocation :class)
   (hysteresis :initform 5                :allocation :class)
   ;; class parameters
   (show-ruler  :accessor show-ruler :initform nil :allocation :class)
   ;; instance-parameters
   (widget-plate :accessor widget-plate :initarg :widget-plate)
   (side  :accessor side :initarg :side)
   (min-pos :accessor min-pos)
   (max-pos :accessor max-pos)
   (inside-obj :accessor inside-obj :initform nil))
  (:documentation "superclass for changing individual side attachments"))

(defun make-side-attach-command (widget-plate x y side
                                 &key (class 'side-attach-command)
                                      (initargs nil)
                                 &aux cmd)
  "create a command to reattach the specified side"
  (with-slots (parent-view x-pos y-pos) widget-plate
    (setq cmd
      (make-mouse-down-command (document parent-view) parent-view 
                               (+ x-pos x) (+ y-pos y)
                               :class class
                               :initargs (append initargs
                                           (list :widget-plate widget-plate 
                                                 :side side)))))
  (calculate-allowed-area cmd)
  cmd)

(defmethod calculate-allowed-area ((cmd side-attach-command))
  (let ((region-x 0) (region-y 0)
	(region-width (width (view cmd)))
        (region-height (height (view cmd)))
        (mini 0)
        (maxi 1000)
        (cmd-plate (widget-plate cmd)))
    ;; determine region where other widgets affect movement
    (if (horizontal (side cmd))
        (progn
           (setq region-x (x-pos cmd-plate))
           (setq region-width (width cmd-plate)))
        (progn
           (setq region-y (y-pos cmd-plate))
           (setq region-height (height cmd-plate))))
    ;; set minimum and maximum position
    (loop for plate in (view-objects (view cmd))
          when (and (is-form-child-plate plate)
                    (not (eql plate cmd-plate))
		    (inside-rectangle plate region-x region-y 
				            region-width region-height))

	  do (if (horizontal (side cmd))
		 (if (> (y-pos plate) (y-pos cmd-plate))
                     (setq maxi (min maxi (y-pos plate)))
                     (setq mini (max mini (+ (y-pos plate) (height plate)))))
	         (if (> (x-pos plate) (x-pos cmd-plate))
		     (setq maxi (min maxi (x-pos plate)))
                     (setq mini (max mini (+ (x-pos plate) (width plate)))))))
    ;; recalcute what that means for the side to be moved
    (case (side cmd)
      ((:left)   (if (passive-member cmd-plate :right)
		     (setq maxi (+ (x-pos cmd-plate) (width cmd-plate) -2))
		     (decf maxi (width cmd-plate))))
      ((:right)  (if (passive-member cmd-plate :left)
		     (setq mini (x-pos cmd-plate))
		     (setq mini (+ mini (width cmd-plate) -1)))
                 (decf maxi 1))
      ((:top)    (if (passive-member cmd-plate :bottom)
		     (setq maxi (+ (y-pos cmd-plate) (height cmd-plate) -2))
		     (decf maxi (height cmd-plate))))
      ((:bottom) (if (passive-member cmd-plate :top)
		     (setq mini (y-pos cmd-plate))
		     (setq mini (+ mini (height cmd-plate) -1)))
                 (decf maxi 1)))
    (setf (min-pos cmd) mini)
    (setf (max-pos cmd) maxi)))
    
(defmethod constrain-mouse ((cmd side-attach-command) x y &aux skip-pos)
  ;; calculate object the mouse is in
  (unless (and (inside-obj cmd) (point-inside (inside-obj cmd) x y))
    (setf (inside-obj cmd)
          (loop for obj in (view-objects (view cmd))
                when (and (is-form-child-plate obj)
                          (point-inside obj x y))
                  do (return obj))))
  ;; to which position will the ruler skip in this object
  (when (inside-obj cmd)
    (setq skip-pos (case (side cmd)
		     ((:top)     (y-pos (inside-obj cmd)))
		     ((:bottom)  (+ (y-pos (inside-obj cmd)) 
				    (height (inside-obj cmd)) -1))
		     ((:left)    (x-pos (inside-obj cmd)))
		     ((:right)   (+ (x-pos (inside-obj cmd))
				    (width (inside-obj cmd)) -1)))))
  ;; constrain to allowed region
  (case (side cmd)
     ((:left :right)   (setq x (min x (max-pos cmd)))
                       (setq x (max x (min-pos cmd))))
     ((:top :bottom)   (setq y (min y (max-pos cmd)))
                       (setq y (max y (min-pos cmd)))))
  ;; constrain to skip pos if allowed
  (when (and (inside-obj cmd) (not (eql (inside-obj cmd) (widget-plate cmd))))
    (if (or ;(not (point-inside (inside-obj cmd) x y))
            (< skip-pos (min-pos cmd)) (> skip-pos (max-pos cmd)))
	(setf (inside-obj cmd) nil)
        (if (horizontal (side cmd)) (setq y skip-pos) (setq x skip-pos))))
  (values x y))

(defmethod draw-feedback ((cmd side-attach-command) x y &key clear &aux plate)
  "draw feedback while moving widget around"
  (declare (ignore clear))
  (setq plate (widget-plate cmd))
  (xlib:with-gcontext ((gcontext (view cmd)) :line-style :dash)
    ;; draw outline of widget
    (let ((draw-x 0) (draw-y 0) 
          (draw-width (1- (width plate))) (draw-height (1- (height plate))))
     (case (side cmd)
       ((:top)    (setf draw-y (- y (y-pos plate)))
                  (when (passive-member plate :bottom)
      	            (decf draw-height draw-y)))
       ((:bottom) (if (passive-member plate :top)
      		      (setf draw-height (- y (y-pos plate)))
                      (setf draw-y (- y draw-height (y-pos plate)))))
       ((:left)   (setf draw-x (- x (x-pos plate)))
                  (when (passive-member plate :right)
                    (decf draw-width draw-x)))
       ((:right)  (if (passive-member plate :left)
                      (setf draw-width (- x (x-pos plate)))
                      (setf draw-x (- x draw-width (x-pos plate))))))
     (draw-rectangle plate draw-x draw-y draw-width draw-height))
    ;; draw new ruler position
    (when (show-ruler cmd)
      (if (horizontal (side cmd))
          (draw-line (view cmd) 0 y (width (view cmd)) y)
          (draw-line (view cmd) x 0 x (height (view cmd))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class form-spacing-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass form-spacing-command (side-attach-command)
  (;; overrides
   (name       :initform "Change Spacing" :allocation :class)
   (show-ruler :initform nil              :allocation :class)
   ;; instance-variables
   (old-offset  :accessor old-offset)
   (new-offset  :accessor new-offset)
   (preceding-plate :accessor preceding-plate  :initform nil)))

(defun make-form-spacing-command (widget-plate x y side)
  (make-side-attach-command widget-plate x y side :class 'form-spacing-command))

(defmethod track-mouse ((cmd form-spacing-command) x y &key started finished)
  (declare (ignore started))
  (clear-feedback-string (feedback-view (main-shell (document cmd))))
  (unless finished 
    (draw-feedback-string (feedback-view (main-shell (document cmd)))
       (format nil "New Spacing: ~d"
                               (- (if (horizontal (side cmd)) y x) 
                                  (min-pos cmd))))))

(defmethod doit ((cmd form-spacing-command) &aux attachment)
  (setf (new-offset cmd) (- (if (horizontal (side cmd)) 
				(last-y cmd) (last-x cmd)) 
			    (min-pos cmd)))
  (setq attachment 
        (side-attachment (info (widget-plate cmd)) (side cmd)))
  (if (eql (atype attachment) :none)
      (progn 
	(setf (preceding-plate cmd) 
	      (preceding-child (widget-plate cmd) (ortho (side cmd))))
	(setq attachment
	      (side-attachment (info (preceding-plate cmd)) 
			       (opposite (side cmd))))
	(setf (old-offset cmd) (offset attachment))
	(setf (offset attachment) (new-offset cmd))
	(update-constraints (constraints (info (preceding-plate cmd))) 
			    (plate (info (preceding-plate cmd)))))
      (progn
	(setf (old-offset cmd) (offset attachment))
	(setf (offset attachment) (new-offset cmd))
	(update-constraints (constraints (info (widget-plate cmd)))
			    (plate (info (widget-plate cmd))))))
  (adapt-to-widget-size (form-plate (view cmd))))

(defmethod undoit ((cmd form-spacing-command) &aux attachment)
  (if (preceding-plate cmd)
      (progn
	(setq attachment
	      (side-attachment (info (preceding-plate cmd)) 
			       (opposite (side cmd))))
	(setf (offset attachment) (old-offset cmd))
	(update-constraints (constraints (info (preceding-plate cmd)))
			    (plate (info (preceding-plate cmd)))))
      (progn
	(setq attachment 
	      (side-attachment (info (widget-plate cmd)) (side cmd)))
	(setf (offset attachment) (old-offset cmd))
	(update-constraints (constraints (info (widget-plate cmd))) 
			    (plate (info (widget-plate cmd))))))
  (adapt-to-widget-size (form-plate (view cmd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class new-ruler-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass new-ruler-command (side-attach-command)
  (;; overrides
   (name       :initform "Attach Side" :allocation :class)
   (show-ruler :initform t             :allocation :class)
   ;; instance variables
   (new-ruler  :accessor new-ruler)
   (old-size   :accessor old-size)))

(defun make-new-ruler-command (widget-plate x y side)
  (make-side-attach-command widget-plate x y side :class 'new-ruler-command))

(defmethod track-mouse ((cmd new-ruler-command) x y &key started finished)
  (declare (ignore started))
  (clear-feedback-string (feedback-view (main-shell (document cmd))))
  (unless finished
    (draw-feedback-string (feedback-view (main-shell (document cmd)))
             (format nil "Ruler Pos: ~d" 
                               (if (horizontal (side cmd)) y x)))))

(defmethod doit ((cmd new-ruler-command) &aux attachment)
  (setq attachment (side-attachment (info (widget-plate cmd)) (side cmd)))
  (setf (old-size cmd)
        (if (horizontal (side cmd)) 
	    (height (widget-plate cmd)) 
	    (width (widget-plate cmd))))
  (let ((new-pos (if (horizontal (side cmd)) (last-y cmd) (last-x cmd))))
    (setf (new-ruler cmd)
          (make-ruler (side cmd) (widget-plate cmd) :atype :form))
    (setf (atype attachment) :form)
    (setf (offset attachment)
          (if (horizontal (side cmd))
              (- (height (view cmd)) new-pos)
              (- (width (view cmd)) new-pos)))
    (setf (pos (new-ruler cmd)) (offset attachment))
    (update-constraints (constraints (info (widget-plate cmd)))
                        (plate (info (widget-plate cmd))))
    (consolidate-and-install (new-ruler cmd) (view cmd)))
  (adapt-to-widget-size (form-plate (view cmd))))

(defmethod undoit ((cmd new-ruler-command) &aux attachment)
  (setq attachment (side-attachment (info (widget-plate cmd)) (side cmd)))
  (setf (atype attachment) :none)
  (update-constraints (constraints (info (widget-plate cmd)))
                      (plate (info (widget-plate cmd))))
  (if (horizontal (side cmd))
      (setf (height (gina-widget (plate (info (widget-plate cmd)))))
            (old-size cmd))
      (setf (width (gina-widget (plate (info (widget-plate cmd)))))
            (old-size cmd)))
  (case (side cmd)
     ((:left)    (setf (left-ruler (widget-plate cmd)) nil))
     ((:right)   (setf (right-ruler (widget-plate cmd)) nil))
     ((:top)     (setf (top-ruler (widget-plate cmd)) nil))
     ((:bottom)  (setf (bottom-ruler (widget-plate cmd)) nil)))
  (deinstall (new-ruler cmd))
  (adapt-to-widget-size (form-plate (view cmd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class detach-side-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass detach-side-command (side-attach-command)
  (;; overrides
   (name       :initform "Detach Side" :allocation :class)
   (show-ruler :initform nil           :allocation :class)
   ;; instance variables
   (old-attachment  :accessor old-attachment)
   (old-ruler       :accessor old-ruler    :initform nil)
   (old-pos         :accessor old-pos)))

(defun make-detach-side-command (widget-plate x y side)
  (make-side-attach-command widget-plate x y side :class 'detach-side-command))

(defmethod track-mouse ((cmd detach-side-command) x y &key started finished)
  (declare (ignore x y started))
  (clear-feedback-string (feedback-view (main-shell (document cmd))))
  (unless finished
    (draw-feedback-string (feedback-view (main-shell (document cmd)))
             "Detach")))

(defmethod doit ((cmd detach-side-command) &aux attachment)
  (setq attachment (side-attachment (info (widget-plate cmd)) (side cmd)))
  (setf (old-attachment cmd) (atype attachment))
  (setf (atype attachment) :none)
  (let ((ruler (remove-from-ruler (widget-plate cmd) (side cmd))))
    (setf (old-pos cmd) (if (horizontal (side cmd)) 
			    (x-pos (widget-plate cmd))
                            (y-pos (widget-plate cmd))))
    (setf (old-ruler cmd) ruler)
    (when (zerop (length (members ruler)))
      (deinstall ruler)))
  (update-constraints (constraints (info (widget-plate cmd)))
                      (plate (info (widget-plate cmd))))
  (if (horizontal (side cmd))
      (setf (height (gina-widget (plate (info (widget-plate cmd)))))
            (- (last-y cmd) (y-pos (widget-plate cmd))))
      (setf (width (gina-widget (plate (info (widget-plate cmd)))))
            (- (last-x cmd) (x-pos (widget-plate cmd)))))
  (adapt-to-widget-size (form-plate (view cmd))))

(defmethod undoit ((cmd detach-side-command) &aux attachment)
  (setq attachment (side-attachment (info (widget-plate cmd)) (side cmd)))
  (setf (atype attachment) (old-attachment cmd))
  (add-to-ruler (widget-plate cmd) (side cmd) (old-ruler cmd) (old-pos cmd))
  (update-constraints (constraints (info (widget-plate cmd)))
                      (plate (info (widget-plate cmd))))
  (unless (parent-view (old-ruler cmd))
    (install (old-ruler cmd) (view cmd) 
	     (x-pos (old-ruler cmd)) (y-pos (old-ruler cmd))))
  (adapt-to-widget-size (form-plate (view cmd)))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class form-child-mover
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass form-child-mover (mouse-down-command)
  (;; overrides
   (name       :initform "Move Form Child" :allocation :class)
   (hysteresis :initform 5                :allocation :class)
   ;; instance-parameters
   (widget-plate :accessor widget-plate :initarg :widget-plate)
   (old-x :accessor old-x :initarg :old-x)
   (old-y :accessor old-y :initarg :old-y)
   ;; instance-variables
   (new-x :accessor new-x)
   (new-y :accessor new-y)
   (x-off :accessor x-off :initarg :x-off)
   (y-off :accessor y-off :initarg :y-off)
  )
  (:documentation "a mouse-down-command to move a widget-plate around"))

(defun make-form-child-mover (widget-plate x y)
  "create a new mouse-down-command object to move the specified widget-plate"
  (with-slots (parent-view x-pos y-pos) widget-plate
    (make-mouse-down-command (document parent-view) parent-view 
                             (+ x-pos x) (+ y-pos y)
               :class 'form-child-mover
               :initargs (list :widget-plate widget-plate 
                               :old-x x-pos :old-y y-pos :x-off x :y-off y))))

(defun preceding-child (widget-plate side)
  (let ((ruler (if (horizontal side)
                   (if (top-ruler widget-plate) (top-ruler widget-plate)
                                                (bottom-ruler widget-plate))
                   (if (left-ruler widget-plate) (left-ruler widget-plate)
                                                 (right-ruler widget-plate)))))
    (when ruler
       (let ((my-pos (position widget-plate (members ruler))))
          (when (> my-pos 0)
            (nth (1- my-pos) (members ruler)))))))

(defun nearest-ruler (widget-plate posi side &optional (dist 1000) 
                      &aux (result nil))
  (let ((my-pos 
          (case side
	    ((:left)    posi)
	    ((:right)   (+ posi (width widget-plate) -1))
	    ((:top)     posi)
	    ((:bottom)  (+ posi (height widget-plate) -1)))))
    (loop for obj in (view-objects (parent-view widget-plate))
          when (and (not (is-form-child-plate obj))
                    (eql (side obj) side)
                    (< (abs (- my-pos (pixel-pos obj))) dist))
          do (setq dist (abs (- my-pos (pixel-pos obj))))
             (setq result obj))
    result))

(defmethod constrain-mouse ((cmd form-child-mover) x y)
  (let ((x-ruler (nearest-ruler (widget-plate cmd) (- x (x-off cmd)) :left 8))
         (y-ruler (nearest-ruler (widget-plate cmd) (- y (y-off cmd)) :top 8)))
    (if x-ruler
        (setq x (+ (pixel-pos x-ruler) (x-off cmd)))
        (progn
           (setq x-ruler (nearest-ruler (widget-plate cmd) 
					(- x (x-off cmd)) :right 8))
           (when x-ruler
             (setq x (+ (- (pixel-pos x-ruler) (width (widget-plate cmd)) -1)
                        (x-off cmd))))))
    (if y-ruler
        (setq y (+ (pixel-pos y-ruler) (y-off cmd)))
        (progn
           (setq y-ruler (nearest-ruler (widget-plate cmd) 
					(- y (y-off cmd)) :bottom 8))
           (when y-ruler
             (setq y (+ (- (pixel-pos y-ruler) (height (widget-plate cmd)) -1)
                        (y-off cmd)))))))
  (values x y))

(defmethod draw-feedback ((cmd form-child-mover) x y &key clear)
  "draw feedback while moving widget around"
  (declare (ignore clear))
  (with-slots (widget-plate start-x start-y view 
               old-x old-y new-x new-y) cmd
    (setq new-x (- x (- start-x old-x)) new-y (- y (- start-y old-y)))
    (xlib:with-gcontext ((gcontext view) :line-style :dash)
      (draw-rectangle view new-x new-y 
                      (1- (width widget-plate)) (1- (height widget-plate)))))
  )

(defmethod doit ((cmd form-child-mover))
  "move widget and widget-plate to new pos"
  (with-slots (widget-plate new-x new-y old-x old-y new-width new-height) cmd
    ))

(defmethod undoit ((cmd form-child-mover))
  "move back widget and widget-plate"
  (with-slots (widget-plate old-x old-y new-x new-y new-width new-height) cmd
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class ruler-mover
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass ruler-mover (mouse-down-command)
  (;; overrides
   (name       :initform "Move Ruler" :allocation :class)
   (hysteresis :initform 5                :allocation :class)
   ;; instance-parameters
   (ruler :accessor ruler :initarg :ruler)
   (old-pos    :accessor old-pos  :initarg :old-pos)
   (new-pos    :accessor new-pos))
  (:documentation "a mouse-down-command to move a ruler around"))

(defun make-ruler-mover (ruler x y)
  "create a new mouse-down-command object to move the specified ruler"
  (with-slots (parent-view x-pos y-pos) ruler
      (make-mouse-down-command (document parent-view) parent-view 
                               (+ x-pos x) (+ y-pos y)
                               :class 'ruler-mover
                               :initargs (list :ruler ruler 
                                               :old-pos (pos ruler)))))

(defmethod constrain-mouse ((cmd ruler-mover) x y)
  (if (horizontal (side (ruler cmd)))
      (setq y (max y (- (start-y cmd) (pixel-pos (ruler cmd)))))
      (setq x (max x (- (start-x cmd) (pixel-pos (ruler cmd))))))
  (values x y))

(defmethod draw-feedback ((cmd ruler-mover) x y &key clear)
  "draw feedback while moving widget around"
  (declare (ignore clear))
  (xlib:with-gcontext ((gcontext (view cmd)) :line-style :dash)
    (if (horizontal (side (ruler cmd)))
      (draw-line (view cmd) 0 (+ (pixel-pos (ruler cmd)) (- y (start-y cmd)))
         (width (view cmd)) (+ (pixel-pos (ruler cmd)) (- y (start-y cmd))))
      (draw-line (view cmd) (+ (pixel-pos (ruler cmd)) (- x (start-x cmd))) 0 
         (+ (pixel-pos (ruler cmd)) (- x (start-x cmd))) (height (view cmd))))) 
  )

(defmethod doit ((cmd ruler-mover))
  "move widget and widget-plate to new pos"
  (let ((new-pixel (pixel-pos (ruler cmd)))
        (total (if (horizontal (side (ruler cmd))) 
                   (height (parent-view (ruler cmd)))
                   (width (parent-view (ruler cmd))))))
     (if (horizontal (side (ruler cmd)))
         (incf new-pixel (- (last-y cmd) (start-y cmd)))
         (incf new-pixel (- (last-x cmd) (start-x cmd))))
     (case (atype (ruler cmd))
       ((:form) 
             (case (which-dir (atype (ruler cmd)) (side (ruler cmd)))
                 ((:right :bottom) (setf (new-pos cmd) (- total new-pixel)))
                 ((:top :left) (setf (new-pos cmd) new-pixel))))
       ((:opposite-form)
             (case (which-dir (atype (ruler cmd)) (side (ruler cmd)))
                 ((:right :bottom) (setf (new-pos cmd) (- (- total new-pixel))))
                 ((:top :left) (setf (new-pos cmd) (- new-pixel)))))
       ((:position) (setf (new-pos cmd) (round (/ (* 100 new-pixel) total))))))
  (setf (pos (ruler cmd)) (new-pos cmd))
  (propagate-ruler-changes (ruler cmd)))

(defmethod undoit ((cmd ruler-mover))
  "move back widget and widget-plate"
  (setf (pos (ruler cmd)) (old-pos cmd))
  (propagate-ruler-changes (ruler cmd)))

(defmethod redoit ((cmd ruler-mover))
  (setf (pos (ruler cmd)) (new-pos cmd))
  (propagate-ruler-changes (ruler cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class ruler-type-changer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass ruler-type-changer (command)
  ((name         :initform "Change Ruler Type"      :allocation :class)
   (ruler        :accessor ruler :initarg :ruler)
   (old-pos      :accessor old-pos   :initarg :old-pos)
   (old-type     :accessor old-type  :initarg :old-type)
   (new-pos      :accessor new-pos)
   (new-type     :accessor new-type  :initform :none))
  (:documentation "A command to change the type of ruler"))

;; has no effect when type is :none
(defun make-ruler-type-changer (view ruler)
  (make-command (document view) :class 'ruler-type-changer
                :initargs (list :ruler ruler :old-pos (pos ruler)
                                :old-type (atype ruler))))

(defmethod doit ((cmd ruler-type-changer))
  (let* ((ruler (ruler cmd))
         (total (if (horizontal (side ruler)) 
                    (height (parent-view ruler))
                    (width (parent-view ruler)))))
    (setf (old-type cmd) (atype ruler))
    (case (atype ruler)
      ((:form) 
            (setf (new-type cmd) :position)
            (setf (new-pos cmd) (round (/ (* 100 (pixel-pos ruler)) total))))
      ((:position) 
            (if (member (side ruler) '(:left :top))
              (progn                     ;; skip opposite-form
                 (setf (new-type cmd) :form)
                 (setf (new-pos cmd) (pixel-pos ruler)))
              (progn
                (setf (new-type cmd) :opposite-form)
                (setf (new-pos cmd)
                      (case (which-dir (new-type cmd) (side ruler))
                         ((:top :left) (- (pixel-pos ruler)))
                         (t            (- (- total (pixel-pos ruler)))))))))
      ((:opposite-form) 
            (setf (new-type cmd) :form)
            (setf (new-pos cmd) (- total (- (old-pos cmd))))))
    ;; execute it
    (setf (atype ruler) (new-type cmd))
    (setf (pos ruler) (new-pos cmd))
    (propagate-ruler-changes ruler)))

(defmethod undoit ((cmd ruler-type-changer))
  (let ((ruler (ruler cmd)))
    (setf (atype ruler) (old-type cmd))
    (setf (pos ruler) (old-pos cmd))
    (propagate-ruler-changes ruler)))

(defmethod redoit ((cmd ruler-type-changer))
  (let ((ruler (ruler cmd)))
    (setf (atype ruler) (new-type cmd))
    (setf (pos ruler) (new-pos cmd))
    (propagate-ruler-changes ruler)))



