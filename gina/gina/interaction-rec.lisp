;;;-*-Mode:LISP;Syntax: Common-Lisp;Package:gina;Base:10-*-
;;;
;;; Copyright 1992 GMD (German National Research Center for Computer Science)
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

(in-package :gina)
(setq *sccs-id* "@(#)interaction-rec.lisp	1.5 11/8/93")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This is an experimental extension. USE AT YOUR OWN RISK !
;;;
;;; The interfaces contained in this file will probably change drastically
;;; in the future.
;;;
;;; This extension is only required for the graphic-recorder demo.
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; animation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *animation-interval* 50)
(defparameter *animation-speed* 15)

(defclass history-animator ()
  ((active           :accessor active           :initform nil)
   (animation-shell  :accessor animation-shell  :initform nil)
   (animation-child  :accessor animation-child  :initform nil)
   (animation-x      :accessor animation-x)
   (animation-y      :accessor animation-y)
   (animation-time   :accessor animation-time)
   (animation-stop   :accessor animation-stop)
   (play-button      :accessor play-button)))

(defginafun make-history-animator ()
  "create a history animator and thereby activate the interaction recorder"
  (make-object 'history-animator nil))

(defmethod pause ((a history-animator) interval)
  (xlib:display-force-output *display*)
  (sleep (/ interval 1000))
  (incf (animation-time a) interval))

(defmethod move-to ((a history-animator) widget
		    &key (partly 0) (x-off 0) (y-off 0))
  (multiple-value-bind (root-x root-y) (root-coordinates widget)
    (update-slots widget)
    ;; always keep it on top
    ;(xtk:raise-widget (widget-id (animation-shell a)))
    (setf (xlib:window-priority (get-clx-window (animation-shell a))) :above)
    (xlib:display-force-output *display*)
    (when (> partly 0)
      (incf root-x (round (/ (width widget) partly)))
      (incf root-y (round (/ (height widget) partly))))
    (incf root-x x-off)
    (incf root-y y-off)
    (let ((xdiff (- root-x (animation-x a)))
	  (ydiff (- root-y (animation-y a))))
      (loop while (not (and (zerop xdiff) (zerop ydiff))) do
	(let* ((dist (sqrt (+ (* xdiff xdiff) (* ydiff ydiff))))
	       (xmove (round (/ (* xdiff *animation-speed*) dist)))
               (ymove (round (/ (* ydiff *animation-speed*) dist))))
	  ;; check for roundoff errors
	  (when (> (abs xmove) (abs xdiff)) (setq xmove xdiff))
	  (when (> (abs ymove) (abs ydiff)) (setq ymove ydiff))
	  (incf (animation-x a) xmove)
	  (incf (animation-y a) ymove)
	  (decf xdiff xmove)
	  (decf ydiff ymove)
	  (move (animation-shell a) (animation-x a) (animation-y a))
	  (pause a *animation-interval*))))))

(defmethod activate ((a history-animator) doc)
  (if (animation-shell a)
      (setf (label-string (animation-child a)) "mouse")
      (progn
	(setf (animation-shell a)
              (make-shell (main-shell doc) nil
				:motif-widget-class 'override-shell
				:motif-resources '(:border-width 1)))
        (setf (animation-child a)
              (make-label (animation-shell a) "mouse" :label-type :pixmap
		    :motif-resources '(:margin-width 0 
				       :margin-height 0)))))
  (setf (animation-time a)
        (parse-integer (xtk:last-timestamp-processed)))
  (setf (active a) t)
  (setf (animation-stop a) nil)
  (multiple-value-bind (gx gy) (root-coordinates (main-shell doc))
    (setf (animation-x a) (- gx 10))
    (setf (animation-y a) (- gy 10)))
  (move (animation-shell a) (animation-x a) (animation-y a))
  (pop-up (animation-shell a)))

(defmethod deactivate ((a history-animator) doc)
  (declare (ignore doc))
  (setf (active a) nil)
  (pop-down (animation-shell a)))

(defmethod send-event ((a history-animator) widget event-type button)
  (let ((window (get-clx-window widget)))
    (multiple-value-bind (root-x root-y)
	(root-coordinates widget)
      (case event-type
        ((:button-press)
         (setf (label-string (animation-child a)) "mouse1")
         (xlib:send-event window :button-press '(:button-press)
		:window window :event-window window
		:root (xlib:screen-root (first (xlib:display-roots *display*)))
		:code button :x 2 :y 2 :state 0
		:time (animation-time a)
		:root-x root-x :root-y root-y :child nil))
        ((:button-release)
         (setf (label-string (animation-child a)) "mouse")
         (xlib:send-event window :button-release '(:button-release)
                :window window :event-window window
		:root (xlib:screen-root (first (xlib:display-roots *display*)))
		:code button :x 2 :y 2 :state (* 256 button)
		:time (animation-time a)
		:root-x root-x :root-y root-y :child nil))
	((:enter-notify)
	 (xlib:send-event window :enter-notify '(:enter-window)
		:window window :event-window window
		:x 2 :y 2 :mode :normal :state (* 256 button)
		:root (xlib:screen-root (first (xlib:display-roots *display*)))
		:time (animation-time a)
		:root-x root-x :root-y root-y :child nil))
	((:leave-notify)
	 (xlib:send-event window :leave-notify '(:leave-window)
		:window window :event-window window
		:x 2 :y 2 :mode :normal :state (* 256 button)
		:root (xlib:screen-root (first (xlib:display-roots *display*)))
		:time (animation-time a)
		:root-x root-x :root-y root-y :child nil)))
      ;; needed for popup actions
      (xtk:raise-widget (widget-id (animation-shell a)))
   )))

(defmethod simulate-button ((a history-animator) nr)
  (case nr
    (0        (setf (label-string (animation-child a)) "mouse"))
    (1        (setf (label-string (animation-child a)) "mouse1"))))

(defmethod in-animation ((doc document))
  (and (animator doc) (active (animator doc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; animate-redo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod animate-redo ((cmd command) a)
  (declare (ignore a)))

(defmethod animate-redo ((cmd mouse-down-command) a)
  (let ((x (start-x cmd))
        (y (start-y cmd))
	(steps (if (mouse-already-moved cmd)
		   (min 20 (1+ (abs (- (start-x cmd) (last-x cmd))))
			(1+ (abs (- (start-y cmd) (last-y cmd)))))
		   1)))
    (loop for i from 0 upto steps do 
      (when (and (> i 0) (mouse-already-moved cmd))
	(draw-feedback cmd x y :clear t))
      (when (= i steps) 
	(setq x (last-x cmd))
	(setq y (last-y cmd)))
      (when (and (> i 0) (< i steps)) 
	(incf x (round (/ (- (last-x cmd) x) (- steps i))))
	(incf y (round (/ (- (last-y cmd) y) (- steps i)))))
      (multiple-value-setq (x y) (constrain-mouse cmd x y))
      (move-to a (view cmd) :x-off x :y-off y)
      (when (zerop i)
	(pause a 300)
        (simulate-button a 1)
        (pause a 300))
      (when (mouse-already-moved cmd)
        (draw-feedback cmd x y :clear nil)
        (when (and (auto-scrolling cmd) (scroller (view cmd)))
	   ;; scroll the view if necessary
	   (show-point 
	    (scroller (view cmd)) x y
	    ;; get the size of the clipper and the position of the work-area 
	    :ask-for-work-area-pos t
	    :ask-for-clipper-size  t
	    ;; just move the X-window most of the time
	    :x-window-only (< i steps)
	    ;; always tell the toolkit the final position
	    :force-scrolling (= i steps)))
        (track-mouse cmd x y
		      :started (= 1 i) 
		      :finished (= steps i)))
      (pause a 50))
    (pause a 500)
    (when (mouse-already-moved cmd)
      (draw-feedback cmd x y :clear t))
    (simulate-button a 0)
    (pause a 50)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; widget animation methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod animate-activation ((button push-button) a)
  (move-to a button :partly 3)
  (pause a 500)
  (send-event a button :button-press 1)
  (pause a 1000)
  (send-event a button :button-release 1)
  (pause a 1000))

(defmethod animate-activation ((entry menu-entry) a)
  (let* ((prev nil)
	 (w (widget entry))
	 (pane (parent w))
	 (menu (parent (parent pane)))
	 (topic (loop for tp in (gina::topics menu)
		      when (eq (gina::box tp) pane)
		        do (return tp)))
         (casc (gina::cascade topic))
	 (widgets (loop for item in (gina::items topic)
		        until (eq (gina::entry item) entry)
		        collect (widget (gina::entry item)))))
    (move-to a casc :partly 3)
    (pause a 500)
    (send-event a casc :button-press 1)
    (pause a 1000)
    (setq prev casc)
    (loop for next in widgets do
      (move-to a next :partly 3)
      (send-event a prev :leave-notify 1)
      (send-event a next :enter-notify 1)
      (setq prev next)
      (pause a 1))
    (move-to a w :partly 3)
    (send-event a prev :leave-notify 1)
    (send-event a w :enter-notify 1)
    (pause a 1000)
    (send-event a w :button-release 1)
    (pause a 500)))

(defmethod animate-value-change ((w widget) a new-value old-value)
  (declare (ignore old-value))
  (move-to a w :partly 3)
  (pause a 200)
  (setf (value w) new-value)
  (pause a 500))

(defmethod animate-value-change ((entry toggle-group-entry) a 
				 new-value old-value)
  (declare (ignore old-value))
  (let ((w (widget entry)))
    (move-to a w :partly 3)
    (pause a 200)
    (setf (value w) new-value)
    (pause a 500)))
  
(defmethod animate-value-change ((group toggle-button-group) a
				 new-value old-value)
  (declare (ignore old-value))
  (let* ((button (loop for v in (value-list group)
                       for w in (toggle-buttons group)
                       when (equal v new-value)
                         do (return w))))
    (move-to a button :partly 3)
    (pause a 500)
    (send-event a button :button-press 1)
    (pause a 1000)
    (send-event a button :button-release 1)
    (pause a 1000)))

;; does not work for negative movement !!
(defmethod animate-value-change ((sb scrollbar) a new-value old-value)
  (update-slots sb)
  (let* ((res (get-motif-resources sb :orientation :minimum :maximum
				   :slider-size))  ;; slider-size not used
         (vertical (eql (first res) :vertical))
	 (diff (- (+ (third res) 0) (second res)))
	 (pixels (- (if vertical (height sb) (width sb)) 30))
	 (inc (- new-value old-value)))
    (if vertical
	(progn
	  (move-to a sb :x-off 6 :y-off 20)
          (simulate-button a 1)
	  (loop for i from 1 upto 10 
	        for pp = (* i (/ inc 10)) do
            (move-to a sb :x-off 6
		     :y-off (+ 20 (round (/ (* pp pixels) diff))))
	    (scroll-to (parent sb)
		       (value (horizontal-scrollbar (parent sb)))
		       (+ old-value (round pp)))
            ;(setf (value sb) (+ old-value (round pp)))
            (pause a 50))
	  (simulate-button a 0)
	  (setf (value sb) new-value)
	  (pause a 500))
        (progn
	  (move-to a sb :x-off 20 :y-off 6)
	  (simulate-button a 1)
	  (loop for i from 1 upto 10 
	        for pp = (* i (/ inc 10)) do
            (move-to a sb :y-off 6
		     :x-off (+ 20 (round (/ (* (* i (/ inc 10)) pixels) diff))))
	    (scroll-to (parent sb)
		       (+ old-value (round pp))
		       (value (vertical-scrollbar (parent sb))))
            ;(setf (value sb) (+ old-value (round (* i (/ inc 10)))))
            (pause a 50))
	  (simulate-button a 0)
	  (setf (value sb) new-value)
	  (pause a 500)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; interactors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass interactor ()
  ((widget       :accessor widget      :initarg :widget)))

(defun make-interactor (widget 
			&key (initargs nil)
			     (class 'interactor))
  (make-object class initargs :widget widget))

(defmethod animate ((in interactor) a)
  (declare (ignore a))
  )

(defclass activate-interactor (interactor)
  ())

(defun make-activate-interactor (widget)
  (make-interactor widget :class 'activate-interactor))

(defmethod animate ((in activate-interactor) a)
  (animate-activation (widget in) a))

(defclass value-changed-interactor (interactor)
  ((new-value    :accessor new-value    :initarg :new-value)
   (old-value    :accessor old-value    :initarg :old-value)))

(defun make-value-changed-interactor (widget new-value old-value)
  (make-interactor widget :class 'value-changed-interactor
		   :initargs (list :new-value new-value :old-value old-value)))

(defmethod animate ((in value-changed-interactor) a)
  (animate-value-change (widget in) a (new-value in) (old-value in)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class activation-callback
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defginaclass activation-callback (callback)
  (description "a callback from a button")
  ((document   :accessor document   :initarg :document)))

(defginafun make-activation-callback (command-name document &rest static-args)
  (description "construct an activation callback")
  (make-instance 'activation-callback
                 :function-name command-name :document document
		 :static-args static-args))

(defmethod execute ((cb activation-callback) call-data)
  (declare (ignore call-data))
  (error "should never occur"))

(defmethod execute-from-widget ((cb activation-callback) widget call-data)
  (unless (in-animation (document cb))
    (let ((cmd (make-activate-command (document cb) 
				      (make-activate-interactor widget)
				      :immediately-submit nil
				      :class (function-name cb))))
      (apply #'initialize-parameters (cons cmd
					 (append (static-args cb) call-data)))
      (submit cmd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class value-changing-callback
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defginaclass value-changing-callback (activation-callback)
  (description "a callback from a value widget")
  ((transformer   :accessor transformer    :initarg :transformer)))

(defginafun make-value-changing-callback (command-name document transformer
				     &rest static-args)
  (description "construct a value-changing-callback")
  (make-instance 'value-changing-callback
		 :function-name command-name :document document
		 :transformer transformer :static-args static-args))

(defmethod execute-from-widget ((cb value-changing-callback) widget call-data)
  (unless (in-animation (document cb))
    (multiple-value-bind (new-value old-value)
		  (extract-callback-information widget call-data)
      (let ((cmd (make-value-changed-command (document cb)
		      (make-value-changed-interactor widget new-value old-value)
		      (if (transformer cb)
		          (funcall (transformer cb) new-value)
			  new-value)
		      :immediately-submit nil
		      :class (function-name cb))))
        (when cmd    ;; may be nil if compress
	  (apply #'initialize-parameters (cons cmd
					   (append (static-args cb) call-data)))
	  (submit cmd))))))

;; for widgets and menu entries
(defginamethod extract-callback-information (w call-data)
  "splice the call-data into new-value and (old-value or diffs)"
  (let ((old-value (slot-value w 'value)))
    (case (length call-data)
        ((1)
          ;; ensure that old value is correct next time
	  (setf (slot-value w 'value) (first call-data))
	  (values (first call-data) old-value))
        ((2)
	  (values (first call-data) (second call-data)))
	((3)
          (values (first call-data)
		  (if (third call-data) ;;set
                      (remove (second call-data) (first call-data))
		      (cons (second call-data) (first call-data))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class activate-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defginaclass activate-command (command)
  (description "a command that is called from a button")
  ((interactor     :accessor interactor      :initarg :interactor
       :documentation "May be a list of interactors.")))

(defginafun make-activate-command (document interactor
			      &key (view (main-view document))
				   (immediately-submit t)
				   (create-checkpoint nil)
				   (reverse-undo nil)
				   (class 'activate-command)
				   (initargs nil))
  (description "construct activation cmd")
  (unless (in-animation document)
    (make-command document :view view :immediately-submit immediately-submit
		  :create-checkpoint create-checkpoint
		  :reverse-undo reverse-undo
		  :class class
		  :initargs (more-initargs :interactor interactor))))

;; use instead of a specialized constructor
(defginamethod initialize-parameters ((cmd activate-command) &rest inits)
  (description "to initialize instance variables from static data in the callback")
  (declare (ignore inits)))

(defmethod animate-redo ((cmd activate-command) a)
  "animate widget"
  (if (listp (interactor cmd))
      (loop for int in (interactor cmd)
	    do (animate int a))
      (animate (interactor cmd) a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class value-changed-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defginaclass value-changed-command (activate-command)
  (description "a command called from a value-changing widget")
  ((compress    :accessor compress      :initform t   :allocation :class)
   (state       :accessor state         :initform nil)
   (old-value   :accessor old-value)
   (new-value   :accessor new-value     :initarg :new-value)))

(defginafun make-value-changed-command (document interactor new-value
				   &key (view (main-view document))
					(immediately-submit t)
					(create-checkpoint nil)
					(reverse-undo nil)
					(class 'value-changed-command)
					(initargs nil))
  (description "construct value-changed-command")
  (if (and (undo-commands document)
	   (for-same-widget (first (undo-commands document)) interactor))
      (let ((previous (first (undo-commands document))))
        ;; should we call command-executed here?
        ;; probably, at least eval taps
	(setf (new-value previous) new-value)
	(if (listp (interactor previous))
	    (setf (interactor previous) 
	          (append (interactor previous) (list interactor)))
	    (setf (interactor previous)
	          (list (interactor previous) interactor)))
	(propagate-value previous new-value)
	(react-to-change document previous :doit nil)
	nil)
      (make-activate-command document interactor 
			 :view view 
			 :immediately-submit immediately-submit
			 :create-checkpoint create-checkpoint
			 :reverse-undo reverse-undo
			 :class class
			 :initargs (more-initargs :new-value new-value))))

(defmethod for-same-widget ((cmd command) interactor)
  (declare (ignore interactor))
  nil)

(defmethod for-same-widget ((cmd value-changed-command) interactor)
  (and (compress cmd)
       (eq (widget (if (listp (interactor cmd))
		       (first (interactor cmd))
		       (interactor cmd))) 
	   (widget interactor))))

(defginamethod propagate-value ((cmd value-changed-command) value)
  (description "set the value inside the application")
  (declare (ignore value)))

(defmethod doit ((cmd value-changed-command))
  (save-initial-state cmd)
  (propagate-value cmd (new-value cmd))
  (setf (state cmd) :new))

(defmethod undoit ((cmd value-changed-command))
  (propagate-value cmd (old-value cmd))
  (setf (state cmd) :old))

(defmethod redoit ((cmd value-changed-command))
  (propagate-value cmd (new-value cmd))
  (setf (state cmd) :new))

(defginamethod save-initial-state ((cmd value-changed-command))
  (description "override to call get-state")
  (setf (old-value cmd) (get-state cmd)))

(defginamethod save-final-state ((cmd value-changed-command))
  (description "override to call get-state")
  (setf (new-value cmd) (get-state cmd)))

;; default could be to inquire transformed value from interactor
(defginamethod get-state ((cmd value-changed-command))
  (description "always override to get current state of the value")
  nil)

(defginamethod value-description ((cmd value-changed-command) value)
  (description "override to return a string describing the value")
  (format nil "~s" value))

(defginamethod command-description ((cmd value-changed-command))
  (description "build description from the two values")
  (let ((obj (object-description cmd)))
    (if obj
        (concatenate 'simple-string (name cmd) " of " obj " from "
		     (value-description cmd (old-value cmd)) " to "
		     (value-description cmd (new-value cmd)))
        (concatenate 'simple-string (name cmd) " from "
		     (value-description cmd (old-value cmd)) " to "
		     (value-description cmd (new-value cmd))))))

(defginamethod reverse-command-description ((cmd value-changed-command))
  (description "build description from the two values")
  (let ((obj (object-description cmd)))
    (if obj
        (concatenate 'simple-string (name cmd) " of " obj " from "
		     (value-description cmd (new-value cmd)) " to "
		     (value-description cmd (old-value cmd)))
        (concatenate 'simple-string (name cmd) " from "
		     (value-description cmd (new-value cmd)) " to "
		     (value-description cmd (old-value cmd))))))

;; for use from taps
(defmethod current-value ((cmd value-changed-command))
  (case (state cmd)
    ((:new) (new-value cmd))
    ((:old) (old-value cmd))
    (t      (error "Command not yet executed!"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class pop-up-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defginaclass pop-up-command (activate-command)
  (description "a command to pop up a dialog")
  ((name   :initform "Pop up" :allocation :class)
   (causes-change :initform nil :allocation :class)
   (box   :accessor box    :initarg :box)))

(defmethod initialize-parameters ((cmd pop-up-command) &rest inits)
  (setf (box cmd) (first inits)))

(defmethod doit ((cmd pop-up-command))
  (pop-up (box cmd)))

(defmethod undoit ((cmd pop-up-command))
  (pop-down (box cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class pop-down-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defginaclass pop-down-command (activate-command)
  (description "a command to pop down a dialog")
  ((name   :initform "Pop down" :allocation :class)
   (causes-change :initform nil :allocation :class)
   (box   :accessor box    :initarg :box)))

(defmethod initialize-parameters ((cmd pop-down-command) &rest inits)
  (setf (box cmd) (first inits)))

(defmethod doit ((cmd pop-down-command))
  (pop-down (box cmd)))

(defmethod undoit ((cmd pop-down-command))
  (pop-up (box cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; new history scroller with play button
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-replay-features ((h-scroller history-scroller) parent-widget
				&aux (doc (document h-scroller)))
  ;;animator must be overridden by document in order to activate recorder
  ;;(unless (animator doc)
  ;;  (setf (animator doc) (make-history-animator)))
  (if (animator doc)
      (setf (play-button (animator doc))
	    (make-push-button parent-widget "Play"
			      :activate-callback 
			      (make-callback #'play-history doc))) 
      (make-push-button parent-widget "Play"
		    :activate-callback (make-callback #'replay-history 
						      doc))))

(defmethod play-history ((doc document) 
			 &aux (button (play-button (animator doc))))
  (if (in-animation doc)
      (progn
	(setf (animation-stop (animator doc)) t)
	(set-sensitivity button nil))
      (progn
	(in-background-process (doc)
          (do-play doc))
	(setf (label-string button) "Stop"))))

(defmethod do-play ((doc document))
  (with-clock-cursor
    (unwind-protect
	(xtk:with-immediate-update-enabled
	  (activate (animator doc) doc)
          (loop while (and (redo-commands doc)
			   (not (animation-stop (animator doc))))
	        do (sleep 0.3)
	           (animate-redo (first (redo-commands doc)) (animator doc))
	           (redo doc)
	           (xlib:display-force-output *display*))
	  (unless (animation-stop (animator doc))
	    (pause (animator doc) 1000)))
      (deactivate (animator doc) doc)
      (set-sensitivity (play-button (animator doc)) t)
      (setf (label-string (play-button (animator doc))) "Play"))))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; selective undo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defginamethod clone ((cmd command))
  (description "clone a command")
  (let ((new (make-instance (gina::clos-class-of cmd))))
    (loop for (slot-name slot-value) in (slots cmd)
          do (when (slot-boundp cmd slot-name)
                (setf (slot-value new slot-name)
		      (slot-value cmd slot-name))))
    new))

(defginamethod save-initial-state ((cmd command))
  (description "collect any state information before selective redo")
  )

(defginamethod save-final-state ((cmd command))
  (description "fix outcome status before selective undo")
  )

(defmethod referred-command ((cmd command))
  "to be overridden by reverter"
  cmd)

(defmethod selective-undo ((cmd command))
  (and (selective-undo-possible cmd)
       (let ((cmd-copy (clone (referred-command cmd))))
	 (if (reverse-undo cmd)
	     (save-initial-state cmd-copy)
             (save-final-state cmd-copy))
         (make-command (document cmd) :reverse-undo (not (reverse-undo cmd))
                :class 'reverter
                :initargs (list :cmd-to-revert cmd-copy)))))

(defmethod selective-redo ((cmd command))
  (and (selective-redo-possible cmd)
       (let ((cmd-copy (clone (referred-command cmd))))
         (if (reverse-undo cmd)
             (save-final-state cmd-copy)
             (save-initial-state cmd-copy))
         (make-command (document cmd) :reverse-undo (reverse-undo cmd)
                :class 'reverter
                :initargs (list :cmd-to-revert cmd-copy)))))

(defginamethod selective-undo-possible ((cmd command))
  (description "whether selective undo is possible")
  nil)

(defginamethod selective-redo-possible ((cmd command))
  (description "whether selective redo is possible")
  nil)

(defginamethod command-description ((cmd command))
  (description "the description of the command for the undo box")
  (let ((obj (object-description cmd)))
    (if obj
        (concatenate 'simple-string (name cmd) " " obj)
        (name cmd))))

(defginamethod object-description ((cmd command))
  "override to return a string describing the target objects, if any"
  nil)

(defginamethod reverse-name ((cmd command))
  (description "the name of the reversed command")
  (concatenate 'simple-string "Reverse " (name cmd)))

(defginamethod reverse-command-description ((cmd command))
  (description "the description of the reverse command")
  (concatenate 'simple-string "Reverse " (command-description cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class reverter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass reverter (command)
  ((cmd-to-revert  :accessor cmd-to-revert :initarg :cmd-to-revert)))

(defmethod class-identifier ((cmd reverter))
  (class-identifier (cmd-to-revert cmd)))

(defmethod referred-command ((cmd reverter))
  (cmd-to-revert cmd))

(defmethod react-to-change ((doc document) (cmd reverter) reason batch)
  (react-to-change doc (cmd-to-revert cmd) reason batch))

(defmethod name ((cmd reverter))
  (if (reverse-undo cmd)
      (reverse-name (cmd-to-revert cmd))
      (name (cmd-to-revert cmd))))

(defmethod executable ((cmd reverter))
  (executable (cmd-to-revert cmd)))

(defmethod doit ((cmd reverter))
  (redoit cmd))

(defmethod scroll-before-undo ((cmd reverter))
  (scroll-before-undo (cmd-to-revert cmd)))

(defmethod scroll-before-redo ((cmd reverter))
  (scroll-before-redo (cmd-to-revert cmd)))

(defmethod undoit ((cmd reverter))
  (if (reverse-undo cmd)
      (redoit (cmd-to-revert cmd))
      (undoit (cmd-to-revert cmd))))

(defmethod redoit ((cmd reverter))
  (if (reverse-undo cmd)
      (undoit (cmd-to-revert cmd))
      (redoit (cmd-to-revert cmd))))

(defmethod commit ((cmd reverter) undone)
  (commit (cmd-to-revert cmd) undone))

(defmethod selective-undo-possible ((cmd reverter))
  (selective-redo-possible (cmd-to-revert cmd)))

(defmethod selective-redo-possible ((cmd reverter))
  (selective-undo-possible (cmd-to-revert cmd)))

(defmethod command-description ((cmd reverter))
  (if (reverse-undo cmd)
      (reverse-command-description (cmd-to-revert cmd))
      (command-description (cmd-to-revert cmd))))

;; animate redo should animate the selective undo dialog box!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; selective undo for predefined commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod installed ((obj view-object))
  (parent-view obj))

;; have to export view-for
(defginamethod view-for (obj)
  (description "backward pointer into app data")
  obj)

;; ... and objects. That should definitely not belong here!!
(defginamethod objects ((doc document))
  (description "horrible hack for now")
  nil)

;; should be removed in the future
;; not possible because only selection is stored    
(defmethod new-view-object ((obj view-object) view) ;; should be view-object
  (if (installed obj)
      obj
    (progn
      (format t "NOT found:~%")
      (loop for new in (view-objects view)
	    when (eq (view-for new) (view-for obj))
	      do (return new)))))

;;; object-mover

(defmethod selective-undo-possible ((cmd object-mover))
  ;;(installed (move-object cmd))
  (and (mouse-already-moved cmd)  ;; else only a selection
       (member (view-for (move-object cmd)) (objects (document cmd)))))

(defmethod selective-redo-possible ((cmd object-mover))
  ;;(installed (move-object cmd))
  (and (mouse-already-moved cmd)
       (member (view-for (move-object cmd)) (objects (document cmd)))))

(defmethod save-initial-state ((cmd object-mover))
  (setf (move-object cmd) (new-view-object (move-object cmd) (view cmd)))
  (setf (start-x cmd) (+ (x-pos (move-object cmd)) (x-off cmd)))
  (setf (start-y cmd) (+ (y-pos (move-object cmd)) (y-off cmd))))

(defmethod save-final-state ((cmd object-mover))
  ;(setf (move-object cmd) (new-view-object (move-object cmd) (view cmd)))
  (setf (new-x cmd) (x-pos (move-object cmd)))
  (setf (new-y cmd) (y-pos (move-object cmd))))

;; the move object must even be regenerated in the normal case because a
;; different view object is created by the tap!
(defmethod undoit :before ((cmd object-mover))
  (setf (move-object cmd) (new-view-object (move-object cmd) (view cmd))))

(defmethod redoit :before ((cmd object-mover))
  (setf (move-object cmd) (new-view-object (move-object cmd) (view cmd))))

(defmethod command-description ((cmd object-mover))
  (if (mouse-already-moved cmd)
  (format nil "Move ~a from ~d,~d to ~d,~d" 
	  (representation-in-history (view-for (move-object cmd)))
          (- (start-x cmd) (x-off cmd)) (- (start-y cmd) (y-off cmd))
          (new-x cmd) (new-y cmd)) "Select One"))

(defmethod reverse-command-description ((cmd object-mover))
  (if (mouse-already-moved cmd)
  (format nil "Move ~a from ~d,~d to ~d,~d" 
	  (representation-in-history (view-for (move-object cmd)))
          (new-x cmd) (new-y cmd) 
          (- (start-x cmd) (x-off cmd)) (- (start-y cmd) (y-off cmd)))
  "Select One"))

;;; object-resizer

(defmethod selective-undo-possible ((cmd object-resizer))
  ;(installed (size-object cmd))
  (member (view-for (size-object cmd)) (objects (document cmd))))

(defmethod selective-redo-possible ((cmd object-resizer))
  ;(installed (size-object cmd))
  (member (view-for (size-object cmd)) (objects (document cmd))))

(defmethod save-initial-state ((cmd object-resizer))
  (setf (size-object cmd) (new-view-object (size-object cmd) (view cmd)))
  (setf (old-x cmd) (x-pos (size-object cmd)))
  (setf (old-y cmd) (y-pos (size-object cmd)))
  (setf (old-width cmd) (width (size-object cmd)))
  (setf (old-height cmd) (height (size-object cmd))))

(defmethod save-final-state ((cmd object-resizer))
  (setf (size-object cmd) (new-view-object (size-object cmd) (view cmd)))
  (setf (new-x cmd) (x-pos (size-object cmd)))
  (setf (new-y cmd) (y-pos (size-object cmd)))
  (setf (new-width cmd) (width (size-object cmd)))
  (setf (new-height cmd) (height (size-object cmd))))

(defmethod undoit :before ((cmd object-resizer))
  (setf (size-object cmd) (new-view-object (size-object cmd) (view cmd))))

(defmethod redoit :before ((cmd object-resizer))
  (setf (size-object cmd) (new-view-object (size-object cmd) (view cmd))))

(defmethod command-description ((cmd object-resizer))
  (format nil "Resize ~a from ~dx~d to ~dx~d" 
	  (representation-in-history (view-for (size-object cmd)))
          (old-width cmd) (old-height cmd)
          (new-width cmd) (new-height cmd)))

(defmethod reverse-command-description ((cmd object-resizer))
  (format nil "Resize ~a from ~dx~d to ~dx~d" 
	  (representation-in-history (view-for (size-object cmd)))
          (new-width cmd) (new-height cmd)
          (old-width cmd) (old-height cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class undo-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defginaclass undo-box (modeless-dialog-box)
  (description "a box for the selective undo dialog")
  ((item-list          :accessor item-list)
   (command-selection  :accessor command-selection)
   (filter-toggle      :accessor filter-toggle)
   (filter-text        :accessor filter-text)
   (object-dragger     :accessor object-dragger)
   (left-button        :accessor left-button)
   (left-arrow         :accessor left-arrow)
   (right-button       :accessor right-button)
   (right-arrow        :accessor right-arrow)
   (current-label      :accessor current-label)
   (partial-label      :accessor partial-label)
   (all-toggle         :accessor all-toggle)
   (button-separator   :accessor button-separator)
   (undo-button        :accessor undo-button)
   (dismiss-button     :accessor dismiss-button)
   (warning-label      :accessor warning-label)
   (num-partial        :accessor num-partial :initform 0)
   (current-partial    :accessor current-partial :initform 0)
   (in-partial-move    :accessor in-partial-move :initform nil)))

(defginafun make-undo-box (document &aux box)
  (description "construct an undo-box")
  (setq box (make-modeless-dialog-box "Selective Undo"
			  :document document
			  :resize t
			  :class 'undo-box
			  :motif-resources '(:vertical-spacing 10
					     :horizontal-spacing 10)))
  (let ((frame nil)
	(rc nil)
	(header (make-label box "Undoable commands:")))
    (make-tap box nil #'update-undo-box)
    (setf (command-selection box)
          (make-scrollable-selection-list box nil :visible-item-count 6
                        :selection-policy :extended))
    (setf (filter-toggle box)
          (make-toggle-button box "Filter"))
    (setf (filter-text box)
          (make-text box :columns 40))
    (setf (object-dragger box)
          (make-drag-label box document "magnet" :cursor-mask "magnet-mask"))
    (setf (partial-label box)
          (make-label box "Partial History"))
    (setq frame (make-frame box :margin-width 10 :margin-height 10
			    :shadow-type :etched-in))
    (setq rc (make-row-column frame :orientation :horizontal 
			      :entry-alignment :none))
    (setf (left-button box)
          (make-push-button rc 
		"                              " :alignment :beginning
		:recompute-size nil))
    (setf (left-arrow box)
          (make-label rc "<"))
    (setf (current-label box)
          (make-label (make-frame rc :shadow-type :etched-in) "   "
		 :alignment :center :recompute-size nil))
    (setf (right-arrow box)
          (make-label rc ">"))
    (setf (right-button box)
          (make-push-button rc
		">                              " :alignment :end
		:recompute-size nil))
    (setf (all-toggle box)
          (make-toggle-button box "Show Disabled"))
    (setf (button-separator box)
          (make-separator box :orientation :horizontal))
    (setf (undo-button box)
          (make-push-button box "Undo"))
    (setf (dismiss-button box)
          (make-push-button box "Cancel"))
    (setf (warning-label box)
          (make-label box " "))
    (setf (map-callback box)
          (make-callback #'update-undo-command-list
			 document box))
    (setf (value-changed-callback (command-selection box))
          (make-callback #'follow-command-selection box document))
    (setf (value-changed-callback (filter-toggle box))
          (make-callback #'switch-filtering box document))
    (setf (value-changed-callback (filter-text box))
          (make-callback #'reset-filter-toggle box))
    (setf (activate-callback (object-dragger box))
          (make-callback #'make-undo-object-grabber document box 
			 (object-dragger box)))
    (setf (activate-callback (left-button box))
          (make-callback #'move-partial-history box document -1))
    (setf (activate-callback (right-button box))
          (make-callback #'move-partial-history box document +1))
    (setf (value-changed-callback (all-toggle box))
          (make-callback #'switch-filtering box document))
    (setf (activate-callback (dismiss-button box))
          (make-callback #'execute-cancel box document))
    (setf (activate-callback (undo-button box))
          (make-callback #'execute-undo document 
			 (command-selection box) box))
    (define-form-constraint header
          :top-attachment :form :left-attachment :form)
    (define-form-constraint (command-selection box)
	  :top-attachment :widget :top-widget header
	  :left-attachment :form :right-attachment :form
          :bottom-attachment :widget :bottom-widget (filter-text box))
    (define-form-constraint (filter-toggle box)
	  :left-attachment :form :top-attachment :none
          :bottom-attachment :widget :bottom-widget (partial-label box))
    (define-form-constraint (filter-text box)
          :left-attachment :widget :left-widget (filter-toggle box)
	  :top-attachment :none
          :right-attachment :widget :right-widget (object-dragger box)
          :bottom-attachment :widget :bottom-widget (partial-label box))
    (define-form-constraint (object-dragger box)
          :left-attachment :none :top-attachment :none
          :right-attachment :form
          :bottom-attachment :widget :bottom-widget (partial-label box))
    (define-form-constraint (partial-label box)
          :left-attachment :opposite-widget :left-widget frame :left-offset 15
          :bottom-attachment :widget :bottom-widget frame :bottom-offset -10)
    (define-form-constraint frame
	  :left-attachment :form
	  :top-attachment :none
          :bottom-attachment :widget :bottom-widget (button-separator box))
    (define-form-constraint (all-toggle box)
          :left-attachment :widget :left-widget frame
          :top-attachment :opposite-widget :top-widget frame :top-offset 0
	  :bottom-attachment :widget :bottom-widget (button-separator box))
    (define-form-constraint (button-separator box)
          :top-attachment :none :left-attachment :form
          :right-attachment :form
          :bottom-attachment :widget :bottom-widget (undo-button box))
    (define-form-constraint (undo-button box)
          :top-attachment :none :left-attachment :form 
          :bottom-attachment :form)
    (define-form-constraint (dismiss-button box)
          :left-attachment :widget :left-widget (undo-button box)
	  :top-attachment :none :bottom-attachment :form)
    (define-form-constraint (warning-label box)
          :left-attachment :widget :left-widget (dismiss-button box)
          :top-attachment :none :bottom-attachment :form))
  box)

(defmethod reset-filter-toggle ((box undo-box) &rest args)
  (declare (ignore args))
  (when (value (filter-toggle box))
    (setf (value (filter-toggle box)) nil)))

(defmethod switch-filtering ((box undo-box) document &rest args)
  (declare (ignore args))
  (update-undo-command-list document box))

(defmethod restore-initial-state ((box undo-box) document)
  (unless (string= (slot-value (warning-label box) 'label-string) " ")
    (setf (label-string (warning-label box)) " "))
  (unless (= (num-partial box) (current-partial box))
    ;(format t "Restoring ~d to ~d~%" (current-partial box) (num-partial box))
    (setf (in-partial-move box) t)
    (loop repeat (- (num-partial box) (current-partial box))
          do (undo document))
    ;; remove superfluous reverse commands
    (setf (redo-commands document) nil)
    (command-executed document)
    (setf (in-partial-move box) nil)))

(defmethod follow-command-selection ((box undo-box) document 
				     new-value old-value)
  (declare (ignore old-value))
  (restore-initial-state box document)
  (setf (num-partial box) (length new-value))
  (setf (current-partial box) (length new-value))
  (set-sensitivity (partial-label box) new-value)
  (set-sensitivity (undo-button box) new-value)
  (move-partial-history box document 0))

(defmethod move-partial-history ((box undo-box) document increment)
  "returns nil when not successful"
  (incf (current-partial box) increment)
  (when (> (current-partial box) (num-partial box))
    (setf (current-partial box) (num-partial box))
    (return-from move-partial-history))
  (when (< (current-partial box) 0)
    (setf (current-partial box) 0)
    (return-from move-partial-history))
  (setf (in-partial-move box) t)
  (when (= increment -1)
    (unless (selective-undo (current-partial-command box +1))
      (set-sensitivity (left-button box) nil)
      (set-sensitivity (undo-button box) nil)
      (setf (label-string (warning-label box))
            (format nil "Cannot undo ~a." 
		    (name (current-partial-command box +1))))
      (incf (current-partial box))
      (setf (in-partial-move box) nil)
      (return-from move-partial-history)))
  (when (= increment 1)
    (undo document))
  (setf (in-partial-move box) nil)
  (setf (label-string (left-button box))
        (if (zerop (current-partial box))
	    ""
	    (name (current-partial-command box 0))))
  (set-sensitivity (left-button box) (not (zerop (current-partial box))))
  (set-sensitivity (left-arrow box) (not (zerop (current-partial box))))
  (setf (label-string (right-button box)) 
        (if (= (current-partial box) (num-partial box))
	    ""
	    (name (current-partial-command box +1))))
  (set-sensitivity (right-button box) 
		   (not (= (current-partial box) (num-partial box))))
  (set-sensitivity (right-arrow box) 
		   (not (= (current-partial box) (num-partial box))))
  (setf (label-string (current-label box))
    (if (zerop (num-partial box))
        "   "
        (format nil "~3d" (current-partial box)))))

(defmethod current-partial-command ((box undo-box) inc)
  "index is from 1"
  (nth (- (num-partial box) (current-partial box) inc) 
       (value (command-selection box))))

(defmethod update-undo-box ((box undo-box) cmd)
  "the tap to update the box"
  (declare (ignore cmd))
  (update-undo-command-list (document box) box))

(defmethod update-undo-command-list ((doc document) box)
  (unless (in-partial-move box)
    (follow-command-selection box doc nil nil)
    (setf (item-list box)
          (loop for cmd in (undo-commands doc)
                for i from (length (undo-commands doc)) by -1
                when (selective-undo-possible cmd)
                collect (list (format nil "~3d ~a" i (command-description cmd))
			      cmd)
                when (and (value (all-toggle box))
			  (not (selective-undo-possible cmd)))
                collect (list (format nil "~3d [X] ~a" i
					   (command-description cmd))
			      cmd)))
    (if (and (value (filter-toggle box))
             (not (string= (value (filter-text box)) "")))
        (set-item-list (command-selection box)
              (loop for item in (item-list box)
                    when (search (value (filter-text box)) (first item))
		    collect item))
        (set-item-list (command-selection box) (item-list box)))))

(defmethod execute-undo ((doc document) selection box)
  (when (value selection)
    (loop until (not (move-partial-history box doc -1)))
    (when (zerop (current-partial box))
      (setf (current-partial box) (num-partial box)) ;; prevent restoring
      (update-undo-command-list doc box))))

(defmethod execute-cancel ((box undo-box) document)
  (restore-initial-state box document)
  (pop-down box))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class undo-object-grabber
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass undo-object-grabber (drag-command)
 (;;overrides
  (name    :initform "Grab Object"  :allocation :class)
  (causes-change :allocation :class :initform nil)
  (undoable      :allocation :class :initform nil)
  ;; new slots
  (object-hit    :accessor object-hit   :initform nil)
  (undo-box      :accessor undo-box     :initarg :undo-box)  
  ))

(defun make-undo-object-grabber (document box widget start-x start-y
                               &key (class 'undo-object-grabber)
                                    (initargs nil)
                               &aux cmd)
  (setq cmd 
        (make-drag-command document widget start-x start-y 
                           (list (list (main-view document) nil))
                                 :cursor (cursor widget)
                                 :class class 
				 :initargs (more-initargs :undo-box box)))
  cmd)

(defmethod track-target ((cmd undo-object-grabber) view view-x view-y value)
  "determine object that is hit"
  (declare (ignore value))
  (setf (object-hit cmd)
        (and view 
	     (loop for obj in (view-objects view)
                   when (point-inside obj view-x view-y)
                     do (return obj)))))

(defmethod draw-target-feedback ((cmd undo-object-grabber) view view-x view-y
                                 value &key (clear nil))
  (declare (ignore view view-x view-y value clear))
  (when (object-hit cmd)
      (let* ((obj (object-hit cmd))
	     (parent-view (main-view (document cmd))))
        (xlib:with-gcontext ((gcontext parent-view) 
                              :function boole-xor
			      :foreground (xor-foreground parent-view)
                              :line-style :dash)
          (draw-rectangle parent-view (x-pos obj) (y-pos obj) 
			  (width obj) (height obj))
          (draw-rectangle parent-view (1+ (x-pos obj)) (1+ (y-pos obj)) 
			  (- (width obj) 2) (- (height obj) 2))))))

(defmethod executable ((cmd undo-object-grabber))
  (object-hit cmd))

(defmethod insert-at-cursor ((widget text) new-text)
  (multiple-value-bind (from to really)
      (xtk:text-get-selection-position (widget-id widget))
    (if really
      (xtk:text-replace (widget-id widget) from to new-text)
      (xtk:text-insert (widget-id widget)
                       (first (get-motif-resources widget :cursor-position))
                       new-text))))

(defmethod doit ((cmd undo-object-grabber))
  (insert-at-cursor (filter-text (undo-box cmd))
                    (representation-in-history (view-for (object-hit cmd)))))

(defmethod representation-in-history (obj)
  (format nil "~s" obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; skipping commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod first-redo-command ((doc document))
  (loop for cmd in (redo-commands doc)
        when (causes-change cmd)
          do (return cmd)))

(defmethod first-undo-command ((doc document))
  (loop for cmd in (undo-commands doc)
        when (causes-change cmd)
          do (return cmd)))

(defmethod update-new-undo-redo-menu-entries ((doc document))
  (with-slots (undo-commands redo-commands history-scroller) doc
    ;; update undo menu entry
    (let ((command-to-undo (first-undo-command doc)))
      (setf (label-string (undo-menu-entry doc))
	    (if command-to-undo
	        (format nil "Undo ~a" (name command-to-undo))
	        "Undo"))
      (setf (sensitive (undo-menu-entry doc)) (when command-to-undo t)))

    ;; update redo menu entry
    (let ((command-to-redo (first-redo-command doc)))
      (setf (label-string (redo-menu-entry doc))
	    (if command-to-redo
	        (format nil "Redo ~a" (name command-to-redo))
	        "Redo"))
      (setf (sensitive (redo-menu-entry doc)) (when command-to-redo t)))))

(defmethod new-undo ((doc document))
  (loop while (and (undo-commands doc)
                   (not (causes-change (first (undo-commands doc)))))
        do (undo doc :update-history-scroller nil))
  (when (undo-commands doc)
    (undo doc)))

(defmethod new-redo ((doc document))
  (loop while (and (redo-commands doc)
                   (not (causes-change (first (redo-commands doc)))))
        do (redo doc :update-history-scroller nil))
  (loop for next-command = (first (rest (redo-commands doc)))
        for continue = (and next-command
			    (not (causes-change next-command)))
        while (redo-commands doc)
        do (redo doc :update-history-scroller (not continue))
        until (not continue)))
