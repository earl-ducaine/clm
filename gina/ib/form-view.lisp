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

(setq *sccs-id* "@(#)form-view.lisp	1.14	11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class feedback-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass feedback-view (view)
  ()
  (:documentation "a view showing feedback strings"))

(defun make-feedback-view (parent document width height)
  (make-view parent :width width :height height
	     :document document :class 'feedback-view))

(defmethod button-press ((view feedback-view) code repetition x y)
  "react to button-press event in the window"
  (declare (ignore code repetition x y)))

(defmethod determine-window-id :after ((view feedback-view))
  "set font in gcontext"
  (setf (xlib:gcontext-font (gcontext view)) "9x15"))

(defmethod draw-feedback-string ((view feedback-view) string)
  (unless (eql (width view) 200) (setf (width view) 200))
  (draw-glyphs view 30 15 string))

(defmethod clear-feedback-string ((view feedback-view))
  (xlib:clear-area (x-window view) :exposures-p nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class form-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass form-view (view)
  ((form-plate   :accessor form-plate    :initarg :form-plate)
   (form-info    :accessor form-info     :initarg :form-info)
   (zig-zag-mask :accessor zig-zag-mask  :initform nil)
   (line-mask    :accessor line-mask     :initform nil))
  (:documentation "a view editing form constraints"))

(defun make-form-view (parent info &aux new-view)
  (setf new-view (make-view parent 
			    :width (width info) :height (height info)
			    :document (document info) 
			    :class 'form-view
                            :motif-resources '(:mapped-when-managed nil)
                            :initargs (list :form-info info
                                            :form-plate (plate info))))
  new-view)

(defmethod button-press ((view form-view) code repetition x y)
  "react to button-press event in the window"
  (declare (ignore repetition x y))
  (select-plate (form-plate view) code))

(defmethod draw :before ((view form-view) count x y width height)
  "create masks on the first expose"
  (declare (ignore count x y width height))
  (unless (zig-zag-mask view)
    (create-bitmasks view)))

(defmethod draw ((view form-view) count x y width height)
  "draw lines for all rulers in the view"
  (declare (ignore x y width height))
  (when (zerop count)
    (xlib:with-gcontext ((gcontext view) :line-style :dash :dashes 2)
      (loop for obj in (view-objects view)
        when (not (is-form-child-plate obj))
        do ;; draw a ruler
           (case (side obj)
              ((:left :right)
                  (draw-line view (pixel-pos obj) 0 
                                  (pixel-pos obj) (height view)))
              ((:top :bottom)
                  (draw-line view 0 (pixel-pos obj) 
                                  (width view) (pixel-pos obj))))))))

(defmethod create-bitmasks ((view form-view) &aux gc)
  "create pixmaps to be used as stipple patterns"
  (declare (special *application* *display*))
  (setf (zig-zag-mask view)
        (xlib:create-pixmap :width 16 :height 16 :depth 1
                            :drawable (x-window view)))
  (setq gc (xlib:create-gcontext :background (xlib:screen-white-pixel
						       (screen *application*))
				 :foreground (xlib:screen-black-pixel
						       (screen *application*))
                                 :drawable (zig-zag-mask view)))
  (xlib:with-gcontext (gc :foreground (xlib:screen-white-pixel
						       (screen *application*)))
    (xlib:draw-rectangle (zig-zag-mask view) gc 0 0 16 16 t))
  (xlib:draw-line (zig-zag-mask view) gc 0 0 15 15)
  (xlib:draw-line (zig-zag-mask view) gc 8 0 15 7)
  (xlib:draw-line (zig-zag-mask view) gc 0 8 7 15)
  (xlib:draw-line (zig-zag-mask view) gc 15 1 1 15)
  (xlib:draw-line (zig-zag-mask view) gc 9 15 15 9)
  (xlib:draw-line (zig-zag-mask view) gc 8 0 0 8)
  (setf (line-mask view)
        (xlib:create-pixmap :width 4 :height 4 :depth 1
                            :drawable (x-window view)))
  (xlib:with-gcontext (gc :foreground (xlib:screen-white-pixel
						       (screen *application*)))
    (xlib:draw-rectangle (line-mask view) gc 0 0 4 4 t))
  (xlib:draw-line (line-mask view) gc 0 0 3 3)
  (xlib:draw-line (line-mask view) gc 2 0 3 1)
  (xlib:draw-line (line-mask view) gc 0 2 1 3)
  (xlib:display-force-output *display*))

(defmethod fill-form-view ((view form-view))
  "update the form view according to the current status of the children"
  (setf (view-objects view) nil)
  (let ((child-plates
           (loop for child in (children (form-info view))
                 do (update-slots (gina-widget (plate child)))
                 collect (make-form-child-plate child))))
    (loop for plate in child-plates
          do (install plate view (real-x-pos plate) (real-y-pos plate)
                      :redraw nil))
    (create-rulers view child-plates :left nil)
    (create-rulers view child-plates :top nil)
    (create-rulers view child-plates :right t)
    (create-rulers view child-plates :bottom t)))

(defmethod create-rulers ((view form-view) child-plates side is-other-side
                          &aux list-of-rulers ruler attachment)
  "create a set of rulers for one side from the form constraints"
  (declare (ignore is-other-side))
  (loop for child in child-plates
        when (is-form-child-plate child)
        do (setq attachment (side-attachment (info child) side))
           (case (atype attachment)
             ((:opposite-widget)
                 (when (in-line (info child) (widget attachment) side)
                     ;;both in ruler
                     (setq ruler 
                           (find-ruler list-of-rulers 
                                 (find-child-plate attachment child-plates)))
                     (unless ruler
                        (setq ruler 
                              (make-ruler side 
                                 (find-child-plate attachment child-plates)))
                        (push ruler list-of-rulers))
                     (unless (member child (members ruler))
                        (setf (members ruler)
                              (append (members ruler) (list child))))
                     ))
             ((:none) ) ;; not in ruler
             ((:form :opposite-form :position)
                 (let ((pos (if (eql (atype attachment) :position)
                                (form-pos attachment)
                                (offset attachment))))
                   (setq ruler 
                         (find-ruler-pos list-of-rulers (atype attachment) pos))
                   (unless ruler
                        (setq ruler (make-ruler side child 
                                                :atype (atype attachment)))
                        (setf (pos ruler) pos)
                        (push ruler list-of-rulers))
                   (unless (member child (members ruler))
                        (setf (members ruler)
                              (append (members ruler) (list child))))
                     ))
             ))
  ;;remove double rulers
  ;;necessary only for old files
  (loop for ruler in list-of-rulers
        when (leader ruler)
        do (loop for r in list-of-rulers
                 when (and (not (eql r ruler))
                           (member (leader ruler) (members r)))
                 do (setf (members r) (append (members r) (members ruler)))
                    (setq list-of-rulers (remove ruler list-of-rulers))))
  ;;consolidate and install the rulers
  (loop for ruler in list-of-rulers
        ;unless (and (= (length (members ruler)) 1)
        ;            (eql (atype (side-attachment (info (leader ruler)) side)) 
        ;                 :widget))
        do (consolidate-and-install ruler view))
  )

(defun in-line (info other-info side)
  "check whether two widgets are aligned on a side"
  (case side
    ((:left :right)
       (= (real-x-pos info) 
          (real-x-pos other-info)))
    ((:top :bottom)
       (= (real-y-pos info)
          (real-y-pos other-info)))))

(defun find-child-plate (attachment plates)
  "find the form plate belonging to a :widget attachment"
  (loop for plate in plates
        when (eql (info plate) (widget attachment))
        do (return-from find-child-plate plate)))

(defun find-ruler (list-of-rulers leader)
  "find whether a ruler with a given leader exists"
  (loop for ruler in list-of-rulers
        when (eql (leader ruler) leader)
        do (return-from find-ruler ruler))
  nil)

(defun find-ruler-pos (list-of-rulers atype pos)
  "find whether a ruler with a given position exists"
  (loop for ruler in list-of-rulers
        when (and (eql (atype ruler) atype)
                  (eql (pos ruler) pos))
        do (return-from find-ruler-pos ruler))
  nil)

(defmethod update-geometry ((view form-view))
  (loop for plate in (view-objects view) 
        when (is-form-child-plate plate)
        do (if (parent-info (info plate))
             (progn
               (update-slots (gina-widget (plate (info plate))))
                 (let ((new-x (real-x-pos plate))
                       (new-y (real-y-pos plate))
	               (new-width (real-width plate))
	               (new-height (real-height plate)))
	           (when (or (/= new-x (x-pos plate)) (/= new-y (y-pos plate)))
	             (move plate new-x new-y :redraw nil))
	           (when (or (/= new-width (width plate)) 
                             (/= new-height (height plate)))
	             (resize plate new-width new-height :redraw nil))))
	     (deinstall plate)))
  (loop for plate in (view-objects view) 
        unless (is-form-child-plate plate)
        do (move plate (symbol-x-position plate) 
                       (symbol-y-position plate) :redraw nil))
  (force-redraw view))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; real widget geometry given an info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun real-x-pos (info) (x-pos (gina-widget (plate info))))
(defun real-y-pos (info) (y-pos (gina-widget (plate info))))
(defun real-width (info) (width (gina-widget (plate info))))
(defun real-height (info) (height (gina-widget (plate info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; side calculations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun horizontal (side)
  (or (eql side :top) (eql side :bottom)))

(defun topleft (side)
  (case side ((:left :right) :top)
             ((:top :bottom) :left)))

(defun bottomright (side)
  (case side ((:left :right) :bottom)
             ((:top :bottom) :right)))

(defun opposite (side)
  (case side ((:left) :right) ((:right) :left)
             ((:top) :bottom) ((:bottom) :top)))

(defun ortho (side)
  (case side ((:left) :bottom) ((:top) :left)
             ((:right) :top) ((:bottom) :right)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class form-child-plate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass form-child-plate (view-object)
  ((info         :accessor info         :initarg :info)
   (plate        :accessor plate        :initarg :plate)
   (left-ruler   :accessor left-ruler   :initform nil)
   (right-ruler  :accessor right-ruler  :initform nil)
   (top-ruler    :accessor top-ruler    :initform nil)
   (bottom-ruler :accessor bottom-ruler :initform nil)))

(defun make-form-child-plate (info)
  (make-view-object (width (gina-widget (plate info)))
                    (height (gina-widget (plate info)))
                    :class 'form-child-plate
                    :initargs (list :info info :plate (plate info))))

(defmethod draw ((plate form-child-plate) count x y width height)
  (declare (ignore x y width height))
  (when (zerop count)
    (let ((half-x (round (/ (width plate) 2)))
          (half-y (round (/ (height plate) 2)))
          (min-x (min 3 (round (/ (width plate) 2))))
          (min-y (min 3 (round (/ (height plate) 2)))))
      (draw-rectangle plate 0 0 (1- (width plate)) (1- (height plate)))
      ;; the widget name
      (xlib:with-gcontext ((gcontext (parent-view plate)) :font "5x8"
                :clip-mask (list (x-pos plate) (y-pos plate) 
                                 (width plate) (height plate)))
        (draw-glyphs plate 7 14 (widget-name (info plate))))
      ;; mark var member
      (xlib:with-gcontext ((gcontext (parent-view plate))
                :fill-style :stippled
                :stipple (line-mask (parent-view plate)))
        (when (is-var-member (left-ruler plate) plate)
           (draw-rectangle plate 0 0 7 (height plate) t))
        (when (is-var-member (right-ruler plate) plate)
           (draw-rectangle plate (- (width plate) 7) 0 7 (height plate) t))
        (when (is-var-member (top-ruler plate) plate)
           (draw-rectangle plate 0 0 (width plate) 7 t))
        (when (is-var-member (bottom-ruler plate) plate)
           (draw-rectangle plate 0 (- (height plate) 7) (width plate) 7 t)))
      ;; draw handles
      (draw-handle plate (- half-x min-x) 0 (+ min-x min-x) (+ min-y min-y)
		   :top)
      (draw-handle plate (- half-x min-x) (- (height plate) min-y min-y 1)
		   (+ min-x min-x) (+ min-y min-y) :bottom)
      (draw-handle plate 0 (- half-y min-y) (+ min-x min-x) (+ min-y min-y)
		   :left)
      (draw-handle plate (- (width plate) min-x min-x 1) (- half-y min-y)
		   (+ min-x min-x) (+ min-y min-y) :right)
      ;; draw leader bars
      (when (is-leader (top-ruler plate) plate)
          (draw-line plate 0 1 (1- (width plate)) 1))
      (when (is-leader (bottom-ruler plate) plate)
          (draw-line plate 0 (1- (height plate)) 
                           (1- (width plate)) (1- (height plate))))
      (when (is-leader (left-ruler plate) plate)
          (draw-line plate 1 0 1 (1- (height plate))))
      (when (is-leader (right-ruler plate) plate)
          (draw-line plate (1- (width plate)) 0
                           (1- (width plate)) (1- (height plate))))
   )))

(defun draw-handle (plate x y width height side)
  "draw one handle"
  (let ((this-side-fix (passive-member plate side))
        (other-side-fix (passive-member plate (opposite side))))
      (when (and this-side-fix other-side-fix (member side '(:right :bottom)))
         ;; draw pin
        (xlib:clear-area (x-window (parent-view plate)) 
               :x (+ (x-pos plate) x) :y (+ (y-pos plate) y) 
               :width width :height height :exposures-p nil)
        (draw-line plate x y (+ x width) (+ y height))
        (draw-line plate x (+ y height) (+ x width) y)
        (draw-rectangle plate x y width height))
      (when (and (not this-side-fix)
                 (or other-side-fix (member side '(:right :bottom))))
        ;(xlib:clear-area (x-window (parent-view plate)) 
        ;       :x (+ (x-pos plate) x) :y (+ (y-pos plate) y) 
        ;       :width (1+ width) :height (1+ height) :exposures-p nil)
        (case side
          ((:top)  (draw-rectangle plate x (1+ y) (1+ width) 2 t)
                   (draw-rectangle plate x (+ y height -2) (1+ width) 2 t))
          ((:left) (draw-rectangle plate (1+ x) y 2 (1+ height) t)
                   (draw-rectangle plate (+ x width -2) y 2 (1+ height) t))
          (t       (draw-rectangle plate x y width height))))))

(defmethod is-form-child-plate ((plate form-child-plate))
  "discriminate form children from other view-objects"
  t)

(defmethod is-form-child-plate ((plate view-object))
  "discriminate form children from other view-objects"
  nil)

(defun is-leader (ruler plate)
  (and ruler
       (eql plate (leader ruler))))

(defun is-var-member (ruler plate)
  (and ruler
       (eql plate (variable-member ruler))))

(defmethod button-press ((plate form-child-plate) code repetition x y)
  (declare (ignore code repetition))
  (let ((side (which-attach-handle plate x y)))
    (if side
        (if (passive-member plate side)
            (if (and (passive-member plate (opposite side))
                     (member side '(:right :bottom)))
                (make-detach-side-command plate x y side)
                (make-form-child-mover plate x y))
            (if (member side '(:top :left))
                (if (passive-member plate (opposite side))
                    (make-form-spacing-command plate x y side)
                    (make-form-child-mover plate x y))
                (make-new-ruler-command plate x y side)))
        (make-form-child-mover plate x y))))

(defmethod which-attach-handle ((plate form-child-plate) x y)
  "determine which of the four handles the position is in"
  (let ((half-x (round (/ (width plate) 2)))
        (half-y (round (/ (height plate) 2)))
        (min-x (min 3 (round (/ (width plate) 2))))
        (min-y (min 3 (round (/ (height plate) 2)))))
    (when (and (>= x (- (width plate) min-x min-x))
               (>= y (- half-y min-y))
               (<= y (+ half-y min-y)))
        (return-from which-attach-handle :right))
    (when (and (>= y (- (height plate) min-y min-y))
               (>= x (- half-x min-x))
               (<= x (+ half-x min-x)))
        (return-from which-attach-handle :bottom))
    (when (and (<= x (+ min-x min-x))
               (>= y (- half-y min-y))
               (<= y (+ half-y min-y)))
        (return-from which-attach-handle :left))
    (when (and (<= y (+ min-y min-y))
               (>= x (- half-x min-x))
               (<= x (+ half-x min-x)))
        (return-from which-attach-handle :top))
    nil))    

;(defmethod near-which-side ((plate form-child-plate) x y)
;  (let ((rel-x (round (/ (* x (height plate)) (width plate)))))
;    (if (< y (- (height plate) rel-x))
;        (if (< y rel-x) :top :left)
;        (if (< y rel-x) :right :bottom))))

(defun passive-member (plate side)
  "whether plate is constrained by a ruler on this side"
  (let ((ruler (case side
                   ((:left) (left-ruler plate))
                   ((:right) (right-ruler plate))
                   ((:top) (top-ruler plate))
                   ((:bottom) (bottom-ruler plate)))))
    (and ruler
         (not (eql (leader ruler) plate)))))

(defmethod remove-from-ruler ((plate form-child-plate) side)
  (let* ((ruler (case side ((:left) (left-ruler plate))
                          ((:right) (right-ruler plate))
			  ((:top) (top-ruler plate))
			  ((:bottom) (bottom-ruler plate)))))
    (when (not ruler)
       (format t "WARNING: attempt to remove from non-existing ruler~%"))
    (when (eql (leader ruler) plate)
       (format t "WARNING: attempt to remove leader~%"))
    (remove-plate ruler plate)
    (case side 
      ((:left) (setf (left-ruler plate) nil))
      ((:right) (setf (right-ruler plate) nil))
      ((:top) (setf (top-ruler plate) nil))
      ((:bottom) (setf (bottom-ruler plate) nil)))
    ruler))

(defmethod add-to-ruler ((plate form-child-plate) side ruler ruler-pos)
  (add-plate ruler plate ruler-pos)
  (case side 
      ((:left) (setf (left-ruler plate) ruler))
      ((:right) (setf (right-ruler plate) ruler))
      ((:top) (setf (top-ruler plate) ruler))
      ((:bottom) (setf (bottom-ruler plate) ruler))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class ruler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass ruler (view-object)
  ((side            :accessor side            :initarg :side)
   (leader          :accessor leader          :initarg :leader)
   (members         :accessor members         :initarg :members)
   (fixed-interval  :accessor fixed-interval  :initform nil)
   (variable-member :accessor variable-member :initform nil)
   (pos             :accessor pos)
   (atype           :accessor atype           :initarg :atype)
                    ;; one of :form :opposite-form :position :none
   ))

(defun make-ruler (side form-plate &key (atype :none))
  (make-view-object 9 9
                    :class 'ruler
                    :initargs (list :side side 
                                    :leader (when (eql atype :none) form-plate)
                                    :members (list form-plate)
                                    :atype atype)))

(defmethod pixel-pos ((ruler ruler))
  (let ((reference (first (members ruler))))
    (case (side ruler)
       ((:left)   (x-pos reference))
       ((:right)  (+ (x-pos reference) (width reference) -1))
       ((:top)    (y-pos reference))
       ((:bottom) (+ (y-pos reference) (height reference))))))

;(defun ruler-start-x (ruler member)
;  (- (real-x-pos member) (x-pos ruler)))
;
;(defun ruler-end-x (ruler member)
;  (- (+ (real-x-pos member) (real-width member)) (x-pos ruler)))
;
;(defun ruler-start-y (ruler member)
;  (- (real-y-pos member) (y-pos ruler)))
;
;(defun ruler-end-y (ruler member)
;  (- (+ (real-y-pos member) (real-height member)) (y-pos ruler)))

(defmethod draw ((ruler ruler) count x y width height)
  (declare (ignore x y width height))
  (when (zerop count)
    (case (which-dir (atype ruler) (side ruler))
      ((:left)
         (draw-lines ruler '(0 4 8 0 8 8 0 4) :fill-p t))
      ((:top)
         (draw-lines ruler '(4 0 8 8 0 8 4 0) :fill-p t))
      ((:right)
         (draw-lines ruler '(8 4 0 8 0 0 8 4) :fill-p t))
      ((:bottom)
         (draw-lines ruler '(4 8 0 0 8 0 4 8) :fill-p t))
      (t
         (draw-lines ruler '(4 0 8 4 4 8 0 4 4 0) :fill-p nil)))))

(defun which-dir (atype side)
  "direction of the type indicator"
  (case atype
    ((:form) side)
    ((:opposite-form)
       (case side 
          ((:left) :right) ((:top) :bottom) ((:right) :left) ((:bottom) :top)))
    ((:position)
       (case side
          ((:left) :top) ((:top) :left) ((:right) :top) ((:bottom) :left)))
    ((:none) nil)))

(defmethod button-press ((ruler ruler) code repetition x y)
  (declare (ignore code))
  (unless (eql (atype ruler) :none)
    (when (= repetition 1)
      (make-ruler-mover ruler x y))
    (when (> repetition 1)
      (make-ruler-type-changer (parent-view ruler) ruler))))

(defmethod consolidate-and-install ((ruler ruler) view)
  "complete and install an automatically generated ruler"
  ;; sort members
  (setf (members ruler)
        (sort (members ruler)
              (if (horizontal (side ruler))
                  #'(lambda (p1 p2) (< (real-x-pos p1) (real-x-pos p2)))
                  #'(lambda (p1 p2) (< (real-y-pos p1) (real-y-pos p2))))))
  ;; register ruler in members
  (loop for plate in (members ruler)
        do (case (side ruler)
              ((:left)   (setf (left-ruler plate) ruler))
              ((:right)  (setf (right-ruler plate) ruler))
              ((:top)    (setf (top-ruler plate) ruler))
              ((:bottom) (setf (bottom-ruler plate) ruler))))
  ;; find variable member
  (loop for plate in (members ruler)
        for one-attach = 
            (side-attachment (info plate) (topleft (side ruler)))
        for other-attach = 
            (side-attachment (info plate) (bottomright (side ruler)))
        unless (or (eql (atype one-attach) :none)
                   (eql (atype other-attach) :none))
        do (setf (variable-member ruler) plate))
  ;; install in view
  (install ruler view (symbol-x-position ruler) (symbol-y-position ruler)
	   :redraw nil))

(defmethod symbol-x-position ((ruler ruler))
  (if (horizontal (side ruler))
      0
      (- (pixel-pos ruler)
         (case (which-dir (atype ruler) (side ruler)) 
             ((:right) 8) ((:top) 4) ((nil) 4) (t 0)))))

(defmethod symbol-y-position ((ruler ruler))
  (if (horizontal (side ruler))
      (- (pixel-pos ruler)
         (case (which-dir (atype ruler) (side ruler))
             ((:bottom) 8) ((:left) 4) ((nil) 4) (t 0)))
      0))

(defmethod propagate-ruler-changes ((ruler ruler))
  "propagate changes in ruler pos and size to attachments of infos"
  (loop for plate in (members ruler)
        for attachment = (side-attachment (info plate) (side ruler))
        do (if (eql (atype ruler) :position)
               (progn
                 (setf (form-pos attachment) (pos ruler))
                 (setf (offset attachment) 0))
               (setf (offset attachment) (pos ruler)))
           (setf (atype attachment) (atype ruler))
           (update-constraints (constraints (info plate)) (plate (info plate))))
  (adapt-to-widget-size (form-plate (parent-view ruler))))

(defmethod remove-plate ((ruler ruler) plate)
  "reconstruct proper attachments along the ruler"
  (let* ((plate-pos (position plate (members ruler)))
         (pred      (when (> plate-pos 0) 
		      (first (subseq (members ruler) 
				     (1- plate-pos) plate-pos))))
         (succ      (when (< plate-pos (1- (length (members ruler))))
		      (first (subseq (members ruler) 
				     (1+ plate-pos) (+ plate-pos 2)))))
         (pred-attachment 
                    (when pred 
		      (side-attachment (info pred) (bottomright (side ruler)))))
         (succ-attachment 
                    (when succ 
		      (side-attachment (info succ) (topleft (side ruler))))))
    ;; update ruler structure
    (setf (members ruler) (remove plate (members ruler)))
    (when (eql (variable-member ruler) plate)
      (setf (variable-member ruler) nil))
    ;; update attachments
    (when pred
      (when (eql (atype pred-attachment) :widget)
        (if succ
            (progn
              (setf (widget pred-attachment) (info succ))
              (setf (offset pred-attachment)
		    (if (horizontal (side ruler))
                        (- (+ (real-x-pos pred) (real-width pred) -1)
                           (real-x-pos succ))
                        (- (+ (real-y-pos pred) (real-height pred) -1)
                           (real-y-pos succ)))))
            (progn
              (setf (atype pred-attachment) :form)
              (setf (offset pred-attachment)
		    (if (horizontal (side ruler))
			(- (width (parent-view ruler))
                           (+ (real-x-pos pred) (real-width pred)))
		        (- (height (parent-view ruler))
			   (+ (real-y-pos pred) (real-height pred)))))))
	(update-constraints (constraints (info pred)) (plate (info pred)))))
    (when succ
      (when (or (eql (atype succ-attachment) :widget)
                ;; a preliminary solution to prevent crashes
                (eql (variable-member ruler) plate))
        (when (eql (variable-member ruler) plate)
          (setf (variable-member ruler) succ))   ;; preliminary!
        (if pred
            (progn
              (setf (widget succ-attachment) (info pred))
              (setf (offset succ-attachment)
                    (if (horizontal (side ruler))
                        (- (+ (real-x-pos pred) (real-width pred) -1)
                           (real-x-pos succ))
                        (- (+ (real-y-pos pred) (real-height pred) -1)
                           (real-y-pos succ)))))
            (progn
	      (setf (atype succ-attachment) :form)
              (setf (offset succ-attachment)
                    (if (horizontal (side ruler))
			(real-x-pos succ)
		        (real-y-pos succ)))))
	(update-constraints (constraints (info succ)) (plate (info succ)))))))

(defmethod add-plate ((ruler ruler) plate plate-pos &aux insert-pos)
  (setq insert-pos
        (loop for rplate in (members ruler)
              for index from 0
              when (> (if (horizontal (side ruler)) (x-pos plate) 
			                            (y-pos plate))
                      (if (horizontal (side ruler)) (x-pos rplate) 
			                            (y-pos rplate)))
                do (return index)))
  (unless insert-pos (setq insert-pos 0))
  (setf (members ruler)
        (append (subseq (members ruler) 0 insert-pos)
                (list plate)
                (subseq (members ruler) insert-pos)))
  (let* ((pred      (when (> insert-pos 0) 
		      (first (subseq (members ruler) 
				     (1- insert-pos) insert-pos))))
         (succ      (when (< insert-pos (1- (length (members ruler))))
		      (first (subseq (members ruler) 
				     insert-pos (1+ insert-pos) ))))
         (pred-attachment 
                    (when pred 
		      (side-attachment (info pred) (bottomright (side ruler)))))
         (succ-attachment 
                    (when succ 
		      (side-attachment (info succ) (topleft (side ruler)))))
         (one-attachment 
                    (side-attachment (info plate) (bottomright (side ruler))))
         (other-attachment
                    (side-attachment (info plate) (topleft (side ruler)))))
    (when pred
      (when (eql (atype pred-attachment) :widget)
        (setf (widget pred-attachment) (info plate))
        (setf (offset pred-attachment) 
              (- plate-pos
		 (if (horizontal (side ruler))
		     (+ (x-pos pred) (width pred))
		     (+ (y-pos pred) (height pred)))))
        ;; set new attachments of inserted widget
        (unless (passive-member plate (topleft (side ruler)))
          (setf (atype one-attachment) :none))
        (unless (passive-member plate (bottomright (side ruler)))
          (if succ
            (progn
              (setf (atype other-attachment) :widget)
              (setf (offset other-attachment)
                    (if (horizontal (side ruler))
                        (- (x-pos succ) (width plate) plate-pos)
                        (- (y-pos succ) (height plate) plate-pos))))
            (progn
              (setf (atype other-attachment) :form)
              (setf (offset other-attachment)
                    (if (horizontal (side ruler)) 
                        (- (width (parent-view ruler)) (width plate) plate-pos)
                        (- (height (parent-view ruler)) 
			   (height plate) plate-pos))))))
        (update-constraints (constraints (info pred)) (plate (info pred)))
      ))
    (when succ
      (when (eql (atype succ-attachment) :widget)
        (setf (widget succ-attachment) (info plate))
        (setf (offset succ-attachment)
              (- (if (horizontal (side ruler)) (x-pos succ) (y-pos succ))
                 plate-pos
                 (if (horizontal (side ruler)) (width plate) (height plate))))
        ;; set new attachments of inserted widget
        (unless (passive-member plate (topleft (side ruler)))
	  (if pred
            (progn
              (setf (atype one-attachment) :widget)
              (setf (offset one-attachment)
                    (if (horizontal (side ruler))
                        (- plate-pos (x-pos pred) (width pred))
                        (- plate-pos (y-pos pred) (height pred)))))
            (progn
              (setf (atype one-attachment) :form)
              (setf (offset one-attachment) plate-pos))))
        (unless (passive-member plate (bottomright (side ruler)))
          (setf (atype other-attachment) :none))
        (update-constraints (constraints (info succ)) (plate (info succ)))
      ))
  ))

