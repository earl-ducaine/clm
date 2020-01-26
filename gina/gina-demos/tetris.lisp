;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: tetris ; Base: 10 -*-
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
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN 
;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
;;; IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;;
;;; Authors: Project GINA (Spenke@GMD.de)
;;;          P.O. Box 1316
;;;          D-5205 Sankt Augustin 1
;;;

(in-package :GINA)
(defginapackage :tetris)
(in-package :tetris)
(setq *sccs-id* "@(#)tetris.lisp	1.5  9/11/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class tetris-application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tetris-application (application)
  ((name          :initform "Tetris"    :allocation :class)
   (signature     :initform "tetris"    :allocation :class)
   (file-type     :initform nil         :allocation :class) ;; no documents
   (document-type :initform 'tetris-doc :allocation :class)
   (inspect-click :initform nil)
   (tetris-pieces :accessor tetris-pieces :initform nil)   
   (color-palette :accessor color-palette :initform nil)))

(defun make-tetris-application ()
  "start the tetris-application"
  (make-application :class 'tetris-application))

(defmethod startup-actions ((app tetris-application))
  "initialize colors"
  (setf (color-palette app)
	'("red" "mediumturquoise" "blue" "orange" "violet" "yellowgreen"
	  "gold" "white"))
  (setf (tetris-pieces app)
    '(((0 0 0 0)(0 1 1 0)(0 1 1 0)(0 0 0 0))
      ((0 1 0 0)(0 1 0 0)(0 1 0 0)(0 1 0 0))
      ((0 0 0 0)(0 1 1 0)(0 0 1 0)(0 0 1 0))
      ((0 0 1 0)(0 0 1 0)(0 1 1 0)(0 0 0 0))
      ((0 0 1 0)(0 1 1 0)(0 0 1 0)(0 0 0 0))
      ((0 1 0 0)(0 1 1 0)(0 0 1 0)(0 0 0 0))
      ((0 0 0 0)(0 0 1 0)(0 1 1 0)(0 1 0 0)))))

;; other colors
;	  "aquamarine"  "blue"
;	  "CadetBlue" "CornflowerBlue" "DarkSlateBlue"
;	  "LightBlue" "LightSteelBlue" "MediumBlue"
;	  "MediumSlateBlue" "MidnightBlue" "NavyBlue" "navy"
;	  "SkyBlue" "SlateBlue" "SteelBlue" "coral"
;	  "cyan" "firebrick" "brown" "sandybrown" "gold"
;	  "goldenrod"  "green"
;	  "DarkGreen" "DarkOliveGreen" "ForestGreen" "LimeGreen"
;	  "PaleGreen" "SeaGreen" "SpringGreen"
;	  "YellowGreen" "DarkSlateGrey" "DarkSlateGray"
;	  "gray" "grey" "khaki" "magenta" "maroon" "orange" "orchid"
;	  "DarkOrchid" "MediumOrchid" "pink"
;	  "plum" "red" "IndianRed" "MediumVioletRed" "OrangeRed"
;	  "VioletRed" "salmon" "sienna" "tan"
;	  "thistle" "turquoise" "DarkTurquoise" "MediumTurquoise"
;	  "violet" "BlueViolet" "wheat" "white" "yellow"

(defmethod idle-action ((app tetris-application))
  "give control to each doc to animate the falling pieces"
  (loop for doc in  (document-list app)
	do (tick doc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; forward declaration of class tetris-piece
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tetris-piece (view-object)
   ((my-doc    :accessor my-doc :initarg :my-doc)
   ;; coordinates and size in units
   (my-squares :accessor my-squares :initarg :my-squares)
   (my-x-pos  :accessor my-x-pos)  ;; 1..(width-in-units doc)
   (my-y-pos  :accessor my-y-pos)  ;; 1..(height-in-units doc)
   (my-width  :accessor my-width  :initarg :my-width)  ;; 1..4
   (my-height :accessor my-height :initarg :my-height) ;; 1..4
   (my-color-index :accessor my-color-index) ;; index in the color-map
   (my-white :accessor my-white)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class tetris-doc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tetris-doc (document)
  ((running :accessor running :initform nil) ;; indicator whether game is running
   (level   :accessor level   :initform 4)   ;; level from 0 to 10 i.e. speed 
   (score   :accessor score   :initform 0)   ;; the current score of the game
   (filled  :accessor filled  :initform 0)   ;; no of completed lines
   (falling-piece :accessor falling-piece :initform nil);; holds the current falling piece

   (board :accessor board)                   ;; array of nil (free) t (boarder) or squares
   (unit :accessor unit :initform 25)        ;; length of one unit in pixels
   (width-in-units  :accessor width-in-units  :initform 10)
   (height-in-units :accessor height-in-units :initform 20)

   ;; widgets in the main-shell
   (pause-button  :accessor pause-button)
   (level-group   :accessor level-group)
   (score-display :accessor score-display)
   (lines-display :accessor lines-display)))

(defmethod initialize-instance :after ((doc tetris-doc) &rest initargs)
   "some initializations"
   (declare (ignore initargs))
   ;; the name of the document is used for icons. Default is "Untitled 1"
   (setf (name doc) "Tetris")
   (with-slots (board height-in-units width-in-units) doc
     (setq board
	   (make-array (list (+ 2 width-in-units) (+ 2 height-in-units))
		       :initial-element nil));; board is empty at start-up

     ;; set the borders of the game-board to t (occupied)
     (loop for col in (list 0 (+ 1 width-in-units))
	   do (loop for row from 0 to (+ 1 height-in-units)
		    do (setf (aref board col row) t)))
     (loop for col from 1 to width-in-units
	   do (loop for row in (list 0 (+ 1 height-in-units))
		    do (setf (aref board col row) t)))))

(defmethod (setf score) :after (new-value (doc tetris-doc))
  "show the new score in the score-display"
  (setf (value (score-display doc)) (format nil "~6d" new-value)))

(defmethod (setf filled) :after (new-value (doc tetris-doc))
  "show the new value of filled in the lines-display"
  (setf (value (lines-display doc)) (format nil "~6d" new-value)))

(defmethod create-windows ((doc tetris-doc)
			   &aux scroller frame form column)
  "create the windows belonging to this document"
  (with-slots (main-shell main-view shell-width unit width-in-units
	                  height-in-units pause-button level-group
	                  score-display lines-display level) doc
    (setq main-shell
      (make-document-shell doc
			   :with-menu nil
			   :width  (+ 175 (* unit width-in-units))
			   :height (+  10 (* unit height-in-units))))
    (setf (title main-shell) "Tetris")
    (setq form       (make-form main-shell))
    (setq column     (make-row-column form :spacing 10))
    (setq scroller   (make-scroller form))
    (setq frame      (make-frame scroller :shadow-type :etched-in))
    (setq main-view  (make-tetris-view frame doc))

    (setq score-display
	  (make-labeled-text column "Score" :value "0" :editable nil
			     :columns 6))
    (setq lines-display
	  (make-labeled-text column "Lines" :value "0" :editable nil
			     :columns 6))
    (make-push-button column "Quit"
		      :activate-callback
		      (make-callback #'quit-app *application*))
    (make-push-button column "New Game"
		      :activate-callback (make-callback #'new-game doc))
    (setq pause-button
	  (make-toggle-button column "Pause"
			      :value-changed-callback
			      (make-callback #'start-stop-game doc)))
    (setq level-group
      (make-radio-button-group column
			       (loop for level from 0 to 10
				   collect (list (format nil " ~2d" level)
						 level))
			       :initial-value level
			       :label-string "Level"
			       :value-changed-callback
			       (make-callback #'new-level doc)))

    (define-form-constraint column
			    :left-attachment :none
			    :top-attachment :form
			    :top-offset 20
			    :bottom-attachment :form
			    :right-offset 20
			    :right-attachment :form)
    
    (define-form-constraint scroller
			    :right-attachment :widget :right-widget column
			    :right-offset 20
			    :top-attachment :form :left-attachment :form
			    :bottom-attachment :form)

    ))

(defmethod new-level ((doc tetris-doc) the-level &optional (old-level nil))
  "modify idle timeout"
  (declare (ignore old-level))
  (setf (level doc) the-level)
  (when (running doc)
    (setf (idle-timeout *application*) (* 0.04 (- 10 (level doc))))))

(defmethod start-game ((doc tetris-doc))
  "set idle timeout"
  (with-slots (running level pause-button) doc
    (setf (value pause-button) nil)
    (set-keyboard-focus (main-view doc))
    (setq running t)
    (setf (idle-timeout *application*) (* 0.04 (- 10 (level doc))))))

(defmethod stop-game ((doc tetris-doc))
  "turn off timeout"
  (with-slots (running pause-button) doc
    (setq running nil)
    (setf (value pause-button) t))
  (setf (idle-timeout *application*) nil))

(defmethod new-game ((doc tetris-doc))
  "start a new game"
  (stop-game doc)
  (clear-game doc)
  (start-game doc))

(defmethod clear-game ((doc tetris-doc))
  "reset everything"
  
  (setf (score doc)   0)
  (setf (filled doc)  0)
  (setf (falling-piece doc) nil)
  
  (with-slots (width-in-units height-in-units board main-view) doc

    ;; clear array
    (loop for y from 1 to height-in-units
	  do (loop for x from 1 to width-in-units
		   do (setf (aref board x y) nil)))

    ;; deinstall all view-objects:
    (setf (view-objects main-view) nil)
    (force-redraw main-view)))

(defmethod start-stop-game ((doc tetris-doc) &optional on)
  (declare (ignore on))
  (with-slots (running) doc
    (if running (stop-game doc) (start-game doc))))

(defmethod collapse-line ((doc tetris-doc) line)
  "remove all squares in line and move all squares above one unit down"
  
  (with-slots (width-in-units height-in-units board unit) doc

    (loop for col from 1 to width-in-units
	  as square = (aref board col line)
	  do (when (not square) (print "Internal error in collaps-line!"))
	     (deinstall square :redraw nil)
	     (setf (aref board col line) nil))
    (loop for col from 1 to width-in-units
	  do (loop for row from (1- line) downto 1
		   as square = (aref board col row)
		   when square
		     do ;; move squares down
			(move square (x-pos square)(+ unit (y-pos square))
			      :redraw nil)
			(setf (aref board col row) nil)
			(setf (aref board col (+ 1 row)) square)))
    (force-redraw (main-view doc))))
			

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class tetris-piece
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-tetris-piece (doc &aux new-piece type)
  "create a new tetris-piece"
  (with-slots (unit main-view) doc
    (setq new-piece (make-view-object (1- (* 4 unit))
				      (1- (* 4 unit))
				      :class 'tetris-piece
				      :initargs (list :my-doc doc
						      :my-width 4
						      :my-height 4))))
  (with-slots (my-white my-color-index my-squares my-x-pos my-y-pos) new-piece
    (with-slots (color-palette screen tetris-pieces) *application*
      (setq type (random 7))
      (setq my-white (xlib:screen-white-pixel screen))
      (when (= my-white
	       (setq my-color-index
		 (xlib:alloc-color
		  (elt (xlib:installed-colormaps (x-window (main-view doc))) 0)
		  (elt color-palette type))))
	(setq my-color-index (xlib:screen-black-pixel screen)))
      (setq my-squares (make-array '(4 4)
				   :initial-contents (elt tetris-pieces type)))
      (setq my-x-pos (+ 1 (random 7)))
      (setq my-y-pos 0)))
  new-piece)


(defmethod install ((piece tetris-piece) view 
		    &optional (x-in-units 1) (y-in-units 1)
		    &key &allow-other-keys)
  "install a tetries piece using x y in units of the board not in pixels"
  (with-slots (my-x-pos my-y-pos my-doc) piece
    (with-slots (unit) my-doc
      (setq my-x-pos x-in-units)
      (setq my-y-pos y-in-units)
      (call-next-method piece view
			(* (1- my-x-pos) unit)(* (1- my-y-pos) unit)))))

(defmethod start-falling ((piece tetris-piece) main-view)
  (with-slots (my-x-pos my-y-pos my-doc) piece
    (install piece main-view my-x-pos my-y-pos)))

(defmethod bouncing ((piece tetris-piece))
  (with-slots (my-x-pos my-y-pos my-squares my-doc) piece
    (with-slots (board) my-doc
      (loop for col from 0 to 3
	  as x = (+ my-x-pos col)
	  when (loop for row from 0 to 3
		   as y = (+ my-y-pos row)
		   when (and (= (aref my-squares col row) 1)
			     (aref board x y))
		   do (return t))
	  do (return t)))))
  
(defmethod move-down ((piece tetris-piece))
  "move one unit towards the ground"
  (with-slots (my-x-pos my-y-pos my-width x-pos y-pos my-doc my-squares) piece
    (with-slots (height-in-units unit board) my-doc
      (setq my-y-pos (+ 1 my-y-pos))
      (if (bouncing piece)
	  (progn
	    (setq my-y-pos (- my-y-pos 1))
	    (settle-down piece)
	    nil)	; return nil: could not move anymore
	(progn
	  (move piece x-pos (* unit (1- my-y-pos)))
	  t))))) 

(defmethod move-left ((piece tetris-piece))
  "move one unit to the right (when not bouncing)"
  (with-slots (my-x-pos x-pos y-pos my-doc) piece
    (with-slots (unit) my-doc
      (setq my-x-pos (- my-x-pos 1))
      (if (not (bouncing piece))
	  (move piece (- x-pos unit) y-pos)
	(setq my-x-pos (+ my-x-pos 1))))))
	  
(defmethod move-right ((piece tetris-piece))
  "move one unit to the right (when not bouncing)"
  (with-slots (my-x-pos x-pos y-pos my-doc) piece
    (with-slots (unit) my-doc
      (setq my-x-pos (+ my-x-pos 1))
      (if (not (bouncing piece))
	  (move piece (+ x-pos unit) y-pos)
	(setq my-x-pos (- my-x-pos 1))))))
	  
(defmethod rotate ((piece tetris-piece) &aux old-squares)
  "rotate 90 degrees clockwise (when not bouncing)"
  (declare (ignore new-width new-height new-x-pos))
  (with-slots (my-squares) piece
      (setq old-squares my-squares)
      (setq my-squares (make-array '(4 4)))
      (loop for x from 0 to 3 do
	    (loop for y from 0 to 3 do
		  (setf (aref my-squares x (- 3 y))(aref old-squares y x))))
      (when (bouncing piece)
	(setq my-squares old-squares))))

(defmethod drop ((piece tetris-piece))
  "drop the piece to the ground"
  (loop while (move-down piece)))

(defmethod settle-down ((piece tetris-piece) &aux a-square)
  (deinstall piece :redraw nil)
  (with-slots (my-squares my-doc my-x-pos my-y-pos my-color-index) piece
    (with-slots (board unit main-view height-in-units width-in-units) my-doc
      (setf (score my-doc) (+ (score my-doc) 4))
      (loop for row from 0 to 3
	  as x = (+ my-x-pos row)
	  do (loop for col from 0 to 3
		 as y = (+ my-y-pos col)
		 when (= (aref my-squares row col) 1)
		 do (setq a-square (make-small-tetris-square my-doc))
		    (setf (my-color-index a-square) my-color-index)
		    (setf (aref board x y) a-square)
		    (install a-square main-view
			     (* (- x 1) unit)(* (- y 1) unit) :redraw nil)))
      (loop for y from 1 to height-in-units
	  when (not (loop for x from 1 to width-in-units
			when (not (aref board x y))
			do (return t)))
	  do (collapse-line my-doc y)
	     (setf (filled my-doc) (+ (filled my-doc) 1))))))
       
(defmethod draw ((piece tetris-piece) count x y width height)
  "draw view-object into the view"
  (declare (ignore count x y width height))
  (with-slots (parent-view x-pos y-pos my-doc my-squares) piece
    (with-slots (gcontext) parent-view
      (with-slots (unit) my-doc
	(loop for col from 0 to 3 do
	      (loop for row from 0 to 3 do
		    (when (= (aref my-squares col row) 1)
		      (draw-rectangle parent-view (+ x-pos (* unit col))
				      (+ y-pos (* unit row))
				      (- unit 1) (- unit 1) nil))))
	(xlib:with-gcontext (gcontext :foreground (my-white piece))
	  (loop for col from 0 to 3 do
		(loop for row from 0 to 3 do
		      (when (= (aref my-squares col row) 1)
			(draw-rectangle parent-view (+ x-pos (* unit col))
					(+ y-pos (* unit row))
					(- unit 2) (- unit 2) nil)))))
	(xlib:with-gcontext (gcontext :foreground (my-color-index piece))
	  (loop for col from 0 to 3 do
		(loop for row from 0 to 3 do
		      (when (= (aref my-squares row col) 1)
			(draw-rectangle parent-view (+ 1 x-pos (* unit row))
					(+ 1 y-pos (* unit col))
					(- unit 2) (- unit 2) t)))))))))

(defmethod tick ((doc tetris-doc))
  (with-slots (running falling-piece main-view) doc
    (when running
      (if (null falling-piece) ;; generate new one
	  (progn 
	    (setq falling-piece (make-tetris-piece doc))
	    (if (not (bouncing falling-piece))
		(start-falling falling-piece main-view)
	      (progn
		(warning-dialog "Game over" :dialog-title "Sorry!" :document doc)
		(setq falling-piece nil)
		(stop-game doc))))
	(when (not (move-down falling-piece))
	  (setq falling-piece nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class tetris-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tetris-view (view)
  ((overlapping-objects :initform t)))

(defun make-tetris-view (parent doc)
  "create a new tetris-view"
  (with-slots (unit width-in-units height-in-units) doc
    (make-view parent :document doc
			      :width (* width-in-units unit)
			      :height (* height-in-units unit)
			      :class 'tetris-view
			      :motif-resources '(:traversal-on t)
			      :double-buffering t)))

(defmethod key-press ((view tetris-view) char code modifiers x y 
		      &aux (doc (document view)))
  (declare (ignore code modifiers x y))

  ;;(format t "code=~s char=~a x=~a y=~a~%" code char x y)
  (with-slots (falling-piece unit height-in-units running) doc
    (when (equal char "p") (start-stop-game (document view)))
    (when falling-piece
      (when (equal char "j")     (move-left  falling-piece))
      (when (equal char "l")     (move-right falling-piece))
      (when (equal char "k")     (rotate     falling-piece))
      (when (equal char "space") (drop       falling-piece)
	    (setq falling-piece nil))
      ;; make main loop wait for the full time interval
      (restart-timer *application*)
  )))

(register-application "tetris" 'tetris-application "tetris-game")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class small-tetris-piece
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass small-tetris-square (view-object)
   ((my-color-index :accessor my-color-index)
    (my-white :accessor my-white)))

(defun make-small-tetris-square (doc &aux new-piece)
  (with-slots (unit main-view) doc
    (setq new-piece (make-view-object unit unit
				      :class 'small-tetris-square)))
  (setf (my-white new-piece)(xlib:screen-white-pixel (screen *application*)))
  new-piece)

(defmethod draw ((square small-tetris-square) count x y width height)
  "draw view-object into the view"
  (declare (ignore count x y width height))
  (with-slots (parent-view x-pos y-pos width height) square
    (with-slots (gcontext) parent-view
      (draw-rectangle parent-view x-pos y-pos (- width 1) (- height 1) nil)
      (xlib:with-gcontext (gcontext :foreground (my-white square))
	(draw-rectangle parent-view x-pos y-pos (- width 2) (- height 2) nil))
      (xlib:with-gcontext (gcontext :foreground (my-color-index square))
	(draw-rectangle parent-view (+ 1 x-pos) (+ 1 y-pos)
			(- width 2)  (- height 2) t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; write the signature "tetris" into file >gina>documents>tetris.appl !!!!!

'(make-tetris-application)


