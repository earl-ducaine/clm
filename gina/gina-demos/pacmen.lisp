;;; -*- Mode:LISP;Syntax: Common-Lisp;Package:pac;Base:10-*-

(in-package :GINA)
(defginapackage :pac)
(in-package :pac)
(setq *sccs-id* "@(#)pacmen.lisp	1.7  9/16/92")


;; This demo is much less than a real Pacman game. It only demonstrates
;; the use of multiple processes within a single application.
;; 
;; - You can create a pacman by the menu entry NEW PACMAN. It will run
;;   down the window line by line at a random speed and disappear when
;;   the bottom is reached.
;; - On SUNs, the death of a pacmen is indicated by a "drip" sound.
;; - Because each pacman runs in his own process the user interface remains
;;   fully operable. The pacmen continue to run even during modal dialogs.
;; - You can start multiple pacmen (as many as your Lisp can stand).
;; - Try to kill a pacman by clicking the mouse at him.
;; - A pacman overtaking a slower one will eat it up.
;; - A pacman can also be started with a progress bar indicating how
;;   many lines are already done. The progress bar can also be used to kill
;;   the pacman.
;; - You can explicitly kill all pacmen. This is implicitly done by GINA
;;   when a document is closed.
;; 
;; Implementation:
;; - Pacmen are view objects together with a background process.
;;   This differs from the Tetris demo where a timer is used instead.
;; - Each pacmen loops through ist possible positions in the window. 
;;   In each repetition the view-object is moved, other men are eaten,
;;   the progress bar is updated if necessary and a sleep is done.
;; - Access to the data structures of the document which are shared by all
;;   pacmen is synchronized using the macro WITH-DOCUMENT.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class pacmen-application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass pacmen-application (application)
  ((name          :initform "Pacmen")
   (signature     :initform "Pac")
   (file-type     :initform "pac")
   (document-type :initform 'pacmen-document :allocation :class)))

(defun make-pacmen-application ()
  (make-application :class 'pacmen-application))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this forward declaration is necessary because of PCL only !!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass pacman (view-object)
 ((mouth-open :accessor mouth-open :initform t)
  (my-number     :accessor my-number     :initarg :my-number)
  (right      :accessor right      :initform t)
  (my-speed      :accessor my-speed      :initform (+ 0.1 (* (random 3) 0.1)))
  (process    :accessor process    :initarg :process)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class pacmen-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass pacmen-document (document)
  ((nr-of-pacmen :accessor nr-of-pacmen :initform 0)
   (shell-width  :initform 550)
   (shell-height :initform 550)))

(defmethod create-windows((doc pacmen-document) &aux scroller)
  (with-slots (main-shell main-view) doc
    (setq main-shell (make-document-shell doc))
    (setq scroller   (make-scroller main-shell))
    (setq main-view  (make-view scroller
				:document doc 
				:width 500 :height 500))

    (add-menu-command (main-menu main-shell)
		      "Play" "New Pacman" 
		      (make-callback 'create-pacman doc)
                      :accelerator "P")
    (add-menu-command (main-menu main-shell)
		      "Play" "New Pacman with Progress Bar" 
		      (make-callback 'create-pacman doc :progress-bar t))
    (add-menu-command (main-menu main-shell)
                      "Play" "Kill All Pacmen"
                      (make-callback 'kill-all-background-processes doc))
  ))

(defmethod create-pacman ((doc pacmen-document) &key (progress-bar nil))
  (when (not progress-bar)
    (in-background-process (doc)
      (run-pacman doc)))

  (when progress-bar
    (with-progress-bar
        (doc :kill-on-abort t :centered nil :modal nil
         :title (format nil "Pacman ~d" (1+ (nr-of-pacmen doc)))
	 :message "Running ...             ")
      (run-pacman doc :indicate-progess t))))

(defmethod run-pacman ((doc pacmen-document) 
		       &key (indicate-progess nil)
		       &aux pacman percent)
  "code executed by each background process"
   (setq pacman (make-pacman 40 (mod (incf (nr-of-pacmen doc)) 100)))
   (with-document (doc)
      (install pacman (main-view doc) 30 30))
   (unwind-protect
       (loop for y from 1 to 10 do
	 (loop for x from 1 to 20 do
	   (when indicate-progess
	     ;; update progress bar
             (setq percent (+ (* 10 (1- y)) (round (1- x) 2)))
	     (if (= x 1)
		 (indicate-progress percent 
		  :new-message (format nil "Running in line ~d ..." y))
	         (indicate-progress percent)))

	   (setf (mouth-open pacman) (not (mouth-open pacman)))
	   (setf (right pacman) (oddp y))
	   (move pacman (if (right pacman) (* 20 x) (- 400 (* 20 x))) (* 40 y))
	   (xlib:display-force-output *display*)

	   ;; eat other men if at same position
	   (loop for p in (view-objects (main-view doc))
	    when (and (not (eq p pacman))
		      (= (x-pos pacman) (x-pos p))
		      (= (y-pos pacman) (y-pos p)))
	    do (kill-pacman p))
	   
	   (sleep (my-speed pacman))
	   ))
     ;; cleanup
     (with-document (doc)
	(deinstall pacman)
        (xlib:display-force-output *display*))
     #-genera
     (shell-command 
      "/usr/demo/SOUND/play -v100 /usr/demo/SOUND/sounds/drip.au &")))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class pacman
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass pacman (view-object)
 ((mouth-open :accessor mouth-open :initform t)
  (my-number     :accessor my-number     :initarg :my-number)
  (right      :accessor right      :initform t)
  (my-speed      :accessor my-speed      :initform (+ 0.1 (* (random 3) 0.1)))
  (process    :accessor process    :initarg :process)))

(defun make-pacman (size my-number)
  (make-view-object size size :class 'pacman 
		    :initargs (list :process (xtk::current-process)
				    :my-number (format nil "~d" my-number))))

(defmethod kill-pacman ((p pacman))
  "kill pacmen, but wait until he is out of critical sections"
  (kill-background-process (document (parent-view p)) (process p)))

(defmethod draw ((p pacman) count x y width height)
  (declare (ignore x y height))
  (when (zerop count)
    (if (not (mouth-open p))
	(draw-arc p 0 0 width width 0 (* 2 pi) t)
      (if (right p)
	  (draw-arc p 0 0 width width 0.4 (- (* 2 pi) 0.8) t)
	  (draw-arc p 0 0 width width (- 0.4 pi) (- (* 2 pi) 0.8) t)))
    (xlib:with-gcontext ((gcontext (parent-view p)) :function boole-xor)
      (draw-glyphs p 10 25 (my-number p)))))

(defmethod button-press ((p pacman) code repetition x y)
  "react to button-press event in the window"
  (declare (ignore code repetition x y))
  (kill-pacman p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; main-program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(register-application "pac" 'pacmen-application "pac")
'(make-pacmen-application)




