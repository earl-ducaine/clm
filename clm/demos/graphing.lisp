(in-package 'graph)

; This file describes a simple CLM window which draws tree graphs,
; using CLX graphics calls inside a CLM :drawing-area widget.
;
; The object being graphed is bound to *graph-item*.
;
; The CAR of a list is a node, and each of the elements of the CDR
; is a new subtree.
;
; You can extend how a tree is walked and displayed by modifying 
;   NODE-CHILDREN - returns a list of the children of a given node
;   DRAW-NODE     - defines how to draw a node
; 
;
; Written by Steve Strassmann, 28 Jan 1991
; MIT Media Lab
; 20 Ames St.
; Cambridge, MA 02139 USA
; straz@media-lab.media.mit.edu
; This code is in the public domain.
;------------------------------------------------------------------------------

(defparameter *graph-item* 
  '(user (hair brown)
	 (snack chocolate (style dark))
	 (car (make honda) (model accord) (year 84)))
  "The object being graphed")

(defparameter *vertical-spacing* 10 "Vertical spacing between nodes")
(defparameter *horizontal-spacing* 14 "Horizontal spacing between nodes")
(defparameter *x-offset* 40 "Upper left corner of graph")
(defparameter *y-offset* 40 "Upper left corner of graph")

(defparameter *window-width* 400 "Starting size of graph window")
(defparameter *window-height* 300 "Starting size of graph window")

(defparameter *regular-font* "*Lucida-bold-r*-14-*"  "Regular text font")
(defparameter *italic-font* "*Lucida-bold-i*-14-*"   "Italic text font")

; You can use either rgb or named colors
(defparameter *text-color* (xlib:make-color :red .5 :green .2 :blue .2) 
  "Color for drawing text")
(defparameter *line-color* "black" 
  "Color for drawing lines")

;------------------------------------------------------------------------------

(defun graph (&optional (item *graph-item*) (run-in-background? nil))
  (setq *graph-item* item)
  (xtk:run-motif-application 'create-graph :use-clx t :extra-process run-in-background?))
  
(defun create-graph ()
  (let* ((shell (xtk:create-application-shell))
	 (main (xtk:create-widget :form shell :vertical-spacing 5 :horizontal-spacing 5))
	 (menubar (xtk:create-widget :row-column main :row-column-type :menu-bar 
				     :shadow-thickness 1 :spacing 5))
	 (refresh-btn (xtk:create-widget :push-button menubar :label-string "Refresh"))
	 (quit-btn (xtk:create-widget :push-button menubar :label-string "Quit"))
	 (draw (xtk:create-widget :drawing-area main
		 :top-attachment :widget :top-widget menubar
		 :width *window-width* :height *window-height*)))
    (xtk:realize-widget shell)		; The shell must be realized before
					; these other calls can work.
    (let* ((window (xtk:make-clx-window draw))
	   (display (xlib:window-display window))
	   (colormap (xlib:window-colormap window))
	   (gc1 (xlib:create-gcontext	; regular graphics context
		 :drawable window
		 :foreground (xlib:alloc-color colormap *text-color*)
		 :font (xlib:open-font display *regular-font*)))
	   (gc2 (xlib:create-gcontext	; italic graphics context
		 :drawable window
		 :foreground (xlib:alloc-color colormap *text-color*)
		 :font (xlib:open-font display *italic-font*)))
	   (gc3 (xlib:create-gcontext	; for drawing dark lines
		 :drawable window
		 :foreground (xlib:alloc-color colormap *line-color*))))
      (xtk:add-callback draw :expose
			#'redraw-graph (list window draw gc1 gc2 gc3))
      (xtk:add-callback refresh-btn :activate
			#'redraw-graph (list window draw gc1 gc2 gc3))
      (xtk:add-callback quit-btn :activate #'xtk::quit-application shell))))

(defun redraw-graph (widget client-data &rest call-data)
  "This is called to redisplay the image."
  (declare (special xtk:*x-display*)
	   (ignore widget call-data))
  (let ((window (first client-data))
	(draw-widget (second client-data))
	(gcontexts (cddr client-data)))
    (xlib:clear-area window)
    (let ((size (draw-subtree window gcontexts *graph-item* *x-offset* *y-offset*)))
      (xtk::resize-widget draw-widget
			 (+ *x-offset* (first size))
			 (+ *y-offset* (second size)) 0))
    (xlib:display-force-output xtk:*x-display*)))

;------------------------------------------------------------------------------
; This determines the children of a given object.
; Extend this to support your objects.

(defun node-children (node)
  "Returns a list of NODE's children."
  (typecase node
    (list (if (listp (car node))
	      node
	    (cdr node)))
    (t nil)))

;------------------------------------------------------------------------------
; These functions draw the objects.
; Extend DRAW-NODE to support your objects.

(defun draw-subtree (window gcontexts node x y)
  "Draws NODE and all its children.
   Must return a list of (WIDTH HEIGHT) of the drawn subtree"
  (let* ((size (draw-node window gcontexts node x y))
	 (node-width (first size))
	 (node-height (second size))
	 (line-dy (- (round (* node-height .5))))
	 (new-x (+ x (first size)))
	 (kid-x (+ new-x *horizontal-spacing*))
	 (kid-y y)
	 (width node-width))
    (dolist (kid (node-children node))
      (xlib:draw-line window (third gcontexts) 
		      (+ new-x 2) (+ y line-dy) 
		      (- kid-x 3) (+ kid-y line-dy))
      (let ((subtree-size (draw-subtree window gcontexts kid kid-x kid-y)))
	(incf kid-y (+ *vertical-spacing* (second subtree-size)))
	(setf width (max width 
			 (+ node-width *horizontal-spacing* (first subtree-size))))))
    (list width
	  (max (- kid-y y *vertical-spacing*)
	       node-height))))


(defun draw-node (window gcontexts node x y)
  "Draws NODE at given position on WINDOW. 
   Change this to suit your taste.
   Must return a list of (WIDTH HEIGHT) of the drawn node."
  (typecase node
    (list (if (listp (car node))
	      '(0 0)			; Use italics for the CAR of a list
	    (draw-atom window (second gcontexts) (car node) x y)))
					; Use normal text for other atoms
    (atom (draw-atom window (first gcontexts) node x y))
    (t '(0 0))))

(defun draw-atom (window gcontext node x y)
  "Assuming NODE is an atom, displays it on the screen"
  (xlib:draw-glyphs window gcontext x y (format nil "~A" node))
  (atom-size node gcontext))

(defun atom-size (object gcontext)
  "An atom's size is simply the bounding box of its printed form."
  (let ((string (format nil "~A" object))
	(font (xlib:gcontext-font gcontext)))
    (multiple-value-bind (width ascent descent)
	(xlib:text-extents font string)
      (list width (- ascent descent)))))

