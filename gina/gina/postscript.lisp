;;; -*- Mode:LISP;Syntax: Common-Lisp;Package: gina;Base:10-*-
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
;;;

(in-package :gina)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; experimental extension for postscript printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *postscript-printer* "$PRINTER")
(defparameter *postscript-print-string* "| lpr -h -P$PRINTER")
(defparameter *postscript-format* :din-a4-hoch) ;; :din-a4-hoch or :din-a4-quer 
(defparameter *postscript-scale*  1.0)          ;; scaling-factor
(defparameter *postscript-unit*  0.86)          ;; scaling-factor for xwindow->ps 
(defparameter *postscript-height* 937)          ;; for try
(defparameter *postscript-height-p*  937)       ;; din-a4 height
(defparameter *postscript-fullpage*  t)         ;; fullpage or EPSF 
(defparameter *postscript-copies* 1)            ;; number of copies

;; override empty method of standard GINA to install Postscript printing

(defginamethod postscript-print ((doc document))
  (description "hardcopy document on postscript printer"
	       :called-by-gina "in reaction to the postscript-print menu entry"
	       :called-by-application :rarely)
  (with-slots (main-view) doc
    (when (not (and main-view 
		    (eq (gina-class-of main-view)
			(find-gina-class 'view))))
      (warning-dialog "Document does not have a printable main view!"
		      :document doc)
      (return-from postscript-print)))
  (open-printer-dialog doc)

  (when *postscript-print-string*
    (with-clock-cursor
	(let ((ps-file (concatenate 'string "/tmp/" (name doc) ".ps")))
	  (with-slots (main-view) doc
	    (with-open-file
		(ps-stream ps-file :direction :output :if-exists :supersede)
	      (unwind-protect
		  (progn 
		    (setf (printing main-view) ps-stream)
		    (ps-header main-view)
		    (draw main-view 0 0 0  
			  (width main-view) (height main-view)))
		;; cleanup:
		(ps-end ps-stream)	
		(setf (printing main-view) nil)))
	    (force-redraw main-view)   ;; from dei , cause graphic doesn't appear
	    (shell-command
	     (concatenate 'string "cat " (namestring (truename ps-file))
			  " " *postscript-print-string*))
	    (shell-command
	     (concatenate 'string "rm "
			  (namestring (truename ps-file)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :after methods for drawing primitives do the printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defginamethod draw-point :after ((view view) x y)
  (description "after deamon for postscript printing")
  (when (printing view)
    (ps-point x y view)))

(defginamethod draw-points :after ((view view)
			    points "a flat list of alternating x and y values"
			    &optional relative-p)
  (description "after deamon for postscript printing")
  (when (printing view)
    (ps-points points relative-p view )))

(defginamethod draw-line :after ((view view) x1 y1 x2 y2 
					     &optional (relative-p nil))
  (description "after deamon for postscript printing")
  (when (printing view)
    (ps-line x1 y1 x2 y2 relative-p view)))


(defginamethod draw-lines :after ((view view)
			   points "a flat list of alternating x and y values"
			   &key relative-p fill-p (shape :complex))
   (description "after deamon for postscript printing")
   (declare (ignore shape))
   (when (printing view)
     (ps-lines points fill-p relative-p view)))

(defginamethod draw-segments :after ((view view) segments)
  (description "after deamon for postscript printing")
  (when (printing view)
    (ps-segments segments view)))

(defginamethod draw-rectangle :after ((view view) 
			       x y width height &optional (fill-p nil))
  (description "after deamon for postscript printing")
  (when (printing view)
    (ps-rectangle x y width height fill-p view)))

(defginamethod draw-rectangles :after ((view view) 
				rectangles &optional (fill-p nil))
  (description "after deamon for postscript printing")
  (when (printing view)
    (ps-rectangles rectangles fill-p view)))

(defginamethod draw-arc :after ((view view) 
			x y width height angle1 angle2 &optional (fill-p nil))
  (description "after deamon for postscript printing")
  (when (printing view)
    (ps-arc x y (/ width 2) (/ height 2) angle1 angle2
	    fill-p (xlib::gcontext-arc-mode (gcontext view)) view)))

(defginamethod draw-arcs :after ((view view) arcs &optional (fill-p nil))
  (description "after deamon for postscript printing")
  (when (printing view)
    (ps-arcs arcs fill-p (xlib::gcontext-arc-mode (gcontext view)) view)))

(defginamethod draw-glyph :after ((view view)  x y elt
					&key translate width (size :default))
  (description "after deamon for postscript printing")
  (when (printing view)
    (ps-glyph x y elt size width translate view)))

(defginamethod draw-glyphs :after ((view view)  x y sequence
			 &key (start 0) end translate width (size :default))
  (description "after deamon for postscript printing")
  (when (printing view)
    (ps-glyphs x y sequence size width translate end start view)))

(defginamethod copy-plane :after ((view view) pixmap depth source-x source-y
				       source-width source-height 
				       dest-x dest-y)
  (description "after deamon for postscript printing")
  (declare (ignore depth source-x source-y 
		   source-width source-height))
  (when (printing view)
    (ps-bitmap  pixmap view dest-x dest-y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions for postscript output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *pi/2* (/ pi 2))

(defun make-ps-color-or-pattern (view)
  (let ((gc (gcontext view))(arr nil) (z 0)
	(stream (printing view))
	(window (x-window view)))
    (let ((stipp (xlib::gcontext-stipple gc))
	  (fcol (xlib::gcontext-foreground gc))
	  (bcol (xlib::gcontext-background gc)))
      (if stipp 
	  (progn 
	    (let ((wi (xlib::drawable-width stipp))
		  (he (xlib::drawable-height stipp)))
	      (format stream "% a pattern ~%")
	      (format stream "/pattern (\\~%")
	      (setf arr (xlib::image-z-pixarray
			 (xlib::get-image stipp 
					  :x (xlib::drawable-x stipp)
					  :y (xlib::drawable-y stipp)
					  :width  wi
					  :height he
					  :format :z-pixmap)))
	      (loop for i from (1- wi) downto 0 do
		    (loop for j from (1- he) downto 0 do
			  (format stream "~d" (aref arr i j))
			  (when (eql 1 (aref arr i j)) (incf z)))
		    (format stream "\\~%"))
	      (format stream ") def~%")
	      (cond
		((eql z (* wi he));; pattern filled , *p-black*
		 (format stream "  ~d ~d ~d srgb ~%"
			 (red-value   fcol window)
			 (green-value fcol window)
			 (blue-value  fcol window)))
		((eql z 0);; pattern empty , *p-white*
		 (format stream "  ~d ~d ~d srgb ~%"
			 (red-value   bcol window)
			 (green-value bcol window)
			 (blue-value  bcol window)))
		(T (format stream "    pattern ~d 0 sms~%" (/ (* wi wi) 2)  )))))
	  (format stream "  ~d ~d ~d srgb ~%"
			 (red-value   fcol window)
			 (green-value fcol window)
			 (blue-value  fcol window)))
      
      )))

(defun make-ps-line-style (gc stream)
  (format stream "  ~d slw ~%" (xlib::gcontext-line-width gc))
  (format stream "  ~d setlinejoin ~%"
	  (cond 
	    ((eql (xlib::gcontext-join-style gc) :bevel) 2)
	    ((eql (xlib::gcontext-join-style gc) :round) 1)
	    (T 0)))
  (format stream "  ~d setlinecap ~%"
	  (cond 
	    ((eql (xlib::gcontext-cap-style gc) :projecting) 2)
	    ((eql (xlib::gcontext-cap-style gc) :round) 1)
	    (T 0)))
  (cond 
    ((eql (xlib::gcontext-line-style gc) :dash)        (format stream "  [2 2] 0 setdash ~%" ))
    ((eql (xlib::gcontext-line-style gc) :double-dash) (format stream "  [1 1] 0 setdash ~%" ))
    (T nil))
  )
  
(defun ps-end (stream)
  (format stream " ~%")
  (when (> *postscript-copies* 1)
    (format stream "/#copies ~d def ~%" *postscript-copies*))
  (format stream "showpage grestore ~%")
  )

(defun ps-point (x y view)
  (let ((stream (printing view)))
    (format stream "% a point ~%")
    (format stream "  n~%")
    (format stream "  ~d ~d ~d ~d ~d ~d ellipse~%" x (try y) 1 1 0 360)
    (make-ps-color-or-pattern view)
    (format stream "  c f~%~%")
    )
  )

(defun ps-points (points relative-p view)
  (let ((oldx 0)(oldy 0)(stream (printing view)))
    (format stream "% some points ~%")
    (make-ps-color-or-pattern view)
    (loop for i from 0 to (- (/ (length points) 2) 1)
	  do
	  (let ((x (if relative-p
		       (+ (nth (* i 2) points) oldx)
		       (nth (* i 2) points)
		       ))
		(y (if relative-p
		       (+ (nth (+ (* i 2) 1) points) oldy)
		       (nth (+ (* i 2) 1) points)
		       )))
	    (format stream "  n~%")
	    (format stream "  ~d ~d ~d ~d ~d ~d ellipse~%"
		    x (try y) 1 1 0 360))
	  (format stream " c f~%~%")
	  )
    ))
(defun ps-line (x0 y0 x1 y1 relative-p view)
  (let ((gc (gcontext view))
	(stream (printing view))
	)
    (format stream "% a line ~%")
    (format stream "  n ~d ~d m ~d ~d l ~%"
	    x0 (try y0)
	    (if relative-p (+ x0 x1) x1 )
	    (if relative-p (try (+ y1 y0))(try y1)))
    (make-ps-color-or-pattern view)
    (make-ps-line-style gc stream)
    (format stream "  s~%~%")
    ))

(defun try (y)
  (if *postscript-fullpage*
      (- (/ *postscript-height* *postscript-scale*) y)
      (- (/ *postscript-height* 1) y))
  )

(defun ps-lines (points fill relative-p view)
  (let ((oldx 0)(oldy 0)
	(gc (gcontext view))
	(stream (printing view))
	)
    (format stream "% a polyline~%")
    (format stream "  n~%")
    (format stream "  ~d ~d m~%"
	    (setf oldx (first points))
	    (setf oldy (try (second points))))
    (loop for l from 2 to (/ (length points) 2)
	  do (format stream "  ~d ~d l~%"
		(if relative-p      (+ oldx (nth (- (* 2 l) 2) points))(nth (- (* 2 l) 2) points))
		(try (if relative-p (+ oldy (nth (- (* 2 l) 1) points))(nth (- (* 2 l) 1) points)))))

    (make-ps-color-or-pattern view)
    (if fill
	(format stream "  c f~%")
	(progn
	  (make-ps-line-style gc stream)
	  (format stream "  s~%~%")
	  ))
    ))

(defun ps-segments (points view)
  (let ((gc (gcontext view))
	(stream (printing view))
	)
    (format stream "% some segments~%")
    (make-ps-color-or-pattern view)
    (make-ps-line-style gc stream)
    (loop for l from 0 to (1- (/ (length points) 4))
	  do
	  (format stream "  n~%")
	  (format stream "  ~d ~d m~%" (nth (+ (* 4 l) 0) points) (try (nth (+ (* 4 l) 1) points)))
	  (format stream "  ~d ~d l~%" (nth (+ (* 4 l) 2) points) (try (nth (+ (* 4 l) 3) points)))
	  (format stream "  s~%~%"))
    ))

(defun ps-rectangle (x y width height fill view)
  (let* ((gc (gcontext view))
	 (clip-mask-pixmap (xlib::gcontext-clip-mask gc))
	 (window (x-window view))
	 (stream (printing view))
	 )
    ;; when clip-mask is set , print bitmap
    (if (xlib::drawable-p clip-mask-pixmap)
	 (let* ((arr (xlib::image-z-pixarray
		      (xlib::get-image clip-mask-pixmap
				:x (xlib::drawable-x clip-mask-pixmap)
				:y (xlib::drawable-y clip-mask-pixmap)
				:width  (xlib::drawable-width  clip-mask-pixmap)
				:height (xlib::drawable-height clip-mask-pixmap)
				:format :z-pixmap)))
		(clip-x (xlib::gcontext-clip-x gc))
		(clip-y (xlib::gcontext-clip-y gc))
		(clip-width  (xlib::drawable-width  clip-mask-pixmap))
		(clip-height (xlib::drawable-height clip-mask-pixmap))
		(fcol (xlib::gcontext-foreground gc)))

	   (format stream "% a bitmap as clippingmask for a rect~%")
	   (format stream "  ~d ~d ~d srgb ~%"
		   (red-value   fcol window)
		   (green-value fcol window)
		   (blue-value  fcol window))
	   (format stream "   ~d ~d ~%" clip-width clip-height)
	   (format stream "   true ~%")
	   (format stream "   [1 0 0 1 ~d ~d]~%" (- 0 clip-x) (- clip-height (try clip-y)))
	   (format stream "{<~%")   ;; here comes the Bitmap as a Hex-String
	   ;(format t "~%")
	   (loop for  i from  (1- clip-height) downto 0 do
		 (let ((z 0))
		   (loop for j from 0 to (1- clip-width) do
			; (format t "~d" (aref arr i j))
			 (case (mod j 4)
			   ((0) (incf z (* 8 (aref arr i j))))
			   ((1) (incf z (* 4 (aref arr i j))))
			   ((2) (incf z (* 2 (aref arr i j))))
			   ((3) (incf z (aref arr i j))))
			 (when (or (= (mod j 4) 3) (= j (1- clip-width)))
			   (format stream "~x" z)
			   (setf z 0)))
		   ;(format t ".")
		   (format stream "~%")))
	   ;(format t "~%")
	   (format stream ">} imagemask~%~%"))

	 ;; else print normal rectangle
	 (progn 
	   (format stream "% a rectangle~%")
	   (format stream "  n~%")
	   (format stream "  ~d ~d m~%" x (try y))
	   (format stream "  ~d ~d l~%" (+ x width) (try y))
	   (format stream "  ~d ~d l~%" (+ x width) (try (+ y height)))
	   (format stream "  ~d ~d l~%" x (try (+ y height)))
	   (format stream "  ~d ~d l~%" x (try y))
	   (make-ps-color-or-pattern view)
	   (if fill
	       (format stream "  c f~%")
	       (progn
		 (make-ps-line-style gc stream)
		 (format stream "  s~%~%")
		 )))
    )))

(defun ps-rectangles (rects fill view)
  (let ((gc (gcontext view))
	(stream (printing view))
	)
    (format stream "% some rectangles~%")
    (make-ps-color-or-pattern view)
    (let (x y  width height)
      (loop for l from 0 to (1- (/ (length rects) 4))
	    do
	    (setf x      (nth (+ (* 4 l) 0) rects)
		  y      (nth (+ (* 4 l) 1) rects)
		  width  (nth (+ (* 4 l) 2) rects)
		  height (nth (+ (* 4 l) 3) rects))
	    (format stream "  n~%")
	    (format stream "  ~d ~d m~%" x (try y))
	    (format stream "  ~d ~d l~%" (+ x width) (try y))
	    (format stream "  ~d ~d l~%" (+ x width) (try (+ y height)))
	    (format stream "  ~d ~d l~%" x (try (+ y height)))
	    (format stream "  ~d ~d l~%" x (try y))
	    (if fill
		(format stream "  c f~%")
		(progn
		  (make-ps-line-style gc stream)
		  (format stream "  s~%~%")
		  ))))
    ))
(defun ps-arc (x y x-rad y-rad start-angle angle fill arc-mode view)
  (let ((gc (gcontext view))
	(stream (printing view))
	(sa (round (* start-angle 180) pi))
	(se (round (* (+ start-angle angle) 180) pi)))
    (format stream "% an arc ~%")
    (format stream "  n~%")
    (format stream "  ~d ~d ~d ~d ~d ~d ellipse~%"
	    (round (+ x x-rad)) (round (try (+ y y-rad))) (round x-rad) (round (- y-rad))
	    (if (plusp angle) sa se) (if (plusp angle) se sa))
    (when (and fill (not (eql sa (mod se 360))) (eq arc-mode :PIE-SLICE))
      (format stream "  ~d ~d  l ~%" (round (+ x x-rad)) (round (try (+ y y-rad)))))
    (make-ps-color-or-pattern view)
    (if fill
	(format stream "  c f~%")
	(progn
	  (make-ps-line-style gc stream)
	  (format stream "  s~%~%")
	  ))
    ))

(defun ps-arcs (arcs fill arc-mode view)
  (let ((gc (gcontext view))
	(stream (printing view))
	)
    (format stream "% some arcs ~%")
    (make-ps-color-or-pattern view)
    (loop for i from 0 to (- (/ (length arcs) 6) 1)
	  do
	  (let ((x       (nth (* i 6) arcs))
		(y       (nth (+ (* i 6) 1) arcs))
		(x-rad   (/ (nth (+ (* i 6) 2) arcs) 2))
		(y-rad   (/ (nth (+ (* i 6) 3) arcs) 2))
		(start-angle (nth (+ (* i 6) 4) arcs))
		(angle   (nth (+ (* i 6) 5) arcs)))
	       (format stream "  n~%")
	    (format stream "  ~d ~d ~d ~d ~d ~d ellipse~%"
		    (round (+ x x-rad))
		    (round (try (+ y y-rad)))
		    (round x-rad) (round y-rad)
		    ;; exchange start and angle
		    (if (plusp angle)
			(round  (* start-angle 180) pi)
			(round  (* (+ start-angle angle) 180) pi))
		    (if (plusp angle)
			(round  (* (+ start-angle angle) 180) pi)
			(round  (* start-angle 180) pi))
		    )
	    (when (and fill (> (mod angle (* 2 pi)) 0.000001) (eq arc-mode :PIE-SLICE))
	      (format stream "  ~d ~d  l ~%" (round (+ x x-rad)) (try (round (+ y y-rad)))))
	    (if fill
		(format stream "  c f~%~%")
		(progn
		  (make-ps-line-style gc stream)
		  (format stream "  s~%~%")
		  ))))
    ))

(defun format-text (string)
  ;;  runde Klammern werden mit \( geschrieben
  (if (or (position #\( string)(position #\) string))
      (loop
       with new      = nil
       with str      = string
       with pos      = 0
       until (not (setf pos (or (position #\( str)(position #\) str))))
       do
       (setf new (concatenate 'string
			      new
			      (subseq str 0 pos)
			      "\\" 
			      (subseq str pos (1+ pos))
			      ))
       (setf str (subseq str (1+ pos)))
       finally (return (concatenate 'string new str)))
      string))

(defun ps-glyph (x0 y0 ele size width translate view)
  (declare (ignore size width translate))
  (xlib::force-gcontext-changes (gcontext view))
  (let ((font-size 14)
	(font-name "Helvetica")
	(stream (printing view))
	(font (xlib::font-name (xlib::gcontext-font (gcontext view)))))
    (format stream "% a glyph ~%")
    ;(format t "font = ~s   " font)
    (make-ps-color-or-pattern view)
    (let ((font-descr (parse-xlib-font-string font)))
      ;(format t " computes to ~a~%" font-descr)
      (setf font-name (find-ps-font-name font-descr))
      (when (integerp (third font-descr))(setf font-size (third font-descr)))
      (format stream "  /~a findfont ~d scalefont setfont~%" font-name font-size)
      (format stream "  ~d ~d m ~%" x0 (try y0))
      (format stream "  (~a) show ~%~%" ele))
  ))

(defun ps-glyphs (x0 y0 string size width translate end start view)
  (declare (ignore size width translate))
;; problem with textfonts in graphic-outputdemo of GINA
;  (xlib::force-gcontext-changes (gcontext view))  
  (let ((font-size 14)
	(font-name "Helvetica")
	(stream (printing view))
	(font (xlib::gcontext-font (gcontext view)))
	(fontname (xlib::font-name (xlib::gcontext-font (gcontext view)))))
;    (setf *view* view) (print font)
    (format stream "% a text ~%")
    (make-ps-color-or-pattern view)
;    (format t "font = ~s computes to " fontname)
    (let ((font-descr (parse-xlib-font-string fontname)))
;      (format t "~a ~%" font-descr)
      (setf font-name (find-ps-font-name font-descr))
      (if (third font-descr)
	  (setf font-size (third font-descr))
	  (setf font-size (+ (xlib::font-ascent font) (xlib::font-descent font))))
      )
    (format stream "  /~a findfont ~d scalefont setfont~%" font-name font-size)
    (format stream "  ~d ~d moveto ~%" x0 (try y0))
    (format stream "  (~a) show ~%~%" (format-text (subseq string start end)))
    ))

(defun ps-bitmap (pixmap view x-pos y-pos)
  (let ((gc (gcontext view))
	(stream (printing view))
	(window (x-window view))
	(image (xlib::get-image pixmap
				:x (xlib::drawable-x pixmap)
				:y (xlib::drawable-y pixmap)
				:width  (xlib::drawable-width pixmap)
				:height (xlib::drawable-height pixmap)
				:format :z-pixmap)))
    (let* (
	   (arr (xlib::image-z-pixarray image))
	   (wi  (xlib::image-width  image))
	   (he  (xlib::image-height image))
	   (fcol (xlib::gcontext-foreground gc))
	   (bcol (xlib::gcontext-background gc))
	   ;; fetch clip-mask
	   (clip-mask-pixmap (xlib::gcontext-clip-mask gc)))
      
      ;; when clip-mask is a pixmap
      (when  (xlib::drawable-p clip-mask-pixmap)
	     (let* (
		   (clip-width  (xlib::drawable-width  clip-mask-pixmap))
		   (clip-height (xlib::drawable-height clip-mask-pixmap))
		   (clip-arr (xlib::image-z-pixarray
			(xlib::get-image
			 clip-mask-pixmap
			 :x (xlib::drawable-x clip-mask-pixmap)
			 :y (xlib::drawable-y clip-mask-pixmap)
			 :width  clip-width
			 :height clip-height
			 :format :z-pixmap))))

	     (format stream "% a background bitmap ~%")
	     (format stream "  ~d ~d ~d srgb ~%"
		     (red-value   bcol window)
		     (green-value bcol window)
		     (blue-value  bcol window))
	     (format stream "   ~d ~d ~%" clip-width clip-height)
	     (format stream "   true ~%")
	     (format stream "   [1 0 0 1 -~d -~d]~%" x-pos (- (try y-pos) clip-height))
	     (format stream "{<~%");; here comes the Bitmap as a Hex-String
	     (loop for  i from  (1- clip-height) downto 0 do
		   (let ((z 0))
		     (loop for j from 0 to (1- clip-width) do
					;(format t "~d" (aref clip-arr i j))
			   (case (mod j 4)
			     ((0) (incf z (* 8 (aref clip-arr i j))))
			     ((1) (incf z (* 4 (aref clip-arr i j))))
			     ((2) (incf z (* 2 (aref clip-arr i j))))
			     ((3) (incf z (aref clip-arr i j))))
			   (when (or (= (mod j 4) 3) (= j (1- clip-width)))
			     (format stream "~x" z)
			     (setf z 0)))
					;(format t ".")
		     (format stream "~%")))
					;(format t "~%")
	     (format stream ">} imagemask~%~%")))

      ;; when clip-mask is a list 
      (when  (consp clip-mask-pixmap)
	;; 
	(format stream "% a background bitmap ~%")
	(format stream "  ~d ~d ~d srgb ~%"
		(red-value   bcol window)
		(green-value bcol window)
		(blue-value  bcol window))
	(format stream "% a backfround rectangle~%")
	(format stream "  n~%")
	(format stream "  ~d ~d m~%" x-pos (try y-pos))
	(format stream "  ~d ~d l~%" (+ x-pos wi) (try y-pos))
	(format stream "  ~d ~d l~%" (+ x-pos wi) (try (+ y-pos he)))
	(format stream "  ~d ~d l~%" x-pos (try (+ y-pos he)))
	(format stream "  ~d ~d l~%" x-pos (try y-pos))
	(format t "~d ~d ~d ~d ~%" x-pos y-pos wi he)
	(format stream "  c f~%")	
	)
      
      (format stream "% a bitmap ~%")
	   (format stream "  ~d ~d ~d srgb ~%"
		   (red-value   fcol window)
		   (green-value fcol window)
		   (blue-value  fcol window))
	   (format stream "   ~d ~d ~%" wi he)
	   (format stream "   true ~%")
	   (format stream "   [1 0 0 1 ~d ~d]~%" (- 0 x-pos) (- he (try y-pos)))
	   (format stream "{<~%");; here comes the Bitmap as a Hex-String
	   (loop for  i from  (1- he) downto 0 do
		 (let ((z 0))
		   (loop for j from 0 to (1- wi) do
					;(format t "~d" (aref arr i j))
			 (case (mod j 4)
			   ((0) (incf z (* 8 (aref arr i j))))
			   ((1) (incf z (* 4 (aref arr i j))))
			   ((2) (incf z (* 2 (aref arr i j))))
			   ((3) (incf z (aref arr i j))))
			 (when (or (= (mod j 4) 3) (= j (1- wi)))
			   (format stream "~x" z)
			   (setf z 0)))
		   (format stream "~%")))
	   (format stream ">} imagemask~%~%")
	   )))
 
(defun red-value (pixel window)
  (xlib::color-red
   (first (xlib::query-colors (xlib::window-colormap window)
		       (list pixel)))))

(defun green-value (pixel window)
  (xlib::color-green
   (first (xlib::query-colors (xlib::window-colormap window)
		       (list pixel)))))

(defun blue-value (pixel window)
  (xlib::color-blue
   (first (xlib::query-colors (xlib::window-colormap window)
		       (list pixel)))))

(defun parse-xlib-font-string (xlib-font-string)
;; "-adobe-times-bold-i-normal--17-100-..."
  (loop for char across   xlib-font-string
	with count      = 0
	with foundry    = nil
	with family     = nil
	with weight     = nil
	with slant      = nil
	with pixel-size = nil
	with char-list  = nil
	until   (= count 8)
	finally
	  (return (process-font-descriptor-components foundry family weight slant pixel-size))
	do
    (push char char-list)
    (when (char-equal char #\-) (incf count)
	  (when (= count 2)
	    (setq foundry   (concatenate 'string (butlast (cdr (reverse char-list))))
		  char-list nil))
	  (when (= count 3)
	    (setq family    (concatenate 'string
					 (substitute #\- #\space (butlast (reverse char-list))
						     :test #'char=))
		  char-list nil))
	  (when (= count 4)
	    (setq weight    (concatenate 'string (butlast (reverse char-list)))
		  char-list nil))
	  (when (= count 5)
	    (setq slant     (concatenate 'string (butlast (reverse char-list)))
		  char-list nil))
	  (when (= count 7) (setq char-list nil))
	  (when (= count 8)
	    (setq pixel-size
		  (read-from-string
		    (concatenate 'string (butlast (reverse char-list)))))))))

(defun process-font-descriptor-components
       (foundry family-name weight-name slant pixel-size)
  (let (family face)
    (setq family (concatenate 'string
		     foundry
		     (if (not (or (string-equal "" foundry)
				  (string-equal "" family-name))) "-" "")
		     family-name)
	  face
	(concatenate 'string
		     weight-name
		     (if (not (or (string-equal "" weight-name)
				  (string-equal "" slant))) "-" "")
		     (when (not (string-equal "" slant))
		       (cond ((string-equal "i" slant) "italic")
			     ((string-equal "r" slant) "roman")
			     ((string-equal "o" slant) "oblique")
			     ((string-equal "ri" slant) "reverse-italic")
			     ((string-equal "ro" slant) "reverse-oblique")
			     ((string-equal "ot" slant) "other")
			     (t slant)))))
    (list family
	  (if (string-equal face "") "normal" face) ;; Face may be empty.
	  pixel-size)))

(defun find-ps-font-name (font-descr)
  (let ((family (string (first font-descr)))
	(face   (string (second font-descr))))
    (cond     
      ((< (length family) 6) "Courier")

      ((string-equal family "courier" :start1 6 :end1 12 :end2 6)
       (if  (string-equal face "bold" :start1 0 :end1 3 :end2 3)
	    (if (string-equal face "oblique" :start1 5 :end1 11 :end2 6)
		"Courier-BoldOblique"
		"Courier-Bold")
	    (if (string-equal face "oblique" :start1 7 :end1 13 :end2 6)
		"Courier-Oblique"
		"Courier")))
	  
      ((string-equal family "times" :start1 6 :end1 10 :end2 4)
       (if  (string-equal face "bold" :start1 0 :end1 3 :end2 3)
	    (if (string-equal face "italic" :start1 5 :end1 10 :end2 5)
		"Times-BoldItalic"
		"Times-Bold")
	    (if (string-equal face "italic" :start1 7 :end1 12 :end2 5)
		"Times-Italic"
		"Times-Roman")))
      
      ((string-equal family "helvetica" :start1 6 :end1 14 :end2 8) 
       (if  (string-equal face "bold" :start1 0 :end1 3 :end2 3)
	    (if (string-equal face "oblique" :start1 5 :end1 11 :end2 6)
		"Helvetica-BoldOblique"
		"Helvetica-Bold")
	    (if (string-equal face "oblique" :start1 7 :end1 13 :end2 6)
		"Helvetica-Oblique"
		"Helvetica")))

      ((string-equal family "new-century-schoolbook" :start1 6 :end1 27 :end2 21)
       (if  (string-equal face "bold" :start1 0 :end1 3 :end2 3 :end2 3)
	    (if (string-equal face "italic" :start1 5 :end1 10 :end2 5)
		"NewCenturySchlbk-BoldItalic"
		"NewCenturySchlbk-Bold")
	    (if (string-equal face "italic" :start1 7 :end1 12 :end2 5)
		"NewCenturySchlbk-Italic"
		"NewCenturySchlbk-Roman")))
      
      ((string-equal family "palatino")
       (if  (string-equal face "bold" :start1 0 :end1 3 :end2 3 :end2 3)
	    (if (string-equal face "italic" :start1 5 :end1 10 :end2 5)
		"Palatino-BoldItalic"
		"Palatino-Bold")
	    (if (string-equal face "italic" :start1 7 :end1 12 :end2 5)
		"Palatino-Italic"
		"Palatino-Roman")))
    
      ((string-equal family "symbol" :start1 6 :end1 11 :end2 5) "Symbol")
      ((string-equal family "zapfdingbats") "ZapfDingBats")

      (T "Courier"))
    ))
(defun max-x (obj)
  (+ (gina::x-pos obj) (gina::width obj)))
(defun max-y (obj)
  (+ (gina::y-pos obj) (gina::height obj)))

(defun ps-header (view)
  (let (w h maxx maxy (stream (printing view)))
    (with-slots (x-pos y-pos width height view-objects) view
       (if (and view-objects (not *postscript-fullpage*))
	   (progn 
	     (setf maxx (apply #'max (mapcar #'max-x view-objects)))
	     (setf maxy (apply #'max (mapcar #'max-y view-objects)))
	     (setf w maxx)
	     (setf h maxy)
	     (setf *postscript-height* h)
	     ;; do autoscaling if necessary
	     (when (> maxx 650)
	       (setf *postscript-scale* (float (min *postscript-scale* (/ 650 maxx))))))
	   (progn
	     (setf w width)
	     (setf h height)
	     (setf *postscript-height* *postscript-height-p*)))

       (format stream "%!PS-Adobe-2.0 EPSF-2.0~%")
       (format stream "%%BoundingBox: 0 0 ~d ~d ~%"
	       (* w *postscript-scale*) (* h *postscript-scale*))
       (format stream "%%Title: Postscript-Output fuer GINA
%%Creator: F. Leugner , W.Deisel, Fa. S.E.P.P.
%%End Comments 
gsave 

 %  a4
   /m /moveto load def /r /rlineto load def /l /lineto load def 
   /s /stroke load def /n /newpath load def /c /closepath load def
   /srgb /setrgbcolor load def    /f /fill load def
   /a /add load def /mu /mul load def /slw /setlinewidth load def

% CTM berechnen
% clippath verschieben nach Nullpunkt
  /llx clippath pathbbox pop pop pop def
  /lly clippath pathbbox pop pop exch pop def
% matrixelemente der ctm berechnen
  /m23 0 0 transform exch pop def 
  /m22 0 1 transform exch pop m23 sub def 
% y-wertebereich bestimmen
  /cy0  llx lly transform exch pop def
  /cx0  llx lly transform pop def
  /nullx 0 0 transform pop def
  /nully 0 0 transform exch pop def
  nullx cx0 lt { llx 0 translate } if
  nully cy0 lt m22 0 gt and { 0 lly translate } if
  nully cy0 gt m22 0 lt and { 0 lly translate } if 
")
       (when (eql *postscript-format* :din-a4-quer)
	    (format stream "% dia-a4-quer ~%")
	    (if *postscript-fullpage*
		(format stream " 806 0 translate  90 rotate ~%")
		(format stream " 560 0 translate  90 rotate ~%")
		))
       (when (and (numberp *postscript-scale*)(not (= 1.0 *postscript-scale*)))
	  (format stream "% scaling with factor ~%")
	  (format stream "  ~f ~f scale ~%" *postscript-scale* *postscript-scale*))
	
       (format stream "  ~f ~f scale ~%" *postscript-unit* *postscript-unit*)
       (format stream "
%  Arc-definition x y r a1 a2
/ellipsedict 8 dict def
ellipsedict /mtrx matrix put

/ellipse
  { ellipsedict begin
    /endangle exch def
    /startangle exch def
    /yrad exch def
    /xrad exch def
    /y exch def
    /x exch def

  /savematrix mtrx currentmatrix def
  x y translate
  xrad yrad neg scale
  0 0 1 startangle endangle arc
  savematrix setmatrix
  end
 } def

  2 setlinecap

% ****************** here begins pattern part ********

/sms { % SetMusterScreen
   /winkel exch def   % gewuenscter winkel
   /breite exch def   % gewuenschte breite in Punkt
   /muster exch def   % Musterstring
   /samplebreite muster length sqrt cvi def
   /aufl matrix defaultmatrix 3 get abs 72 mu def
   gsave
      winkel rotate
      samplebreite dup scale
      1 0 dtransform
   grestore
   2 copy	% Entfernungsberechnung
   dup mu	% Ergebnis in gbreite speichern
   exch
   dup mu
   a sqrt
   /gbreite exch def
   exch atan	% Winkel berechnen
   /winkel exch def
   /aus 0 def	% ausgeschaltete Punkte
   /an  0 def	% eingeschaltete Punkte
   /freq aufl breite div def
   freq winkel {SF} setscreen
   aus aus an a div  setgray
   {} settransfer
} bind def

/SF { % hier steht die eigentliche Screenfunktion 
   /Y exch def	% Koordinaten der Halbtonzelle
   /X exch def	% zwischen -1 und 1
   /XX X 1 a 2 div  samplebreite mu cvi def
   /YY Y 1 a 2 div  samplebreite mu cvi def
   /zeiger XX YY samplebreite mu a def
   muster zeiger 1 getinterval (1) eq {
      /an an 1 a def 1
   }{
      /aus aus 1 a def 0.
   } ifelse
} bind def

% ****************** here begins the variable part ********
")))
  
  (when *postscript-fullpage* (ps-background view))
  )

(defun ps-background (view)
   (let ((color (first (xlib::query-colors (xlib::window-colormap (x-window view))
		      (get-motif-resources view :background))))
	 (stream (printing view)))
    (with-slots (x-pos y-pos width height) view
      (format stream "% the background ~%")
      (format stream "  n~%")
      (format stream "  ~d ~d m~%" 0 0)   ;; x-pos and und y-pos sometimes are unbound
      (format stream "  ~d ~d r~%" (/ width *postscript-scale* *postscript-unit*) 0)
      (format stream "  ~d ~d r~%" 0 (/ height *postscript-scale* *postscript-unit*))
      (format stream "  ~d ~d r~%" (- 0 (/ width *postscript-scale* *postscript-unit*)) 0)
      (format stream "  ~f ~f ~f srgb ~%"
	      (xlib::color-red   color)
              (xlib::color-green color)
              (xlib::color-blue  color))
      (format stream "  c f~%~%")
    ))
   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dialog-menu for postscript-parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defginamethod open-printer-dialog ((doc document))
  (description "popup a printer menu")
   (let* (
	 (mdb (make-modal-dialog-box "Test" :document doc
					       :resize t
					       :allow-shell-resize nil))
	 (printerlist
	  (make-scrollable-selection-list mdb (append (list "$PRINTER") (get-printer))
					  :initial-value *postscript-printer*))
					 
	 (rc  (make-row-column mdb :orientation :horizontal))
	 (rc2 (make-row-column mdb :orientation :vertical))
	 (pprint (make-push-button
                rc
                "Print"
                :activate-callback (make-callback #'cb-pprint mdb)))
	 (pcancle (make-push-button
                rc
                "Cancel"
                :activate-callback (make-callback #'cb-cancle mdb)))
	 (pwrite (make-labeled-text
                  rc
                  "Print to Disk"
		  :value (name doc)
                  ))
	 (pformat (make-radio-button-group
		  rc2
		  '("Portrait" "Landscape" )
		  :initial-value "Portrait"
		  :value-changed-callback (make-callback #'cb-format)))
	 (pfullp (make-radio-button-group
		  rc2
		  '("fullpage" "EPSF")
		  :initial-value "fullpage"
		  :value-changed-callback (make-callback #'cb-fullp)))
	 (pscale (make-scale
		    rc2
		    :title-string "Scale Image:"
		    :value 100
		    :minimum 1
		    :maximum 500
		    :orientation :horizontal
		    ))
	 (pcopies (make-scale
		  rc2
		  :title-string "Number of Copies:"
		  :value 1
		  :minimum 1
		  :maximum 20
		  :orientation :horizontal))
	 )
;; set globals to default
    (setf *postscript-print-string*
	  (concatenate 'string "| lpr -h -P" *postscript-printer*))
    (setf *postscript-format* :din-a4-hoch) 
    (setf *postscript-scale*  1.0)
    (setf *postscript-fullpage* t)
    (setf *postscript-copies* 1)

    (setf (value-changed-callback pscale)
            (make-callback #'cb-scale))
    (setf (activate-callback pwrite)
            (make-callback #'cb-write mdb))
    (setf (value-changed-callback pcopies)
            (make-callback #'cb-copies))
    (setf (value-changed-callback printerlist)
            (make-callback #'cb-printerlist))
    (define-form-constraint printerlist
	:top-attachment :form
	:left-attachment :form
	:right-attachment :widget :right-widget rc2
	:bottom-attachment :widget :bottom-widget rc)
    (define-form-constraint rc2
	:top-attachment :form
	:right-attachment :form
	:bottom-attachment :widget :bottom-widget rc)
    (define-form-constraint rc
	:bottom-attachment :form
	:left-attachment :form
	:right-attachment :form)
    (pop-up mdb)
    ))

(defun cb-pprint (box)
  (setf *postscript-print-string* (concatenate 'string "| lpr -h -P" *postscript-printer*))
  (pop-down box))
(defun cb-cancle (box)
  (setf *postscript-print-string* nil)
  (pop-down box)
  )
(defun cb-write (box new)
  (setf *postscript-print-string*
	(concatenate 'string " > " new ".ps"))
  (pop-down box)
  )
(defun cb-format (new old)
  (declare (ignore old))
  (if (string= new "Portrait")
      (setf *postscript-format* :din-a4-hoch)
      (setf *postscript-format* :din-a4-quer))
  )
(defun cb-fullp (new old)
  (declare (ignore old))
  (if (string= new "EPSF")
      (setf *postscript-fullpage* nil)
      (setf *postscript-fullpage* t))
  )
(defun cb-scale (new)
  (declare (ignore old))
  (setf *postscript-scale* (float (/ new 100)))
  )
(defun cb-copies (new)
  (declare (ignore old))
  (setf *postscript-copies* new)
  )
(defun cb-printerlist (new old)
  (declare (ignore old))
  (setf *postscript-printer* new)
  )

(defun get-printer ()
  (with-open-file
        (open-file (make-pathname :directory "/etc" :name "printcap"))
      (let ((o nil))
	(loop
	 (let ((line (read-line open-file nil nil))
	       )
	   (if (and (search "|" line)(not (char= (aref line 0) #\#)))
	       (push (subseq line 0 (search "|" line)) o))
	   (unless line (return o)))
	 )
	)))

