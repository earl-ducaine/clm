;;; -*- Mode:LISP;Syntax: Common-Lisp;Package: GINA ;Base:10-*-
;;;
;;; Copyright 1990 GMD (German National Research Center for Computer Science)
;;;

(in-package :GINA)

;;(setq *sccs-id* "@(#)views.lisp	1.26  2/5/93")


(defun get-intersecting-rectangle (x0 y0 w0 h0 x1 y1 w1 h1 &aux x y w h)
  (setq x (max x0 x1))
  (setq y (max y0 y1))
  (setq w (if (< x0 x1)
	      (- (min (+ x0 w0) (+ x1 w1)) x1)
	    (- (min (+ x1 w1) (+ x0 w0)) x0)))
  (setq h (if (< y0 y1)
	      (- (min (+ y0 h0) (+ y1 h1)) y1)
	    (- (min (+ y1 h1) (+ y0 h0)) y0)))
  (values x y (if (< w 0) 0 w) (if (< h 0) 0 h)))


(defginamethod draw ((icon movable-icon)
		     count
		     "number of calls to draw method that will immediately follow"
		     x  "upper left corner of rectangle which has to be drawn"
		     y  "upper left corner of rectangle which has to be drawn"
		     width           "size of rectangle which has to be drawn"
		     height          "size of rectangle which has to be drawn"
		     &aux (gcontext (gcontext (parent-view icon)))
		     (clipping-mask-pixmap nil)
		     (mask-gc nil)
		     old-clip-mask)
  (description "Draw icon optionally with shadow from stored pixmap"
	       :called-by-gina  "in response to expose events"
	       :default-version "draws icon with or without shadow"
	       :override        :rarely)
  (comment "You can add a deamon to display more information e.g. a text.")
  (declare (ignore count x y width height))

  ;; this should print always zero; because we need no events for clx
  ;;(format t "events: ~a~%" (xlib::event-listen *display* 0))
  (with-slots (icon-pixmap mask-image mask-pixmap parent-view selected shadow-offset
			   icon-image left-margin top-margin) icon

     ;; load pixmap when icon is drawn for the first time:
     (when (null icon-pixmap)  (get-pixmap icon))

     ;; Bug in xlib: remember old clip-mask to restore later
     (setq old-clip-mask (xlib:gcontext-clip-mask gcontext))
     
     (when mask-image
       (setq clipping-mask-pixmap
	 (xlib:create-pixmap :width (xlib:image-width  mask-image) 
			     :height (xlib:image-height mask-image) :depth 1
			     :drawable mask-pixmap
			     ))
       
       (setq mask-gc 
	 (xlib:create-gcontext 
	  :exposures :off
	  :drawable mask-pixmap :foreground 1 :background 0))
       ;; set clipping-mask-pixmap to zero by drawing a filled rectangle:
       (xlib:with-gcontext (mask-gc :foreground 0 :background 0)
	 (xlib:draw-rectangle clipping-mask-pixmap 
			      mask-gc
			      0 0 
			      (xlib:image-width mask-image)
			      (xlib:image-height mask-image) t))
       
       ;; BUG in xlib; see comment below!
       (setf (xlib:gcontext-clip-mask gcontext) old-clip-mask)
 
       (when (and mask-pixmap (> shadow-offset 0))
	 ;; get mask for drawing a shadow
	 (multiple-value-bind (x y w h)
	     (get-intersecting-rectangle 
	       (+ (x-pos icon) (left-margin icon) (shadow-offset icon))
	       (+ (y-pos icon) (top-margin icon) (shadow-offset icon)) 
	       (xlib:image-width  mask-image) (xlib:image-height mask-image)
	       (first (xlib:gcontext-clip-mask gcontext))
	       (second (xlib:gcontext-clip-mask gcontext))
	       (third (xlib:gcontext-clip-mask gcontext))
	       (fourth (xlib:gcontext-clip-mask gcontext)))
 
	   (xlib:copy-plane mask-pixmap 
			  mask-gc
			  1 
			  (- x (x-pos icon) (left-margin icon) (shadow-offset icon))
			  (- y (y-pos icon) (top-margin icon)  (shadow-offset icon)) 
			  w 
			  h
			  clipping-mask-pixmap 
			  (- x (x-pos icon) (left-margin icon) (shadow-offset icon)) 
			  (- y (y-pos icon) (top-margin icon)  (shadow-offset icon))))

	   ;; draw shadow
	 (xlib:with-gcontext
	       (gcontext
		:clip-mask clipping-mask-pixmap
		:clip-x (+ shadow-offset (x-pos icon) left-margin)
		:clip-y (+ shadow-offset (y-pos icon) top-margin))
	     (draw-rectangle icon 0 0 (width icon) (height icon) t))
	   ;; BUG in xlib; see comment below!
	 (setf (xlib:gcontext-clip-mask gcontext) old-clip-mask)

	   ;; clear the clipping-mask-pixmap again, when used for shadow:
	 (xlib:with-gcontext (mask-gc :foreground 0 :background 0)
	     (xlib:draw-rectangle clipping-mask-pixmap 
				  mask-gc
				  0 0 
				  (xlib:image-width mask-image)
				  (xlib:image-height mask-image) t))
	 
	 ;; BUG in xlib; see comment below!
	 (setf (xlib:gcontext-clip-mask gcontext) old-clip-mask))

       ;; get mask for the icon itself
       (multiple-value-bind (x y w h)
	   (get-intersecting-rectangle 
	          (+ (x-pos icon) (left-margin icon))
		  (+ (y-pos icon) (top-margin icon)) 
		  (xlib:image-width  mask-image) (xlib:image-height mask-image)
		  (first (xlib:gcontext-clip-mask gcontext))
		  (second (xlib:gcontext-clip-mask gcontext))
		  (third (xlib:gcontext-clip-mask gcontext))
		  (fourth (xlib:gcontext-clip-mask gcontext)))
	 (xlib:copy-plane mask-pixmap 
			  mask-gc
			  1 
			  (- x (x-pos icon) (left-margin icon)) 
			  (- y (y-pos icon) (top-margin icon)) 
			  w 
			  h
			  clipping-mask-pixmap 
			  (- x (x-pos icon) (left-margin icon)) 
			  (- y (y-pos icon) (top-margin icon)))))
       
     (xlib:with-gcontext
      (gcontext
       :exposures :off
       :clip-mask (or clipping-mask-pixmap mask-pixmap (xlib:gcontext-clip-mask gcontext))
       :clip-x (if mask-pixmap
		   (+ (x-pos icon) left-margin)
		   (xlib:gcontext-clip-x gcontext))
       :clip-y (if mask-pixmap
		   (+ (y-pos icon) top-margin)
		   (xlib:gcontext-clip-y gcontext))
       :background (if selected
		       (xlib:gcontext-foreground gcontext)
		       (xlib:gcontext-background gcontext))
       :foreground (if selected
		       (xlib:gcontext-background gcontext)
		     (xlib:gcontext-foreground gcontext)))
       (copy-plane parent-view 
		   icon-pixmap
		   1 ;; depth
		   0 0 ;;  source
		   (xlib:image-width  icon-image)
		   (xlib:image-height icon-image)
		   (+ (x-pos icon) left-margin) ;; destination
		   (+ (y-pos icon) top-margin)))

     (when clipping-mask-pixmap (xlib:free-pixmap clipping-mask-pixmap))
     (when mask-gc (xlib:free-gcontext mask-gc))
     
     ;; BUG in xlib: after macro with-gcontext the clip-mask is damaged
     ;; when a pixmap was used as clip-mask.
     ;; The following call leads to an error before restoring the clip-mask:
     ;; (format t "~a~%" (xlib:gcontext-clip-mask gcontext))
     (setf (xlib:gcontext-clip-mask gcontext) old-clip-mask)
     ))

