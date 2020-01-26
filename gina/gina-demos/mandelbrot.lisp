;;; -*- Mode:LISP;Syntax:Common-Lisp;Package:mandel;Base: 10 -*-
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


(in-package :GINA)
(defginapackage :mandel)
(in-package :mandel)
(setq *sccs-id* "@(#)mandelbrot.lisp	1.7  11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class mandelbrot-application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass mandelbrot-application (application)
  (;; overrides
   (name          :initform "Mandelbrot"         :allocation :class)
   (document-type :initform 'mandelbrot-document :allocation :class)
   (signature     :initform "mandel"             :allocation :class)
   (file-type     :initform "mandel"             :allocation :class)))

(defun make-mandelbrot-application ()
  (make-application :class 'mandelbrot-application))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class mandelbrot-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass mandelbrot-document (document)
  ((shell-width :initform 600)
   (shell-height :initform 400)
   (label :accessor label)
   (scale1 :accessor scale1)
   (scale2 :accessor scale2)
   (scale3 :accessor scale3)
   (button :accessor button)
   (min-x  :accessor min-x :initform -2.0) 
   (max-x  :accessor max-x :initform  1.0)
   (min-y  :accessor min-y :initform -1.0)
   (max-y  :accessor max-y :initform  1.0)))

(defmethod create-windows ((doc mandelbrot-document) &aux form scroller)
  "create the windows belonging to this document"
  (with-slots (main-shell main-view label scale1 scale2 scale3 button) doc
    (setq main-shell (make-document-shell doc))
    (setq form       (make-form main-shell))
    (setq label      (make-label form "-2.0 < X < 1.0      -1.0 < Y < 1.0"))
    (setq scale1     (make-scale form :orientation :horizontal
				 :title-string "Width"
				 :minimum 1 :maximum 1200 :value 60
                                 :value-changed-callback
                                 `(lambda (&rest ignore) 
				   (setf (modified ',doc) t))))
    (setq scale2     (make-scale form :orientation :horizontal
				 :title-string "Height"
				 :minimum 1 :maximum 800 :value 40
				 :value-changed-callback
                                 `(lambda (&rest ignore) 
				   (setf (modified ',doc) t))))
    (setq scale3     (make-scale form :orientation :horizontal
				 :title-string "Limit"
				 :minimum 1 :maximum 128 :value 8
				 :value-changed-callback
                                 `(lambda (&rest ignore) 
				   (setf (modified ',doc) t))))
    (setq button     (make-push-button form "Recompute"
                         :activate-callback
                         (make-callback 'draw-mandelbrot-set doc)))
    (setq scroller   (make-scroller form))
    (setq main-view  (make-mandelbrot-view scroller :document doc)) 
    
    (define-form-constraint label 
                            :top-attachment :form
                            :left-attachment :form
                            :right-attachment :form)
    (define-form-constraint scale1 
			    :left-attachment :form
			    :left-offset 10
			    :top-attachment :widget :top-widget label
			    :right-attachment :position :right-position 25)
    (define-form-constraint scale2 
                            :top-attachment :widget :top-widget label
                            :left-attachment :widget :left-widget scale1
			    :left-offset 10
                            :right-attachment :position :right-position 50)
    (define-form-constraint scale3
                            :top-attachment :widget :top-widget label
                            :left-attachment :widget :left-widget scale2
			    :left-offset 10
                            :right-attachment :position :right-position 75)
    (define-form-constraint button 
                            :top-attachment :widget :top-widget label
			    :top-offset 5
                            :left-attachment :widget :left-widget scale3
			    :left-offset 10
                            :right-attachment :form :right-offset 10
                            :bottom-attachment :widget :bottom-widget scroller
                            :bottom-offset 5)
    (define-form-constraint scroller
                            :top-attachment :widget :top-widget scale1
                            :left-attachment :form
                            :right-attachment :form
                            :bottom-attachment :form)
    ))

(defmethod write-to-stream ((doc mandelbrot-document) stream)
  "write the current parameter setting to the specified stream"
  (format stream "~d ~d ~d ~d ~d ~d ~d ~%"
          (value (scale1 doc)) (value (scale2 doc)) (value (scale3 doc))
          (min-x doc) (max-y doc) (min-y doc) (max-y doc)))

(defmethod read-from-stream ((doc mandelbrot-document) stream) 
  "read the stored parameter settings from the specified stream"
  (with-slots (scale1 scale2 scale3) doc
    (setf (value scale1) (read stream))
    (setf (value scale2) (read stream))
    (setf (value scale3) (read stream))
    (setf (min-x doc) (read stream))
    (setf (max-y doc) (read stream))
    (setf (min-y doc) (read stream))
    (setf (max-y doc) (read stream))))

(defun draw-mandelbrot-set (document 
			    &aux view width height limit 
				 min-x max-x min-y max-y
				 step-x step-y)
  "loop through user-specified range of points"

  ;; kill all other computations in progress
  (kill-all-background-processes document)
  
  (setq view   (main-view document))
  (setq width  (value (scale1 document)))
  (setq height (value (scale2 document)))
  (setq limit  (value (scale3 document)))
  (setq min-x (min-x document) 
	max-x (max-x document)
	step-x (/ (- max-x min-x) width))
  (setq min-y (min-y document) 
	max-y (max-y document)
	step-y (/ (- max-y min-y) height))

  ;; adapt view size to parameters
  (resize view width height)

  ;; clear both window and pixmap buffer:
  (clear-area view 0 0 width height)
  (progn (setf (drawable view) (pixmap-buffer view))
	 (clear-area view 0 0 width height)
	 (setf (drawable view) (x-window view)))

  (#-cmu with-progress-bar #-cmu (document :modal nil 
			       :centered nil
			       :message "Recomputing Mandelbrot Set ...")
   #+cmu progn
    (loop for x from min-x to max-x by step-x 
          for i from 0 
          while #-cmu (not (progress-bar-aborted)) #+cmu t do
      ;; move slider of progress bar
      #-cmu (indicate-progress (round (* (/ i (float width)) 100)))
      (loop for y from min-y to max-y by step-y
            for j from 0 do
	(when (in-mandelbrot-set x y limit)
	  ;; draw into both window and pixmap buffer:
	  (draw-point view i j)
	  (progn  (setf (drawable view) (pixmap-buffer view))
		  (draw-point view i j)
		  (setf (drawable view) (x-window view)))))
      (xlib:display-force-output *display*))))

(defun in-mandelbrot-set (x y limit 
			  &aux (new-x x) (new-y y) z (k 0))
  "test whether point (x,y) is member of Mandelbrot set"
  (setq z (complex x y))
  (loop 
    while (and (<= (+ (* new-x new-x) (* new-y new-y)) 4) (< k limit)) 
    do (setq z (+ (complex x y) (* z z)))
       (setq new-x (realpart z))
       (setq new-y (imagpart z))
       (incf k))
   ;; return whether number of iterations reached the limit
   (= k limit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class mandelbrot-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass mandelbrot-view (view)
  ())

(defun make-mandelbrot-view (parent &key document)
  (make-view parent :document document
	            :class 'mandelbrot-view
	            :double-buffering t))

(defmethod determine-window-id :after ((view mandelbrot-view))
  "immediately start computation of mandelbrot-set when view is ready"
  (draw-mandelbrot-set (document view)))


(defmethod button-press ((view mandelbrot-view) code repetition x y)
  (declare (ignore repetition code))
  (make-zoom-command view x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class zoom-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass zoom-command (mouse-down-command)
  ((name :initform "Zoom" :allocation :class)
   (new-min-x  :accessor new-min-x) 
   (new-max-x  :accessor new-max-x)
   (new-min-y  :accessor new-min-y)
   (new-max-y  :accessor new-max-y)
   (old-min-x  :accessor old-min-x :initarg :old-min-x) 
   (old-max-x  :accessor old-max-x :initarg :old-max-x)
   (old-min-y  :accessor old-min-y :initarg :old-min-y)
   (old-max-y  :accessor old-max-y :initarg :old-max-y)))

(defun make-zoom-command (view x y)
  (make-mouse-down-command (document view) view x y
			   :cursor :crosshair
			   :class 'zoom-command
			   :initargs
			   (list :old-min-x (min-x (document view))
				 :old-max-x (max-x (document view)) 
				 :old-min-y (min-y (document view))
				 :old-max-y (max-y (document view)))))

(defmethod constrain-mouse ((cmd zoom-command) x y)
  "make sure width and height of area is > 0"
  (with-slots (start-x start-y) cmd
    (setq x (max x (1+ start-x)))
    (setq y (max y (1+ start-y))))
  (values x y))

(defmethod draw-feedback ((cmd zoom-command) x y &key clear)
  "draw rectangular feedback"
  (declare (ignore clear))
  (with-slots (start-x start-y view) cmd
    (xlib:with-gcontext ((gcontext view) :line-style :dash)
      (draw-rectangle view start-x start-y (- x start-x)  (- y start-y)))))

(defmethod track-mouse ((cmd zoom-command) x y &key &allow-other-keys)
  (with-slots (start-x start-y new-min-x new-max-x new-min-y new-max-y 
		       view document) cmd
     (setq new-min-x (+ (min-x document) 
			(* (- (max-x document) (min-x document)) ;; range
			   (/ start-x (float (width view))))))
     (setq new-min-y (+ (min-y document) 
			(* (- (max-y document) (min-y document)) ;; range
			   (/ start-y (float (height view))))))
     (setq new-max-x (+ (min-x document) 
			(* (- (max-x document) (min-x document)) ;; range
			   (/ x (float (width view))))))
     (setq new-max-y (+ (min-y document) 
		        (* (- (max-y document) (min-y document)) ;; range
			   (/ y (float (height view))))))
     (setf (label-string (label document))
       (format nil "~8d < X < ~8d    ~8d < Y < ~8d" 
		new-min-x new-max-x new-min-y new-max-y))
     ))

(defmethod doit ((cmd zoom-command))
  "change range parameters in document"
  (setf (min-x (document cmd)) (new-min-x cmd))
  (setf (max-x (document cmd)) (new-max-x cmd))
  (setf (min-y (document cmd)) (new-min-y cmd))
  (setf (max-y (document cmd)) (new-max-y cmd))
  (setf (label-string (label (document cmd)))
       (format nil "~8d < X < ~8d    ~8d < Y < ~8d" 
	       (new-min-x cmd) (new-max-x cmd) 
	       (new-min-y cmd) (new-max-y cmd)))
  (draw-mandelbrot-set (document cmd)))

(defmethod undoit ((cmd zoom-command))
  "restore range parameters in document"
  (setf (min-x (document cmd)) (old-min-x cmd))
  (setf (max-x (document cmd)) (old-max-x cmd))
  (setf (min-y (document cmd)) (old-min-y cmd))
  (setf (max-y (document cmd)) (old-max-y cmd))
  (setf (label-string (label (document cmd)))
       (format nil "~8d < X < ~8d    ~8d < Y < ~8d" 
	       (old-min-x cmd) (old-max-x cmd) 
	       (old-min-y cmd) (old-max-y cmd)))
  (draw-mandelbrot-set (document cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; main program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(register-application "mandel" 'mandelbrot-application "mandel")
'(make-mandelbrot-application)
