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

(setq *sccs-id* "@(#)proto-view.lisp	1.12	11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class prototype-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass prototype-view (view)
  ()
  (:documentation "a view to use in the IB"))

(defun make-prototype-view (parent &key (class 'prototype-view) 
                                        (motif-resources nil)
                                        (managed t)
                                        (width 1000)
                                        (height 1000)
                                        (needs-doc t)
                                   &aux new-view)
  (declare (ignore class motif-resources needs-doc))
  (declare (special *new-views*))
  (setq new-view
	(make-widget parent
		     :class 'prototype-view
		     :initargs 
                       (list :document (first (document-list *application*))
			     :scroller (when (eq (class-of parent)
				                 (find-class 'scroller))
					    parent)
			     :width width
			     :height width
			     :backing-store nil)
		     :motif-widget-class :gina-view
                     :managed managed
		     :motif-resources `(:width ,width :height ,height
					:margin-height 0 :margin-width 0
					:translations ""
					:resize-policy :none)))

  ;; make resize-callback call a method
  (setf (resize-callback new-view) (make-callback #'gina::resized new-view))
  ;;(setf (destroy-callback new-view) (make-callback #'gina::destroyed new-view))

  ;; tell the toolkit-server to send events
  (add-event-handler new-view :exposure-mask
		     (make-callback #'gina::handle-expose new-view))
  (add-event-handler new-view :button-press-mask
		     (make-callback #'gina::handle-button-press new-view))
  (add-event-handler new-view :button-release-mask
		     (make-callback #'gina::handle-button-release 
				    (first (document-list *application*))))
  (add-event-handler new-view :button-motion-mask
		     (make-callback #'gina::handle-button-motion 
				    (first (document-list *application*))))
  (add-event-handler new-view :key-press-mask
		     (make-callback #'gina::handle-key-press new-view))
  (add-event-handler new-view :key-release-mask
		     (make-callback #'gina::handle-key-release new-view))


  ;; register the view in special variable, if desired
  (when (boundp '*new-views*)
    (push new-view *new-views*))

  ;; register the view in its enclosing shell object
  (push new-view (views (enclosing-shell parent)))
  
  new-view)

(defmethod draw ((view prototype-view) count x y width height &aux x-pos y-pos)
  "draw a pattern"
  (declare (ignore count))
  (declare (type fixnum x-pos y-pos))
  (setq y-pos (* (floor (/ y 10)) 10))
  (loop while (< y-pos (+ y height))
        do (setq x-pos (* (floor (/ x 10)) 10))
           (draw-lines view
                       (loop while (< x-pos (+ x width))
                             collect x-pos collect (+ y-pos 5)
                             collect (+ x-pos 5) collect y-pos
                             collect (+ x-pos 10) collect (+ y-pos 5)
                             collect (+ x-pos 5) collect (+ y-pos 10)
                             do (setq x-pos (+ x-pos 10))))
           (setq y-pos (+ y-pos 10))))

(defun make-test-view (parent &optional (document nil) &rest rest)
  (declare (ignore document rest))
  (make-prototype-view parent))

