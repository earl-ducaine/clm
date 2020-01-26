;;;-*-Mode:LISP;Syntax: Common-Lisp;Package:ib-demo;Base:10-*-
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

(in-package :ib-demo)

(setq *sccs-id* "@(#)demo-view.lisp	1.13	11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class ib-demo-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This view can be used in an IB demonstration to connect callbacks.
;;; If this class is loaded, sin-demo.ibuild can be exercised.

(defclass ib-demo-view (view)
  ((amplitude  :accessor amplitude :initform 45)
   (start      :accessor start :initform 0)
   (wavelength :accessor wavelength :initform 100)
   (mode       :accessor mode    :initform :sinus)
   (styles     :accessor styles  :initform nil)
   (thickness  :accessor thickness :initform 0)
   (point-array :accessor point-array))
  (:documentation "a view with special reaction to clicks"))

(defun make-ib-demo-view (parent doc &aux new-view)
  (setf new-view (make-view parent 
			    :width 400 :height 210
			    :document doc 
			    :class 'ib-demo-view))
  (setf (point-array new-view) (make-array 400 :element-type 'xlib:int16))
  new-view)

(defmethod resized ((view ib-demo-view) ignore)
  (declare (ignore ignore))
  (update-slots view)
  (force-redraw view))

(defmethod draw ((view ib-demo-view) count x y width height)
  (declare (ignore x y width height))
  (when (zerop count)
     (xlib:with-gcontext ((gcontext view) :line-width 0)
        (draw-line view 0 (round (/ (height view) 2)) 
                        (width view) (round (/ (height view) 2)))
        (draw-line view (start view) 0 (start view) (height view)))
     (when (member :sinus (styles view)) (draw-sinus view))
     (when (member :sawtooth (styles view)) (draw-sawtooth view))
     (when (member :square (styles view)) (draw-square view))
  ))

(defmethod draw-sinus ((view ib-demo-view))
  (let ((amp (amplitude view))
        (off (/ (height view) 2))
        (start (start view))
        (multi (/ (* 2 pi) (wavelength view))))
      (loop for i from 0 to (1- 400) by 2
            do (setf (aref (point-array view) i) i)
               (setf (aref (point-array view) (1+ i)) 
                     (round (- off (* amp (sin (* (- i start) multi)))))))
      (xlib:draw-lines (x-window view) (gcontext view) (point-array view))))

(defmethod draw-sawtooth ((view ib-demo-view))
  (let* ((len (wavelength view))
         (len1 (round (* len 0.6)))
         (len2 (round (* len 0.4)))
         (off (/ (height view) 2)))
    (loop for i from 0 to (ceiling (/ 400 len))
          do (draw-lines view
          (list (+ (* (1- i) len) (start view)) off
                (+ (* (1- i) len) (start view) len2) (- off (amplitude view))
                (+ (* (1- i) len) (start view) len1) (+ off (amplitude view))
                (+ (* i len) (start view)) off)))))

(defmethod draw-square ((view ib-demo-view))
  (let* ((len (wavelength view))
         (len2 (round (/ len 2)))
         (off (/ (height view) 2)))
    (loop for i from 0 to (ceiling (/ 400 len))
          do (draw-lines view
          (list (+ (* (1- i) len) (start view)) off
                (+ (* (1- i) len) (start view)) (- off (amplitude view))
                (+ (* (1- i) len) (start view) len2) (- off (amplitude view))
                (+ (* (1- i) len) (start view) len2) (+ off (amplitude view))
                (+ (* i len) (start view)) (+ off (amplitude view))
                (+ (* i len) (start view)) off)))))

(defmethod set-wavelength ((view ib-demo-view) len &aux old-phase)
  (setq old-phase (/ (start view) (wavelength view)))
  (setf (wavelength view) (max 1 (round len)))
  (setf (start view) (round (* old-phase (wavelength view))))
  (force-redraw view))

(defmethod set-phase ((view ib-demo-view) s)
  (setf (start view) (max 0 (round s)))
  (setf (start view) (round (/ (* (wavelength view) (start view)) 360)))
  (force-redraw view))

(defmethod set-amplitude ((view ib-demo-view) amp)
  (setf (amplitude view) (max 0 (round amp)))
  (force-redraw view))

(defmethod set-style ((view ib-demo-view) style &rest rest)
  (declare (ignore rest))
  (if (listp style)
      (setf (styles view) style)
      (setf (styles view) (list style)))
  (force-redraw view))

(defmethod set-thickness ((view ib-demo-view) thickness &rest rest)
  (declare (ignore rest))
  (setf (xlib:gcontext-line-width (gcontext view)) thickness)
  (force-redraw view))

(defmethod set-background ((view ib-demo-view) color &rest rest)
  (declare (ignore rest))
  (set-motif-resources view :background color))
