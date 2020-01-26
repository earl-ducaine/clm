;;;-*-Mode:LISP;Syntax: Common-Lisp; Package:ib; -*-
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

(setq *sccs-id* "@(#)prefs.lisp	1.12	11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Definitions for class 'color-spec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass color-spec (modeless-dialog-box)
  ((current-color :accessor current-color :initarg :current-color)
   (current-red   :accessor current-red :initform 0)
   (current-green :accessor current-green :initform 0)
   (current-blue  :accessor current-blue :initform 0)
   (resource :accessor resource :initarg :resource)
   (selection :accessor selection)
   (intensity :accessor intensity)
   (red-scale :accessor red-scale)
   (green-scale :accessor green-scale)
   (blue-scale :accessor blue-scale))
  (:documentation "The color wheel"))

(defun make-color-spec (document resource &aux
				  box row)
  (setf box (make-modeless-dialog-box
	     (symbol-name (resource-name resource))
	     :document document
             :class 'color-spec
             :initargs (list :resource resource :current-color "Blue")
	     :motif-resources '(:accelerators "")))
  (setf row (make-row-column box :orientation :vertical))
  (setf (selection box)
        (make-scrollable-selection-list row
               (loop for color in (xtk:get-x-colors)
                     when (and (not (equal (fourth color) "MediumGoldenrod"))
                               (not (equal (fourth color) "MediumForestGreen")))
                     collect (fourth color))
					    :visible-item-count 20 
					    :selection-policy :single))
  (make-label row "Red:")
  (setf (red-scale box)
	(make-scale row :orientation :horizontal :show-value t 
		    :title-string "Green:" :maximum 255 :value 255))
  (setf (green-scale box)
	(make-scale row :orientation :horizontal :show-value t 
		    :title-string "Blue:" :maximum 255 :value 0))
  (setf (blue-scale box)
	(make-scale row :orientation :horizontal :show-value t 
		    :title-string "Intensity:" :maximum 255 :value 0))
  (setf (intensity box)
	(make-scale row :orientation :horizontal :show-value t 
		    :maximum 100 :value 100))
  (setf (value-changed-callback (red-scale box))
        (make-callback 'set-red box))
  (setf (value-changed-callback (green-scale box))
        (make-callback 'set-green box))
  (setf (value-changed-callback (blue-scale box))
        (make-callback 'set-blue box))
  (setf (value-changed-callback (intensity box))
        (make-callback 'adjust-intensity box))
  (gina::set-callback (selection box) :single-selection 
                      (make-callback 'set-color box))
  (unless (equal "" "Blue")
	  (set-color box "Blue" nil))
  box)

(defmethod change-color ((wheel color-spec))
  (set-motif-resources (intensity wheel) :value 
		       (min 100 (round (* (/ 100 256)
					  (max (current-red wheel) 
					       (current-green wheel)
					       (current-blue wheel))))))
  (let ((infos (loop for plate in (view-objects (main-view (document wheel)))
                     when (mouse-sensitive plate)
                     collect (info plate))))
    (when infos
        (make-change-multi-resource-command (document wheel) infos 
                     (resource wheel) (current-color wheel)))))

(defmethod make-rgb-string ((wheel color-spec))
  (setf (current-color wheel)
	(format nil "#~4,'0x~4,'0x~4,'0x" 
		(* 256 (current-red wheel))
		(* 256 (current-green wheel))
		(* 256 (current-blue wheel)))))

(defmethod adjust-intensity ((wheel color-spec) new-intensity 
			     &aux old-max factor)
  (setf old-max (max (current-red wheel) (current-green wheel)
		     (current-blue wheel)))
  (if (zerop old-max)
      (progn
       (setf (current-red wheel) 
	     (min 255 (round (* (/ 256 100) new-intensity))))
       (setf (current-green wheel) 
	     (min 255 (round (* (/ 256 100) new-intensity))))
       (setf (current-blue wheel) 
	     (min 255 (round (* (/ 256 100) new-intensity)))))
      (progn
       (setf factor (/ (* (/ 256 100) new-intensity) old-max))
       (setf (current-red wheel) 
	     (min 255 (round (* factor (current-red wheel)))))
       (setf (current-green wheel) 
	     (min 255 (round (* factor (current-green wheel)))))
       (setf (current-blue wheel) 
	     (min 255 (round (* factor (current-blue wheel)))))))
  (set-motif-resources (red-scale wheel) :value (current-red wheel))
  (set-motif-resources (green-scale wheel) :value (current-green wheel))
  (set-motif-resources (blue-scale wheel) :value (current-blue wheel))
  (make-rgb-string wheel)
  (change-color wheel))

(defmethod set-red ((wheel color-spec) new-value)
  (setf (current-red wheel) new-value)
  (make-rgb-string wheel)
  (change-color wheel))

(defmethod set-green ((wheel color-spec) new-value)
  (setf (current-green wheel) new-value)
  (make-rgb-string wheel)
  (change-color wheel))

(defmethod set-blue ((wheel color-spec) new-value)
  (setf (current-blue wheel) new-value)
  (make-rgb-string wheel)
  (change-color wheel))

(defmethod set-color ((wheel color-spec) new-color color-no)
  (declare (ignore color-no))
  (setf (current-color wheel) new-color)
  (loop for color in (xtk:get-x-colors) do
	(when (equal new-color (fourth color)) 
	      (setf (current-red wheel) (first color))
	      (setf (current-green wheel) (second color))
	      (setf (current-blue wheel) (third color))
	      (set-motif-resources (red-scale wheel) 
				  :value (current-red wheel))
	      (set-motif-resources (green-scale wheel)
				  :value (current-green wheel))
	      (set-motif-resources (blue-scale wheel)
				  :value (current-blue wheel))
	      (change-color wheel)
	      (return-from set-color))))
