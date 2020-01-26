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

(setq *sccs-id* "@(#)main-view.lisp	1.12	11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class ib-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass ib-view (view)
  ((main-plate   :accessor main-plate    :initform nil)
   (hack-init :accessor hack-init :initform nil))
  (:documentation "a view with special reaction to clicks"))

(defun make-ib-view (parent doc &aux new-view)
  (setf new-view (make-view parent 
			    :width 1000 :height 1000 
			    :document doc 
			    :class 'ib-view))
  new-view)

(defmethod button-press ((view ib-view) code repetition x y)
  "react to button-press event in the window"
  (declare (ignore repetition))
  (make-widget-selector view x y code))

(defmethod select-all ((view ib-view) &optional (plates (view-objects view)))
  "select all widget objects in the view"
  (loop for plate in plates
	do (setf (mouse-sensitive plate) t)
	   (force-redraw plate)))

(defmethod deselect-all ((view ib-view) &optional (plates (view-objects view)))
  "deselect all widget objects in the view"
  (loop for plate in plates
	do (setf (mouse-sensitive plate) nil)
	   (force-redraw plate)))

(defmethod draw ((view ib-view) count x y width height)
  "hack to update row-column sizes at initial popup"
  (declare (ignore count x y width height))
  (when (not (hack-init view))
        (loop for plate in (view-objects view)
              do (adapt-to-plate-size plate)))
  (setf (hack-init view) t))

(defmethod selected-objects ((view ib-view))
  "collect all mouse-sensitive plates"
  (loop for plate in (view-objects view)
        when (mouse-sensitive plate)
        collect plate))

;;;;;;;;;;;;;;;;;;;;;;;;;; recreate all objects ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod recreate ((view ib-view))
  "create new view-objects from document contents"

  ;; destroy old objects
  (loop for plate in (view-objects view)
        do (deinstall plate)
           (remove-dialogs (info plate))
           (destroy (gina-widget plate)))

  ;; create hierarchy of widget plates according to info
  (loop for child in (children (main-info (document view)))
        do (adapt-to-plate-size (install-plate child view)))
  )
