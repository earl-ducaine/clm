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

(setq *sccs-id* "@(#)tree-window.lisp	1.13	11/8/93")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class ib-tree-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass ib-tree-window (tool-dialog-box)
  ((selection-list      :accessor selection-list)
   (name-text           :accessor name-text)
   (label-text          :accessor label-text))
  (:documentation "The IB tree tool window"))

(defun make-tree-window (doc &aux box)
  (setq box (make-tool-dialog-box "Tree Tool"
                                  :class 'ib-tree-window
                                  :allow-shell-resize t
                                  :document doc))

  (setf (name-text box)
        (make-labeled-text box "Name:"))
  (setf (value-changed-callback (name-text box))
        (make-callback #'set-widget-name doc box))
  (setf (label-text box)
        (make-labeled-text box "Label:"))
  (setf (value-changed-callback (label-text box))
        (make-callback #'set-widget-label doc box))

  (setf (selection-list box) (make-scrollable-selection-list box '()
                                            :width 250))
  (setf (default-action-callback (selection-list box))
        (make-callback #'show-dialog-for-selection doc))
  (setf (value-changed-callback (selection-list box))
        (make-callback #'update-text-fields doc box))

  (make-tap box '(clear-command clear-all-command frame-group-command
		  scroller-group-command row-column-group-command
		  form-group-command paned-window-group-command
		  ungroup-command widget-dragger delete-command
		  paste-command)
	    #'refresh-tree)
  
  (define-form-constraint (name-text box)
                          :top-attachment :widget :top-widget (main-menu box)
                          :top-offset 4
                          :left-attachment :form)
  (define-form-constraint (label-text box)
                          :top-attachment :widget :top-widget (name-text box)
                          :top-offset 4
                          :left-attachment :form)
  (define-form-constraint (selection-list box) 
                          :top-attachment :widget :top-widget (label-text box)
                          :top-offset 4
                          :left-attachment :form :right-attachment :form
                          :bottom-attachment :form)

  (add-menu-command (main-menu box)
                    "Show" "Resources..."
                    (make-callback #'open-resource-dialog doc box))
  ;;(add-menu-command (main-menu box)
  ;;                  "Show" "Refresh Tree"
  ;;                  (make-callback #'refresh-tree box))

  box)

(defmethod open-resource-dialog ((doc ib-document) box &aux plate)
  (setq plate (value (selection-list box)))
  (when plate (resource-dialog plate)))

(defmethod show-dialog-for-selection ((doc ib-document) item item-no
                                          &aux plate)
  (declare (ignore item-no))
  (setq plate (find-widget-info (main-info doc) 
                                 (string-left-trim " " item)))
  (when plate (resource-dialog plate)))

;;(defmethod pop-up :before ((win ib-tree-window))
;;  (refresh-tree win))

(defmethod refresh-tree ((win ib-tree-window) &optional cmd)
  (declare (ignore cmd))
  (set-item-list (selection-list win) 
        (collect-hierarchy (children (main-info (document win))) ""))
  (update-text-fields (document win) win (value (selection-list win)) nil))

(defmethod update-window ((win ib-tree-window) document)
  (set-item-list (selection-list win) 
        (collect-hierarchy (children (main-info document)) ""))
  (update-text-fields document win (value (selection-list win)) nil))

(defun collect-hierarchy (list-of-plates prefix)
  (loop for item in list-of-plates
        append (list 
                 (list
                   (concatenate 'simple-string prefix (widget-name item))
                   item))
        append (collect-hierarchy (children item)
                                  (concatenate 'simple-string "   " prefix))))
  
(defmethod update-text-fields ((doc ib-document) box value old-value)
  (declare (ignore old-value))
  ;; value is a widget plate
  (when value
      (setf (value (name-text box)) (widget-name value))
      (let ((resource (find-resource value :label-string)))
          (setf (value (label-text box)) 
              (if resource
                  (resource-value resource value)
                  ""))
          (set-motif-resources (label-text box) :sensitive (when resource t)))))

(defmethod set-widget-name ((doc ib-document) box value &aux plate)
  (setq plate (value (selection-list box)))
  (when plate
    (unless (equal value (resource-value (find-resource plate :name) plate))
      (set-new-resource-value (find-resource plate :name) plate value))))

(defmethod set-widget-label ((doc ib-document) box value &aux plate res)
  (setq plate (value (selection-list box)))
  (when plate
    (setq res (find-resource plate :label-string))
    (when res
      (unless (equal value (resource-value res plate))
        (set-new-resource-value res plate value)))))



