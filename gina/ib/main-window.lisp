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

(setq *sccs-id* "@(#)main-window.lisp	1.13	11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class ib-main-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass ib-main-window (document-shell)
  ((view          :accessor view)
   (class-option  :accessor class-option)
   (temp          :accessor temp          :initform nil)
   ;(palette       :accessor palette)
   (scroller      :accessor scroller)
   (grab-row      :accessor grab-row)
   (remove-grab   :accessor remove-grab)
   (delete-grab   :accessor delete-grab)
   (edit-grab     :accessor edit-grab)
   (coder-tool    :accessor coder-tool     :initform nil)
   (tree-tool     :accessor tree-tool      :initform nil)
   (preferences   :accessor preferences)
   (feedback-view :accessor feedback-view))
  (:documentation "The IB main window"))

(defun make-ib-main-window (doc &aux shell)
  (setq shell 
        (make-document-shell doc :class 'ib-main-window))
  (setf (main-shell doc) shell)

  (setf (scroller shell) (make-scroller shell))
  (setf (view shell) (make-ib-view (scroller shell) doc))

  (setf (grab-row shell) (make-row-column shell :orientation :horizontal))

  (make-label (grab-row shell) "Cut:")
  (setf (remove-grab shell)
        (make-drag-label (grab-row shell) doc "magnet" 
			 :cursor-mask "magnet-mask"))
  (setf (activate-callback (remove-grab shell))
        (make-callback 'make-delete-command doc (remove-grab shell)))

  (make-label (grab-row shell) "    Copy:")
  (let ((copy-grab (make-drag-label (grab-row shell) doc "copy"
				    :cursor-mask "copy-mask")))
    (setf (activate-callback copy-grab)
          (make-callback 'make-copy-command doc copy-grab)))

  (make-label (grab-row shell) "    Paste:")
  (let ((paste-grab (make-drag-label (grab-row shell) doc "paste"
			   :cursor-mask "paste-mask")))
    (setf (activate-callback paste-grab)
          (make-callback 'make-paste-command doc paste-grab)))

  (make-label (grab-row shell) "    Resources:")
  (setf (delete-grab shell)
        (make-drag-label (grab-row shell) doc "lupe" 
			 :cursor-mask "lupe-mask"))
  (setf (activate-callback (delete-grab shell))
        (make-callback 'make-grab-resources-command doc (delete-grab shell)))

  (setf (feedback-view shell)
        (make-feedback-view (grab-row shell) doc 200 20))

  (set-motif-resources (main-window shell)
                       :command-window (widget-id (grab-row shell)))

  (setf (preferences shell) (make-color-spec doc 
          (find-resource (main-info doc) :background)))

  (add-menu-command (main-menu shell)
                    "Tools" "Palette..."
                    (make-callback 
         `(lambda () 
             (unless (palette-shell ',*application*)
                 (with-clock-cursor
                   (setf (palette-shell ',*application*)
                         (make-template-window ',doc))))
	     (pop-up (palette-shell ',*application*))
             (expose (palette-shell ',*application*)))))
  (add-menu-command (main-menu shell)
                    "Tools" "Tree..."
                    (make-callback 
          `(lambda () 
              (unless (tree-tool ',shell)
                  (with-clock-cursor
                    (setf (tree-tool ',shell) (make-tree-window ',doc))))
	      (pop-up (tree-tool ',shell)))))
  (add-menu-command (main-menu shell)
                    "Tools" "Coder..."
                    (make-callback 
          `(lambda () 
              (unless (coder-tool ',shell)
                  (with-clock-cursor
                    (setf (coder-tool ',shell) (make-coder-window ',doc))))
	      (pop-up (coder-tool ',shell)))))
  ;(add-menu-command (main-menu shell)
  ;                  "Tools" "Code Sheet..."
  ;                  (make-callback #'pop-up (sheet-window shell)))

  ;; add some menu commands to the "Widgets" menu
  (add-menu-command (main-menu shell)
       	            "Widgets" "Select All" 
       	            (make-callback #'select-all (view shell)))
  (add-menu-command (main-menu shell)
		    "Widgets" "Clear" 
		    (make-callback #'make-clear-command doc))
  (add-menu-command (main-menu shell)
		    "Widgets" "Clear all" 
		    (make-callback #'make-clear-all-command doc))
  (add-menu-command (main-menu shell)
		    "Widgets" "Ungroup" 
		    (make-callback #'make-ungroup-command (view shell)))

  (add-menu-command (main-menu shell)
                    "Arrange" "Make Row-Column"
                    (make-callback #'make-row-column-group-command 
                                   (view shell)))
  (add-menu-command (main-menu shell)
                    "Arrange" "Make Form"
                    (make-callback #'make-form-group-command (view shell)))
  (add-menu-command (main-menu shell)
                    "Arrange" "Make Paned-Window"
                    (make-callback #'make-paned-window-group-command 
                                   (view shell)))
  (add-menu-command (main-menu shell)
                    "Arrange" "Make Frame"
                    (make-callback #'make-frame-group-command (view shell)))
  (add-menu-command (main-menu shell)
                    "Arrange" "Make Scroller"
                    (make-callback #'make-scroller-group-command (view shell)))

  (add-menu-command (main-menu shell)
                    "Dialog" "Resources..."
                    (make-callback #'main-plate-resources doc))
  (setf (class-option shell)
        (make-radio-group-entry "Class" '(     ;:separator
                                     ("Main" :main)
                                     ("Tool" :tool)
                                     ("Dialog" :dialog)
                                     ("Modal" :modal))
                                (make-callback #'make-dialog-type-command doc)
                                :is-submenu t
                                :initial-value 
                                   (case (widget-class (main-info doc))
                                     ((document-shell) :main)
                                     ((tool-dialog-box) :tool)
                                     ((modeless-dialog-box) :dialog)
                                     ((modal-dialog-box) :modal))))
  (insert-menu-entry (main-menu shell)
                     "Dialog" "Class"
                     (class-option shell))
  (add-menu-command (main-menu shell)
                    "Dialog" "Compile and Load"
                    (make-callback #'compile-code doc :load t))

  (add-menu-command (main-menu shell)
                    "Preferences" "Background..."
                    (make-callback #'pop-up (preferences shell)))
  shell)

(defmethod main-plate-resources ((doc ib-document))
  "pop-up resource dialog for whole dialog"
  (resource-dialog (main-info doc)))

(defmethod update-window ((win ib-main-window) document)
  "update all windows if doc has changed"
  (recreate (view win))
  (setf (value (class-option win)) 
        (case (widget-class (main-info document))
          ((document-shell) :main)
          ((tool-dialog-box) :tool)
          ((modeless-dialog-box) :dialog)
          ((modal-dialog-box) :modal)))
  (when (tree-tool win)
    (update-window (tree-tool win) document))
  (when (coder-tool win)
    (update-window (coder-tool win) document)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; palette-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-template-window (doc &aux box scroller form 
                                      row1 row11 row2 row3 frame view)
  (setq view (view (main-shell doc)))
  (update-slots (main-shell doc))
  (setq box (make-toplevel-shell "Palette"
              :name "IB-Palette"
              :motif-resources (list :geometry
                    (format nil "~dx~d+~d+~d" 280 550 
                      (+ (x-pos (main-shell doc)) (width (main-shell doc)) 12)
                      (- (y-pos (main-shell doc)) 27)))))
  (setq scroller (make-scroller box
                        :motif-resources '(:shadow-thickness 0)))
  (setq form (make-form scroller
                   :motif-resources '(:horizontal-spacing 6
                                      :vertical-spacing 6)))
  (setq row1 (make-row-column form :entry-alignment :none :spacing 10
                               :motif-resources '(:adjust-margin nil)))
  (setq row2 (make-row-column form :entry-alignment :none :spacing 10
                               :margin-width 6))

  (make-info-and-template row1 view 'make-label-info nil)
  (make-info-and-template row1 view 'make-push-button-info
      '((:label-string "A button")))
  (make-info-and-template row1 view 'make-toggle-button-info 
      '((:label-string "A toggle")))

  (setq row11 (make-row-column row1 :spacing 6 :entry-alignment :none
                               :orientation :horizontal
                               :motif-resources '(:adjust-margin nil)))
  (setq frame (make-frame row11 :margin-width 6 :margin-height 0))
  (set-motif-resources frame :shadow-thickness 0)
  (make-info-and-template frame view 'make-separator-info
      '((:orientation :vertical)))
  (make-info-and-template row11 view 'make-scrollbar-info
      '((:orientation :vertical)))
  (make-info-and-template row11 view 'make-scale-info
      '((:orientation :vertical)))

  (setq frame (make-frame row2 :margin-width 0 :margin-height 6))
  (set-motif-resources frame :shadow-thickness 0)
  (make-info-and-template frame view 'make-separator-info nil)
  (make-info-and-template row2 view 'make-scrollbar-info
      '((:orientation :horizontal)))
  (make-info-and-template row2 view 'make-scale-info
      '((:orientation :horizontal)))

  (make-info-and-template row2 view 'make-text-info
      '((:columns 13)))
  (make-info-and-template row2 view 'make-labeled-text-info 
      '((:label-string "Text:") (:columns 8)))
  (make-info-and-template row2 view 'make-option-menu-info
      '((:label-string "Brand") (:item-list ("Mazda" "BMW" "VW"))))

  (setq row3 (make-row-column form :spacing 10 :num-columns 2))
  (make-info-and-template row3 view 'make-toggle-button-group-info
      '((:label-string "Brand")))
  (make-info-and-template row3 view 'make-radio-button-group-info
      '((:label-string "Brand")))
  ;(make-info-and-template row3 view 'make-selection-list-info
  ;    '((:visible-item-count 3)))
  (make-info-and-template row3 view 'make-scrolled-text-info
      '((:rows 4) (:columns 8)))
  (make-info-and-template row3 view 'make-scrollable-selection-list-info
      '((:visible-item-count 3)))

  (setq frame (make-frame form :margin-width 3 :margin-height 10
                  :motif-resources '(:shadow-thickness 0)))
  (set-motif-resources frame :shadow-thickness 0)
  (make-info-and-template frame view 'make-view-info nil)

  (define-form-constraint row1
     :top-attachment :form :left-attachment :form)
  (define-form-constraint row2
     :top-attachment :form :left-attachment :widget :left-widget row1)
  (define-form-constraint row3
     :top-attachment :widget :top-widget row2 :left-attachment :form)
  (define-form-constraint frame
     :top-attachment :widget :top-widget row3 :top-offset 0
     :bottom-attachment :opposite-widget :bottom-widget row3
     :bottom-offset -100
     :left-attachment :form
     :right-attachment :opposite-widget :right-widget row2)

  box)
  
(defun make-info-and-template (parent target-view info-constructor
                               initial-resources)
  (let ((info (funcall info-constructor nil)))
    (loop for entry in initial-resources do
        (setf (resource-value (find-resource info (first entry)) info)
              (second entry)))
    (make-drag-template parent target-view info info-constructor)))

  






