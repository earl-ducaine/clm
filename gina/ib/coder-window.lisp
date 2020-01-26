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

(setq *sccs-id* "@(#)coder-window.lisp	1.13	11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class ib-coder-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass ib-coder-window (tool-dialog-box)
  ((package-text     :accessor package-text)
   (doc-class-text   :accessor doc-class-text)
   (code-display     :accessor code-display)
   (lisp-toggle      :accessor lisp-toggle)
   (show-lisp        :accessor show-lisp :initform t)
   (setting-value    :accessor setting-value :initform nil))
  (:documentation "The IB coder tool window"))

(defun make-coder-window (doc &aux box scroller)
  (setq box (make-tool-dialog-box "Coder Tool"
                                  :class 'ib-coder-window
                                  :document doc
                                  :dialog-shell-resources 
                                     '(:width 400 :height 200)))

  (setf (relative-widget box) (main-shell doc))
  (setf (relative-x box) :left-align)
  (setf (relative-y box) :bottom)

  ;; create a form containing the different parts
  (setf (package-text box) (make-labeled-text box "Package"
                                :value (what-package (lisp-details doc))))
  (setf (value-changed-callback (package-text box))
        (make-callback #'package-value-changed box doc))

  (setf (doc-class-text box) (make-labeled-text box "Document Class"
                                :value (doc-class (cplus-details doc))))
  (setf (value-changed-callback (doc-class-text box))
        (make-callback #'doc-class-value-changed box doc))

  (setq scroller (make-scroller box
                                :scrolling-policy :application-defined))
  (setf (code-display box) (make-text scroller
                                   :edit-mode :multi-line-edit
                                   :rows 20 :columns 80))
  (define-form-constraint (package-text box)
          :left-attachment :form
          :top-attachment :widget  :top-widget (main-menu box))
  (define-form-constraint (doc-class-text box)
          :left-attachment :widget :left-widget (package-text box)
          :top-attachment :widget  :top-widget (main-menu box))
  (define-form-constraint scroller
          :top-attachment :widget  :top-widget (package-text box)
          :left-attachment :form   )

  ;; add some menu commands to the "Code" menu
  (add-menu-command (main-menu box)
                    "Code" "Load"
                    (make-callback #'load-code doc))
  (add-menu-command (main-menu box)
                    "Code" "Compile and Load"
                    (make-callback #'compile-code doc :load t))
  (add-menu-command (main-menu box)
                    "Code" "Start IB-Demo"
                    '(lambda () 
                       (start-file "IB-demo.appl" :signature "IB-demo")))
  (add-menu-command (main-menu box)
                    "Code" "Create instance"
                    (make-callback #'create-instance doc))

  ;; add some menu commands to the "Show" menu
  (add-menu-command (main-menu box)
		    "Show" "Class Code"
		    (make-callback #'show-code-for-dialog-class box doc))
  (add-menu-command (main-menu box)
		    "Show" "Constructor Code"
		    (make-callback #'show-code-for-dialog-constructor box doc))
  (add-menu-command (main-menu box)
                    "Show" "Class Definition"
                    (make-callback #'show-code-for-interface box doc))
  (setf (lisp-toggle box)
        (make-toggle-entry "Use C++"
                    (make-callback #'make-prog-lang-command doc)
                    :value (active (cplus-details doc))))
  (insert-menu-entry (main-menu box) "Show" "C++" (lisp-toggle box))
  (setf (show-lisp box) (active (lisp-details doc)))

  box)

(defmethod pop-up :after ((win ib-coder-window))
  (define-form-constraint (parent (code-display win))
         :right-attachment :form   :bottom-attachment :form))

(defmethod update-window ((win ib-coder-window) document)
  "update window contents if doc has changed"
  ;; prevent recursive value-changed-callback
  (setf (setting-value win) t)
  (setf (value (package-text win)) (what-package (lisp-details document)))
  (setf (value (doc-class-text win)) (doc-class (cplus-details document)))
  (setf (value (lisp-toggle win)) (active (cplus-details document)))
  (setf (show-lisp win) (active (lisp-details document)))
  (setf (setting-value win) nil))

(defun package-value-changed (coder-window document value)
  (unless (setting-value coder-window)
    (make-change-package-command document value)))

(defun doc-class-value-changed (coder-window document value)
  (unless (setting-value coder-window)
    (make-change-doc-class-command document value)))

(defun pp-to-string (string package &aux old-package pp-string)
  (declare (special *package*))
  (setf old-package *package*)
  (ensure-package-exists package)
  (setf *package* (find-package (string-upcase package)))
  (setq pp-string (write-to-string
		   (read-from-string string)
		   :pretty t :level nil :length nil :case :downcase))
  (setf *package* old-package)
  pp-string)

;(defmethod show-code-of-widget ((box ib-coder-window) doc)
;  "show code of first selected widget object in the view"
;  (loop for plate in (view-objects (main-view doc))
;        when (mouse-sensitive plate)
;        do (setf (value (code-display box))
;                 (pp-to-string (code (info plate) (lisp-details doc) "parent") 
;                               (what-package (lisp-details doc))))))

(defmethod show-code-for-dialog-class ((box ib-coder-window) doc)
  "show the code for the new dialog class"
  (setf (value (code-display box))
        (if (show-lisp box)
            (pp-to-string (code-for-dialog-class doc (lisp-details doc)) 
                          (what-package (lisp-details doc)))
            (with-output-to-string (ss)
              (write-code-for-dialog-class (cplus-details doc) doc ss)))))

(defmethod show-code-for-dialog-constructor ((box ib-coder-window) doc)
  "show the code for the new dialog class"
  (setf (value (code-display box))
        (if (show-lisp box)
            (pp-to-string (code-for-dialog-constructor doc (lisp-details doc)) 
                          (what-package (lisp-details doc)))
            (with-output-to-string (ss)
              (write-code-for-constructor (cplus-details doc) doc ss)))))

(defmethod show-code-for-interface ((box ib-coder-window) doc)
  "show the interface definition"
  (setf (value (code-display box))
        (with-output-to-string (stream)
            (interface-definition doc stream))))

