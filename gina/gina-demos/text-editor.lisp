;;;-*-Mode:LISP;Syntax: Common-Lisp;Package: txedit ;Base:10-*-
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
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS WHETHER IN AN ACTION
;;; OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
;;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;;
;;; Authors: Project GINA (spenke@gmd.de)
;;;          P.O. Box 1316
;;;          D-5205 Sankt Augustin 1
;;;

(in-package :GINA)
(defginapackage :txedit)
(in-package :txedit)
(setq *sccs-id* "@(#)text-editor.lisp	1.7  11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class text-editor-application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass text-editor-application (application)
  (;; overrides
   (name          :initform "Text-Edit"           :allocation :class)
   (document-type :initform 'text-editor-document :allocation :class)
   (signature     :initform "txedit"              :allocation :class)
   (file-type     :initform "txedit"              :allocation :class))
  (:documentation "a simple text-editor demo application"))

(defun make-text-editor-application ()
  "start the text-editor-application"
  (make-application :class 'text-editor-application))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class text-editor-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass text-editor-document (document)
  (;; overrides
   (shell-width  :initform 400)
   (shell-height :initform 300)))

(defmethod create-windows ((doc text-editor-document))
  "create the windows belonging to this document"
  (with-slots (main-shell main-view) doc
    ;; create document-shell containing a scroller and the text
    (setq main-shell (make-document-shell doc))
    ;; create the text-view
    (setq main-view (make-scrolled-text main-shell))
    ;; typing a character modifies the document
    (setf (value-changed-callback main-view)
	  (make-callback #'mark-modified doc))
    ))

(defmethod mark-modified ((doc text-editor-document) new-value)
  "mark document as modified"
  (declare (ignore new-value))
  (setf (modified doc) t)
  ;; no further value-changed-callbacks necessary
  (setf (value-changed-callback (main-view doc)) nil))

(defmethod write-to-stream ((doc text-editor-document) stream)
  "write the document to the specified stream"
  ;; write the current value of the text widget
  (format stream "~s~%" (value (main-view doc))))

(defmethod read-from-stream ((doc text-editor-document) stream)
  "read the document from the specified stream"
  ;; read and set the current value of the text widget
  (setf (value-changed-callback (main-view doc)) nil) ;;necessary in Motif 1.1 ???
  (setf (value (main-view doc)) (read stream))
  (setf (value-changed-callback (main-view doc))
	(make-callback #'mark-modified doc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; main program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(register-application "txedit" 'text-editor-application "txedit")
'(make-text-editor-application)
