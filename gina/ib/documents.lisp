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

(setq *sccs-id* "@(#)documents.lisp	1.12	11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class ib-application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass ib-application (application)
  (;; overrides
   (name                 :initform "Interface Builder"   :allocation :class)
   (document-type        :initform 'ib-document          :allocation :class)
   (signature            :initform "ibuild"              :allocation :class)
   (file-type            :initform "ibuild"              :allocation :class)
   ;(inspect-click        :initform nil :allocation :class)
   ;; instance variable
   (palette-shell        :accessor palette-shell       :initform nil))
  (:documentation "the interface builder application"))

(register-application "ibuild" 'ib-application "ibuild")

(defun make-ib-application (&optional (document-pathname nil))
  "start the ib-application"
  (make-application :class 'ib-application 
                    :document-pathname document-pathname))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class ib-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass ib-document (document)
  (;; instance variables
   (class-counters :accessor class-counters :initform nil)
   (main-info      :accessor main-info)      
   (lisp-details   :accessor lisp-details  :initform (make-lisp-code-details))
   (cplus-details  :accessor cplus-details :initform (make-cplus-code-details))
   ;; overrides
   (drag-protocols :accessor drag-protocols :allocation :class
                         :initform '(:widget_drag))
   (shell-width    :initform 700)
   (shell-height   :initform 550))
  (:documentation "Interface Builder document type"))

(defmethod initialize-instance :after ((doc ib-document) &rest initargs)
  (declare (ignore initargs))
  (setf (active (lisp-details doc)) t)
  (setf (main-info doc) (make-modeless-dialog-box-info doc)))

(defmethod create-windows ((doc ib-document))
  "create the windows belonging to this document"
  (with-slots (main-shell main-view) doc
    (setq main-shell (make-ib-main-window doc))
    (setq main-view (view main-shell))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Methods for saving documents
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod write-to-stream ((doc ib-document) stream)
  "write all widget information"

  ;; write new header
  (format stream ":version 2~%")

  ;; write coder information
  (write-code-details (lisp-details doc) stream)
  (write-code-details (cplus-details doc) stream)

  ;; write counters
  (write-counters doc stream)

  (format stream ":windows 1~%")

  ;; write dialog plate
  (info-to-stream (main-info doc) stream))

(defun convert-old-size (info)
  "version 1 documents store the size of the surrounding plate"
  (when (x-pos info) (incf (x-pos info) 5))
  (when (y-pos info) (incf (y-pos info) 5))
  (when (width info) (decf (width info) 9))
  (when (height info) (decf (height info) 9))
  (loop for child in (children info)
        do (convert-old-size child)))

(defmethod read-from-stream ((doc ib-document) stream 
                             &aux ident main-class no-plates)
  "read widget information from stream"

  (setq ident (read stream))
  (if (eql ident :code)

    (progn                        ;; read old version file
      ;; NOTE: info size is now equal to real widget size, plates are larger
      ;; Old files are based on the larger size, have to adjust!
      (read-code-details (lisp-details doc) stream)

      ;; read counters
      (read-counters doc stream)

      ;; read dialog plate
      (setq main-class (read stream))
      (setf (main-info doc)
            (info-from-stream main-class nil doc stream))
  
      ;; number of trees following
      (setq ident (read stream)) ;; should be ":windows"
      (setq no-plates (read stream))

      ;; read trees (each level recursively prefixed by number of children)
      (loop for i from 1 to no-plates
            as  class = (read stream)
            do  (info-from-stream class (main-info doc) doc stream))
      (convert-old-size (main-info doc)))

    (progn                         ;; read new version file
      (setq ident (read stream))   ;; should be 2

      ;; read coder information
      (setq ident (read stream))   ;; should be ":lisp-code-details"
      (read-code-details (lisp-details doc) stream)
      (setq ident (read stream))   ;; should be ":cplus-code-details"
      (read-code-details (cplus-details doc) stream)

      ;; read counters
      (read-counters doc stream)

      ;; number of trees following
      (setq ident (read stream)) ;; should be ":windows"
      (setq no-plates (read stream))  ;; should be 1

      ;; read dialog plate
      (setq main-class (read stream))
      (setf (main-info doc)
            (info-from-stream main-class nil doc stream))))
  
  ;; update all views
  (update-window (main-shell doc) doc)
  )

(defmethod write-to-file :after ((doc ib-document) pathname)
  "active code is always written with the document"
  (with-clock-cursor
    (when (active (lisp-details doc))
        (write-code (lisp-details doc) doc (pathname pathname)))
    (when (active (cplus-details doc))
        (write-code (cplus-details doc) doc (pathname pathname)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Switching between alternate code generators
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass general-code-details ()
  ((detail-name    :reader detail-name      :initform :unspecified-code-details
                   :allocation :class)
   (code-file      :accessor code-file      :initform "ib-demo")
   (active         :accessor active         :initform nil))
  (:documentation "information nedded for all code generators"))

(defmethod write-code-details ((details general-code-details) stream)
  (format stream "~s ~s~%" (detail-name details) (code-file details)))

(defmethod read-code-details ((details general-code-details) stream)
  (setf (code-file details) (read stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Drop commands
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod create-drop-command ((doc ib-document) shell x y protocol-id 
				transfer-value received-from)
  (when (eql protocol-id :widget_drag)
      (make-drag-command doc shell x y 
                         (list (list (main-view doc) nil))
			 :received-from received-from
                         :transfer-value transfer-value
			 :class 'widget-dragger)))











