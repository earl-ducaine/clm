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

(in-package :gina)
(defginapackage :ib-demo)
(in-package :ib-demo)

(setq *sccs-id* "@(#)ib-demo.lisp	1.15	11/8/93")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; class ib-demo-application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This application can be used to exercise IB-created windows.
;;; All dialogs of open IB documents are presented for selection.
;;; By overriding the class ib-demo-shell a new main window can be tested.

(defclass ib-demo-application (application)
  ((name          :initform "IB-demo"         :allocation :class)
   (document-type :initform 'ib-demo-document :allocation :class)
   (signature     :initform "IB-demo"         :allocation :class)
   (file-type     :initform nil               :allocation :class)))

(defun make-ib-demo-application ()
  (make-application :class 'ib-demo-application))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; class ib-demo-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ib-demo-document (document)
  ())

(defmethod create-windows ((doc ib-demo-document))
  (make-ib-demo-shell doc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; class ib-demo-shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ib-demo-shell (document-shell)
  ((dialog-list :accessor dialog-list)))

(defun make-ib-demo-shell (document &aux shell)
  (setq shell (make-document-shell document :class 'ib-demo-shell))
  (setf (dialog-list shell)
        (gina:make-scrollable-selection-list shell nil))
  (setf (default-action-callback (dialog-list shell))
        (make-callback 'pop-up-item shell document))
  (refresh-class-list shell)
  (add-menu-command (main-menu shell)
                    "Classes" "Refresh"
                    (make-callback 'refresh-class-list shell))
  (add-menu-command (main-menu shell)
                    "Classes" "Pop-Up"
                    (make-callback 'show-dialog shell document))
  shell)

(defmethod refresh-class-list ((shell ib-demo-shell) &aux docs)
  (declare (special *running-applications*))
  (setf docs
        (loop for app in *running-applications*
              when (eql (document-type app) 'ib::ib-document)
              append (document-list app)))
  (setf docs
        (loop for doc in docs
              when (equal (ib::what-package (ib::lisp-details doc)) "ib-demo")
              collect doc))
  (set-item-list (dialog-list shell) 
                 (loop for doc in docs 
                       collect (ib::dialog-class (ib::main-info doc)))))
    
(defmethod show-dialog ((shell ib-demo-shell) doc)
  (let ((creator (read-from-string
                    (format nil "ib-demo::make-~a" 
                                (value (dialog-list shell))))))
    (ignore-errors (with-clock-cursor (pop-up (funcall creator doc))))))

(defmethod pop-up-item ((shell ib-demo-shell) doc item item-no)
  (declare (ignore item-no))
  (let ((creator (read-from-string
                    (format nil "ib-demo::make-~a" 
                                item))))
    (ignore-errors (with-clock-cursor (pop-up (funcall creator doc))))))

(register-application "IB-demo" 'ib-demo-application nil)
'(make-ib-demo-application)
