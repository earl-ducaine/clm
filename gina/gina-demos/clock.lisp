;;;-*-Mode:LISP;Syntax: Common-Lisp; Package:clock; Base:10-*-
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
;;;
(in-package :GINA)
(defginapackage :clock)
(in-package :clock)
(setq *sccs-id* "@(#)clock.lisp	1.7  11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class clock-application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass clock-application (application)
  ((name          :initform "Clock"          :allocation :class)
   (document-type :initform 'clock-document  :allocation :class)
   (signature     :initform "clock"          :allocation :class)
   (file-type     :initform nil              :allocation :class)
   (idle-timeout  :initform 1.0              :allocation :class))
  (:documentation "a simple digital clock application"))

(defun make-clock-application ()
  "start the clock-application"
  (make-application :class 'clock-application))

(defmethod idle-action ((clock clock-application))
  "periodically refresh the displayed time"
  (loop for document in (document-list clock)
	do (tick document)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class clock-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass clock-document (document)
  ())

(defmethod create-windows ((doc clock-document))
  "create the windows belonging to this document"
  (with-slots (main-shell main-view) doc
    (setq main-shell (make-document-shell doc :with-menu nil))
    (setf (title main-shell) "Clock")
    (set-motif-resources main-shell :icon-name "Clock")
    (setq main-view (make-label main-shell " 00:00:00 "))))

(defmethod tick ((doc clock-document) &aux now)
  "one second is over => refresh the clock"
  (with-slots (main-view) doc
    (setq now (multiple-value-list (decode-universal-time (get-universal-time))))
    (setf (label-string main-view)
	  (format nil " ~2,'0d:~2,'0d:~2,'0d " (third now) (second now) (first now)))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; main program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(register-application "clock" 'clock-application nil)
'(make-clock-application)
