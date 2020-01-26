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

(setq *sccs-id* "@(#)interface.lisp	1.12	11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; describe the interface of the new dialog class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod interface-definition ((doc ib-document) stream &aux main-info)
  (setq main-info (main-info doc))
  (format stream "Class:  ~a~%" (dialog-class main-info))
  (format stream "Subclass of:  ~a~%~%" 
                 (string-downcase (symbol-name (widget-class main-info))))
  (format stream "Slots:~%")
  (loop for info in (find-all-widget-infos main-info) do
      (case (named-resource-value info :export)
        ((:local) )
        ((:widget)
            (format stream "~a (a ~a widget)~%" 
                           (widget-name info) 
                           (string-downcase (symbol-name (widget-class info)))))
        ((:abstract)
            (format stream "~a (an arbitrary ~a widget)~%"
                           (widget-name info) (widget-category info)))
        ((:value)
            (format stream "~a (a ~a value)~%"
                           (widget-name info) (widget-category info)))))
  (format stream "~%Constructor:~%")
  (format stream "(make-~a (document &key (class '~a)~%"
                 (dialog-class main-info) (dialog-class main-info))
  (format stream "           (initargs nil) (motif-resources nil)))~%"))

(defmethod widget-category ((info widget-info))
  "passive")

(defmethod widget-category ((info push-button-info))
  "button")

(defmethod widget-category ((info toggle-button-info))
  "boolean")

(defmethod widget-category ((info toggle-button-group-info))
  "n-of-many")

(defmethod widget-category ((info selection-list-info))
  (case (named-resource-value info :selection-policy)
    ((:single :browse) "one-of-many")
    (t                 "n-of-many")))

(defmethod widget-category ((info radio-button-group-info))
  "one-of-many")

(defmethod widget-category ((info option-menu-info))
  "one-of-many")

(defmethod widget-category ((info scrollbar-info))
  "numeral")

(defmethod widget-category ((info scale-info))
  "numeral")


