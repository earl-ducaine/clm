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

(setq *sccs-id* "@(#)counter.lisp	1.13	11/8/93")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class class-counter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Every IB document has a linked list of class counters. A class counter
;;; is present in the list if this widget class has been used in the
;;; document. The counter is incremented for each new widget of a class
;;; to generate unique numbers for new widget-names

'(defclass class-counter ()
  ((widget-class  :accessor widget-class   :initarg :widget-class)
   (next          :accessor next           :initform 0))
  (:documentation "Next number to use for a certain widget class"))

(defun make-class-counter (widget-class)
  (make-instance 'class-counter :widget-class widget-class))

(defmethod new-instance-count ((doc ib-document) widget-class &aux counter)
  "get next counter value for a class, adding a counter if necessary"
  (setq counter nil)
  (loop for c in (class-counters doc)
        when (eql (widget-class c) widget-class)
        do (setq counter c))
  (unless counter
        (setq counter (make-class-counter widget-class))
        (setf (class-counters doc) (cons counter (class-counters doc))))
  (setf (next counter) (1+ (next counter)))
  (1- (next counter)))

(defmethod write-counters ((doc ib-document) stream)
  "save all counters of a document"
  (format stream ":instance-counters ~d~%" (length (class-counters doc)))
  (loop for count in (class-counters doc)
        do (format stream "ib::~a ~d "
                          (symbol-name (widget-class count))
                          (next count))))

(defmethod read-counters ((doc ib-document) stream &aux dummy no count)
  "initialize counter list from stream"
  (setq dummy (read stream))
  (setq no (read stream))
  (setf (class-counters doc) nil)
  (loop for i from 1 to no
        do (setq count (make-class-counter (read stream)))
           (setf (next count) (read stream))
           (setf (class-counters doc) (cons count (class-counters doc)))))

