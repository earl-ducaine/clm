;;;-*-Mode:LISP;Syntax: Common-Lisp;Package:listen ;Base:10-*-
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
;;; Thanks to Dieter Bolz who gave some important hints concerning streams!

(in-package :GINA)
(defginapackage :listen)
(in-package :listen)
(setq *sccs-id* "@(#)lisp-listener.lisp	1.7  11/9/92")

;; Lisp Listener Demo:
;; - you can enter Lisp expressions into a text widget.
;; - input is entered by pressing the ENTER (not RETURN) key
;;   (or whatever is necessary to cause an ACTIVATE callback).
;; - the line containing the insertion-cursor is sent off.
;;   if there is a text selection, it is sent off instead. 
;; - note that prompts are also regarded as input!
;;   normally however, the cursor is at the beginning of a line after
;;   each output. Functions like y-or-n-p might cause problems.
;; - the class text-widget-stream is designed to be reusable
;;   i.e. you can attach a stream to any text widget

;; Implementation:
;; - this demo shows how a stream subclass can be defined which
;;   is connected to a text widget
;; - this demo only runs under Allegro4.0, where streams are CLOS classes
;; - output to the stream is appended at the end of the text widget
;; - reading from the stream blocks until some input is in the buffer
;; - the buffer is filled, when the user initiates an ACTIVATE callback
;;   (by pressing the ENTER key)
;; - a read-print-eval-loop is started in a lightweight process
;;   this process waits for input most of the time

;; Restrictions: 
;; - a prompt is regarded as input!!
;;   (newline after prompt solves only half of the problem)
;; - output to stream cannot come from a foreign application,
;;   because eventually CLM-Functions are called, which are then
;;   sent off over the wrong connection.
;;   To allow output from foreign applications, write-char must
;;   place output into a buffer which is read by another lightweight
;;   process (similar to input)

;; open questions:
;; - how to tell text widget to start a new line if more than 80 columns
;;   resource :resize-width does not work
;;   something like *print-width* ??
;; - delete part of the text in the text widget without rolling in 
;;   the complete text ??
;; - search for newline without rolling in the complete text??

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class lisp-listener
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass lisp-listener (application)
  (;; overrides
   (name          :initform "Lisp Listener"    :allocation :class)
   (document-type :initform 'listener-document :allocation :class)
   (signature     :initform "listener"         :allocation :class)
   (file-type     :initform "listener"         :allocation :class))
  (:documentation "a simple lisp listener demo application"))

(defun make-lisp-listener ()
  (make-application :class 'lisp-listener))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class listener-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass listener-document (document)
  ((text-stream :accessor text-stream)))

(defmethod create-windows ((doc listener-document))
  "create the windows belonging to this document"
  (with-slots (main-shell main-view) doc
    ;; create document-shell containing a scroller and the text
    (setq main-shell (make-document-shell doc))
    ;; create the text-view
    (setq main-view (make-scrolled-text main-shell :columns 80 :rows 25))

    ;; create a stream connected to the text-widget
    (setf (text-stream doc) (make-text-widget-stream main-view))

    ;; start a read-eval-print-loop
    (in-background-process (doc :terminate-on-error nil)
       (setq tpl:*prompt* "~%Lisp>~%~8*")
       (tpl:start-interactive-top-level 
	(text-stream doc) #'tpl:top-level-read-eval-print-loop nil))

    ;; document is always modified by Lisp Listener prompt
    (setf (modified doc) t)
   ))

(defmethod write-to-stream ((doc listener-document) stream)
  "write the document to the specified stream"
  ;; write the current value of the text widget
  (format stream "~s~%" (value (main-view doc))))

(defmethod read-from-stream ((doc listener-document) stream)
  "read the document from the specified stream"
  (setf (value (main-view doc)) (read stream)))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class text-widget-stream
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass text-widget-stream (stream:fundamental-character-output-stream
			      stream:fundamental-character-input-stream)
  ((text-widget :accessor text-widget :initarg :text-widget)
   (input-buffer :accessor input-buffer :initform "")
   (input-pos    :accessor input-pos    :initform 0)
   (max-output-length :accessor max-output-length :initform 1000)
   (output-length :accessor output-length :initform 0)))

(defun make-text-widget-stream (text-widget 
				&key (class 'text-widget-stream)
				     (initargs nil)
				&aux new)
  (setq new (make-object class initargs :text-widget text-widget))
  (setf (activate-callback text-widget) (make-callback 'activated new))
  new)

(defmethod stream:stream-write-char ((tws text-widget-stream) character)
  "append character at the end of the text-widget"
  (incf (output-length tws))
  (clear-old-output-when-necessary tws)  
  (xtk:append-text (widget-id (text-widget tws)) 
		   (string character)
		   :scroll-window t))

(defmethod stream:stream-write-string 
              ((tws text-widget-stream) string &optional (start 0) (end nil)
	       &aux output-string)
  "append string at the end of the text-widget"
  (setq output-string (subseq string start end))
  (incf (output-length tws) (length output-string))
  (clear-old-output-when-necessary tws)
  (xtk:append-text (widget-id (text-widget tws))
		   output-string
		   :scroll-window t))

(defmethod clear-old-output-when-necessary ((tws text-widget-stream))
  "cut off prefix of the text, when limit exceeded by 100%"
  (when (and (max-output-length tws)
             (> (output-length tws) (* 2 (max-output-length tws))))
    (setf (value (text-widget tws)) 
      (subseq (value (text-widget tws)) (max-output-length tws)))
    (decf (output-length tws) (max-output-length tws))))

(defmethod activated ((tws text-widget-stream) &rest parms 
		      &aux text-selection cursor-position string 
		      prev-newline next-newline
		      first-half second-half input-line)
  "enter text selection or line containing the cursor"
  (declare (ignore parms))
  ;; if some text is selected, this is the input
  (setq text-selection (xtk:text-get-selection (widget-id (text-widget tws))))
  (if (not (equal text-selection ""))
    ;; then: something selected
    (setq input-line text-selection)
    ;; else: determine line containing the cursor
    (progn
      (setq cursor-position 
	(first (get-motif-resources (text-widget tws) :cursor-position)))
      ;; get complete text from CLM 
      (setq string (value (text-widget tws)))
      ;; search for next and previous newline
      (setq first-half (subseq string 0 cursor-position))
      (setq second-half (subseq string cursor-position))
      (setq prev-newline (position #\newline first-half :from-end t))
      (when (not prev-newline) (setq prev-newline -1))
      (setq next-newline (position #\newline second-half))
      (when next-newline
	(incf next-newline cursor-position))
      (setq input-line (subseq string (1+ prev-newline) next-newline))))
  ;;(print input-line)
  ;; append input line to stream buffer
  (mp:without-scheduling ;; modify buffer in an atomic operation
   (setf (input-buffer tws)
     (concatenate 'string (input-buffer tws) input-line (string #\newline)))))

(defmethod stream:stream-read-char ((tws text-widget-stream))
  (xtk::process-wait "input wait" #'listen tws)
  (prog1
      (elt (input-buffer tws) (input-pos tws))
    (incf (input-pos tws))
    (when (> (input-pos tws) 110)
      ;; throw away old input from time to time
      ;; but keep some (?) characters for unread
      (mp:without-scheduling
       (setf (input-buffer tws) (subseq (input-buffer tws) 100))
       (decf (input-pos tws) 100)))
    ))

(defmethod stream:stream-unread-char ((tws text-widget-stream) char)
  (declare (ignore char))
  (decf (input-pos tws))
  nil)

(defmethod stream:stream-read-char-no-hang ((tws text-widget-stream))
  (when (stream:stream-listen tws)
    (stream:stream-read-char tws)))

(defmethod stream:stream-peek-char ((tws text-widget-stream))
  (xtk::process-wait "input wait" #'listen tws)
  (elt (input-buffer tws) (input-pos tws)))

(defmethod stream:stream-listen ((tws text-widget-stream))
  (> (length (input-buffer tws)) (input-pos tws)))

(defmethod stream:stream-clear-input ((tws text-widget-stream))
  (setf (input-pos tws) (length (input-buffer tws)))
  nil)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; main program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(register-application "listener" 'lisp-listener "listener")
'(make-lisp-listener)




