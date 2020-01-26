;;; -*- Mode:LISP;Syntax: Common-Lisp;Package: gina;Base:10-*-
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

(in-package :gina)
(setq *sccs-id* "@(#)background.lisp	1.13  11/8/93")

(ginachapter "Background Processes")

(defclass progress-bar-box (modeless-dialog-box)
  ((message            :accessor message)
   (scale              :accessor scale         :initform nil)
   (abort-button       :accessor abort-button)
   (kill-on-abort      :accessor kill-on-abort :initarg :kill-on-abort)
   (aborted            :accessor aborted       :initform nil)
   (background-process :accessor background-process :initform nil))
  (:documentation "a box indicating the progress of some long action"))

(defun make-progress-bar-box (document title message
			      &key (abortable t)
			           (kill-on-abort nil)
			           (modal t)
				   (centered t)
				   (with-scale t)
			      &aux box icon)
  (setq box (make-modeless-dialog-box
	     title
	     :motif-resources 
	     (list :dialog-style (if modal :full-application-modal :modeless))
	     :document document
	     :class 'progress-bar-box
	     :allow-shell-resize t
	     :resize t
	     :return-is-accelerator nil
	     :initargs (list :kill-on-abort kill-on-abort)))

  (when (not centered)
    (setf (relative-y box) :bottom)
    (setf (relative-widget box) (main-shell document)))
  
  (setq icon (make-label box "default_xm_working" :label-type :pixmap))
  (setf (message box)
    (make-label box message :alignment :beginning))

  (when with-scale
    (setf (scale box)
      (make-scale box
		  :orientation :horizontal
		  :processing-direction :max-on-right
		  :title-string "Percent"
		  :sensitive nil)))

  (setf (abort-button box)
    (make-push-button box " Abort "
		      :sensitive abortable
		      :activate-callback (make-callback 'do-abort box)))

  ;; form constraints
  (define-form-constraint icon 
			  :top-attachment :form :top-offset 20
			  :left-attachment :form :left-offset 20)
  (define-form-constraint (message box) :top-attachment :form :top-offset 25
			  :left-attachment :widget :left-widget icon
			  :left-offset 20
			  :right-attachment :form :right-offset 20
			  :bottom-attachment :none)
  (when with-scale
    (define-form-constraint (scale box) 
			    :top-attachment :widget :top-widget icon
			    :top-offset 20 :left-attachment :form
			    :left-offset 20 :right-attachment :form 
			    :right-offset 20 
			    :bottom-attachment :none))

  (define-form-constraint (abort-button box) :top-attachment :widget
			  :top-widget (or (scale box) icon) 
			  :top-offset 20
			  :left-attachment :form :left-offset 20
			  :right-attachment :none :bottom-attachment :form
			  :bottom-offset 20)
  box)

(defmethod do-abort ((box progress-bar-box))
  "kill the background process or just set flag for it"
  (setf (aborted box) t)
  (when (kill-on-abort box)
    (kill-background-process (document box) (background-process box))))
 

(defmethod closed-by-wm ((box progress-bar-box))
   "prevent dialog-box from being popped down"
   (declare (special *display*))
   (xlib:bell *display*))

'(defginamacro in-background-process 
	       ((document &key (terminate-on-error t))
		&body body)
   (description "execute body in a background process"
    :called-by-gina nil
    :called-by-application :sometimes)
   (comment "It returns the background process created.")
   (comment "See also method kill-background-process of class document")
   (example (with-application-stopped
	     (in-background-process ((first (document-list *application*)))
	      (replay-history (first (document-list *application*)))))) 
   (let ((application-var (gensym))
	 (display-var (gensym))
	 (motif-connection-var (gensym)))
     `(xtk::process-run-function
       (format nil "~a on ~a Background"
	       (name *application*) (display-host *application*))
       #'(lambda (,application-var ,display-var ,motif-connection-var)
	   (let ((*application* ,application-var)
		 (*display* ,display-var)
		 (xtk:*x-display* ,display-var)
		 (xtk:*motif-connection* ,motif-connection-var))
	     (declare (special *application*  *display*
			       xtk:*x-display* xtk:*motif-connection*))
	     (unwind-protect
	       (progn
		 (with-document (,document)
		   (push (xtk::current-process) 
			 (background-processes ,document)))
		 ,(if terminate-on-error 
		      `(catch 'xtk:proceed-from-error
			(handler-bind
			    ((error #'error-handler))
			  ,(cons 'progn body)))
		    ;; no handler => fall into debugger
		    (cons 'progn body)))
	       ;; cleanup: unregister process at document
	       (with-document (,document)
		   (setf (background-processes ,document) 
		     (remove (xtk::current-process) 
			     (background-processes ,document))))
	       (xlib:display-force-output *display*)
	       )))
       *application* *display* xtk:*motif-connection*)))

(defginamethod kill-background-process ((doc document) process)
  (description 
    "kill process, but wait until he is out of critical sections"
    :called-by-gina nil
    :called-by-application :sometimes)

  (with-document (doc)
   (with-clm-connection
    (xtk::process-kill process))))

(defginamethod kill-all-background-processes ((doc document))
  (description "kill all background processes and wait for them to die"
	       :called-by-gina "when the document is closed"
	       :called-by-application :sometimes)

  ;; send kill signals, make sure no process is in critical section
  (with-document (doc)
    (with-clm-connection 
     ;;(print (background-processes doc))
     (loop for process in (background-processes doc)
      do (xtk::process-kill process))))
  
  ;; wait for processes to die
  (xtk::process-wait "Waiting for background processes to die"
		     #'(lambda (doc) 
			 (null (background-processes doc)))
		     doc)

  ;; wait for processes to be really finished
  (with-document (doc) nil))

'(defginamacro with-application (&body body)
   (description "execute body with exclusive access to the application"
    :called-by-gina t
    :called-by-application :sometimes)
   (example (with-application (print (eval (semaphor *application*)))))
   `(xtk::with-process-lock ((semaphor *application*)) ,@body))

'(defginamacro with-document ((document) &body body)
   (description "execute body with exclusive access to the document"
    :called-by-gina t
    :called-by-application :sometimes)
   (example (with-document ((first (document-list *application*)))
	     (print "Hallo")))
   `(xtk::with-process-lock ((semaphor ,document)) ,@body))

'(defginamacro with-clm-connection (&body body)
   (description "prevent clm calls by other processes for body"
    :called-by-gina t
    :called-by-application :sometimes)
  `(xtk::with-process-lock 
    ((xtk::toolkit-connection-lock xtk:*motif-connection*))
    ,@body))

'(defginamacro with-progress-bar 
	       ((document
		 &key (modal t)
		      (title "Progress Bar")
		      (message "Performing your request ...")
		      (abortable t)
		      (kill-on-abort nil)
		      (centered t)
		      (with-scale t))
		&body body)
   (description "execute body while displaying a progress bar"
    :called-by-gina nil
    :called-by-application :sometimes)
   (comment "modal: whether all other input is blocked")
   (comment "title: title string of the dialog box")
   (comment "message: message displayed in the box")
   (comment "abortable: whether user can abort the action")
   (comment 
    "kill-on-abort: whether process is killed or abort flag is set only")
   (comment "centered: whether dialog box is centered over the document")
   (comment "with-scale: whether a scale indicating progress is displayed")

   (example (with-application-stopped
	      (with-progress-bar ((first (document-list *application*))
				  :kill-on-abort t :centered nil)
	       (loop for i from 1 to 100
		do (sleep 0.1)
		   (indicate-progress i
		     :new-message (format nil "~d more steps to go ..." 
					  (- 100 i))))
	      )))
   (example (with-application-stopped 
	     (with-progress-bar ((first (document-list *application*))
				 :kill-on-abort nil)
		(loop for i from 1 to 10
		      until (progress-bar-aborted)
		  do (sleep 1)
		     (indicate-progress (* 10 i))))))

   (let ((box-variable (gensym)))
   `(let ((,box-variable
	   (make-progress-bar-box 
	    ,document ,title ,message
	    :abortable ,abortable
	    :kill-on-abort ,(and kill-on-abort abortable)
	    :modal ,modal
	    :centered ,centered
	    :with-scale ,with-scale)))
      (pop-up ,box-variable)
      (setf (background-process ,box-variable)
	(in-background-process (,document)
	 (let ((*progress-bar* ,box-variable))
	   (declare (special *progress-bar*))
	   (unwind-protect
	       ,(cons 'progn body)
	     ;; cleanup
	     (pop-down *progress-bar*)
	     (destroy *progress-bar*))))))))

(defginafun progress-bar-aborted ()
   (description "test whether progress bar has already been aborted"
		:called-by-gina nil
		:called-by-application "inside a WITH-PROGRESS-BAR macro"
		:result "whether ABORT has been pressed")
   (declare (special *progress-bar*))
   (when (not (boundp '*progress-bar*))
     (error "PROGRESS-BAR-ABORTED called outside WITH-PROGRESS-BAR macro"))
   (aborted *progress-bar*))

(defginafun indicate-progress (percent "percent of action already performed"  
				&key 
				(new-message nil)
				"new label string displayed in dialog box")
   (description "set scale to indicate the current progress"
		:called-by-gina nil
		:called-by-application "inside a WITH-PROGRESS-BAR macro"
		:result "whether ABORT has been pressed")
   (declare (special *progress-bar*))
   (when (not (boundp '*progress-bar*))
     (error "INDICATE-PROGRESS called outside WITH-PROGRESS-BAR macro"))

   (when (scale *progress-bar*)
     (setf (value (scale *progress-bar*)) percent))
   (when new-message
     (setf (label-string (message *progress-bar*)) new-message))

   ;; return whether ABORT has been pressed
   (aborted *progress-bar*))









