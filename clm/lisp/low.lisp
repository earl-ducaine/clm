;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:xtk; Base:10 -*-

(in-package :xtk)

(setq *sccsid* "@(#)low.lisp	1.21 9/8/93")


;;; Open a connection to the Motif server


;; EXPORT (toolkit-initialize)
(defun toolkit-initialize (display-string app-name app-class-name
			   &key (server-host *default-server-host*)
			   &aux new-connection)
  (check-type server-host (or null string))
  (setq new-connection (make-toolkit-connection))
  (open-motif-stream new-connection server-host)
  ;;; Initialize the Motif server
  (let ((*motif-connection* new-connection))
    (setf (toolkit-connection-motif-version new-connection)
      (execute-request 71 (list display-string app-name app-class-name)
		       :num-results 1))
    (setf (toolkit-connection-motif-version new-connection)
      (when (listp (toolkit-connection-motif-version new-connection))
	(first (toolkit-connection-motif-version new-connection)))))
  new-connection)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Application main loop.
;;;;; Must be called after initializing the toolkit
;;;;; and creating all the widgets.
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (app-main-loop)
(defun app-main-loop (&key (recursive nil))
  (declare (ignore recursive))
  (warn "Calling this function externally is no longer possible"))

(defun clm-main-loop (recursive)
  (declare (special *motif-connection*))
  ;; handle any blocked callbacks in advance
  (handle-pending-events recursive)
  (loop until (or (toolkit-connection-dispatcher-terminated *motif-connection*)
                  (toolkit-connection-closed *motif-connection*))
      do (wait-for-input-from-server *motif-connection* #'any-input-available)
	 (handle-pending-events recursive)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Dispatch callbacks for an applications with a clx connection
;;;;; Do a little error handling
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun default-error-handler (condition)
  (declare (special *motif-connection*))
  (when (toolkit-connection-just-print-errors *motif-connection*)
    (format *error-output* "~%Error: ~A~%" condition)
    ;; resume execution at proposed point
    (throw 'proceed-from-error nil))
  ;; when not just-print-errors:
  ;; just return to enter the debugger so that the error can be examined
  )

;; EXPORT (proceed-from-error)
(defun handle-pending-events (recursive)
  (declare (special *motif-connection* *x-display*))
  (loop while t do
    (with-simple-restart
	(next-event "Restart main loop")
      (catch 'proceed-from-error
	(handler-bind
	    ((error (toolkit-connection-error-handler *motif-connection*)))
	  ;; Flag used for external synchronization
	  (setf (toolkit-connection-running *motif-connection*) t)

	  ;; handle callbacks
	  (cond
	   ((or (toolkit-connection-dispatcher-terminated *motif-connection*)
		(toolkit-connection-closed *motif-connection*))
	       (return-from handle-pending-events))
	   ((input-available *motif-connection*)
	       (with-process-lock
		   ((toolkit-connection-lock *motif-connection*))
		 (when (callback-dispatcher :called-from-main-loop
					    (if recursive :recursive t))
		   (setf (toolkit-connection-dispatcher-terminated
			  *motif-connection*) t))))
	   ((other-input-available *motif-connection*)
	       (if (toolkit-connection-other-input-handler *motif-connection*)
		   (funcall (toolkit-connection-other-input-handler
			     *motif-connection*))))
	   (t
	       (return-from handle-pending-events)))
	  ;; send off output requests in reaction to event
	  #+clm-needs-clx
	  (when *x-display*
	    (xlib:display-force-output *x-display*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Establish a connection to the Motif server and to the X server
;;;;; Sets up an environment where *motif-connection* is bound to a
;;;;; connection structure and *x-display* is bound to the X display.
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wrapper-apply (function arguments)
  (if (toolkit-connection-binding-wrapper *motif-connection*)
      (funcall (toolkit-connection-binding-wrapper *motif-connection*)
	     function
	     arguments
	     (toolkit-connection-binding-arguments *motif-connection*))
    (apply function arguments)))

(defun cleanup-actions ()
  (declare (special *motif-connection* *x-display*))
  (when (toolkit-connection-after-function *motif-connection*)
    (wrapper-apply (toolkit-connection-after-function *motif-connection*)
		   (toolkit-connection-after-arguments *motif-connection*)))
  (when *motif-connection*
    (close-motif-stream *motif-connection*))
  #+clm-needs-clx
  (when *x-display*
    (xlib:close-display *x-display*)))

(defun run-main-process (connection display init-function init-arguments)
  (let ((*motif-connection* connection)
	(*x-display* display)
	(result nil))
    (declare (special *motif-connection* *x-display* *debug-mode*))
    (catch 'proceed-from-error
	(handler-bind
	    ((error (toolkit-connection-error-handler *motif-connection*)))
	  (unwind-protect
	      (progn
		(setf result (wrapper-apply init-function init-arguments))
		(wrapper-apply #'clm-main-loop '(nil)))
	    (cleanup-actions))
	  result))))

#-clm-has-processes
(defun start-handler (connection display init-function init-arguments)
  (let ((*motif-connection* connection)
	(*x-display* display)
	(result nil)
	(error-occurred t))
    (declare (special *motif-connection* *x-display* *debug-mode*))
    (catch 'proceed-from-error
      (handler-bind
	  ((error (toolkit-connection-error-handler *motif-connection*)))
	(unwind-protect
	    (progn
	      (setf result (wrapper-apply init-function init-arguments))
	      (add-clm-handler)
	      (setq error-occurred nil))
	  (when error-occurred (cleanup-actions)))))))

;; EXPORT (run-motif-application)
(defun run-motif-application (init-function
			      &key (init-arguments nil)
				   (application-name "clm")
				   (application-class "Clm")
				   (server-host *default-server-host*)
				   (display-host *default-display-host*)
				   (display-number *default-display-number*)
				   (screen-number *default-screen-number*)
				   (use-clx nil)
				   (extra-process t)
				   (sync-clx *debug-mode*)
				   (process-name "Motif application")
				   (after-function nil)
				   (after-arguments nil)
				   (error-handler 'default-error-handler)
				   (just-print-errors t)
				   (binding-wrapper nil)
				   (binding-arguments nil)
			      &aux connection x-display)
  #-clm-has-processes (declare (ignore process-name))
  #-clm-needs-clx (declare (ignore use-clx sync-clx))
  #-(or :clm-has-processes :clm-has-handler) (declare (ignore extra-process))
  (when init-function
    #+clm-needs-clx
    (setf x-display
      (and use-clx
	   (xlib::open-display
	    (subseq display-host 0 (or (position #\: display-host)
				       (length display-host)))
	    :display display-number)))
    #-clm-needs-clx
    (setf x-display nil)
    (setf connection
      (toolkit-initialize (default-display-components display-host
						      display-number
						      screen-number)
			  application-name application-class
			  :server-host server-host))
    (setf
	(toolkit-connection-after-function connection) after-function
	(toolkit-connection-after-arguments connection) after-arguments
	(toolkit-connection-binding-wrapper connection) binding-wrapper
	(toolkit-connection-binding-arguments connection) binding-arguments
	(toolkit-connection-error-handler connection) error-handler
	(toolkit-connection-just-print-errors connection) just-print-errors)
    #+clm-needs-clx
    (when (and x-display sync-clx)
      (setf (xlib:display-after-function x-display)
	#'xlib:display-finish-output))
    #+(or :clm-has-processes :clm-has-handler)
    (if extra-process
	#+clm-has-processes
	(xtk::process-run-function process-name
				   'run-main-process
				   connection x-display
				   init-function init-arguments)
	#-clm-has-processes
	(start-handler connection x-display init-function init-arguments)
      (run-main-process connection x-display init-function init-arguments))
    #-(or :clm-has-processes :clm-has-handler)
    (run-main-process connection x-display init-function init-arguments)
  ))

(defun default-display-components (display-string
				   display-number
				   display-screen)
  (unless (position #\: display-string)
    (setq display-string (format nil "~A:~D" display-string display-number)))
  (unless (position #\. display-string :start (position #\: display-string))
    (setq display-string (format nil "~A.~D" display-string
				 display-screen)))
  display-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Run a secondary Process which is capable of sending requests to the
;;;;; Motif server.
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (run-secondary-process)
(defun run-secondary-process (name function &rest args)
  #-clm-has-processes (declare (ignore name function args))
  (declare (special *motif-connection*))

  #+clm-has-processes
  (xtk::process-run-function name 'secondary-process
			     function *motif-connection* args)
  #-clm-has-processes
  (clm-error "Secondary processes not supported"))

(defun secondary-process (function connection args)
  (let ((*motif-connection* connection))
    (declare (special *motif-connection*))
    (apply function args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Test whether there is input available on the connection to the Motif
;;;;; server.
;;;;; Uses the system dependent function 'listen-for-input
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun input-available (connection)
  "check for input data on the motif server connection"
  (or (listen-for-input connection)
      (toolkit-connection-blocked-callback connection)))

(defun other-input-available (connection)
  (and (toolkit-connection-other-input-detector connection)
       (funcall (toolkit-connection-other-input-detector connection))))

(defun any-input-available (connection)
  "check for all possible event sources"
  (or (input-available connection)
      (other-input-available connection)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Send a request to the Motif server and wait for a confirmation.
;;;;; This function also handles any callbacks which are received before
;;;;; the confirmation message arrives (e.g. SetValues on width and height
;;;;; on an XmDrawingArea widget which has the resize callback attached)
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-request (opcode arguments &key (num-results nil) (confirm t)
			&aux serial result)
  (declare (special *motif-connection*))
  (with-process-lock
   ((toolkit-connection-lock *motif-connection*))
   (setq serial (next-serial))
   ;; Send the request to the Motif server
   (toolkit-send-command opcode serial arguments)
   ;; Process callbacks which are sent during execution of the request.
   (when confirm
     (setq result (dispatch-callbacks-until-confirmation serial))
     (break)
     (case (first result)
       ((28) ;; Confirmation message
	t)
       ((31)
	(cond ((null num-results)
	       (return-from execute-request nil))
	      ((numberp num-results)
	       (if (equal num-results (length (cdr result)))
		   (return-from execute-request (cdr result))
		 (clm-error
		        "Illegal number of results received: ~a instead of ~a~%"
			(length (cdr result)) num-results)))
	      ((eq num-results :arbitrary)
	       (return-from execute-request (cdr result)))))))))



;; Dispatch callbacks until a confirmation message is received A
;; confirmation message is either a confirmation, a result, an error,
;; or a warning message.
(defun dispatch-callbacks-until-confirmation (serial &aux result)
  ;; loop macro in allegro is buggy
  (do () ((setq result (callback-dispatcher :allow-confirmation t
					    :serial serial))))
  result)


;; Receive a callback command from the Motif server Allow confirmation
;; messages if :allow-confirmation it t Returns :confirmation if a
;; confirmation message is received.  Returns nil if a destroy-widget
;; message is received.  Raises an error under the following
;; conditions:
;;    - The received message is not a callback and not a
;;      destroy-widget message and :allow-confirmation is nil
;;    - The received message is not a callback and not a
;;      destroy-widget and not a confirmation message when
;;      :allow-confirmation is t
(defun receive-callback-or-event (&key (allow-confirmation nil)
				    (called-from-main-loop nil)
				  &aux return-code serial-and-num-args)
  (declare (special *motif-connection*))
  ;; Get a blocked callback if called from the main loop Otherwise
  ;; wait for a callback from the Motif server
  (if (and called-from-main-loop
	   (toolkit-connection-blocked-callback *motif-connection*))
      (progn
	(setq return-code
	      (first (toolkit-connection-blocked-callback *motif-connection*)))
	(setq serial-and-num-args
	      (second (toolkit-connection-blocked-callback *motif-connection*)))
      	(log-format "using blocked callback: ~a ~a ~%"
		    return-code serial-and-num-args)
	(setf (toolkit-connection-blocked-callback *motif-connection*) nil))
      (let ((input-error t))
	(unwind-protect
	     (progn
	       (multiple-value-setq (return-code serial-and-num-args)
		 (toolkit-receive-command))
	       (log-format "using toolkit-receive-command: ~a ~a ~%"
			   return-code serial-and-num-args)
	       (setq input-error nil))
	  (when input-error (close-motif-stream *motif-connection*)))))
  ;; Verify that we really received what we want.
  (case (first return-code)
    ((27)  ;; Message is a callback or an event
     (if (eql (second return-code) 3)  ;; destroy-notify message
	 (progn
	   (clean-up-tables (cddr return-code))
	   (return-from receive-callback-or-event
	     (values :destroy-notify nil nil)))
	 ;; Else:: Callback, event, protocol callback or action
	 (return-from receive-callback-or-event
	   (values :callback return-code serial-and-num-args))))
    ((28 31)  ;; Confirmation message
     (if allow-confirmation
	 (return-from receive-callback-or-event
	   (values :confirmation return-code serial-and-num-args))
	 (clm-error "unexpected confirmation message: ~a~%" return-code)))
    (t
     (clm-error "unexpected message: ~a~%" return-code))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Dispatch one callback.
;;;;; Should only be called when a callback is available on the input stream
;;;;; Returns nil if callback was dispatched sucessfully.
;;;;; Returns t if main-loop was terminated during execution of the callback.
;;;;; Returns a confirmation message if :allow-confirmation is t and
;;;;; a confirmation is received instead of a callback.
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun callback-dispatcher (&key (allow-confirmation nil)
				 (called-from-main-loop nil)
				 (serial 0)
			    &aux result message message-type
				 serial-and-num-args timer-args
				 (server-loop-terminated nil))
  (declare (special *motif-connection*))

  ;; Return immediately if there is nothing to dispatch
  ;; This may happen after a callback has collided with a confirmation
  (when (and called-from-main-loop
	     (not (input-available *motif-connection*)))
    (return-from callback-dispatcher nil))

  ;;; get callback or event from stream
  (multiple-value-setq (message-type message serial-and-num-args)
			(receive-callback-or-event
			 :allow-confirmation allow-confirmation
			 :called-from-main-loop called-from-main-loop))

  ;; Return a confirmation message if one was received and allowed
  (when (eq message-type :confirmation)
    (return-from callback-dispatcher message))

  ;; Destroy-notify events are handled in 'receive-callback-or-event
  (when (eq message-type :destroy-notify)
    (return-from callback-dispatcher nil))

  ;;; Test whether the callback must be blocked here
  (when (and allow-confirmation
	     (eq message-type :callback)
	     (< (first serial-and-num-args) serial))
    (when (toolkit-connection-blocked-callback *motif-connection*)
      (clm-error "Unexpected callback: ~a~%" message))
    (setf (toolkit-connection-blocked-callback *motif-connection*)
      (list message serial-and-num-args))
    ;;(format t "Blocking callback: ~a ~a ~%" message serial-and-num-args)
    (return-from callback-dispatcher nil))

  ;;; Callback format: (major-opcode minor-opcode <data>)
  (case (second message)
    ((0) ;;; Callback
     (unwind-protect
	 (dolist
	     (cb (lookup-callback-functions (third message) (fourth message)))
	   (if called-from-main-loop
	       (without-process-lock
		((toolkit-connection-lock *motif-connection*))
		(setq result
		  (apply (first cb)
			 (cons (third message)
			       (cons (second cb)
				     (cddddr message))))))
	     (setq result
		  (apply (first cb)
			 (cons (third message)
			       (cons (second cb)
				     (cddddr message)))))))
       (if (member (fourth message) '(:motion-verify :modify-verify))
	   (terminate result)
	 (terminate nil))))
    ((1)   ;;; Event
     (unwind-protect
	 (dolist (ev (lookup-event-functions (third message) (fourth message)))
	   (if called-from-main-loop
	       (without-process-lock
		((toolkit-connection-lock *motif-connection*))
		(apply (first ev)
		       (cons (third message)
			     (cons (second ev)
				   (cddddr message)))))
	     (apply (first ev)
		       (cons (third message)
			     (cons (second ev)
				   (cddddr message))))))
       (terminate nil)))
    ((2)   ;;; protocol callback
     (unwind-protect
	 (dolist
	     (cb (lookup-callback-functions
		  (third message) (list (fourth message) (fifth message))))
	   (if called-from-main-loop
	       (without-process-lock
		((toolkit-connection-lock *motif-connection*))
		(apply (first cb)
		       (cons (third message)
			     (cons (second cb)
				   (cdr (cddddr message))))))
	     (apply (first cb)
		       (cons (third message)
			     (cons (second cb)
				   (cdr (cddddr message)))))))
       (terminate nil)))
    ((4)  ;; clm action
     (unwind-protect
	 (when (>= (fourth message) 2)  ;; At least two arguments are required
	   (let* ((function (intern (string-upcase (sixth message))
				    (find-package
				     (string-upcase (fifth message)))))
		  (event (nthcdr (+ 4 (fourth message)) message))
		  (args (butlast (nthcdr 6 message) (length event))))
	     ;;(format t "Action: ~a~%" message)
	     ;;(describe function)
	     (funcall function (third message) args event)))
       (terminate nil)))
    ((5)  ;; permanent Timer
     (unwind-protect
	 (when (setq timer-args (gethash (third message)
					 (toolkit-connection-timer-table
					  *motif-connection*)))
	   (funcall (first timer-args) (third message) (second timer-args)))
       (terminate nil)))
    ((6)  ;; simple (once) Timer
     (unwind-protect
	 (when (setq timer-args (gethash (third message)
					 (toolkit-connection-timer-table
					  *motif-connection*)))
	   (funcall (first timer-args) (third message) (second timer-args))
	   (remhash (third message)
		    (toolkit-connection-timer-table *motif-connection*)))
       (terminate nil)))
    ((8)  ;; warning callback
     (terminate nil)
     (warn "~a" (third message))
     (return-from callback-dispatcher '(:warning)))
    ((7)  ;; error callback
     (terminate nil)
     (clm-error "~a" (third message))
     (return-from callback-dispatcher '(:error)))
    ((9) ;; main loop termination message
     (setq server-loop-terminated t)))

  ;;; Test if application is terminated and return t
  ;;; Otherwise return nil
  (when (and (eq called-from-main-loop t)
             (toolkit-connection-dispatcher-terminated *motif-connection*))
	(return-from callback-dispatcher t))
  (when (eq called-from-main-loop :recursive)
	(if server-loop-terminated
	    (return-from callback-dispatcher t)
            (setf (toolkit-connection-dispatcher-terminated
		   *motif-connection*) nil)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Tell toolkit-server to enter it's main event loop
;;;;; Should only be called by app-main-loop
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun recursive-main-loop ()
  (execute-request 30 '(1) :confirm nil)
  (clm-main-loop t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; This function tells the toolkit-server to continue processing
;;;;; X-Events or to send the next callback. It is always called after
;;;;; a callback function has been processed by the callback dispatcher
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun terminate (result)
  (execute-request 29 (if (listp result) result (list result)) :confirm nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; This function sets a flag which terminates the callback dispatcher after
;;;;; all remaining callbacks have been processed
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (terminate-dispatcher)
(defun terminate-dispatcher ()
  (declare (special *motif-connection*))
  (setf (toolkit-connection-dispatcher-terminated *motif-connection*) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Clean up the callback hash table and the event hash table after one
;;;;; or more widgets have been deleted
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clean-up-tables (widgets &aux events callback)
  (declare (special *motif-connection*))
  (dolist (widget widgets)
    (setf events nil)
    ;; Find Event-Handler wich are to be removed from the hash table
    (maphash #'(lambda (key val)
		 (declare (ignore val))
		 (if (eql widget (first key))
		     (push key events)))
	     (toolkit-connection-event-table *motif-connection*))
    ;; Remove Event-Handler from hash table
    (dolist (ev events)
      (remhash ev (toolkit-connection-event-table *motif-connection*)))

    (setq callback nil)
    ;; Find callbacks which are to be removed from the hash table
    (maphash #'(lambda (key val)
		 (declare (ignore val))
		 (if (eql widget (first key))
		     (push key callback)))
	     (toolkit-connection-callback-table *motif-connection*))
    ;; Remove Callbacks from hash table
    (dolist (cb callback)
      (remhash cb (toolkit-connection-callback-table *motif-connection*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; A predifined callback function which terminates the callback dispatcher
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun do-terminate (widget client-data &rest call-data)
  (declare (ignore widget client-data call-data))
  (execute-request 1 nil)
  (terminate-dispatcher))
