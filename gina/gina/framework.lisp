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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;                Application framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :gina) 

(setq *sccs-id* "@(#)framework.lisp	1.34 11/8/93")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;                global application management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ginachapter "Global Application Management")

(defginavar *loaded-applications*
  (description "the list of all loaded and registered applications"
	       :default :none
	       :modify "by calling the function register-application"))

(unless (boundp '*loaded-applications*)
  (setq *loaded-applications* nil))
'(print *loaded-applications*)

;; example value of *loaded-applications*:
;; signature              class                      file-type
;;--------------------------------------------------------------------------------------
;(("ibuild"         IB:MAKE-IB-APPLICATION            "ibuild") 
; ("txedit"         TXEDIT:TEXT-EDITOR-APPLICATION    "txedit")
; ("micky"          MICKY:MICKY-APPLICATION           "micky")
; ("hello"          HELLO:HELLO-WORLD-APPLICATION     "hello")
; ("GINA"           APPLICATION                       "empty")
; ("finder"         FINDER                            "directory")
; ("gredit"         GREDIT:GRAPHIC-EDITOR-APPLICATION "gredit"))

(defginafun register-application (signature "a short string"
				  application-class "a symbol denoting an application"
				  file-type "a short string")
  (description "make application class known to the finder"
	       :called-by-gina nil
	       :called-by-application :always)
  (comment "This Function is called once for each application class.")
  (example (register-application "bitedit" 'bitmap-editor "bitmap")
	   "Signature bitedit denotes application bitmap-editor")

  ;; delete old entry if any
  (setq *loaded-applications*
	(loop for entry in *loaded-applications*
	      when (not (string-equal signature (first entry)))
		collect entry))

  ;; add new entry
  (push (list signature application-class file-type) *loaded-applications*))

(defun find-application-class (signature)
  "search *loaded-applications* for the given signature"
  ;; return the class of the desired application
  (loop for entry in *loaded-applications*
	when (string-equal signature (first entry))
	  do (return (second entry))))

(defun find-file-type (file-type)
  "search *loaded-applications* for the given file-type"
  ;; return whether file-type is known
  (or (null file-type)
      (eq :unspecific file-type)
      (string-equal file-type "appl")
      (loop for entry in *loaded-applications*
	    when (string-equal file-type (third entry))
	      do (return (first entry)))))
'(find-file-type "gredit")
'(find-file-type nil)
'(find-file-type :unspecific)
'(print *loaded-applications*)
;; ----------------------------------------------------------------------------------------

(defun find-application (signature display-host)
  "search *running-applications* for the given signature"
  (loop for appl in *running-applications*
	when (and (string-equal display-host (display-host appl))
		  (string-equal signature (signature appl))
		  (not (process-per-document appl)))
	  do (return appl)))

'(find-application "finder"  "tom")
'(find-application "browser" "gina")

(defun find-document (pathname signature display-host &aux found-doc)
  "search *running-applications* for an application that has the document already open"
  (loop for appl in *running-applications*
	when (and (string-equal display-host (display-host appl))
		  (string-equal signature (signature appl))
		  (setq found-doc (loop for document in (document-list appl)
					when (pathname-equal pathname (file-pathname document))
					  do (return document))))
	  do (return (values found-doc appl))))

'(multiple-value-setq (doc appl)
   (find-document "W1:>GINA>documents.directory.newest" "finder" "gina"))
   

;; ----------------------------------------------------------------------------------------
;; a semaphor is needed to synchronize multiple applications when accessing globals
(defvar *AC-semaphor*)
(setq *AC-semaphor* (xtk::make-process-lock))

;; ----------------------------------------------------------------------------------------
;; functions to start a document or application from a Finder

(defginafun start-file (pathname "pathname-object or string"
		   &key (display-host (display-host *application*))
		        "X display to use. By default the same as the calling application"
			(display-number (display-number *application*))
			"X display number to use. By default the same as the calling application"
			(screen-number (screen-number *application*))
			"X screen number to use. By default the same as the calling application"
		        (toolkit-host (toolkit-host *application*))
			"toolkit server to use. By default the same as the calling application"
		        (signature nil)
			"will be automatically determined if not supplied"
			(from-document (first (document-list *application*)))
			"where to popup possible error messages"	
		   &aux file-type desired-document desired-application)
  (description "start the given file which may be a document or an application"
	       :called-by-application "if a foreign document or application is to be started")
  (example (with-application-stopped 
	    (start-file "/vol/gina/documents/hello.appl"))
	   "start an application")
  (example (with-application-stopped
	    (start-file "/vol/gina/documents/five-rects.gredit"))
	   "open a document")
  (example (with-application-stopped
	    (start-file nil :signature "browser"))
	   "start the browser")

  (when (and (not pathname) (not signature))
    (error "either pathname or a signature must be given"))
  
  (when pathname
    ;; make sure to have really an absolute pathname object with version newest
    (setq pathname (merge-pathnames pathname (current-directory-wildcard)))
    (setq pathname (make-pathname :version :newest :defaults pathname))
  
    ;; determine the file-type
    (setq file-type (pathname-type pathname))
    (when (wildp file-type) (setq file-type "directory"))
    (when (not (find-file-type file-type))
      (warning-dialog "Unknown file format!" :document from-document)
      (return-from start-file nil)))

  (when (not pathname)
    ;; when only a signature is given, an application is started
    (setq file-type "appl"))
  
  ;; determine application signature
  (when (not signature)
    ;; signature not given as a parameter
    (if (wildp (pathname-name pathname))
	(setq signature "finder")
	;; else: read signature from file
	(ignore-errors
	  (with-open-file (stream pathname :direction :input)
	    (setq signature (read stream)))))
    (when (not signature)
      ;; with-open-file failed!
      (warning-dialog (format nil "Cannot read signature of file ~a!" 
			      (or (ignore-errors (namestring pathname))
				  pathname))
		      :document from-document)
      (return-from start-file nil)))
  
  ;; read and modify global list of processes in an atomic operation
  (xtk::with-process-lock (*AC-semaphor*)
    
    ;; check if application already exists
    (setq desired-application (find-application signature display-host))
    (when (and desired-application (not (eq desired-application *application*)))
      ;; it is not ourself: make sure application is completely started up
      (wait-until-running desired-application)) 

    ;; check if document is already open under the desired application
    (setq desired-document
      (find-document pathname signature display-host))
    (when desired-document
      ;; just expose the document
      (when (eq desired-application *application*)
	;; it is the same application
	(expose (main-shell desired-document)))
      (when (not (eq desired-application *application*))
	;; it is another application
	;; tell desired-application to expose the desired document
	(send-message desired-application
		      (make-callback #'expose (main-shell desired-document))))
      (return-from start-file desired-application))

    ;; it is now clear that the document is not already open  
    ;; if the application is already running and pathname denotes a document 
    ;;    => open it
    (when (and desired-application (not (string-equal file-type "appl")))
      (when (eq desired-application *application*)
	;; it is the same application
	(create-new-document desired-application :pathname pathname))
      (when (not (eq desired-application *application*))
	;; it is another application
	;; tell desired application to start the file
	;; => desired application will if doc exists when message is received
	(send-message desired-application
		      (make-callback #'start-file pathname 
				     :display-host display-host
				     :display-number display-number
				     :screen-number screen-number
				     :toolkit-host toolkit-host
				     :signature signature))
	;;;; tell desired application to open the desired document
	;;(send-message desired-application
	;;	      (make-callback #'create-new-document desired-application
	;;			     :pathname pathname))
	)
      (return-from start-file desired-application))

    ;; if the application is already running but started again => focus all docs
    (when (and desired-application (string-equal file-type "appl"))
      ;; tell desired-application to expose all its documents
      (loop for document in (document-list desired-application)
	    do (send-message desired-application
			     (make-callback #'expose (main-shell document))))
      (return-from start-file desired-application))

    ;; if the application is not yet running try to start it
    (when (not desired-application)
      (setq desired-application
	(start-application signature 
			   display-host display-number screen-number
			   toolkit-host
			   :from-document from-document
			   :document-pathname (if (string-equal file-type "appl")
						  nil
						  pathname)))
      ))

    desired-application)

(defun start-application (signature 
			  display-host display-number screen-number
			  toolkit-host
			  &key (from-document 
				(first (document-list *application*)))
			       (document-pathname nil)
			  &aux application-class)
  "start the application denoted by the signature"
  (setq application-class (find-application-class signature))
  (if (not application-class)
      (progn
	(warning-dialog "Unknown Application!" :document from-document)
	nil) ;; no application created
      ;; else
      (make-application :display-host display-host
			:display-number display-number
			:screen-number screen-number
			:toolkit-host toolkit-host
			:document-pathname document-pathname
			:class application-class)
      ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class application 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		    
'(defginaclass application ()
   (description "the superclass of all GINA applications"
		:application-subclasses :always
		:gina-subclasses "only in the demo applications"
		:instantiate "to run the empty application")
  (;; class-parameters
   (name                 :accessor name              :initform "GINA"    :allocation :class
		         :documentation "name of application")
   (document-type        :accessor document-type     :initform 'document :allocation :class
		         :documentation "class of application dependent documents")
   (signature            :accessor signature         :initform "GINA"  :allocation :class
		         :documentation "internal identification of application")
   (file-type            :accessor file-type         :initform "empty"    :allocation :class
		         :documentation "type field of pathnames for saved documents")
   (process-per-document :accessor process-per-document :initform nil    :allocation :class)
			 ;;flag if each document has its own process
   
   ;; instance-variables
   (display-host  :accessor display-host  :initarg :display-host
		  :documentation "where the X-server runs")
   (display-number :accessor display-number :initarg :display-number
    :documentation "number of display on host where X-server runs")
   (screen-number :accessor screen-number  :initarg :screen-number
    :documentation "number of screen on host where X-server runs")
   (toolkit-host  :accessor toolkit-host  :initarg :toolkit-host
		  :documentation "where the toolkit-server runs")
   (debug-out     :accessor debug-out     :initarg :debug-out
		  :documentation "flag for debugging-output")

   (document-list        :accessor document-list        :initform nil
			 :documentation "the document objects currently open")
   (document-counter     :accessor document-counter     :initform 0)
   (process              :accessor process              :initform nil
			 :documentation "the process running this application")
   (mailbox              :accessor mailbox              :initform nil
			 :documentation "a list of messages, sent by other applications")
   (display              :accessor display              :initform nil
			 :documentation "the CLX-display used by this application")
   (clx-synchronous      :accessor clx-synchronous      :initform *clx-synchronous*
			 :documentation "flag if CLX is run in synchronous mode")
   (xtk-connection       :accessor xtk-connection       :initform nil
			 :documentation "the Motif-connection used by this application")
;   (xtk-synchronous      :accessor xtk-synchronous      :initform *xtk-synchronous*
;			 :documentation
;			 "flag if toolkit connection is run in synchronous mode")
   (application-shell    :accessor application-shell
			 :documentation "invisible shell, root of widget hierarchy")
   (toplevel-shells      :accessor toplevel-shells      :initform nil
			 :documentation "List of all toplevel shells")
   (screen               :accessor screen)
   (standard-font        :accessor standard-font        :initform "9x15")
   (cursor-font          :accessor cursor-font)
   (cursor-table         :accessor cursor-table
                         :initform (make-hash-table))
   (pixmap-table         :accessor pixmap-table
                         :initform (make-hash-table :test #'equal))
   (drag-window-hash     :accessor drag-window-hash
			 :initform (make-hash-table))
   (semaphor             :accessor semaphor             :initform (xtk::make-process-lock))
   (idle-timeout         :accessor idle-timeout         :initform nil
			 :documentation
			 "time interval in seconds between calls of method idle-action")
   (timer-id             :accessor timer-id             :initform nil
			 :documentation "id for xtk:change-timer and friends")
   (timer-active-in-handler :accessor timer-active-in-handler :initform nil
			    :documentation "keyword parameter for xtk:create-timer")
   (inspect-click        :accessor inspect-click        :initform *inspect-click*
			 :documentation "flag whether inspect-click is enabled")
   (debug-menu           :accessor debug-menu           :initform *debug-menu*
			 :documentation "flag whether debug menu is present")
   (just-print-errors    :accessor just-print-errors :initform t
    :documentation 
    "flag whether errors are just printed or debugger is invoked")
   (feedback-animation   :accessor feedback-animation :initform nil
    :documentation "flag for experimental mouse-feedback animation")
   (cursor-stack         :accessor cursor-stack :initform nil)
   ))

(defginamethod initialize-instance :after ((app application) &rest initargs)
  (description "initialize a just created instance")
  (comment "This deamon is called immediately after instantiation."
	   "It initializes the slots of class application."
	   "You can add further :after methods to initialize your subclasses.")
  (declare (ignore initargs))
  ;;(setf (window-hash-table app) (make-hash-table))
  )

(defginamethod print-object ((app application) stream)
  (description "specialized printed representation"
	       :called-by-gina "to denote an application object"
	       :override "to get a special printed representation"
	       :default-version "prints name and host")
  (format stream "#<Application ~a on ~a>"
	  (if (slot-boundp app 'name) (name app) "???")
	  (if (slot-boundp app 'display-host) (display-host app) "???")))
			
(defginafun make-application
	    (&key (display-host *default-display-host*)
		  "X display to use."
		  (display-number *default-display-number*)
		  "X display number to use."
		  (screen-number *default-screen-number*)
		  "X screen number to use."
		  (toolkit-host *default-toolkit-host*)
		  "toolkit server to use."
		  (document-pathname nil)
		  "pathname-object or string or nil"
		  (extra-process t)
		  "whether the application runs in an extra process"
		  (debug nil)
		  (class 'application)
		  "class of the desired application"
		  (more-inits nil)
		  "additional initializations for the application subclass"
		  &aux new-application)
  (description "create and run an application object"
	       :constructor-for-class application
	       :called-by-gina "whenever an application is started"
	       :called-by-application "in constructors for subclasses of application")
  (example (make-application)
	   "starts the empty application")
  (example "(make-application :class 'micky:micky-application)"
	   "starts the micky mouse application")
  
  (setq new-application
	(make-object class more-inits
		     :display-host display-host
		     :display-number display-number
		     :screen-number screen-number
		     :toolkit-host toolkit-host
		     :debug-out debug))

  ;; register the application in global table
  (push new-application *running-applications*)

  ;; run the application (in an extra process if desired)
  (xtk::handler-bind
      ((error #'(lambda (condition)
		  (declare (ignore condition))
		  (setq  *running-applications* 
		    (delete new-application *running-applications*)))))
     (xtk:run-motif-application
      #'startup
      :application-name (name new-application)
      :application-class (signature new-application)
      :use-clx t
      ;;:execute-main-loop nil
      :server-host toolkit-host
      :display-host display-host
      :display-number display-number
      :screen-number screen-number
      :error-handler 'error-handler
      :extra-process extra-process
      :process-name (format nil "~a on ~a" (name new-application) display-host)
      :init-arguments (list new-application 
			    :document-pathname document-pathname)
      :after-function #'cleanup
      :after-arguments (list new-application)
      :binding-wrapper #'gina-binding-wrapper
      :binding-arguments new-application
      :sync-clx (clx-synchronous new-application)))
			    
  ;; return the new-application
  new-application)  

'(make-application)

(defun gina-binding-wrapper (function arguments application)
  ;; => all functions dynamically called can find 
  ;; their application and display,...
  (let ((*package* (find-package 'gina))
	(*application* application)
	(*display* xtk:*x-display*))
    (declare (special *application* *display*))
    (apply function arguments)))

;; make empty application startable from finder
(register-application "GINA" 'application "empty")

(defmethod running ((app application))
  "test if new application is properly started up"
  (and (xtk-connection app)
       (xtk::is-running (xtk-connection app))))

(defmethod wait-until-running ((app application))
  "wait until new application is properly started up"
  (xtk::process-wait "startup wait" #'running app))

(defun startup (app &key (document-pathname nil))
  "actions before main event-loop"
  (declare (special *application* *display*))
  (setf (display app) *display*)
  (setf (xtk-connection app) xtk:*motif-connection*)
  (setf (process app) (xtk::current-process))
  #+genera (setf (si:process-priority (process app)) 20)

  (with-slots (display application-shell) app
      (setf (screen app)        (xlib:display-default-screen display))
      (setf (standard-font app) (xlib:open-font display (standard-font app)))
      (setf (cursor-font app)   (xlib:open-font display "cursor"))
	    
      (setq application-shell (xtk::create-application-shell :name (name app)))
      
      ;; create and immediately destroy a message box to make standard
      ;; Motif icons like exclamation mark accessible
      (xtk:destroy-widget
       (xtk:create-named-widget :message-box
				(application-shell *application*)
				"mbox" nil nil))

      ;; give application a chance to do something
      ;; with *display* or *motif-connection*
      (startup-actions app)

      ;; create an untitled document or read a file
      (create-new-document app :pathname document-pathname)

      ;; create a timer if necessary
      (when (idle-timeout app)
	(setf (timer-id app)
	      (xtk:create-timer (max 1 (round (* 1000 (idle-timeout app))))
                                #'gina-callback
                                (make-callback #'idle-action app)
				;#'handle-timeout app
				:active-in-handler (timer-active-in-handler app))))
      ))

(defun cleanup (app)
  "executed after main event loop"
  ;; call application dependent cleanups
  (cleanup-actions app)
  (setq  *running-applications* (delete app *running-applications*))
  ;; other processes should not wait any longer for startup:
  (setf (xtk::is-running) t))   

(defginamethod startup-actions ((app application))
  (description "initializations before application begins to process events"
	       :called-by-gina "when connections to the servers have been opened"
	       :override
	       "to initialize the application using *display* and xtk:*motif-connection*"
	       :default-version :do-nothing)
  )

(defginamethod cleanup-actions ((app application))
  (description "last actions after application has stopped to process events"
	       :called-by-gina "before an application is stopped"
	       :override "to cleanup at the end of the application"
	       :default-version :do-nothing)
  )

;; Better: the timer should invoke a callback, and get rid of handle-timeout
;(defun handle-timeout (timer-id app-or-cmd)
;  "react to timeout in main loop"
;  (declare (ignore timer-id))
;  (idle-action app-or-cmd))

(defginamethod (setf idle-timeout) :after (new-idle-timeout (app application))
  "change timer when slot is modified"
  (when (and (timer-id app) new-idle-timeout)
    (xtk:change-timer (timer-id app) (max 1 (round (* 1000 new-idle-timeout)))))
  
  (when (and new-idle-timeout (not (timer-id app)))
    (setf (timer-id app)
	  (xtk:create-timer (max 1 (round (* 1000 (idle-timeout app))))
                            #'gina-callback
                            (make-callback #'idle-action app)
			    ;#'handle-timeout app
			    :active-in-handler (timer-active-in-handler app))))
  
  (when (and (timer-id app) (null new-idle-timeout))
    (xtk:destroy-timer (timer-id app))
    (setf (timer-id app) nil))
  )

(defun error-handler (condition)
  "this function is passed to CLM as :error-handler"
  (declare (special *application*))
  ;; call overwritable method of application
  (handle-error *application* condition))

(defginamethod handle-error ((app application) 
			     condition "signalled condition")
   (description "handle conditions signalled by Lisp"
	       :called-by-gina "whenever an error is signalled"
	       :override "to define special error handling"
	       :default-version "prints the error or invokes the debugger")
   (comment 
    "(throw 'xtk:proceed-from-error nil) resumes execution of main loop"
	    "Just returning from this will enter the debugger")
  (when (just-print-errors app)
    (format *error-output* "~%Error: ~A~%" condition)
    (print-backtrace condition) ;; OS-dependent
    ;; resume execution where proposed by CLM
    (throw 'xtk:proceed-from-error nil))
  ;; just return to enter the debugger so that the error can be examined
  )

(defmethod read-mailbox ((app application) &rest event-data)
  (declare (ignore event-data))
  ;;(format t "read-mailbox ")
  (with-slots (mailbox) app
      (loop while mailbox ;; ... not empty
	  do (let ((message (xtk::with-process-lock
				((semaphor *application*))
			      (pop mailbox)))) ;; remove message
	       (execute message nil)))))

(defginamethod idle-action ((app application))
  (description "react to timeout when no events come in"
	       :called-by-gina "whenever the specified timeout occurs"
	       :override "to react to timeouts"
	       :default-version :do-nothing)
  (comment "You have to set the slot idle-timeout of your application to get timeouts.")
  )

(defginamethod restart-timer ((app application))
  (description "begin a new full time interval before timeout occurs"
	       :called-by-gina nil
	       :called-by-application :sometimes)
  (when (timer-id app)
    (xtk:stop-timer  (timer-id app))
    (xtk:start-timer (timer-id app)))
  )

;;(defmethod register-window ((app application) widget-object x-window)
;;  "store the mapping x-window --> widget-object in the hash-table"
;;  (setf (gethash x-window (window-hash-table app)) widget-object))

;;(defmethod unregister-window ((app application) widget-object)
;;  "remove the mapping x-window --> widget-object from the hash-table"
;;  (remhash (x-window widget-object) (window-hash-table app)))

(defginamethod send-message ((app application)
			     message "a function or a callback to be executed by the recipient"
			     &aux target-x-window)
  (description "send a message to another application"
	       :called-by-gina "to tell an application to open a document"
	       :called-by-application "to communicate with other applications")
  (example (with-application-stopped
	     (send-message *application* (make-callback #'xlib:bell *display*)))
	   "Tell *application* to beep")
	       
  ;; this method is typically called from another application !!
  (with-slots (semaphor mailbox) app
    ;; put message into mailbox of other application
    (xtk::with-process-lock (semaphor)
      (setq mailbox (append mailbox (list message))))

    ;; send a focus-in event to wakeup the other application
    (setq target-x-window 
      (x-window (main-shell (car (last (document-list app))))))
    (xlib:send-event target-x-window :client-message
		     nil ;; send to owner
		     :display (display app)
		     :window target-x-window 
		     :format 32
		     :type :gina-message
		     :data '(0 0 0 0 0))
    (xlib:display-force-output (display app))
   ))

(defmethod create-new-document ((app application)
				&key (pathname nil)
				(wildcard nil)
				&aux new-document
				     (read-without-error t)
				     (error nil))
  "create a new application specific document"
  (declare (special *display* *application*))
  (with-clock-cursor 
    (when pathname
      (setq pathname (merge-pathnames pathname (current-directory-wildcard))))
    ;; make a new document object
    (setq new-document
      (make-object (document-type app) nil  ;; no additional inits
		   :wildcard (or wildcard ;; explicit parm
				 (if pathname
				     (directory-wildcard pathname)
				   (current-directory-wildcard
				    :alternative :homedir)))
		   :file-pathname pathname))

    (setq pathname (file-pathname new-document)) ;; maybe changed during init !
    (when pathname
      ;; read the document
      (if (just-print-errors *application*)
	  ;; catch errors while reading the file
	  (multiple-value-setq 
	      (read-without-error error)
	    (ignore-errors ;; return NIL on error
		  (read-from-file new-document pathname :create-windows t)
		  ;; indicate success
		  t))
	  ;; else: do not suppress errors while reading the file
	  (read-from-file new-document pathname :create-windows t)
	  ))
    
    (when (or (not pathname) (not read-without-error))
      (create-windows new-document)   ;; content independent
      (display-empty  new-document)) ;; content dependent

    ;; display the new shell
    ;; if there are multiple shells, this is done already in create-windows 
    (pop-up (main-shell new-document))
   
    (push new-document (document-list app))

    (recompute-docs-menu-entries app)
    
    (when (not read-without-error)
      (warning-dialog (format nil "~a" error)
		      :dialog-title
		      (format nil "Cannot read file ~a!" 
			      (or (ignore-errors (namestring pathname))
				  pathname))
		      :document new-document)
      ;; somewhat dirty patch: 
      (define-cursor (main-shell new-document) :restore))
      
    ))

(defmethod recompute-docs-menu-entries ((app application) &aux new-item-list)
    "recompute all item-lists for docs-menu-entries"
    (setq new-item-list '("All"))
    (loop for doc in (document-list app) 
     do (push (list (name doc) doc) new-item-list))
    (loop for doc in (document-list app)
     do (when (docs-menu-entry doc)
	  (set-item-list (docs-menu-entry doc) new-item-list))))
 
(defginamethod quit-app ((app application))
  (description "terminate the application"
	       :called-by-gina "in reaction to the quit menu entry"
	       :called-by-application :rarely
	       :override :rarely
	       :default-version "closes all documents")
  ;; close all documents
  (loop for document in (document-list app)
	;; closing the last document will terminate the application
	until (eq :cancel (do-close document))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defginaclass document ()
   (description "the superclass of all documents"
		:application-subclasses :always
		:gina-subclasses "only in the demo applications"
		:instantiate nil)
   ((file-pathname   :accessor file-pathname :initarg :file-pathname
		     :documentation "where the document is stored")
    (wildcard        :accessor wildcard :initarg :wildcard
		     :documentation "directory where the document is stored")
    (next-number     :accessor next-number
		     :documentation "used for 'Untitled <number>'")
    (name            :accessor name
		     :documentation "the name part of the pathname")
    (modified        :accessor modified        :initform nil
     :documentation "flag if document was modified since last save")
    (undo-commands   :accessor undo-commands   :initform nil
		     :documentation "list of undoable commands")
    (redo-commands   :accessor redo-commands   :initform nil
		     :documentation "list of redoable commands")
    (max-history     :accessor max-history :initform nil :allocation :class
     :documentation
     "maximum number of past commands stored or NIL for unlimited history")
    (active-mouse-command :accessor active-mouse-command :initform nil
     :documentation "pointer to the currently active mouse-command")
    (toplevel-shells :accessor toplevel-shells :initform nil
		     :documentation "list of all toplevel shells")
    (views           :accessor views      :initform nil
		     :documentation "all views showing the document contents")
    (main-view       :accessor main-view :initform nil
     :documentation "the application can store the most important view here")
    (main-shell      :accessor main-shell
		     :documentation "the toplevel-window of the document")
    (shell-width     :accessor shell-width :initform :auto
		     :documentation "width of the shell for this document")
    (shell-height    :accessor shell-height :initform :auto
		     :documentation "height of the shell for this document")
    (history-scroller     :accessor history-scroller     :initform nil)
    (inspector-hash-table :accessor inspector-hash-table 
                          :initform (make-hash-table))
    (active-modal-dialogs :accessor active-modal-dialogs :initform nil
     :documentation "stack of modal dialogs currently visible")
    (undo-menu-entry :accessor undo-menu-entry :initform nil
     :documentation "dynamically changing menu entry for UNDO")
    (redo-menu-entry :accessor redo-menu-entry :initform nil
     :documentation "dynamically changing menu entry for REDO")
    (docs-menu-entry :accessor docs-menu-entry :initform nil
     :documentation "changing menu entry for DOCUMENTS")
    (save-menu-entry :accessor save-menu-entry :initform nil
     :documentation "sometimes insensitive menu entry for SAVE")
    (revert-menu-entry :accessor revert-menu-entry :initform nil
     :documentation "sometimes insensitive menu entry for REVERT")
    (open-dialog-box :accessor open-dialog-box :initform nil)
    (save-dialog-box :accessor save-dialog-box :initform nil)
    (semaphor        :accessor semaphor :initform (xtk::make-process-lock))
    (background-processes :accessor background-processes :initform nil
     :documentation "list of all running background processes")
    (drag-protocols     :accessor drag-protocols :initform nil
     :documentation "a list of keywords of accepted drag-commands")
    (animator           :accessor animator       :initform nil)
    (selection-undoable :accessor selection-undoable :allocation :class
                        :initform nil)
    ))

(defginamethod initialize-instance :after ((doc document) &rest initargs)
  (description "initialize a just created instance")
  (comment "This deamon is called immediately after instantiation."
	   "It initializes the slots of class document."
	   "You can add further :after methods to initialize your subclasses.")
  (declare (ignore initargs))
  (setf (next-number doc) (incf (document-counter *application*)))
  (setf (name doc)
	(if (file-pathname doc)
	(pathname-name (file-pathname doc))
	(format nil "Untitled~d"  (next-number doc)))))

(defginamethod print-object ((doc document) stream)
  (description "specialized printed representation"
	       :called-by-gina "to denote a document object"
	       :override "to get a special printed representation"
	       :default-version "prints name of document")
  (format stream "#<Document ~s>"
	  (if (slot-boundp doc 'name) (name doc) "???")))

(defginamethod (setf modified) :before (new-value (doc document))
   (description "set sensitivity of SAVE and REVERT menu entries")
   (comment "You should never access the slot modified directly.")
   (when (not (eq new-value (slot-value doc 'modified)))
     ;; SAVE and REVERT entries are only sensitive if document is modified
     (setf (sensitive (save-menu-entry doc)) new-value)
     (when (file-pathname doc)
       (setf (sensitive (revert-menu-entry doc)) new-value))))

;; there is no make-document method, because all documents are created by the
;; application object using make-instance

(defginamethod create-windows ((doc document) &aux scroller)
  (description "create the windows belonging to this document"
	       :called-by-gina "whenever a new document is created"
	       :override :always
	       :default-version "creates a window with some randomly distributed buttons")
  (declare (special *display*))
  (with-slots (main-shell main-view) doc
    (setq main-shell (make-document-shell doc))   
    (setq scroller   (make-scroller main-shell))
    (setq main-view  (make-view scroller :width 1000 :height 1000
				:document doc))
    
    (loop repeat 30
	  do (make-push-button main-view "GINA"
			       :name "GINA-button"
			       :activate-callback (make-callback #'xlib:bell *display*)
			       :motif-resources (list :x (random 900) :y (random 900))))
    ))

'(make-application)

(defginamethod standard-menu-items ((document document) (shell document-shell)
				    &aux main-menu entry)
  (description
    "create the standard menu entries for this document"
    :called-by-gina "when a document-shell is created"
    :override "if you want completely different menu entries"
    :default-version "installs all the standard enries")
    
    (setq main-menu (main-menu shell))

    ;; add standard commands to menu-bar
    (add-menu-command main-menu "File" "New"
		      (make-callback #'new document)
		      :accelerator "n")
    (add-menu-command main-menu "File" "Open.."
		      (make-callback #'do-open document)
		      :accelerator "o")
    (add-menu-command main-menu "File" "Minifinder.."
		      (make-callback #'minifinder document))
    (setf (docs-menu-entry document)
          (make-toggle-group-entry 
	     "Documents"
	     nil ;; item-list is computed later
	     '(lambda (ignore1  document ignore2)
	        (declare (ignore ignore1 ignore2))
	        (if (equal document "All")
		    (loop for doc in (document-list *application*)
		          do (expose (main-shell doc)))
		  (expose (main-shell document))))
	     :button-resources '(:indicator-on nil)))
    (insert-menu-entry main-menu "File" "Documents" (docs-menu-entry document))
				  
    (add-menu-command main-menu "File" "Close"
		      (make-callback #'do-close document)
		      :accelerator "c")
    (add-menu-command main-menu "File" "Save as.."
		      (make-callback #'save-as document))
    
    (setf (save-menu-entry document)
	  (make-button-entry "Save" (make-callback #'save document)
			     :accelerator "s")) 
    (insert-menu-entry main-menu "File" "Save" (save-menu-entry document))
    (setf (sensitive (save-menu-entry document)) nil)
    
    (setf (revert-menu-entry document)
	  (make-button-entry "Revert" (make-callback #'revert document)))
    (insert-menu-entry main-menu "File" "Revert" (revert-menu-entry document))
    (setf (sensitive (revert-menu-entry document)) nil)

    (setq entry
	  (make-button-entry "Print (Postscript).."
			     (make-callback #'postscript-print document)))
    (insert-menu-entry main-menu "File" "Print (Postscript)" entry)

    ;;(setq entry
	;;  (make-button-entry "Print (Bitmap)"
	;;		     (make-callback #'bitmap-print document)))
    ;;(insert-menu-entry main-menu "File" "Print (Bitmap)" entry)
    ;;(setf (sensitive entry) nil) ;; not yet implemented
		     
    (add-menu-command main-menu "File" "Quit"
		      (make-callback #'quit-app *application*)
		      :accelerator "q"
		      :separator-before t)

    (setf (undo-menu-entry document)
	  (make-button-entry "Undo" (make-callback #'undo document)
			     :accelerator "u"))
    (insert-menu-entry main-menu "Edit" "Undo" (undo-menu-entry document))
    (setf (sensitive (undo-menu-entry document)) nil)
    
    (setf (redo-menu-entry document)
	  (make-button-entry "Redo" (make-callback #'redo document)
			     :accelerator "r"))
    (insert-menu-entry main-menu "Edit" "Redo" (redo-menu-entry document))
    (setf (sensitive (redo-menu-entry document)) nil)
    
    ;;substituted by play button in history scroller
    ;;(add-menu-command main-menu "Edit" "Replay History"
    ;;      	       (make-callback #'replay-history document))
    (add-menu-command main-menu "Edit" "History Scroller"
		      (make-callback #'display-history-scroller document))

    (when (debug-menu *application*)
      (insert-menu-entry
	main-menu "Debug" "Debug on/off"
	(make-toggle-entry
	  "Debug on/off"
	  #'(lambda (new-value) (setf (debug-out *application*) new-value))))
      (insert-menu-entry
	main-menu "Debug" "Just print errors"
	(make-toggle-entry
	  "Just Print Errors"
	  #'(lambda (new-value) 
	      (setf (just-print-errors *application*) new-value))
	  :value t))
      (insert-menu-entry
	main-menu "Debug" "Feedback animation"
	(make-toggle-entry
	  "Feedback Animation"
	  #'(lambda (new-value) 
	      (setf (feedback-animation *application*) new-value))
	  :value nil))
      
      (add-menu-command main-menu "Debug" "Inspector"
			(make-callback #'inspect-dialog document 
				       :document document))
      (add-menu-command main-menu "Debug" "New Inspector"
			(make-callback #'inspect-dialog document
				       :document document
				       :use-old-one nil))

      ;;(insert-menu-entry main-menu "Debug" "Inspect" 
	;;  (make-toggle-group-entry 
	;;   "Inspect"
	;;   `(("application"  (lambda () ',*application*))
	;;     ("document"     (lambda () ',document))
	;;     ("main-view"    (lambda () (main-view ',document)))
	;;     ("last command" (lambda () (first (undo-commands ',document)))))
	;;   `(lambda (ignore1 object-to-inspect ignore2)
	;;      (declare (ignore ignore1 ignore2))
	;;      #+(or cmu genera) (setq object-to-inspect
	;;			  (coerce object-to-inspect 'function))
	;;      (inspect-dialog (apply object-to-inspect nil)
	;;		      :document ',document))
	;;   :button-resources '(:indicator-on nil)))
				  
      (add-menu-command main-menu "Debug" "Refresh main-view"
			`(lambda ()
			  (when (main-view ',document)
			    (force-redraw (main-view ',document)))))
      ;;(add-menu-command main-menu "Debug" "Refresh inspector"
	;;		(make-callback 'refresh-inspector-boxes document))
      ;;(add-menu-command main-menu "Debug" "Leave inspector"
	;;		(make-callback 'delete-inspector-boxes document))
      (add-menu-command main-menu "Debug" "Focus this application"
			(make-callback 'focus-this-application 
				       *application* document))
      (add-menu-command main-menu "Debug" "GINA Browser"
			(make-callback 'bring-up-gina-browser))
      (add-menu-command main-menu "Debug" "Test-callback"
			(make-callback 'test-callback document))
      )) 

(defginamethod destroy-windows ((doc document))
  (description "destroy all windows belonging to this document"
	       :called-by-gina "whenever a document is closed"
	       :override "if there are more windows than the toplevel shells"
	       :default-version "deletes all toplevel-shells stored in the document")
  (loop for shell in (toplevel-shells doc)
	do (destroy shell)))

(defginamethod select-new-file ((doc document) &aux new-pathname)
  (description "ask user for a  pathname of a new document"
	       :override "only if standard dialog box is not sufficient"
	       :default-version "pops up the standard save-dialog-box"
	       :result "T if a new pathname was selected, NIL otherwise")
  ;; create dialog box if necessary
  (when (not (save-dialog-box doc))
    (setf (save-dialog-box doc)
      (make-save-dialog-box (wildcard doc) (name doc)
			    :file-type (file-type *application*)
			    :document doc)))
  ;; pop it up to ask user
  (setq new-pathname
	(pop-up (save-dialog-box doc)))

  (when new-pathname
    ;; store the pathname in the document
    (rename doc new-pathname)
    ;; return success
    t))

(defginamethod rename ((doc document) new-pathname)
  (description "react when user has changed the name of a document"
	       :called-by-gina "when a document is renamed by the user"
	       :override "if a renaming has further consequences"
	       :default-version "changes the name, wildcard and title of the main window")
  (setf (file-pathname doc) new-pathname)
  (setf (wildcard doc)      (directory-wildcard new-pathname))
  (setf (name doc)          (pathname-name new-pathname))
  (set-motif-resources (main-shell doc) :icon-name (name doc))
  (setf (title (main-shell doc))
	(format nil "~a: ~a" (name *application*) (name doc))))

(defginamethod resize-windows ((doc document))
  (description "resize windows, because the document contents have changed"
	       :called-by-gina "when a document is reverted to an old version"
	       :override "if also the window contents depend on the size"
	       :default-version "resizes the shell")
  
  (with-slots (main-shell shell-width shell-height) doc
    ;; shell-width and shell-height cannot be :auto because header is read from file
    (resize main-shell shell-width shell-height)))

(defginamethod redraw-views ((doc document))
  (description "redraw all views, because the document contents have changed"
	       :called-by-gina "when a document is reverted to an old version"
	       :called-by-application "to redisplay all content dependent information"
	       :override "to redisplay more parts of the display than the views"
	       :default-version "redraws all views known to the doc")
  (loop for view in (views doc)
	do (force-redraw view)))

(defginamethod new ((doc document))
  (description "execute the NEW menu item"
	       :called-by-gina "in reaction to the new menu entry"
	       :called-by-application :rarely
	       :override :rarely
	       :default-version "creates a new document")
  (create-new-document *application*
		       :pathname nil
		       :wildcard (wildcard doc)))

(defginamethod do-open ((doc document) &aux pathname)
  (description "let user select a document of this type and open it"
	       :called-by-gina "in reaction to the open menu entry"
	       :called-by-application :rarely)
  (setq pathname (select-old-file doc (file-type *application*)))
  (when pathname
    (with-clock-cursor
      (start-file pathname
		  :signature (signature *application*)
		  :from-document doc))))

(defginamethod select-old-file ((doc document) file-type)
  (description "ask user for a  pathname of an existing document"
   :override "only if the standard open-dialog-box is not sufficient"
   :default-version "pops up standard the open-dialog-box"
   :result "T if a pathname was selected, NIL otherwise")
  (when (not (and (open-dialog-box doc) 
		  (equal file-type (file-type (open-dialog-box doc)))))
    (setf (open-dialog-box doc)
      (make-open-dialog-box (wildcard doc) 
			    :file-type file-type :document doc)))
  (pop-up (open-dialog-box doc)) 
  ;; return pathname or NIL
  )

(defginamethod minifinder ((doc document) &aux pathname)
  (description "let user select any document or application and open it"
	       :called-by-gina "in reaction to the minifinder menu entry"
	       :called-by-application :rarely)
  (setq pathname (select-old-file doc :wild))
  (when pathname
    (with-clock-cursor
      (start-file pathname :from-document doc))))

(defginamethod save-as ((doc document))
  (description "let user select a new filename and save"
	       :called-by-gina "in reaction to the save-as menu entry"
	       :called-by-application :rarely)
  (when (select-new-file doc)
    ;; the user did not cancel
      (save doc)))

(defginamethod save ((doc document) &aux (written-without-error t) (error nil))
  (description "save document to file; ask for filename if not yet known"
	       :called-by-gina "in reaction to the save menu entry"
	       :called-by-application :rarely)
  (if (null (file-pathname doc))
      (save-as doc)
      (with-clock-cursor
	(if (just-print-errors *application*)
	    ;; catch errors while writing the file
	    (multiple-value-setq 
		(written-without-error error)
	      (ignore-errors ;; return NIL on error
		    (write-to-file doc (file-pathname doc))
		    (setf (modified doc) nil)
		    ;; indicate success
		    t))
	    ;; else: do not suppress errors while writing the file
	    (progn
	      (write-to-file doc (file-pathname doc))
	      (setf (modified doc) nil)))
	
	(when (not written-without-error)
	  (warning-dialog (format nil "~a" error)
			  :dialog-title
			  (format nil "Cannot write to file ~a!" 
				  (or (ignore-errors (namestring (file-pathname doc)))
				      (file-pathname doc)))
			  :document doc))
	)))

(defginamethod revert ((doc document) &aux (read-without-error t) (error nil))
  (description "read last saved version from file"
	       :called-by-gina "in reaction to the revert menu entry"
	       :called-by-application :rarely
	       :override :rarely)
  (declare (special *application*))
  (when (and (file-pathname doc) (modified doc)
	     (confirm-dialog (format nil "Revert to last saved version of ~s?" (name doc))
			     :document doc))
    (with-clock-cursor
      (if (just-print-errors *application*)
	  ;; catch errors while reading the file
	  (multiple-value-setq 
	      (read-without-error error)
	    (ignore-errors 
	     (read-from-file doc (file-pathname doc) :create-windows nil)
	     ;; indicate success
	     t))
	  ;; else: do not suppress errors while reading the file
	  (read-from-file doc (file-pathname doc) :create-windows nil))
      (if read-without-error
	  (progn
	    (redraw-views doc)
	    (irreversible-change doc))
	  ;; else
	  (warning-dialog (format nil "~a" error)
		      :dialog-title
		      (format nil "Cannot read file ~a!"
			      (or (ignore-errors (namestring (file-pathname doc)))
				  (file-pathname doc)))
		      :document doc))
      
      )))

(defginamethod do-close ((doc document) &aux (answer :yes))
  (description "close document; ask for save if modified"
	       :called-by-gina "in reaction to the close menu entry"
	       :called-by-application :rarely)
  (declare (special *application*))
  (expose (main-shell doc)) ;; open icon
  (loop while (and (modified doc) (eq answer :yes))
	;; repetition when user cancels save-dialog
	do (setq answer
		 (alternative-dialog (format nil "Save ~s first?" (name doc))
				     :document doc))
	   (when (eq answer :yes)
	     (save doc)))
    
  (when (not (eq answer :cancel))
    (with-slots (document-list) *application*
      (when (background-processes doc)
	(kill-all-background-processes doc))
      (destroy-windows doc)
      (setq document-list (delete doc document-list))
      (recompute-docs-menu-entries *application*)
      ;; if the last document was deleted, terminate application
      (when (not document-list)
	;; terminate application
	;; stop toolkit-server
	(xtk:quit-application nil (application-shell *application*))
	)))
  ;; return what happened
  answer)

(defginamethod react-to-change ((doc document) 
				cmd "command object, or nil if :end-batch"
				reason ":doit, :undoit, :redoit, :end-batch"
				batch "whether during history scrolling")
  (description "a hook to call eval-taps of the main-shell"
	       :called-by-gina "after commands have been executed"
	       :called-by-application :rarely)
  (declare (ignore reason batch))
  (when (and cmd (main-shell doc))
    (eval-taps (main-shell doc) cmd)))

(defginamethod command-executed ((doc document))
  (description "inform document that a command has been executed "
     :called-by-gina "whenever a command is executed, undone, or redone"
     :called-by-application :rarely
     :override "to implement actions to be done after each command"
     :default-version :do-nothing)
  )

(defginamethod command-executed :after ((doc document))
  (description "adjust undo/redo menu entries")
  (update-undo-redo-menu-entries doc)
  (when (history-scroller doc)
    (command-executed (history-scroller doc))))

(defginamethod update-undo-redo-menu-entries ((doc document))
  (description "update labels and sensitivity")
  (with-slots (undo-commands redo-commands history-scroller) doc
    ;; update undo menu entry
    (setf (label-string (undo-menu-entry doc))
	  (if undo-commands
	      (format nil "Undo ~a" (name (first undo-commands)))
	      "Undo"))
    (setf (sensitive (undo-menu-entry doc)) (when undo-commands t))

    ;; update redo menu entry
    (setf (label-string (redo-menu-entry doc))
	  (if redo-commands
	      (format nil "Redo ~a" (name (first redo-commands)))
	      "Redo"))
    (setf (sensitive (redo-menu-entry doc)) (when redo-commands t))))

(defmethod register-command ((doc document) command)
  "store command object just executed in the document"
  ;; command can be undone, but no further redo is possible
  (with-slots (undo-commands redo-commands max-history history-scroller) doc
    (push command undo-commands)
    
    (loop for command in redo-commands
	  do (commit command t))
    (setq redo-commands nil)
    (when (and max-history (> (length undo-commands) max-history))
      ;; very old commands cannot be undone anymore
      (loop for cmd in (subseq undo-commands max-history)
	    do (commit cmd nil))
      (setq undo-commands (subseq undo-commands 0 max-history)))

    (command-executed doc)
    (react-to-change doc command :doit nil)
    ))

(defginamethod undo ((doc document) &key (update-history-scroller t)
			 &aux command)
  (description "undo the last command executed"
	       :called-by-gina "in reaction to the undo menu entry"
	       :called-by-application :rarely)
  (with-slots (undo-commands redo-commands history-scroller) doc
    (when undo-commands
      (setq command (first undo-commands))
      (undoit command)
      (pop undo-commands)
      (push command redo-commands)
      
      (react-to-change doc command :undoit (not update-history-scroller))
      (when update-history-scroller
	(command-executed doc))
      )))

(defginamethod irreversible-change ((doc document))
  (description "clear the command history"
	       :called-by-gina "if an undoable command is executed that causes a change"
	       :called-by-application "to indicate that no more undo/redo possible now")
  (with-slots (undo-commands redo-commands history-scroller) doc
    (loop for command in redo-commands
	  do (commit command t))
    (loop for command in undo-commands
	  do (commit command nil))
    (setq undo-commands nil redo-commands nil)

    ;; react-to-change here??
    (command-executed doc)
    ))

(defginamethod redo ((doc document) &key (update-history-scroller t) &aux command)
  (description "redo the last command undone"
	       :called-by-gina "in reaction to the redo menu entry"
	       :called-by-application :rarely)
  (with-slots (undo-commands redo-commands history-scroller) doc
    (when redo-commands
      (setq command (first redo-commands))
      (redoit command)
      (pop redo-commands)
      (push command undo-commands)

      (react-to-change doc command :redoit (not update-history-scroller))
      (when update-history-scroller
	(command-executed doc))
      )))

(defginamethod replay-history ((doc document) &aux nr-of-commands)
  (description "redo the commands in the redo list as an animation"
	       :called-by-gina "in reaction to the play button"
	       :called-by-application :rarely)
  (declare (special *display*))
  (with-slots (redo-commands) doc
    (with-clock-cursor
     (xtk:with-immediate-update-enabled
      (setq nr-of-commands (length redo-commands))
      (loop repeat nr-of-commands
	    do (sleep 0.3)
	       (redo doc)
	       (xlib:display-force-output *display*))
      ))))

(defginamethod write-to-file ((doc document) pathname)
  (description "write the document to the specified file"
	       :called-by-gina "when the document is saved"
	       :override :rarely
	       :default-version "opens the the pathname and writes header and contents")
  (with-open-file (stream pathname :direction :output :if-exists :new-version)
    (write-header-to-stream doc stream)
    (write-to-stream doc stream)))

(defginamethod read-from-file ((doc document) pathname &key (create-windows t))
  (description "read the document from the specified file"
	       :called-by-gina "when the document is opened"
	       :override :rarely
	       :default-version "opens the the pathname and reads header and contents")
  (with-open-file (stream pathname :direction :input)
    (read-header-from-stream doc stream)
    (if create-windows
	(create-windows doc)
	(resize-windows doc))
    (read-from-stream doc stream))
  (setf (modified doc) nil))

(defginamethod write-header-to-stream ((doc document) stream)
  (description "write the window-size to the specified stream"
	       :called-by-gina "when the document is saved"
	       :override :rarely
	       :default-version "writes out signature, width and height")
  (declare (special *application*))
  (update-slots (main-shell doc))  ;; get actual size from toolkit server
  (format stream "~s~%~d ~d~%"
	  (signature *application*)
	  (width (main-shell doc))
	  (height (main-shell doc))))

(defginamethod read-header-from-stream ((doc document) stream)
  (description "read the window-size from the specified stream"
	       :called-by-gina "when the document is opened"
	       :override :rarely
	       :default-version "reads signature, width and height")
  (with-slots (shell-width shell-height) doc
    (read stream) ;; skip signature
    (setq shell-width (read stream))
    (setq shell-height (read stream))))

(defginamethod write-to-stream ((doc document) stream)
  (description "write the document contents to the specified stream"
	       :called-by-gina "when the document is saved"
	       :override :always
	       :default-version :do-nothing)
  (declare (ignore stream)))

(defginamethod read-from-stream ((doc document) stream) 
  (description "read the document contents from the specified stream"
	       :called-by-gina "when the document is opened"
	       :override :always
	       :default-version :do-nothing)
  (declare (ignore stream)))

(defginamethod display-empty ((doc document)) 
  (description "init and display empty document"
	       :called-by-gina "after a new empty document is created"
	       :override "if even the empty document needs content-dependent widgets"
	       :default-version :do-nothing)
  )

(defginamethod create-checkpoint ((doc document))
  (description "create a string describing the document contents"
	       :called-by-gina "if an undoable command is implemented by a checkpoint"
	       :override :rarely
	       :default-version "just calls write-to-stream into a string")
  (with-output-to-string (stream)
    (write-to-stream doc stream)))

'(print (create-checkpoint (first (document-list *application*))))

(defginamethod restore-checkpoint ((doc document) checkpoint)
  (description "restore document from a checkpoint"
	       :called-by-gina "if an undoable command is implemented by a checkpoint"
	       :override :rarely
	       :default-version "just calls read-from-stream with a string")
  (with-input-from-string (stream checkpoint)
    (read-from-stream doc stream))
  (redraw-views doc))

(defginamethod postscript-print ((doc document))
  (description "hardcopy document on postscript printer"
	       :called-by-gina "in reaction to the postscript-print menu entry"
	       :called-by-application :rarely
	       :default-version "warns that this feature in unimplemented"
	       :overridden-by-gina "in experimental extension")
  (warning-dialog "Postscript printing is only available as 
an experimental extension!
Load file gina/postscript.lisp" :document doc))

(defginamethod bitmap-print ((doc document))
  (description "hardcopy bitmap of the main view"
	       :called-by-gina "in reaction to the bitmap-print menu entry"
	       :called-by-application :rarely)
  (when (main-view doc)
    (draw-into-pixmap (main-view doc))))

(defginamethod display-history-scroller ((doc document))
  (description "pop-up history scroller, create if necessary"
	       :called-by-gina "in reaction to the history scroller menu entry"
	       :called-by-application :rarely)
  (with-slots (history-scroller) doc
    (when (not history-scroller)
      (setq history-scroller (make-history-scroller doc))
      (setf (relative-y history-scroller) :bottom)
      (setf (relative-widget history-scroller) (main-shell doc)))
    (pop-up history-scroller)))

(defginamethod test-callback ((doc document))
  (description "do some testing within a callback"
	       :called-by-gina "in response to the Test-Callback menu entry"
	       :override "do test some code inside a callback"
	       :default-version "pops up a warning dialog box")
  (warning-dialog "Test-callback executed" :document doc))

(defginamethod create-drop-command ((doc document) 
                                   shell "the shell that received the cmd"
				   x "x-pos relative to shell"
				   y "y-pos relative to shell"
				   protocol-id "symbol identifying the cmd"
				   transfer-value "transfer-value from cmd"
				   received-from "x-window to send response to")
  (description
    "create a duplicate drag-command"
    :called-by-gina "when a drag command enters a shell of this doc"
    :override "to react to drop commands understood by this document"
    :default-version :do-nothing)
  (declare (ignore shell x y protocol-id transfer-value received-from))
  ;(format t "Drop: ~s ~s ~d ~d ~s ~s ~s~%" doc shell x y protocol-id 
  ;           transfer-value received-from)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class history-scroller
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass history-scroller (modeless-dialog-box)
   ((document :accessor document :initarg :document)
    (scrollbar       :accessor scrollbar)
    (previous-button :accessor previous-button)
    (next-button     :accessor next-button)
    (cmd-num-label   :accessor cmd-num-label)
    (play-field      :accessor play-field))
   (:documentation "a dialog box for scrolling through the command history"))

(defun make-history-scroller (document &aux new-history-scroller column row)
  "make a dialog box for scrolling through the command history"
  (setq new-history-scroller
	(make-modeless-dialog-box "History"
				  :document document
				  :initargs (list :document document)
				  :class 'history-scroller))
  (with-slots (scrollbar previous-button next-button cmd-num-label play-field)
              new-history-scroller
    (with-slots (undo-commands redo-commands) document
      (setq column 
	    (make-row-column new-history-scroller :orientation :vertical))
      (setq scrollbar
	    (make-scrollbar column
			    :orientation :horizontal
			    :minimum 0 
			    :maximum (+ 1 (length redo-commands) 
					(length undo-commands))
			    :value   (length undo-commands)
			    :slider-size 1))
      (setf (value-changed-callback scrollbar)
	    (make-callback #'scroll-history new-history-scroller 
			   :value-changed))
      (setf (drag-callback scrollbar)
	    (make-callback #'scroll-history new-history-scroller :drag))

      ;;(setq row (make-row-column column :orientation :horizontal
      ;;			 :motif-resources (list :margin-width 0)))
      (setq row (make-form column))
      (setq previous-button
	    (make-push-button row
			      (format nil "~20a"
				      (if undo-commands 
					  (name (first undo-commands)) " "))
			      :alignment :beginning
			      :recompute-size nil
			      :activate-callback
			      (make-callback #'undo document)))
      
      (setq cmd-num-label
	    (make-label (make-frame row) (format nil "~3d" 
						 (length undo-commands))
			:recompute-size nil))
      (setq next-button
	    (make-push-button row 
			      (format nil "~20@a"
				      (if redo-commands 
					  (name (first redo-commands)) " "))
			      :alignment :end
			      :recompute-size nil
			      :activate-callback
			      (make-callback #'redo document)))
      (define-form-constraint previous-button :top-attachment :form
			      :left-attachment :form
			      :right-attachment :position :right-position 45)
      (define-form-constraint (parent cmd-num-label) :top-attachment :form
			      :left-attachment :widget 
			      :left-widget previous-button
			      :right-attachment :position :right-position 55)
      (define-form-constraint next-button :top-attachment :form
			      :right-attachment :form
			      :left-attachment :widget
			      :left-widget (parent cmd-num-label))
      (setq play-field
	    (add-replay-features new-history-scroller column))
            
      ))
  
  new-history-scroller)

(defmethod scroll-history ((h-scroller history-scroller) reason new-cmd-number
			   &aux undos)
  "scroll history to a specified point"
  (with-slots (previous-button next-button cmd-num-label) h-scroller
    (with-slots (undo-commands redo-commands) (document h-scroller)
      (setq undos (length undo-commands))
      (if (< new-cmd-number undos)
	  ;; execute some undos
	  (loop repeat (- undos new-cmd-number)
		do (undo (document h-scroller) :update-history-scroller nil))
	  ;; execute some redos
	  (loop repeat (- new-cmd-number undos)
		do (redo (document h-scroller) :update-history-scroller nil)))
      ;; do not update menus and history scrollers while dragging the slider
      (when (not (eq reason :drag))
	(react-to-change (document h-scroller) nil :end-batch nil)
	(command-executed (document h-scroller))
	))))

(defmethod command-executed ((h-scroller history-scroller) &aux new-maximum new-value)
  "update history-scroller according to undo-command, redo-commands ..."
  (with-slots (scrollbar previous-button next-button cmd-num-label) h-scroller
    (with-slots (undo-commands redo-commands) (document h-scroller)
      ;; set maximum and value in an atomic operation
      (setq new-maximum (+ 1 (length redo-commands) (length undo-commands))
	    new-value   (length undo-commands))
      (set-motif-resources scrollbar :maximum new-maximum :value  new-value)
      ;; bypass :after deamons
      (with-slots (maximum value) scrollbar
	(setq maximum new-maximum value new-value))
      
      (setf (label-string previous-button)
	    (if undo-commands (name (first undo-commands)) " "))
      (setf (label-string cmd-num-label)
	    (format nil "~3d" (length undo-commands)))
      (setf (label-string next-button)
	    (if redo-commands (name (first redo-commands)) " "))
    )))

(defmethod add-replay-features ((h-scroller history-scroller) parent-widget)
  "add a hook for the future interaction recorder"
  (make-push-button parent-widget "Play"
		    :activate-callback (make-callback #'replay-history 
						      (document h-scroller))))
