;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:xtk; Base:10 -*-

(in-package :xtk)

(setq *sccsid* "@(#)cmucl.lisp	1.6 9/8/93")

;; (export 'clm-error)


;;; This code is executed whenever the compiled CLM code is loaded


;; Ensure that foreign object code is loaded exactly once

(unless (member :motif-server *features*)
  (ext:load-foreign (list "unixsocket.o" "io.o")
		      :libraries '("-lc" "-lm")))
(pushnew :motif-server *features*)
(pushnew :clm-has-handler *features*)


;;; The files that need to be compiled for CLM



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Load a file. Compile before loading if necessary !
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun source-file-name (file-name)
  (format nil "~a~a" file-name ".lisp"))

(defun binary-file-name (file-name)
  (format nil "~a~a" file-name ".sparcf"))

;; ld loads a binary file. The source is compiled if necessary
;; The source is also compiled if the depends-on file was modified after
;; source was compiled

(defun ld (source &rest files-dependent-on &aux source-file binary-file)
  (setf source-file (source-file-name source))
  (setf binary-file (binary-file-name source))
  (when (or (not (probe-file binary-file))
            (> (file-write-date source-file) (file-write-date binary-file))
            (loop for file in files-dependent-on
	          do (when (> (file-write-date (source-file-name file))
			  (file-write-date binary-file))
	          (return t))))
      (compile-file source-file :error-file t))
  (load binary-file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;;   Save a lisp image
;;;;;
;;;;;   After calling (save-lisp "world") two files
;;;;;   "world.core" and "world.obj" are saved.
;;;;;   For loading the world both files are needed in the current directory.
;;;;;   To resume the saved world call:
;;;;;   lisp -core world.core [-init my-inits]
;;;;;   Looks for my-inits in the home-dir of user (if specified)
;;;;;   or init.<obj.type> or init.lisp in the home-dir of user
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-lisp (name &aux name-core-file name-object-file)
  "saves a world under current directory"
  (setq name-core-file   (format nil "~a.core" name))
  (setq name-object-file (format nil "~a.obj"  name))
  (ext:run-program "/bin/sh"
    (list "-c"
     (format nil "cp ~a ~a" system::*previous-linked-object-file* name-object-file)))
  (user::gc t)
  (extensions::save-lisp name-core-file :purify t
           :init-function
           (coerce
	    `(lambda ()
               (setq system::*previous-linked-object-file* nil
                     system::*foreign-segment-free-pointer* #xC00000)
               (system::load-object-file ,name-object-file)
               (throw 'lisp::top-level-catcher nil))
	    'function)
	    :load-init-file t)
  (format nil "Generated: ~a ~a" name-core-file name-object-file))


;;; This is the condition that will be signalled for all errors in CLM.
;;; The primary purpose for this is to make sure the graphics debugger is
;;; not invoked when an error occurs in CLM.

(define-condition clm-error (error)
  ((format-string)
   (format-arguments))
  (:documentation "An error has occurred in the CLM code.")
  (:report (lambda (condition stream)
	     (format stream "A CLM error has occurred.~%~?"
		     (clm-error-format-string condition)
		     (clm-error-format-arguments condition)))))

;;; CLM-ERROR -- Internal
;;;
;;; This function acts just like ERROR except that it signals a CLM-ERROR
;;; instead of a SIMPLE-ERROR.  Using this allows the system to pick up
;;; errors in CLM, in particular the debugger.

(declaim (inline clm-error clm-cerror))
(defun clm-error (string &rest args)
  (error 'clm-error :format-string string :format-arguments args))

(defun clm-cerror (continue-string string &rest args)
  (cerror continue-string 'clm-error
	  :format-string string :format-arguments args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Get the process that is currently running
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun current-process ()
  nil)

;;; PROCESS-RUN-FUNCTION
;;;
;;; Since a function cannot be executed in its own thread, PROCESS-RUN-FUNCTION
;;; will not attempt to do this.  It will now call BREAK and, if the user
;;; decides to proceed, simply invoke the function which was to be run in
;;; its own thread.

(defun process-run-function (name function &rest args)
  "Currently, PROCESS-RUN-FUNCTION cannot run functions in there own threads.
  The best it can do is invoke the given function."
  (break "PROCESS-RUN-FUNCTION called with process name:  ~a.~%~
	  Proceeding will merely invoke the function, not run it in its own ~
	  process." name)
  (apply function args))


;;; WITH-PROCESS-LOCK
;;;
;;; Since the CMUCL version of CLM does not use multiple threads, there is
;;; no need for a locking mechanism; there is only one thread.

(defmacro with-process-lock (lock &body body)
  (declare (ignore lock))
  `(progn ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Define function to create a process lock
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (make-process-lock)
(defun make-process-lock () nil)

(defmacro without-process-lock ((lock) &body body)
  (declare (ignore lock))
  `(progn ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Kill a running process
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro process-kill (process)
  (declare (ignore process))
  nil)

(defmacro process-wait (string function app)
  (declare (ignore string function app))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Test whether the given path is a directory
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(alien:def-alien-routine is-directory
    c-call:int
  (path c-call:c-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Listen for input on the Motif server's connection
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(alien:def-alien-routine listen-to-socket
    c-call:int
  (socket c-call:int))

(defun listen-for-input (connection)
  (let ((status (listen-to-socket (toolkit-connection-stream connection))))
    (when (minusp status)
      (clm-error "Read error on stream"))
    (plusp status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Listen to the Motif server
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wait-for-input-from-server (connection wait-function)
  (unless (funcall wait-function connection)
    (system:wait-until-fd-usable (toolkit-connection-stream connection)
				 :input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; C function to create a socket
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(alien:def-alien-routine connect-to-toolkit-server
    c-call:int
  (host c-call:c-string)
  (port c-call:int))

(alien:def-alien-routine connect-directly-to-toolkits
    c-call:int
  (binary c-call:c-string))

(alien:def-alien-routine perror
    c-call:int
  (msg c-call:c-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Create a bidirectional stream
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-motif-stream (connection host)
  (if host

      ;; try to connect  via socket to existing clmd on host
      (let ((fd (connect-to-toolkit-server host *xt-tcp-port*)))
	(declare (fixnum fd))
	(when (minusp fd)
	  (clm-error "Failed to connect to server: ~A~%" host))
	(setf (toolkit-connection-stream connection) fd)
	)

      ;; directly fork clm-server as child of Lisp process
      (let* ((binary-name (get-clm-binary-name))
	     (fd (connect-directly-to-toolkits binary-name))
	     )
	(declare (fixnum fd))
	(when (minusp fd)
	  (perror "Failed to fork clm-server:")
	  (clm-error "Failed to fork clm-server from Lisp")
	  )
	(setf (toolkit-connection-stream connection) fd)
	))
  )

(defun get-clm-binary-name (&optional (binary *clm-binary-name*))
  "This looks in the path for a directory containing the server and the toolkit"
  (flet ((try-dir (dir)
           (let ((try-path (pathname (format nil "~a/~a" dir binary))))
	     (when (ignore-errors (probe-file try-path))
	       (return-from get-clm-binary-name (namestring try-path))))))
    (let ((dirs (if (listp *clm-binary-directory*)
			    *clm-binary-directory*
			  (list *clm-binary-directory*))))
      (mapc #'try-dir dirs)
      (error "Could not find the binary ~S in any of ~{~A ~}"
	     binary dirs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Close the stream
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(alien:def-alien-routine ("CloseStream" close-xt-stream)
    c-call:int
  (stream c-call:int))

(defun close-motif-stream (connection)
  (setf (toolkit-connection-closed connection) t)
  (close-xt-stream (toolkit-connection-stream connection)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Read/Write a data object from/to the stream
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(alien:def-alien-routine ("SendHeader" send-header)
    c-call:int
  (stream c-call:int)
  (code   c-call:int)
  (serial c-call:int)
  (length c-call:int))

(alien:def-alien-routine ("SendInteger" send-integer)
    c-call:int
  (stream c-call:int)
  (value c-call:int))

(alien:def-alien-routine ("SendString" send-string)
    c-call:int
  (steam c-call:int)
  (value c-call:c-string))

(alien:def-alien-routine ("SendSymbol" send-symbol)
    c-call:int
  (stream c-call:int)
  (value c-call:c-string))

(defun xt-send-command (stream code serial list-of-arguments)
  (declare (list list-of-arguments))
  (send-header stream code serial (length list-of-arguments))
  (dolist (value list-of-arguments)
    (typecase value
      (integer (send-integer stream value))
      (string  (send-string stream value))
      (symbol  (send-symbol stream (symbol-name value)))
      (t       (clm-error "illegal argument type")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Send a command to the toolkit server
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(alien:def-alien-routine ("FlushBuffer" flush-buffer)
    c-call:int
  (socket c-call:int))

(defun toolkit-send-command (code serial list-of-args)
  (declare (special *motif-connection*))
  ;;; scheduling must be inhibited because we have only one output buffer
  ;;; and because the C io functions must not be interrupted
  ;;% was mp:without-scheduling
  (when (eql (system:without-interrupts
	      (xt-send-command (toolkit-connection-stream *motif-connection*)
			       code serial list-of-args)
	      (flush-buffer (toolkit-connection-stream *motif-connection*)))
	     -1)
    (clm-error "xt-send-command failed~%")))

(alien:def-alien-routine ("ReceiveInteger" receive-integer)
    c-call:int
  (stream c-call:int)
  (rc (* c-call:int) :out))

(defun receive-lisp-string (stream)
  (let* ((length (receive-integer stream))
	 (string (make-string length)))
    (multiple-value-bind
	(okay errno)
	(system:without-gcing
	 (unix:unix-read stream (ext::vector-sap string) length))
      (unless okay
	(clm-error "receive-lisp-string failed: ~A"
		   (unix:get-unix-error-msg errno)))
      string)))

(defun xt-receive-command (stream)
  (let ((list-of-args nil)
	(code (receive-integer stream))
	(serial (receive-integer stream))
	(num-args (receive-integer stream)))
    (dotimes (i num-args)
      (push (case (receive-integer stream)
	      (0 (receive-integer stream))
	      (1 (receive-lisp-string stream))
	      (2
	       (let ((print-name (receive-lisp-string stream)))
		 (cond ((equal print-name "NIL") nil)
		       ((equal print-name "T") t)
		       (t (intern print-name 'keyword)))))
	      (t
	       (clm-error "Illegal type")))
	    list-of-args))
    (values (cons code (nreverse list-of-args))
	    (list serial num-args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Receive a command from the toolkit server
;;;;; Do a passive wait until input is available
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun toolkit-receive-command ()
  (declare (special *motif-connection*))
  ;; Suspend process until input is available
  (wait-for-input-from-server *motif-connection* #'listen-for-input)
  ;; Get a message from the motif server
  ;; %% was mp:without-scheduling
  (system:without-interrupts
   (multiple-value-bind
       (r1 r2)
       (xt-receive-command (toolkit-connection-stream *motif-connection*))
     (values r1 r2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; fd handlers
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *active-clm-handlers* ()
  "alist of (fd . System FD handlers returned by system:add-fd-handler).")

(defun add-clm-handler ()
  (let ((fd (toolkit-connection-stream *motif-connection*)))
    (when (assoc fd *active-clm-handlers*)
	(break "Hey, there's already a handler for fd=~d." fd))

    (push (cons fd
		(system:add-fd-handler
		 fd
		 :input
		 (let ((mc *motif-connection*)
		       (xd *x-display*))
		   #'(lambda (fd0)
		       (declare (ignore fd0))
		       (declare (special *motif-connection* *x-display*))
		       (let ((*motif-connection* mc)
			     (*x-display* xd)
			     (thrown-out-of-callback t))
			 (unwind-protect
			     (progn
			       (wrapper-apply #'handle-pending-events '(nil))
			       (setq thrown-out-of-callback nil))
			   ;; cleanup:
			   (when (or thrown-out-of-callback
				     (toolkit-connection-dispatcher-terminated
				      *motif-connection*))
			     (cleanup-actions)
			     (remove-clm-handler fd)))
			 )))))
	  *active-clm-handlers*)
    ;; call handler if already input available
    (when (toolkit-connection-blocked-callback *motif-connection*)
      ;; (format t "in add-clm-handler: extra clm-main-loop~%")
      (wrapper-apply #'handle-pending-events '(nil)))))


(defun remove-clm-handler (fd)
  (let ((handler (cdr (assoc fd *active-clm-handlers*))))
    (cond (handler
	   (system:remove-fd-handler handler)
	   (setf *active-clm-handlers* (remove fd *active-clm-handlers*
					       :key #'car :test #'=))
	   handler)
	  (t
	   (clm-error
	    "Cannot remove handler (fd=~d) because there is none." fd)))))
