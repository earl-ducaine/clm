;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:xtk; Base:10 -*-

(in-package :xtk)

(setq *sccsid* "@(#)excl.lisp	1.19 11/22/93")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; This code is executed whenever the compiled CLM code is loaded
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(excl::provide :clm)
(pushnew :clm-has-processes *features*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Load the foreign files unless already present
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(flet ((foundp (entry-point)
	 (let ((x (make-array 1 :initial-contents
			      (list (ff:convert-to-lang
				     entry-point))))
	       (y 
		 
		(make-array 1 :element-type '(unsigned-byte 32))))
	   (zerop (ff:get-entry-points x y)))))
  (mapc #'(lambda (entry file)
	      (unless (foundp entry)
		(load file  :system-libraries '("m"))))
	  '("connect_to_toolkit_server"  "SendInteger" "ReceiveEXCLString" 
	    "set_run_status")
	  '("unixsocket.o" "io.o" "exclio.o" "runstatus.o")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Compile only if parser uses correct case-mode
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (member excl:*current-case-mode* '(:case-insensitive-upper
					   :case-insensitive-lower
					   :case-sensitive-lower))
  (error "CLM compiles/loads only with excl:*current-case-mode* set to one ~
of ~S. Its current  setting is ~a" 
	 '(:case-insensitive-upper
	   :case-insensitive-lower
	   :case-sensitive-lower)
	 excl:*current-case-mode*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Load required modules and start scheduler
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some macros are required during compiling
(eval-when (compile load eval)
  (require :foreign)
  (require :process)
  #-(or :allegro-v4.0 (and excl (version>= 4 1))) (require :cstructs)
  (require :loop)
  (require :mdproc)
  #+clm-needs-clx (require :clx))

(unless (excl::scheduler-running-p)
  (mp:start-scheduler))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; The files that need to be compiled for CLM
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *clm-files*
  '(("pkg")
    ("defs")
    ("excl" "defs")
    ("low" "excl" "defs")
    ("files" "defs")
    ("display" "defs")
    ("convenience" "defs")
    ("widgets" "defs")
    ("text" "defs")
    ("callbacks" "defs")
    ("events" "defs")
    ("transl" "defs")
    ("dialogs" "defs")
    ("cursor" "defs")
    ("color" "defs")
    ("listw" "defs")
    ("timers" "defs")
    ("graph" "defs")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Load a file. Compile before loading if necessary !
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun source-file-name (file-name)
  (format nil "~a~a" file-name ".lisp"))

(defun binary-file-name (file-name)
  (format nil "~a~a" file-name ".fasl"))
   
;; ld loads a binary file. The source is compiled if necessary
;; The source is also compiled if the depends-on file was modified after
;; source was compiled

(defun ld (source &rest files-dependent-on &aux source-file binary-file)
  (setf source-file (source-file-name source))
  (setf binary-file (binary-file-name source))
  (when (or (not (probe-file binary-file))
            (> (file-write-date source-file) (file-write-date binary-file))
            (loop for file in files-dependent-on
	          when (> (file-write-date (source-file-name file))
			  (file-write-date binary-file))
	          return t))
      (compile-file source-file :output-file binary-file))
  (load binary-file))

;;; Save a lisp image

(defun save-lisp (name)
  (user::gc t)
  (user::dumplisp :name name :read-init-file t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Compute the C language name of a function
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro c-name (name)
  `(ff:convert-to-lang ,name :language :c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Allow CLM to signal its own error condition
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro clm-error (string &rest args)
  `(error ,string ,@args))

(defmacro clm-cerror (cstring string &rest args)
  `(cerror ,cstring ,string ,@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Importing Cltl2 macros
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+allegro-v3.1
(import 'excl::ignore-errors)

#+allegro-v3.1
(import 'excl::with-simple-restart)

#+allegro-v3.1
(import 'excl::handler-bind)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Get the process that is currently running
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro current-process ()
  'mp::*current-process*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Define function to run a lisp process
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf (symbol-function 'process-run-function)
      (symbol-function 'mp:process-run-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Define the wait function
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf (symbol-function 'process-wait)
      (symbol-function 'mp:process-wait))

(setf (symbol-function 'process-wait-with-timeout)
      (symbol-function 'mp:process-wait-with-timeout))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Define function to create a process lock
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf (symbol-function 'make-process-lock)
  (symbol-function 'mp:make-process-lock))

(setf (symbol-function 'with-process-lock)
  (symbol-function 'mp:with-process-lock))

;; only necssary until bug in mp:with-process-lock is not fixed
#+ignore
(defmacro with-process-lock ((lock) &body body)
  `(let ((already-locked-by-myself-before
	  (eq (mp:process-lock-locker ,lock) mp:*current-process*)))
     (unwind-protect
	 (progn 
	   (when (not already-locked-by-myself-before)
	     (mp:process-lock ,lock))
	   . ,body)
       ;; cleanup:
       (when (and (not already-locked-by-myself-before)
		  (eq (mp:process-lock-locker ,lock) mp:*current-process*))
	 (mp:process-unlock ,lock)))))

(defmacro without-process-lock ((lock) &body body)
  `(unwind-protect
       (progn 
	 (mp:process-unlock ,lock)
	 . ,body)
     ;; cleanup
     (mp:process-lock ,lock)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Kill a running process
;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf (symbol-function 'xtk::process-kill) 
      (symbol-function 'mp:process-kill))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Test whether the given path is a directory
;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+unix
(ff:defforeign 'is-directory
	       :entry-point (c-name "is_directory")
	       :arguments '(string)
	       :return-type :fixnum)

#-unix
(defun is-directory (path)
  "Treat directories as ordinary files on non-unix systems"
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Listen for input on the Motif server's connection
;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ff:defforeign 'listen-to-socket :entry-point (c-name "listen_to_socket")
   :arguments '(integer) :return-type :integer)

'(defun listen-for-input (connection)
  (let* ((status (listen-to-socket (toolkit-connection-stream connection))))
    ;; If we get an error here then we are toast because we end up
    ;; signalling an error in the scheduler.  It seems better just to
    ;; return NIL.
    #+ignore
    (when (minusp status)
      (clm-error "Read error on stream"))
    (plusp status)))

(defun listen-for-input (connection)
  #-(version>= 4 2)
  (fd-char-avail-p (toolkit-connection-stream connection))
  #+(version>= 4 2)
  (excl::filesys-character-available-p (toolkit-connection-stream connection)))

;; Return t if there is a character available for reading or on error,
;; otherwise return nil.
;; doesn't yet work in 4.2 !!
#-(version>= 4 2)
(defun fd-char-avail-p (fd)
  (multiple-value-bind (available-p errcode)
      (comp::.primcall-sargs 'sys::filesys #.excl::fs-char-avail fd)
    (excl:if* errcode
       then t
       else available-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Listen to the Motif server 
;;;;; Doesn't check for blocked callbacks
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wait-for-input-from-server (connection wait-function 
				   &optional (wait-string "CLM event wait"))
  (unless (funcall wait-function connection)
    (mp::mpwatchfor (toolkit-connection-stream connection))
    (unwind-protect
	(process-wait wait-string wait-function connection)
      (mp::mpunwatchfor (toolkit-connection-stream connection)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; C function to create a socket
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ff:defforeign 'connect-to-toolkit-server
  :entry-point (c-name "connect_to_toolkit_server")
  :arguments '(string integer)
  :return-type :fixnum)

(ff::defforeign 'connect-directly-to-toolkits 
  :entry-point (c-name "connect_directly_to_toolkits")
  :arguments '(string)
  :return-type :fixnum)

(ff::defforeign 'perror)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Create a bidirectional stream
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-motif-stream (connection host &aux fd)
  (declare (fixnum fd))
  (if host

      ;; try to connect  via socket to existing clmd on host
      (progn
	(setq fd (connect-to-toolkit-server host *xt-tcp-port*))
        (when (minusp fd)
	  (clm-error "Failed to connect to server: ~A~%" host)))

      ;; directly fork clm-server as child of Lisp process
      (let* ((binary-name (get-clm-binary-name)))
        (setq fd (connect-directly-to-toolkits binary-name))
	(when (minusp fd)
	  (perror "Failed to fork clm-server:")
	  (clm-error "Failed to fork clm-server from Lisp"))))

  (setf (toolkit-connection-stream connection) fd))
	
(defun get-clm-binary-name (&optional (binary *clm-binary-name*))
  "This looks in the path for a directory containing the server and the toolkit"
  (flet ((try-dir (dir)
           (let ((try-path (make-pathname :directory (namestring dir)
					  :name binary )))
	     (when (ignore-errors (probe-file try-path))
	       (return-from get-clm-binary-name (namestring try-path))))))
    (let ((dirs (append (if (listp *clm-binary-directory*)
			    *clm-binary-directory*
			  (list *clm-binary-directory*))
			(list excl::*library-code-pathname*
			      excl::*library-pathname*))))
      (mapc #'try-dir dirs)
      (clm-error "Could not find the binary ~S in any of ~{~A ~}"
	     binary dirs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Close the stream
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ff:defforeign 'close-xt-stream :entry-point (c-name "CloseStream")
   :arguments '(integer) :return-type :integer)

(defun close-motif-stream (connection)
  (setf (toolkit-connection-closed connection) t)
  (close-xt-stream (toolkit-connection-stream connection)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Read/Write a data object from/to the stream
;;;;; (LUCID-CL should use C functions for efficiency reasons)
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ff:defforeign 'send-header :entry-point (c-name "SendHeader")
   :arguments '(integer integer integer integer) :return-type :integer)

(ff:defforeign 'send-integer :entry-point (c-name "SendInteger")
   :arguments '(integer integer) :return-type :integer)

(ff:defforeign 'send-string :entry-point (c-name "SendString")
   :arguments '(integer string) :return-type :integer)

(ff:defforeign 'send-symbol 
    :entry-point (c-name (ecase excl:*current-case-mode*
			   ((:case-insensitive-upper)
			    "SendSymbol")
			   ((:case-insensitive-lower :case-sensitive-lower)
			    "SendSymbolL")))
    :arguments '(integer string) :return-type :integer)

;; function to lock out runstatus updating by GC
(ff::defforeign 'excl-clm-lock :entry-point (c-name "exclclmlock")
		:arguments '(integer))

(defun xt-send-command (stream code serial list-of-arguments)
  (declare (list list-of-arguments))
  (unwind-protect
      (progn
	(excl-clm-lock 1)
	(send-header stream code serial (length list-of-arguments))
	(dolist (value list-of-arguments)
	  (typecase value
	    (integer (send-integer stream value))
	    (string  (send-string stream value))
	    
	    (symbol  (send-symbol stream #+mac2 (copy-seq (symbol-name value))
	                                 #-mac2 (symbol-name value)))
	    (t       (clm-error "illegal argument type")))))
    (excl-clm-lock -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Send a command to the toolkit server
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ff:defforeign 'flush-buffer :entry-point (c-name "FlushBuffer")
   :arguments '(integer) :return-type :integer)

(defun toolkit-send-command (code serial list-of-args &aux rc)
  (declare (special *motif-connection*))
  ;;; scheduling must be inhibited because we have only one output buffer
  ;;; and because the C io functions must not be interrupted
  (mp:without-scheduling
   (xt-send-command (toolkit-connection-stream *motif-connection*) 
		    code serial list-of-args)
   (setq rc (flush-buffer (toolkit-connection-stream *motif-connection*))))
  ;;; Error handling while scheduling is allowed
  (if (= -1 rc)
      (clm-error "Sending to CLM socket failed~%")))

(ff:defforeign 'receive-integer :entry-point (c-name "ReceiveInteger")
   :arguments '(integer array) :return-type :integer)

(ff:defforeign 'receive-excl-string :entry-point (c-name "ReceiveEXCLString")
   :arguments '(integer integer integer array) :return-type :lisp)

(defvar *xtk-string-buffer*)
(defvar *xtk-buffer-index* (ff:register-value '*xtk-string-buffer*))

(defun receive-lisp-string (stream rc &aux length)
  (declare (special *xtk-string-buffer* *xtk-buffer-index*))
  (setq length (receive-integer stream rc))
  (setf *xtk-string-buffer* (make-string length))
  (receive-excl-string stream *xtk-buffer-index* length rc))

(defun xt-receive-command (stream &aux param serial sym code no-args
				       list-of-args rc)
  (setq list-of-args nil
	rc (make-array 1 :element-type 'fixnum :initial-element 0))
  (setq code (receive-integer stream rc)
	serial (receive-integer stream rc)
	no-args (receive-integer stream rc))
  (dotimes (dummy-index no-args)
    (case (receive-integer stream rc)
      (0 (push (receive-integer stream rc) list-of-args))
      (1 (push (receive-lisp-string stream rc) list-of-args))
      (2 
       (setf param (receive-lisp-string stream rc))
       (setf sym 
	     (cond
               ((string= param "T") t)
	       ((string= param "NIL") nil)
	       (t (intern (funcall 
			    (#+allegro-v3.1 progn
			     #+(or allegro-v4.0 
				   (and excl (version>= 4 1))) load-time-value
			     (if (eq excl:*current-case-mode* 
				     :case-insensitive-upper)
				 #'identity
			       (if (member excl:*current-case-mode*
					   '(:case-insensitive-lower
					     :case-sensitive-lower))
				   #'nstring-downcase
				 (clm-error "bad case"))))
			    param) 
			   'keyword))))
       (push sym list-of-args))
      (t (clm-error "illegal type~%"))))
  (when (= (aref rc 0) -1)
    (clm-error "Reading from CLM socket failed (core dump?)"))
  (values (cons code (nreverse list-of-args))
	  (list serial no-args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Receive a command from the toolkit server
;;;;; Do a passive wait until input is available
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun toolkit-receive-command (&aux r1 r2)
  (declare (special *motif-connection*))
  ;;; Suspend process until input is available
  (wait-for-input-from-server *motif-connection* #'listen-for-input 
			      "CLM server wait")
  ;;; Get a message from the motif server
  (mp:without-scheduling
   (multiple-value-setq (r1 r2)
     (xt-receive-command (toolkit-connection-stream *motif-connection*))))
  (values r1 r2))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Update the run status
;;;; Takes socket widget status
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(ff:defforeign 'set-run-status :entry-point (c-name "set_run_status")
 	       :arguments '(integer integer integer) :return-type :integer)
