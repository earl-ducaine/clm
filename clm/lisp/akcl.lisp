;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:xtk; Base:10 -*-

(in-package :xtk)
(eval-when (compile load eval)
  (shadow 'loop))


(setq *sccsid* "@(#)akcl.lisp	1.2 9/8/93")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This file was provided by Gerhard Koestler 
;;;; (koestler@informatik.tu-muenchen.de)
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; get-clm-object-name searches the directories listed in
;;;; *clm-library-directory* for the file objectname
;;;; *clm-library-directory* defined in defs.lisp
;;;; necessary because akcl doesn't offer a search mechanism for
;;;; loading files
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-clm-object-name (objectname)
  "This looks in the path for a directory containing the object file"
  (flet ((try-dir (dir)
           (let ((try-path (make-pathname :directory 
					  (list (namestring dir))
					  :name objectname)))
	     (when (probe-file try-path)
	       (return-from get-clm-object-name (namestring try-path))))))
    (let ((dirs (if (listp *clm-library-directory*)
		    *clm-library-directory*
	   	    (list *clm-library-directory*))))
      (mapc #'try-dir dirs)
      (clm-error "Could not find the object file ~S in any of ~{~A ~}"
	     objectname dirs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; This code is executed whenever the compiled CLM code is loaded
;;;;; loading akclio.o and linking unixsocket.o, io.o, libc
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensure that foreign object code is loaded exactly once
(unless (member :motif-server *features*)
  (si:faslink 
	(get-clm-object-name "akclio.o") 
	(si::string-concatenate
		(get-clm-object-name "unixsocket.o")
		" "
		(get-clm-object-name "io.o")
		" -lc"))
  (push :motif-server *features*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; The files that need to be compiled for CLM
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *clm-files*
  '(("pkg")
    ("defs")
    ("akcl" "defs")
    ("low" "akcl" "defs")
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
  (format nil "~a.lisp" file-name))

(defun binary-file-name (file-name)
  (format nil "~a.o" file-name))
   
;; ld loads a binary file. The source is compiled if necessary
;; The source is also compiled if the depends-on file was modified after
;; source was compiled

(defun ld (source &rest files-dependent-on &aux source-file binary-file)
  (setf source-file (source-file-name source))
  (setf binary-file (binary-file-name source))
  (when (or (not (probe-file binary-file))
            (> (file-write-date source-file) (file-write-date binary-file))
            (dolist (file files-dependent-on)
	          (when (> (file-write-date (source-file-name file))
			  (file-write-date binary-file))
	             (return t))))
      (compile-file source-file :output-file binary-file :c-file t))
  (load binary-file))

(defun save-lisp (name)
  (save name))

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
;;;;; Get the process that is currently running
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro current-process ()
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Define function to run a lisp process
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-run-function (name function &rest args)
  (declare (ignore name))
  (warn "process-run-function not implemented")
  (apply function args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Define the wait function
;;;;; (For Lucid, these functions are already defined)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Define function to create a process lock
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (make-process-lock)
(defun make-process-lock () nil)


;; EXPORT (with-process-lock)
(defmacro with-process-lock (lock &body body)
  (declare (ignore lock))
  `(progn ,@body))

(defmacro without-process-lock ((lock) &body body)
  (declare (ignore lock))
  `(progn ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Kill a running process
;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (process-kill)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Listen for input on the Motif server's connection
;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun listen-for-input (connection)
  (let* ((status (listen-to-socket (toolkit-connection-stream connection))))
    (when (minusp status)
      (clm-error "Read error on stream"))
    (plusp status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Listen to the Motif server. Doesn't look for blocked callbacks
;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wait-for-input-from-server (connection wait-function)
  (do () ((funcall wait-function connection))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Create a bidirectional stream
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun open-motif-stream (connection host &aux fd)
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
           (let ((try-path (make-pathname :directory 
					  (list (namestring dir))
					  :name binary )))
	     (when (probe-file try-path)
	       (return-from get-clm-binary-name (namestring try-path))))))
    (let ((dirs (if (listp *clm-binary-directory*)
		    *clm-binary-directory*
	   	    (list *clm-binary-directory*))))
      (mapc #'try-dir dirs)
      (clm-error "Could not find the binary ~S in any of ~{~A ~}"
	     binary dirs))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Close the stream
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun close-motif-stream (connection)
  (setf (toolkit-connection-closed connection) t)
  (close-xt-stream (toolkit-connection-stream connection)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Read/Write a data object from/to the stream
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun send-string (stream value)
  (really-send-string stream 
		      (concatenate 'simple-string value 
				   (string (code-char 0)))))



(defun xt-send-command (stream code serial list-of-arguments)
  (declare (list list-of-arguments))
  ;(format t "sending header ~a ~a ~%" code (length list-of-arguments))
  (send-header stream code serial (length list-of-arguments))
  (dolist (value list-of-arguments)
	  (typecase value
		    (integer (send-integer stream value))
		    (string (send-string stream value))
		    (symbol  (send-symbol stream (symbol-name value)))
		    (t       (clm-error "illegal argument type")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Send a command to the toolkit server
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun toolkit-send-command (code serial list-of-args &aux rc)
  (declare (special *motif-connection*))
  ;;; scheduling must be inhibited because we have only one output buffer
  ;;; and C functions must not be interrupted during execution
  (xt-send-command (toolkit-connection-stream *motif-connection*) 
		    code serial list-of-args)
  (setq rc (flush-buffer (toolkit-connection-stream *motif-connection*)))
  (if (equal -1 rc)
      (clm-error "Sending on CLM socket failed~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Functions to read data from the Motif server
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun xt-receive-command (stream &aux serial param sym code no-args
				       list-of-args (rc 0))
  
  (setq list-of-args nil)
  (setq code (receive-integer stream rc)
	serial (receive-integer stream rc)
	no-args (receive-integer stream rc))
  
  (dotimes (dummy-index no-args)
	   (case (receive-integer stream rc)
		 (0 (push (receive-integer stream rc) list-of-args))
		 (1 (multiple-value-bind (new-string rc)
					 (receive-string stream rc)
		      (when (= rc -1)
		        (clm-error 
			 "Reading from CLM socket failed (core dump?)"))
		      (push new-string list-of-args)))
		 (2 (multiple-value-bind (new-string rc)
                                         (receive-string stream rc)
		      (when (= rc -1)
		        (clm-error 
			 "Reading from CLM socket failed (core dump?)"))
		      (setf param new-string))
		    (setf sym 
		          (if (string-equal param "T")
			      t
			      (if (string-equal param "NIL")
				  nil
				  (intern param 'keyword))))
		    (push sym list-of-args))
		 (t (clm-error "illegal type~%"))))
  (values (cons code (nreverse list-of-args))
	  (list serial no-args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Receive a command from the toolkit server
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun toolkit-receive-command (&aux r1 r2)
  (declare (special *motif-connection*))
  (wait-for-input-from-server *motif-connection* #'listen-for-input)
  (multiple-value-setq (r1 r2)
    (xt-receive-command (toolkit-connection-stream *motif-connection*)))
  (values r1 r2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; A dummy loop macro. Uncomment this if you have the real thing.
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro loop (while-or-until condition do-symbol &rest forms)
  (declare (ignore do-symbol))
  (append (list 'do nil (if (string-equal (symbol-name while-or-until) "WHILE")
	                  (list (list 'not condition))
	                  (list condition)))
	  forms))


(eval-when (load compile eval)
(defvar *error-handler-function* 'si::universal-error-handler)
(or (get   'si::universal-error-handler :old-definition)
   (setf (get 'si::universal-error-handler :old-definition)
	 (symbol-function 'si::universal-error-handler)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; A very incomplete implementation of handler-bind
;;;;; based on the universal error handler of akcl,
;;;;; inspired by the serror package by William Schelter,
;;;;; University of Texas
;;;;; if you got a condition package in akcl, use it!
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *catch-error* nil)

(defmacro handler-bind (((typespec handler)) &rest body)
  (declare (ignore typespec))
  (let ((bod `(
	       (let ((*catch-error* t))
		 (catch ':any-error
		   (return-from cond-error-continue (progn ,@body)))))))
    (setf bod ` (funcall ,handler ,@bod))
    `(block cond-error-continue ,bod)))

(defun si::universal-error-handler (&rest error-handler-args)
  (cond (*catch-error* (throw :any-error 
			 (fifth error-handler-args)))
	(t 
	 (apply (get 'si::universal-error-handler :old-definition)
		error-handler-args))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; with-simple-restart is implemented by a simple progn
;;;;; if you got a condition package in akcl, use it!
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-simple-restart ((restart-name format-string) &body forms)
  (declare (ignore restart-name)
	   (ignore format-string))
  `(progn ,@forms))

