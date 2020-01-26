;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:xtk; Base:10 -*-

(in-package :xtk)

(setq *sccsid* "@(#)genera.lisp	1.13 9/16/92")

;; this file must be loaded before compiling low.lisp

(pushnew :clm-has-processes *features*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; The files that need to be compiled for CLM
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *clm-files*
  '(("pkg")
    ("defs")
    ("genera" "defs")
    ("low" "genera")
    ("files" "low")
    ("display" "low")
    ("convenience" "low")
    ("widgets" "low")
    ("text" "low")
    ("callbacks" "low")
    ("events" "low")
    ("transl" "low")
    ("dialogs" "low")
    ("cursor" "low")
    ("color" "low")
    ("listw" "low")
    ("timers" "low")
    ("graph" "low")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Load a file. Compile before loading if necessary !
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun source-file-name (file-name)
  (format nil "~a~a" file-name ".lisp"))

(defun binary-file-name (file-name)
  (format nil "~a~a" file-name ".bin"))
   
;; ld loads a binary file. The source is compiled if necessary
;; The source is also compiled if the depends-on file was modified after
;; source was compiled

(defun ld (source &optional (depends-on nil) &aux source-file binary-file)
  (setf source-file (source-file-name source))
  (setf binary-file (binary-file-name source))
  (if (not (probe-file binary-file))
      (compile-file source-file :output-file binary-file)
    (if (> (file-write-date source-file) (file-write-date binary-file))
	(compile-file source-file :output-file binary-file)
      (when depends-on
	(when (> (file-write-date (binary-file-name depends-on))
		 (file-write-date binary-file))
	  (compile-file source-file)))))
  (load binary-file))

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
  'scl:*current-process*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Define function to run a lisp process
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf (symbol-function 'process-run-function)
      (symbol-function 'scl:process-run-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Define the wait function
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf (symbol-function 'process-wait)
      (symbol-function 'scl:process-wait))

(setf (symbol-function 'process-wait-with-timeout)
      (symbol-function 'scl:process-wait-with-timeout))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Define function to create a process lock
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(zwei:defindentation (with-process-lock 1 1))

;; EXPORT (make-process-lock)
(defun make-process-lock (&aux gensy)
  (setq gensy (gensym))
  (set gensy nil)
  gensy)

;; EXPORT (with-process-lock)
(defmacro with-process-lock ((place) &body body)
  "execute body with exclusive access to place"
  `(let ((already-locked (eq (eval ,place) sys:current-process)))
     (unwind-protect
	 (progn
	   (when (not already-locked)
	     (scl:process-lock (zl:value-cell-location ,place)))
	   . ,body)
       (when (not already-locked)
	 (scl:process-unlock (zl:value-cell-location ,place))))))
	   
(defmacro without-process-lock ((lock) &body body)
  `(unwind-protect
       (progn 
	 (scl:process-unlock (zl:value-cell-location ,lock))
	 . ,body)
     ;; cleanup
     (scl:process-lock (zl:value-cell-location ,lock))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Kill a running process
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (process-kill)
(setf (symbol-function 'xtk::process-kill) 
      (symbol-function 'scl:process-kill))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Listen for input on the Motif server's connection
;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun listen-for-input (connection)
  (listen (toolkit-connection-stream connection)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Listen to the Motif server
;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wait-for-input-from-server (connection wait-function 
				   &optional (wait-string "CLM event wait"))
  (unless (funcall wait-function connection)
    (process-wait wait-string wait-function connection)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Create a bidirectional stream
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-motif-stream (connection host)
  (setf (toolkit-connection-stream connection)
	(tcp:open-tcp-stream host *xt-tcp-port* nil
			     :direction :io
			     :characters nil
			     :ascii-translation nil)))

'(tcp:open-tcp-stream "GINA" *xt-tcp-port* nil :direction :io
		      :characters nil :ascii-translation nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Close the stream
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun close-motif-stream (connection)
  (setf (toolkit-connection-closed connection) t)
  (close (toolkit-connection-stream connection)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Read/Write a data object from/to the stream
;;;;; (LUCID-CL should use C functions for efficiency reasons)
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype int29 () '(signed-byte 32))
(deftype int8  () '(signed-byte 8))

(defun write-int29 (stream item)
  (write-byte (ldb (byte 8 24) item) stream)
  (write-byte (ldb (byte 8 16) item) stream)
  (write-byte (ldb (byte 8 8) item) stream)
  (write-byte (ldb (byte 8 0) item) stream))

(defun card8->int8 (x)
  (the int8 (if (logbitp 7 x) (the int8 (- x #x100)) x)))

(defun read-int29 (stream)
  (let ((first-byte (read-byte stream nil :eof)))
    (when (eq first-byte :eof)
      (close-motif-stream *motif-connection*)
      (clm-error "Socket closed"))
    (logior (the int29 (ash (card8->int8 first-byte) 24))
            (the int29 (ash (read-byte stream) 16))
            (the int29 (ash (read-byte stream) 8))
            (the int29 (ash (read-byte stream) 0)))))

(defun write-param (stream item &aux len)
  (setq len (length item))
  (write-int29 stream len)
  (dotimes (i len)
	   (write-byte (char-code (schar item i)) stream)))

(defun read-param (stream &aux len str)
  (prog1
   (setq len (read-int29 stream)
	 str (make-string len))
   (dotimes (i len) (setf (schar str i) (code-char (read-byte stream))))))

(defun xt-send-command (stream code serial list-of-arguments)
  (write-int29 stream code)
  (write-int29 stream serial)
  (write-int29 stream (length list-of-arguments))
  (dolist (value list-of-arguments)
    ;(format t "sending ~a~%" value)
    (typecase value
       (integer
          (write-int29 stream 0)
          (write-int29 stream value))
       (string
          (write-int29 stream 1)
          (write-param stream value))
       (symbol
          (write-int29 stream 2)
          (write-param stream (symbol-name value)))
       (t
          (clm-error "illegal argument type")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Send a command to the toolkit server
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun toolkit-send-command (code serial list-of-args)
  (declare (special *motif-connection*))
  (xt-send-command (toolkit-connection-stream *motif-connection*) 
		   code serial list-of-args)
  (force-output (toolkit-connection-stream *motif-connection*)))


(defun xt-receive-command (stream &aux serial param sym code no-args
				       list-of-args)
  (setq code (read-int29 stream))
  (setq serial (read-int29 stream))
  (setq no-args (read-int29 stream))
  (setq list-of-args nil)
  (loop repeat no-args
          do (case (read-int29 stream)
		 (0
		  (setq list-of-args
			(append list-of-args (list (read-int29 stream)))))
		 (1
		  (setq list-of-args
			(append list-of-args (list (read-param stream)))))
		 (2
		  (setf param (read-param stream))
		  (setf sym (if (member param '("T" "NIL") :test #'equal) 
				(equal param "T")
				(intern param 'keyword)))
		  (setq list-of-args
			(append list-of-args (list sym))))))
  (values (cons code list-of-args)
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
