;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:xtk; Base:10 -*-

(in-package :xtk)

(setq *sccsid*"@(#)lucid.lisp	1.15 9/8/93")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; This code is executed whenever the compiled CLM code is loaded
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ensure that foreign object code is loaded exactly once
(unless (member :motif-server *features*)
  (load-foreign-files (list "unixsocket.o" "io.o")
		      #+pa '("-lc" "-lBSD")
		      #+mips '("-lc_G0"))
(pushnew :motif-server *features*))
(pushnew :clm-has-processes *features*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; The files that need to be compiled for CLM
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *clm-files*
  '(("pkg")
    ("defs")
    ("lucid" "defs")
    ("low" "lucid" "defs")
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

'(defun report-optimizations ()
  (declare (special *opt-file*))
  (setf *opt-file* (open "messages" :direction :output))
  (compiler-options :show-optimizations t :messages *opt-file*))
  
'(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

'(proclaim '(optimize (compilation-speed 3) (safety 3) (speed 2)))

(defun source-file-name (file-name)
  (format nil "~a~a" file-name ".lisp"))

(defun binary-file-name (file-name)
  (format nil "~a~a" file-name
	  #+(and lucid mc68000) ".lbin"
	  #+(and lucid pa) ".hbin"
	  #+(and lucid sparc) ".sbin"
	  #+(and lucid rios) ".rbin"
	  #+(and lucid mips) ".mbin"
          #+(and lucid ibm-rt-pc) ".bbin"
  ))
   
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

(defun save-lisp (name)
  (disksave name :verbose t :full-gc t :gc t))

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
  '*current-process*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Define function to run a lisp process
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-run-function (name function &rest args)
  (make-process :name name :function function :args args))

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
;; export symbol existing in LCL

(defmacro without-process-lock ((lock) &body body)
  `(unwind-protect
       (progn 
	 (process-unlock ,lock)
	 . ,body)
     ;; cleanup
     (process-lock ,lock)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Kill a running process
;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (process-kill)
(setf (symbol-function 'xtk::process-kill) 
      (symbol-function 'kill-process))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Test whether the given path is a directory
;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+unix
(def-foreign-function (is-directory (:name #-(or mips pa rios) "_is_directory"
					   #+(or mips pa rios) "is_directory")
				    (:return-type :fixnum))
  (path :simple-string))

#-unix
(defun is-directory (path)
  "Treat directories as ordinary files on non-unix systems"
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Listen for input on the Motif server's connection
;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-foreign-function (listen-to-socket (:name 
					 #-(or mips pa rios) "_listen_to_socket"
					 #+(or mips pa rios) "listen_to_socket")
					(:return-type :fixnum))
  (socket :fixnum))

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

(defun wait-for-input-from-server (connection wait-function 
				   &optional (wait-string "CLM event wait"))
  (unless (funcall wait-function connection)
    (lucid::waiting-for-input-from-stream 
     (toolkit-connection-lisp-stream connection)
     (lucid::with-io-unlocked
      (process-wait wait-string wait-function connection)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; C-function to create a socket
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-foreign-function (connect-to-toolkit-server
		       (:language :c)
		       (:name #-(or mips pa rios) "_connect_to_toolkit_server"
			      #+(or mips pa rios) "connect_to_toolkit_server") 
		       (:return-type :signed-32bit))
  (host :simple-string)
  (port :signed-32bit))

(def-foreign-function (connect-directly-to-toolkits 
		       (:name #-(or mips pa rios) 
			      "_connect_directly_to_toolkits"
			      #+(or mips pa rios) 
			      "connect_directly_to_toolkits" )
		       (:return-type :signed-32bit))
  (binary :simple-string))

(def-foreign-function (perror (:name #-(or mips pa rios) "_perror"
				     #+(or mips pa rios) "perror")
			      (:return-type :signed-32bit))
  (msg :simple-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Create a bidirectional stream
;;;;; Lucid uses a unix file descriptor for IO
;;;;; A lisp stream is required for the macro waiting-for-input-from-stream.
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

  (setf (toolkit-connection-stream connection) fd)
  (setf (toolkit-connection-lisp-stream connection)
    (user::make-lisp-stream
     :input-handle (toolkit-connection-stream connection)
     :output-handle (toolkit-connection-stream connection)
     :desired-buffer-size 0)))
	
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
      (clm-error "Could not find the binary ~S in any of ~{~A ~}"
	     binary dirs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Close the stream
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-foreign-function (close-xt-stream 
		       (:language :c)
		       (:name #-(or mips pa rios) "_CloseStream"
			      #+(or mips pa rios) "CloseStream") 
		       (:return-type :signed-32bit))
  (stream :signed-32bit))

(defun close-motif-stream (connection)
  (setf (toolkit-connection-closed connection) t)
  (close (toolkit-connection-lisp-stream connection))
  (close-xt-stream (toolkit-connection-stream connection)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Read/Write a data object from/to the stream
;;;;; (LUCID-CL uses C functions for efficiency reasons)
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-foreign-function (send-header(:language :c)
				  (:name #-(or mips pa rios) "_SendHeader"
					 #+(or mips pa rios) "SendHeader") 
				  (:return-type :signed-32bit))
  (stream :signed-32bit)
  (code   :signed-32bit)
  (serial :signed-32bit)
  (length :signed-32bit))

(def-foreign-function (send-integer (:name #-(or mips pa rios) "_SendInteger"
					   #+(or mips pa rios) "SendInteger")
				    (:return-type :fixnum))
  (stream :fixnum)
  (value :fixnum))

(def-foreign-function (send-string (:name #-(or mips pa rios) "_SendString"
					  #+(or mips pa rios) "SendString")
				   (:return-type :fixnum))
  (steam :fixnum)
  (value :simple-string))

(def-foreign-function (send-symbol (:name #-(or mips pa rios) "_SendSymbol"
					  #+(or mips pa rios) "SendSymbol")
				   (:return-type :fixnum))
  (stream :fixnum)
  (value :simple-string))

(defun xt-send-command (stream code serial list-of-arguments)
  (declare (list list-of-arguments))
  ;(format t "sending header ~a ~a ~%" code (length list-of-arguments))
  (send-header stream code serial (length list-of-arguments))
  (dolist (value list-of-arguments)
	  ;(format t "sending ~a~%" value)
	  (typecase value
		    (integer (send-integer stream value))
		    (string  (send-string stream value))
		    #+mac2
		    (symbol  
                      (send-symbol stream (copy-seq (symbol-name value))))
		    #-mac2
		    (symbol  (send-symbol stream (symbol-name value)))
		    (t       (clm-error "illegal argument type")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Send a command to the toolkit server
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-foreign-function (flush-buffer (:name #-(or mips pa rios) "_FlushBuffer"
					   #+(or mips pa rios) "FlushBuffer" )
				    (:return-type :fixnum))
  (socket :fixnum))

(defun toolkit-send-command (code serial list-of-args &aux rc)
  (declare (special *motif-connection*))
  ;;; scheduling must be inhibited because we have only one output buffer
  ;;; and C functions must not be interrupted during execution
  (lcl::with-scheduling-inhibited
   (xt-send-command (toolkit-connection-stream *motif-connection*) 
		    code serial list-of-args)
   (setq rc (flush-buffer (toolkit-connection-stream *motif-connection*))))
  (if (equal -1 rc)
      (clm-error "Sending on CLM socket failed~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Functions to read data from the Motif server
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-foreign-function (receive-integer 
		       (:language :c)
		       (:name #-(or mips pa rios) "_ReceiveInteger"
			      #+(or mips pa rios) "ReceiveInteger") 
		       (:return-type :signed-32bit))
  (stream :signed-32bit)
  (rc (:pointer :signed-32bit)))

(def-foreign-function (receive-string 
		       (:language :c)
		       (:name #-(or mips pa rios) "_ReceiveString"
			      #+(or mips pa rios) "ReceiveString") 
		       (:return-type (:pointer :character)))
  (stream :signed-32bit)
  (rc (:pointer :signed-32bit)))

(defun xt-receive-command (stream &aux serial param sym code no-args
				       list-of-args rc new-string)
  
  (setq list-of-args nil
	rc (malloc-foreign-pointer :type '(:pointer :signed-32bit)))
  (setf (foreign-value rc) 0)
  (setq code (receive-integer stream rc)
	serial (receive-integer stream rc)
	no-args (receive-integer stream rc))
  
  (dotimes (dummy-index no-args)
	   (case (receive-integer stream rc)
		 (0 (push (receive-integer stream rc) list-of-args))
		 (1 (setq new-string (receive-string stream rc))
		    (when (= (foreign-value rc) -1)
		      (clm-error "Reading from CLM socket failed (core dump?)"))
		    (push (foreign-string-value new-string) list-of-args))
		 (2 (setq new-string (receive-string stream rc))
		    (when (= (foreign-value rc) -1)
		      (clm-error "Reading from CLM socket failed (core dump?)"))
		    (setf param (foreign-string-value new-string))
		    (setf sym 
		          (if (string-equal param "T")
			      t
			      (if (string-equal param "NIL")
				  nil
				  (intern param 'keyword))))
		    (push sym list-of-args))
		 (t (clm-error "illegal type~%"))))
  (free-foreign-pointer rc)
  (values (cons code (nreverse list-of-args))
	  (list serial no-args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Receive a command from the toolkit server
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun toolkit-receive-command (&aux r1 r2)
  (declare (special *motif-connection*))
  (wait-for-input-from-server *motif-connection* #'listen-for-input 
			      "CLM server wait")
  (lcl::with-scheduling-inhibited
   (multiple-value-setq (r1 r2)
     (xt-receive-command (toolkit-connection-stream *motif-connection*))))
  (values r1 r2))
