;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:xtk; Base:10 -*-

(in-package :xtk)

(setq *sccsid* "@(#)lispworks.lisp	1.2 9/8/93")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; This code is executed whenever the compiled CLM code is loaded
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ensure that foreign object code is loaded exactly once
(unless (member :motif-server *features*)
  (foreign::read-foreign-modules "unixsocket.o" "io.o")
  (pushnew :motif-server *features*))
(pushnew :clm-has-processes *features*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; This variable sets the default priority for background-processes
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *default-process-priority* 20000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; The files that need to be compiled for CLM
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *clm-files*
  '(("pkg")
    ("defs")
    ("lispworks" "defs")
    ("low" "lispworks" "defs")
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
  (format nil "~a~a" file-name ".wfasl"))
   
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
  (system:save-image name))

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
  'mp:*current-process*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Define function to run a lisp process
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-run-function (name function &rest args)
  (declare (special *default-process-priority*))
  (apply 'mp:process-run-function name
	 `((:priority ,*default-process-priority*) ,function ,@args)))

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

(defun make-process-lock ()
  (mp:make-lock :name (gensym)))

(defmacro with-process-lock ((lock) &body body)
  `(mp:with-lock ,(list lock) ,@body))

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

;; EXPORT (process-kill)
(setf (symbol-function 'xtk::process-kill) 
      (symbol-function 'mp:process-kill))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Test whether the given path is a directory
;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+unix
(foreign::define-foreign-function (is-directory "_is_directory")
				  ((path :simple-string))
				  :result-type :fixnum)	
#-unix
(defun is-directory (path)
  "Treat directories as ordinary files on non-unix systems"
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Listen for input on the Motif server's connection
;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(foreign::define-foreign-function (listen-to-socket "_listen_to_socket")
				  ((socket :fixnum))
				  :result-type :fixnum)

(defun listen-for-input (connection)
  (let* ((status (listen-to-socket (toolkit-connection-stream connection))))
    (not (zerop status))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Listen to the Motif server. Doesn't look for blocked callbacks
;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wait-for-input-from-server (connection wait-function 
				   &optional (wait-string "CLM event wait"))
  (unless (funcall wait-function connection)
    (mp::notice-fd (toolkit-connection-stream connection))
    (unwind-protect
	(process-wait wait-string wait-function connection)
      (mp::unnotice-fd (toolkit-connection-stream connection)))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; C-function to create a socket
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(foreign::define-foreign-function (connect-to-toolkit-server
				   "_connect_to_toolkit_server")
				  ((host :simple-string)
				   (port :integer))
				  :result-type :integer :language :c)

(foreign::define-foreign-function (connect-directly-to-toolkits
				   "_connect_directly_to_toolkits" )
				  ((binary :simple-string))
				  :result-type :integer)

(foreign::define-foreign-function (perror "_perror")
				  ((msg :simple-string))       
				  :result-type :integer)


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

  (setf (toolkit-connection-stream connection) fd))
        
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

(foreign::define-foreign-function (close-xt-stream "_CloseStream")
				  ((stream :integer))
				  :result-type :integer :language :c)


(defun close-motif-stream (connection)
  (setf (toolkit-connection-closed connection) t)
  (close-xt-stream (toolkit-connection-stream connection)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Read/Write a data object from/to the stream
;;;;; (LUCID-CL uses C functions for efficiency reasons)
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(foreign::define-foreign-function (send-header "_SendHeader")
				  ((stream :integer)
				   (code   :integer)
				   (serial :integer)
				   (length :integer))
				  :result-type :integer :language :c)

(foreign::define-foreign-function (send-integer "_SendInteger")
				  ((stream :fixnum)
				   (value :fixnum))
				  :result-type :fixnum)

(foreign::define-foreign-function (send-string "_SendString")
				  ((stream :fixnum)
				   (value :simple-string))
				  :result-type :fixnum)

(foreign::define-foreign-function (send-symbol "_SendSymbol")
				  ((stream :fixnum)
				   (value :simple-string))
				  :result-type :fixnum)

(defun xt-send-command (stream code serial list-of-arguments)
  (declare (list list-of-arguments))
; (format t "sending header ~a ~a ~%" code (length list-of-arguments))
  (send-header stream code serial (length list-of-arguments))
  (dolist (value list-of-arguments)
;         (format t "sending ~a~%" value)
          (typecase value
                    (integer (send-integer stream value))
                    (string (send-string stream value))
                    #+mac2
                    (symbol  
                      (send-symbol stream (copy-seq (symbol-name value))))
                    #-mac2
                    (symbol (send-symbol stream (symbol-name value)))
                    (t       (clm-error "illegal argument type")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Send a command to the toolkit server
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(foreign::define-foreign-function (flush-buffer "_FlushBuffer") 
				  ((socket :fixnum))
				  :result-type :fixnum)

(defun toolkit-send-command (code serial list-of-args &aux rc)
  (declare (special *motif-connection*))
  ;;; scheduling must be inhibited because we have only one output buffer
  ;;; and C functions must not be interrupted during execution
  (mp:without-interrupts
   (xt-send-command (toolkit-connection-stream *motif-connection*) 
                    code serial list-of-args)
   (setq rc (flush-buffer (toolkit-connection-stream *motif-connection*))))
  (if (equal -1 rc)
      (clm-error "Sending to CLM socket failed~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Functions to read data from the Motif server
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(foreign::define-foreign-function (receive-integer "_ReceiveInteger")
				  ((stream :integer)
				   (rc :integer :reference))
				  :result-type :integer :language :c)
 
(foreign::define-foreign-function (receive-string "_ReceiveString")
				  ((stream :integer)
				   (rc :integer :reference))
				  :result-type :string
				  :language :c)

(defun xt-receive-command (stream &aux serial param sym code no-args
				       list-of-args rc new-string)
  (setq list-of-args nil)
  (setf rc 0)
  (setq code (receive-integer stream rc)
	serial (receive-integer stream rc)
	no-args (receive-integer stream rc))
  (dotimes (dummy-index no-args)
	   (case (receive-integer stream rc)
		 (0 (push (receive-integer stream rc) list-of-args))
		 (1 (setq new-string (receive-string stream rc))
		    (when (= rc -1)
		      (clm-error "Reading from CLM socket failed (core dump?)"))
		    (push new-string list-of-args))
		 (2 (setq new-string (receive-string stream rc))
		    (when (= rc -1)
		      (clm-error "Reading from CLM socket failed (core dump?)"))
		    (setf param new-string)
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
  (wait-for-input-from-server *motif-connection* #'listen-for-input 
                              "CLM server wait")
  (mp:without-interrupts
   (multiple-value-setq (r1 r2)
			(xt-receive-command
			 (toolkit-connection-stream *motif-connection*))))
   (values r1 r2))
