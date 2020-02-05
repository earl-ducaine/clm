

(in-package :xtk)


;; Ensure that foreign object code is loaded exactly once
(eval-when (:execute :load-toplevel :compile-toplevel)
  (cffi:define-foreign-library lib-clm-client
    (:unix
     "/home/rett/dev/common-lisp/cl-motif/cl-motif.git/clm/server/libclm_client.so"))
  (cffi:use-foreign-library lib-clm-client))

;; Test whether the given path is a directory
(cffi:defcfun is-directory :int
  (path :string))

;; Listen for input on the Motif server's connection
(cffi:defcfun listen-to-socket :int
  (socket :int))

;; C function to create a socket
(cffi:defcfun connect-to-toolkit-server :int
  (host :string)
  (port :int))

(cffi:defcfun connect-directly-to-toolkits :int
  (binary :string))

(cffi:defcfun perror :int
  (msg :string))

;;; Close the stream
(cffi:defcfun ("CloseStream" close-xt-stream) :int
  (stream :int))

;;; Read/Write a data object from/to the stream


(cffi:defcfun ("SendHeader" send-header) :int
  (stream :int)
  (code :int)
  (serial :int)
  (length :int))

(cffi:defcfun ("SendInteger" send-integer) :int
  (stream :int)
  (value :int))

(cffi:defcfun ("SendString" send-string) :int
  (steam :int)
  (value :string))

(cffi:defcfun ("SendSymbol" send-symbol) :int
  (stream :int)
  (value :string))

(cffi:defcfun ("FlushBuffer" flush-buffer) :int
  (socket :int))

(cffi:defcfun ("ReceiveInteger" receive-integer) :int
  (stream :int)
  (rc :pointer :int))

(defun receive-lisp-string (stream)
  (cffi:with-foreign-object (rc :int 1)
    (let* ((length (receive-integer stream rc))
	   (string (make-string length)))
      (multiple-value-bind
	    (okay errno)
	  (sb-sys:without-gcing
	    (sb-posix:read stream (sb-sys:vector-sap string) length))
	(unless okay
	  (clm-error "receive-lisp-string failed: ~A"
		     (sb-int:strerror errno)))
	string))))

(defun xt-receive-command (stream)
  (cffi:with-foreign-object (rc :int 1)
    (let ((list-of-args nil)
	  (code (receive-integer stream rc))
	  (serial (receive-integer stream rc))
	  (num-args (receive-integer stream rc)))
      (log-format
       (str "xt-receive-command: receiving command -- code ~s, serial ~s, num-args ~s~%")
       code serial num-args)
      (dotimes (i num-args)
	(push (case (receive-integer stream rc)
		(0 (receive-integer stream rc))
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
	      (list serial num-args)))))

;;; Following used for troubleshooting, but ordinarily the lisp client
;;; doesn't need to use these directly.

(cffi:defcfun integer-to-bytes :void
  (integer :int)
  (bytes :pointer :unsigned-char))

(cffi:defcfun bytes-to-integer :int
  (bytes :pointer :unsigned-char))

(cffi:defcfun ("float_to_bytes" float-to-bytes) :void
  (number :float)
  (bytes :pointer :unsigned-char))

(cffi:defcfun ("bytes_to_float" bytes-to-float) :float
  (bytes :pointer :unsigned-char))

;;; Limits, again for testing, though could be useful for error
;;; checking too, especially if multiple cross-platform calls are ever
;;; supported again.

(cffi:defcfun max-int :int)
(cffi:defcfun min-int :int)
(cffi:defcfun max-unsigned-int :int)
(cffi:defcfun max-long :long)
(cffi:defcfun min-long :long)
(cffi:defcfun max-unsigned-long :long)
(cffi:defcfun max-float :float)
(cffi:defcfun max-double :double)

;; Long double not supported on sbcl.
;; (cffi:defcfun max-long-double :long-double)
