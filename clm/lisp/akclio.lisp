;;;; Foreign function interface definition for akcl
(in-package :xtk)

;;; SCCS %W %G

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This file was provided by Gerhard Koestler 
;;;; (koestler@informatik.tu-muenchen.de)
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defentry is-directory (string) (int "is_directory"))

(defentry listen-to-socket (int) (int "listen_to_socket"))


(defentry connect-to-toolkit-server (string int) 
  (int "connect_to_toolkit_server"))

(defentry connect-directly-to-toolkits (string) 
  (int "connect_directly_to_toolkits"))

(defentry perror (string) (int "perror"))

(defentry close-xt-stream (int) (int "CloseStream"))

(defentry send-header (int int int int) (int "SendHeader"))

(defentry send-integer (int int) (int "SendInteger"))

(defentry really-send-string (int string) (int "SendString"))

(defentry send-symbol (int string) (int "SendSymbol"))

(defentry flush-buffer (int) (int "FlushBuffer"))

(defentry connect-directly-to-toolkits (string) 
			(int connect_directly_to_toolkits))

(Clines

 "int rc_global;"

 "int ReceiveInteger_akcl (stream)"
 "int stream;"
 "{return (ReceiveInteger (stream, &rc_global));}"


 "char *ReceiveString_akcl (stream)"
 "int stream;"
 "{return (ReceiveString (stream, &rc_global));}"

 "int Receive_rc ()"
 "{return rc_global;}"
)

(defentry receive-rc () (int "Receive_rc"))

(defentry receive-integer-akcl (int) (int "ReceiveInteger_akcl"))
(defun receive-integer (stream rc)
  (declare (ignore rc))
  (values (receive-integer-akcl stream) (receive-rc)))


(defentry receive-string-akcl (int) (string "ReceiveString_akcl"))
(defun receive-string (stream rc)
  (declare (ignore rc))
  (values (receive-string-akcl stream) (receive-rc)))

