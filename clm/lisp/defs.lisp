;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:xtk; Base:10 -*-

(in-package :xtk)

(defvar *sccsid*)
(setq *sccsid* "@(#)defs.lisp	1.12 9/8/93")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; I M P O R T A N T
;; Definitions of parameters that may have to be adapted for your site.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this may also be a list of directories that are searched
(defparameter *clm-binary-directory*
     "/home/rett/dev/common-lisp/cl-motif/cl-motif.git/clm/server/")

;; only used in AKCL, this may be a list of directories to find object files
(defparameter *clm-library-directory*
    '("." "/pubbay/cl/clm/sun4/lib"))

(defparameter *xt-tcp-port* 7000)
(defparameter *clm-binary-name* "clm-server")

;; uncomment this with #+ignore if you do not want to use CLM with CLX
(eval-when (compile load eval)
  (pushnew :clm-needs-clx *features*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Definitions of Variables

;; EXPORT ((*motif-connection* *x-display*))
(defvar *motif-connection*)
(defvar *x-display*)

;; EXPORT ((*default-server-host*
;;	  *default-display-host*
;;	  *default-display-number*
;;	  *default-screen-number*
;;	  *debug-mode*))

(defvar *default-server-host*)
(setf *default-server-host* nil)
(defvar *default-display-host*)
(setf *default-display-host* "localhost")
(defvar *default-display-number*)
(setf *default-display-number* 0)
(defvar *default-screen-number*)
(setq *default-screen-number* 0)
(defvar *debug-mode*)
(setf *debug-mode* nil)

'(trace toolkit-send-command toolkit-receive-command)
'(trace xt-send-command xt-receive-command)
'(untrace listen-for-input input-available)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; This struct contains data which describes an application instance
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct toolkit-connection
  (stream nil)
  (motif-version nil)
  #+lucid (lisp-stream nil)
  (closed nil)
  (lock (xtk::make-process-lock))
  (callback-table (make-hash-table :test #'equal) :type hash-table)
  (event-table (make-hash-table :test #'equal) :type hash-table)
  (timer-table (make-hash-table :test #'equal) :type hash-table)
  (dispatcher-terminated nil)
  (widget-id 0)
  (serial 0)
  (blocked-callback nil)
  (sync-clx nil)
  (event-handler 'default-event-handler)
  (event-handler-args nil)
  (error-handler 'default-error-handler)
  (just-print-errors t)
  (after-function nil)
  (after-arguments nil)
  (binding-wrapper nil)
  (binding-arguments nil)
  ;; To handle other input sources. May only be set directly.
  (other-input-detector nil)
  (other-input-handler nil)
  (last-tick 0)
  (running nil))

;; EXPORT (toolkit-connection-stream)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Test/Set for the 'running slot of the connection
;;;;; (undocumented function)
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (is-running)
(defun is-running (motif-connection)
  (toolkit-connection-running motif-connection))

(defun set-running (new-value)
  (declare (special *motif-connection*))
  (setf (toolkit-connection-running *motif-connection*) new-value))

(defsetf is-running set-running)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Test if the connection is closed
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (connection-closed)
(defun connection-closed (&optional (*motif-connection* *motif-connection*))
  (declare (special *motif-connection*))
  (toolkit-connection-closed *motif-connection*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Generate a new widget-id
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gen-widget-id ()
  (declare (special *motif-connection*))
  (incf (toolkit-connection-widget-id *motif-connection*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Generate the next serial number for a request
;;;;; This function must only be called with exclusive access to the
;;;;; connection (e.g. with scheduling inhibited)
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun next-serial ()
  (declare (special *motif-connection*))
  (incf (toolkit-connection-serial *motif-connection*)))

(defun str (&rest rest)
  (apply #'concatenate 'string rest))

;; (defmacro str (&rest rest)
;;   `(apply #'concatenate 'string ,rest))
