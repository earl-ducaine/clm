;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:(user (lisp)); Base:10 -*-

(in-package 'user)
(use-package 'xtk)

(defvar *sccsid* "@(#)menu-test.lisp	1.3 11/27/91")


'(progn (trace xtk::listen-for-input)
  ;;(setf *scheduling-quantum* 50)
  (run-motif-application 'menu-test :use-clx t))

'(run-motif-application 'menu-test :extra-process nil)
'(run-motif-application 'menu-test :use-clx t)
'(run-motif-application 'menu-test)

(defun menu-test ()
 (let* ((shell (create-application-shell))
        (mb (create-widget :row-column shell :row-column-type :menu-bar))
        (file-shell (create-popup-shell "file_menu" :menu-shell mb
					:width 5 :height 5))
	(file-pane (create-unmanaged-widget :row-column file-shell
					    :row-column-type :menu-pulldown))
        (file-cascade (create-widget :cascade-button mb
				     :label-string "File"
				     :sub-menu-id file-pane))
        (new-button (create-widget :push-button file-pane
				   :label-string "New"))
        (fork-button (create-widget :push-button file-pane
				   :label-string "Fork")))
   (add-event-handler new-button '(:button-release-mask) 'notify-user "hello")
   (add-callback new-button :activate 'notify-user "hello")
   (add-callback new-button :activate 'notify-user "hello")
   (add-callback fork-button :activate 'fork-process new-button)
   (realize-widget shell)))

(defun notify-user (widget client-data &rest call-data)
  (format t "~s~%" client-data))

(defun fork-process (widget client-data &rest call-data)
  (declare (special *motif-connection*))
  (xtk::process-run-function "Protocol test" 'test-process *motif-connection*
			     client-data))

(defun test-process (connection button)
  (let ((*motif-connection* connection))
    (declare (special *motif-connection*))
    (loop 
      (if (xtk::connection-closed *motif-connection*)
	  (return-from test-process)
	(progn
	  (format t "~a~%" (first (get-values button :label-string)))
	  (sleep 2))))))

'(run-motif-application 'menu-test)
