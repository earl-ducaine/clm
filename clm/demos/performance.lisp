;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:(user (lisp)); Base:10 -*-

(in-package 'user)
(use-package 'xtk)

(defvar *sccsid* "@(#)performance.lisp	1.3 11/27/91")

'(proclaim '(optimize (compilation-speed 0) (safety 0) (speed 3)))

'(progn (trace xtk::listen-for-input)
  ;;(setf *scheduling-quantum* 50)
  (run-motif-application 'performance :use-clx t))

#+excl
'(progn
  (excl::function-call-clear)
  (function-call-run
   (run-motif-application 'performance :use-clx t)))

'(run-motif-application 'performance :use-clx t)
'(run-motif-application 'user::performance :display-host "aquire")
'(run-motif-application 'user::performance :server-host "aquire")

(defun performance ()
   (let* ((shell (create-application-shell))
	  (container    (create-row-column shell "container"))
	  (mb           (create-menu-bar container "MenuBar"))
	  (file-pane    (create-pulldown-menu mb "FileMenu"))
	  (file-cascade (create-cascade-button mb "File"
					       :label-string "File"
					       :sub-menu-id file-pane))
	  (new-button   (create-push-button file-pane "New"
					    :label-string "New"))
	  (big-column   (create-row-column container "Big"
					   :num-columns 33
					   :traversal-on nil
					   :packing :pack-column)))
     (declare (ignore file-cascade))
     (add-event-handler new-button '(:button-release-mask) 'notify-user "hello")
     (time
      (loop for i from 0 to 999 do
	(create-push-button-gadget big-column "name"
				   :label-string (format nil "~a" i))))
      (realize-widget shell)))

(defun notify-user (widget client-data &rest call-data)
  (declare (ignore widget call-data))
  (format t "~s~%" client-data))

'(run-motif-application 'user::performance)
