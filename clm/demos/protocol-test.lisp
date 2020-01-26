;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:(user (lisp xtk)); Base:10 -*-

(in-package 'user)
(use-package 'xtk)

(defvar *sccsid* "@(#)protocol-test.lisp	1.3 11/27/91")


'(progn (trace xtk::listen-for-input)
  ;;(setf *scheduling-quantum* 50)
  (run-motif-application 'protocol-test :use-clx t))

'(run-motif-application 'protocol-test :use-clx t)
'(run-motif-application 'protocol-test
  :timeout-detector #'(lambda () 1000)
  :timeout-handler 'notify-user :timeout-handler-args '(nil nil nil))

'(trace xtk::toolkit-send-command xtk::toolkit-receive-command
  xtk::receive-callback-or-event)

(defun protocol-test ()
 (let* ((shell (create-application-shell))
	(container (create-widget :row-column shell))
        (mb (create-widget :row-column container :row-column-type :menu-bar))
        (file-shell (create-popup-shell "file_menu" :menu-shell mb
					:width 5 :height 5))
	(file-pane (create-unmanaged-widget :row-column file-shell
					    :row-column-type :menu-pulldown))
        (file-cascade (create-widget :cascade-button mb
				     :label-string "File"
				     :sub-menu-id file-pane))
        (new-button (create-widget :push-button file-pane
				   :label-string "New"))
	(enlarge (create-widget :push-button file-pane
				   :label-string "Enlarge"))
	(smaller (create-widget :push-button file-pane
				   :label-string "Smaller"))
	(big-column (create-widget :row-column container :num-columns 30
				   :packing :pack-column))
	(draw (create-widget :drawing-area big-column :width 200 :height 100))
	(eb (create-widget :push-button big-column :label-string "Resize")))
   (add-event-handler new-button '(:button-release-mask) 'notify-user "hello")
   (add-callback eb :activate 'resize-area (list draw 200))
   (add-callback eb :activate 'resize-area (list draw 100))
   (add-callback smaller :activate 'fork-process (list draw 100))
   (add-callback enlarge :activate 'fork-process (list draw 50))
   (add-callback draw :resize 'notify-user nil)
   (realize-widget shell)))

(defun resize-area (widget client-data &rest call-data)
  (set-values (first client-data) :width (second client-data)
	      :height (second client-data))
  (get-values (first client-data) :width :height))

(defun notify-user (widget client-data &rest call-data)
  (format t "RESEIZED~%" client-data))

(defun fork-process (widget client-data &rest call-data)
  (format t "~a~%" client-data)
  (run-secondary-process "Protocol test" 'resize-process client-data))

(defun resize-process (args)
  (loop 
    (when (connection-closed)
      (return-from resize-process))
    (set-values (first args) :width (second args) :height (second args))
    (sleep 2)))

'(run-motif-application 'protocol-test)
