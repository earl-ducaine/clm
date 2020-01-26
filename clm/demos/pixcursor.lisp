(in-package 'xtk)

(defun createpixcursor (widget client-data &rest call-data)
  (setf cursor-id (car (create-pixmap-cursor "/home/thomas/bitmaps/cursorsource"
				       "/home/thomas/bitmaps/cursormask"
				       "red" "white")))
  (new-define-cursor cursor-id  client-data))

(defun createfontcursor (widget client-data &rest call-data)
  (setf cursor-id (car (create-font-cursor :watch)))
  (new-define-cursor cursor-id  client-data))

(defun createnonefontcursor (widget client-data &rest call-data)
  (new-define-cursor -1 client-data))
  
(setf *debug-mode* t)

(defun init-test-clm ()
  (setf shell (create-application-shell
	       :geometry "520x220"
	       )
	rc (create-widget :row-column shell)
	pb (create-widget :push-button rc
			  :label-string "Pixmap Cursor"
			  )
	pb1 (create-widget :push-button rc
			  :label-string "Font Cursor"
			  )
	pb2 (create-widget :push-button rc
			  :label-string "Font Cursor"
			  )

	)

  (add-callback pb ':activate #'createpixcursor rc)
  (add-callback pb1 ':activate #'createfontcursor rc)
  (add-callback pb2 ':activate #'createnonefontcursor rc)
  (realize-widget shell))

(run-motif-application 'init-test-clm :use-clx nil :extra-process nil)
