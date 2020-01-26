'(start)

(defun start ()
  (xtk:run-motif-application 'form-tester
			     )
  )

(defun form-tester ()
  (let* ((app-shell (xtk:create-application-shell
		     :allow-shell-resize t
		     ))
	 (rc1 (xtk:create-row-column app-shell "rc1"
				     :orientation :vertical
				     :packing :pack-tight
				     ))
	 (form1 (xtk:create-form rc1 "form1"
				 :horizontal-spacing 5
				 :vertical-spacing 5
				 ;;:allow-overlap nil
				 ))
	 (form2 (xtk:create-form rc1 "form2"
				 :horizontal-spacing 5
				 :vertical-spacing 5
				 ;;:allow-overlap nil
				 ))	 
	 (label1 (xtk:create-label form1 "label1"
				   :label-string "Question1?"
				   :left-attachment :form
				   ))
	 (button1 (xtk:create-push-button form1 "button1"
					  :right-attachment :form
					  :left-attachment :widget
					  :left-widget label1
					  ))
	 (label2 (xtk:create-label form2 "label2"
				   :label-string "Question2?"
				   :left-attachment :form
				   ))
	 (button2 (xtk:create-push-button form2 "button2"
					  :right-attachment :form
					  :left-attachment :widget
					  :left-widget label2
					  ))
	 )
    (xtk:realize-widget app-shell)
))    

