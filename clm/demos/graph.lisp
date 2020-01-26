;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:(user (lisp)); Base:10 -*-

(in-package 'user)
(use-package 'xtk)

'(run-motif-application 'init-test-clm :use-clx nil :application-class "Graph")

(setq flag 0)
(setq zaehler 0)

(setf *debug-mode* t)

(defun markiere (widget client-data &rest call-data)
  (cond ((= 0 flag)
	 (setq flag 1)
	 (setq ancestor widget)
	 (set-values widget :sensitive nil))
	((= 1 flag)
	 (setq flag 2)
	 (setq decedent widget)
	 (set-values widget :sensitive nil))
	(t
	 (set-values ancestor :sensitive t)
	 (set-values decedent :sensitive t)
	 (setq ancestor widget)
	 (set-values widget :sensitive nil)
	 (setq flag 1))))

(defun add-rel1 (widget client-data &rest call-data)
  (cond ((= flag 2)
	 (add-graph-relations 'solid-line ancestor decedent)
	 (set-values ancestor :sensitive t)
	 (set-values decedent :sensitive t)
	 (do-layout client-data))))

(defun remove-rel (widget client-data &rest call-data)
   (cond ((= flag 2)
	 (remove-graph-relations ancestor decedent)
	 (set-values ancestor :sensitive t)
	 (set-values decedent :sensitive t)
	 (do-layout client-data))))

(defun add-rel2 (widget client-data &rest call-data)
   (cond ((= flag 2)
	 (add-graph-relations 'dashed-line ancestor decedent)
	 (set-values ancestor :sensitive t)
	 (set-values decedent :sensitive t)
	 (do-layout client-data))))

(defun dynamic (widget client-data &rest call-data)
  (let ((new (create-named-widget :push-button client-data
				  (format nil "PB ~D" zaehler) t
				  `(:label-string ,(format nil "PB ~D" zaehler)
						  :background "white"))))
    (setq zaehler (+ zaehler 1))
    (add-callback new :activate #'markiere nil)))

					   
(defun expand (widget client-data &rest call-data)
  (if (car call-data)
      (set-values ancestor :node-type 'normal-node)
    (set-values ancestor :node-type 'invisible-node)))

(defun focus (widget client-data &rest call-data)
  (set-values client-data :focus-node ancestor :focus (car call-data)))

(defun global (widget client-data &rest call-data)
  (set-values client-data :show-global (car call-data)))

(defun edge-func (widget client-data &rest call-data)
  (if (car call-data)
      (set-values client-data :edge-mode 'direct)
    (set-values client-data :edge-mode 'orthogonal)))

(defun show-info (widget client-data &rest call-data)
  (set-values client-data :state-information (car call-data)))

(defun update (widget client-data &rest call-data)
  (do-layout client-data))
  
(defun init-test-clm ()
  (setf shell (create-application-shell )
	form(create-widget :form shell
			    :horizontal-spacing 10
			    :vertical-spacing 10
			    :background "LightGrey")
	rc(create-widget :row-column form
			  :top-attachment 'form
			  :left-attachment 'form
			  :orientation 'vertical)
	func0(create-widget :push-button rc
			    :label-string "new-node")
	func1(create-widget :push-button rc
			    :label-string "remove-relation")
	func2(create-widget :push-button rc
			    :label-string "add-solid-relation")
	func3(create-widget :push-button rc
			    :label-string "add-dashed-relation")
	func4(create-widget :toggle-button rc
		    :label-string "(un-)expand")
	func5(create-widget :toggle-button rc
			    :label-string "(un-)focus")
	func6(create-widget :toggle-button rc
			    :label-string "global view")
	func7(create-widget :toggle-button rc
			    :label-string "state-information")
	func8(create-widget :push-button rc
			    :label-string "update")
	edgemode(create-widget :toggle-button rc
			    :label-string "edge mode")
	sw (create-widget :scrolled-window form
			  :top-attachment 'form
			  :left-attachment 'widget
			  :left-widget rc
			  :right-attachment 'form
			  :bottom-attachment 'form
			  :scroll-bar-display-policy 'static
			  :scrolling-policy 'automatic
			  :width 300
			  :height 400)
	da(create-widget :drawing-area form
			 :border-width 3
			 :top-attachment 'widget
			 :top-widget rc
			 :left-attachment 'form
			 :bottom-attachment 'form
			 :right-attachment 'widget
			 :right-widget sw
			 :width 300
			 :height 400)
	graph ( create-widget :graph sw
			     :show-single-nodes t
			     :explicit-layout t
			     :global da
			     :line-width 3
			     :edge-mode 'orthogonal
			     :horizontal-spacing 30
			     :vertical-spacing 20)
	root (create-widget :push-button graph
			    :label-string "root1"
			    :node-type 'visible-root)
	root1(create-widget :push-button graph
			    :label-string "root2"
			    :node-type 'visible-root)
	pb0 (create-named-widget :push-button graph "PB0" t 
			   '(label-string "PB 0"))
	pb1 (create-named-widget :push-button graph "PB1" t 
			   '(label-string "PB 1"))
	pb2 (create-named-widget :push-button graph "PB2" t
			   '(label-string "PB 2"))
	pb3 (create-named-widget :push-button graph "PB3" t
			   '(label-string "PB 3"))
	pb4 (create-named-widget :push-button graph "PB4" t
			   '(label-string "PB 4"))
	pb5 (create-named-widget :push-button graph "PB5" t
			   '(label-string "PB 5"))
	pb6 (create-named-widget :push-button graph "PB6" t
			   '(label-string "PB 6"))
	pb7 (create-named-widget :push-button graph "PB7" t
			   '(label-string "PB 7"))
	pb8 (create-named-widget :push-button graph "PB8" t
			   '(label-string "PB 8"))
	pb9 (create-named-widget :push-button graph "PB9" t
			   '(label-string "PB 9"))
	pb10 (create-named-widget :push-button graph "PB10" t
			   '(label-string "PB 10"))
	pb11 (create-named-widget :push-button graph "PB11" t
			   '(label-string "PB 11"))
	pb12 (create-named-widget :push-button graph "PB12" t
			   '(label-string "PB 12"))
	pb13 (create-named-widget :push-button graph "PB13" t
			   '(label-string "PB 13"))
	pb14 (create-named-widget :push-button graph "PB14" t
			   '(label-string "PB 14"))
	pb15 (create-named-widget :push-button graph "PB15" t
			   '(label-string "PB 15"))
	pb16 (create-named-widget :push-button graph "PB16" t
			   '(label-string "PB 16"))
	pb17 (create-named-widget :push-button graph "PB17" t
			   '(label-string "PB 17"))
	pb18 (create-named-widget :push-button graph "PB18" t
			   '(label-string "PB 18"))
	pb19 (create-named-widget :push-button graph "PB19" t
			   '(label-string "PB 19"))
	)
 

  (add-callback root :activate #'markiere nil)
  (add-callback root1 :activate #'markiere nil)
  (add-callback pb0 :activate #'markiere nil)
  (add-callback pb1 :activate #'markiere nil)
  (add-callback pb2 :activate #'markiere nil)
  (add-callback pb3 :activate #'markiere nil)
  (add-callback pb4 :activate #'markiere nil)
  (add-callback pb5 :activate #'markiere nil)
  (add-callback pb6 :activate #'markiere nil)
  (add-callback pb7 :activate #'markiere nil)
  (add-callback pb8 :activate #'markiere nil)
  (add-callback pb9 :activate #'markiere nil)
  (add-callback pb10 :activate #'markiere nil)
  (add-callback pb11 :activate #'markiere nil)
  (add-callback pb12 :activate #'markiere nil)
  (add-callback pb13 :activate #'markiere nil)
  (add-callback pb14 :activate #'markiere nil)
  (add-callback pb15 :activate #'markiere nil)
  (add-callback pb16 :activate #'markiere nil)
  (add-callback pb17 :activate #'markiere nil)
  (add-callback pb18 :activate #'markiere nil)
  (add-callback pb19 :activate #'markiere nil)
  (add-callback func0 :activate #'dynamic graph)
  (add-callback func1 :activate #'remove-rel graph)
  (add-callback func2 :activate #'add-rel1 graph)
  (add-callback func3 :activate #'add-rel2 graph)
  (add-callback func4 :value-changed #'expand graph)
  (add-callback func5 :value-changed #'focus graph)
  (add-callback func6 :value-changed #'global graph)
  (add-callback func7 :value-changed #'show-info graph)
  (add-callback func8 :activate #'update graph)
  (add-callback edgemode :value-changed #'edge-func graph)
  
  
  (add-graph-relations 'solid-line root pb0
		                   root pb6
				   pb0 pb1
				   pb1 pb2
				   pb2 pb3
				   pb3 pb4
				   pb4 pb5
				   pb6 pb7
				   pb7 pb2
				   pb6 pb8
				   root1 pb9
				   root1 pb10
				   root1 pb11
				   root1 pb12
				   root1 pb13
				   root1 pb14
				   root1 pb15)
  (add-graph-relations 'dashed-line pb13 pb16
		                    pb13 pb17
				    pb13 pb18)

  (realize-widget shell))


