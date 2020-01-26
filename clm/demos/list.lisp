;;; -*- mode: lisp -*-

(in-package 'user)
(use-package 'xtk)

(defvar *sccsid* "@(#)list.lisp	1.3 11/27/91")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Main program
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(run-motif-application 'list-test)

(defun list-test (&aux app-shell app-rc list-b sw-b
		  lbl list-s sw-s list-e sw-e list-m sw-m)
  (setf app-shell (create-application-shell :allow-shell-resize t)
	app-rc (create-widget :row-column app-shell)
	lbl (create-widget :label app-rc :label-string "Single:")
	sw-s (create-widget :scrolled-window app-rc)
	list-s (create-widget :list sw-s 
			      :selection-policy :single
			      :visible-item-count 6)
	lbl (create-widget :label app-rc :label-string "Browse:")
	sw-b (create-widget :scrolled-window app-rc)
	list-b (create-widget :list sw-b 
			      :selection-policy :browse
			      :visible-item-count 6)
	lbl (create-widget :label app-rc :label-string "Extended:")
	sw-e (create-widget :scrolled-window app-rc)
	list-e (create-widget :list sw-e
			      :selection-policy :extended
			      :visible-item-count 6)
	lbl (create-widget :label app-rc :label-string "Multiple:")
	sw-m (create-widget :scrolled-window app-rc)
	list-m (create-widget :list sw-m
			      :selection-policy :multiple
			      :visible-item-count 6))
  (set-items list-b "Andreas" "Thomas" "Kristine" "Mike" "Cici" "Robert"
	     "Peter" "Anton" "Kasper" "Guenther" "Otto" "Karl-Heinz" 
	     "Werner"  "Alexander" "Kerstin" "Rita")
  (set-items list-e "Andreas" "Thomas" "Kristine" "Mike" "Cici" "Robert"
	     "Peter" "Anton" "Kasper" "Guenther" "Otto" "Karl-Heinz" 
	     "Werner"  "Alexander" "Kerstin" "Rita")
  (set-items list-s "Andreas" "Thomas" "Kristine" "Mike" "Cici" "Robert"
	     "Peter" "Anton" "Kasper" "Guenther" "Otto" "Karl-Heinz" 
	     "Werner"  "Alexander" "Kerstin" "Rita")
  (set-items list-m "Andreas" "Thomas" "Kristine" "Mike" "Cici" "Robert"
	     "Peter" "Anton" "Kasper" "Guenther" "Otto" "Karl-Heinz" 
	     "Werner"  "Alexander" "Kerstin" "Rita")
  
  (add-callback list-s :single-selection #'single-sel nil)
  (add-callback list-m :multiple-selection #'multi-sel nil)
  (add-callback list-e :extended-selection #'ext-sel nil)
  (add-callback list-b :browse-selection #'browse-sel nil)
  
  (add-callback list-b :default-action #'def-sel nil)
  (add-callback list-e :default-action #'def-sel nil)
  (add-callback list-m :default-action #'def-sel nil)
  (add-callback list-s :default-action #'def-sel nil)
  
  (realize-widget app-shell))

(defun single-sel (widget client-data &rest call-data)
  (declare (ignore list client-data))
  (format t "All items:        ~a~%" (get-items widget))
  (format t "Selected items:   ~a~%" (get-selected-items widget))
  (format t "Single selection: ~a~%" (first call-data)))

(defun def-sel (widget client-data &rest call-data)
  (declare (ignore client-data))
  (format t "All items:        ~a~%" (get-items widget))
  (format t "Selected items:   ~a~%" (get-selected-items widget))
  (format t "Default selection: ~a~%" (first call-data)))

(defun browse-sel (widget client-data &rest call-data)
  (declare (ignore client-data))
  (format t "All items:        ~a~%" (get-items widget))
  (format t "Selected items:   ~a~%" (get-selected-items widget))
  (format t "Browse selection: ~a~%" (first call-data)))

(defun multi-sel (widget client-data &rest call-data)
  (declare (ignore client-data))
  (format t "All items:        ~a~%" (get-items widget))
  (format t "Selected items:   ~a~%" (get-selected-items widget))
  (format t "Multiple selection: selected ~a items: ~a~%" 
	  (first call-data) (cdr call-data)))

(defun ext-sel (widget client-data &rest call-data)
  (declare (ignore client-data))
  (format t "All items:        ~a~%" (get-items widget))
  (format t "Selected items:   ~a~%" (get-selected-items widget))
  (format t "Extended selection: type: ~a count: ~a items: ~a~%" 
	  (first call-data) (second call-data) (cddr call-data)))
