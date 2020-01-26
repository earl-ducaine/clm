;;; -*- Mode:lisp; Syntax:Common-Lisp; Package: (user (lisp)); Base:10 -*-

(in-package 'user)
(use-package 'xtk)

(defvar *sccsid* "@(#)list-func.lisp	1.3 11/27/91")


'(run-motif-application 'list-func-application :process-name "List Test")
   
(defun list-func-application ()
  (let* ((app-shell (create-application-shell :allow-shell-resize t))
	 (app-rc (create-widget :row-column app-shell))
	 (lbl (create-widget :label app-rc :label-string "Item text:"))
	 (item-t (create-widget :text app-rc :value "bla"))
	 (lbl (create-widget :label app-rc :label-string "Item position:"))
	 (count-t (create-widget :text app-rc :value "1"))
	 (pb-1 (create-widget :push-button app-rc 
			      :label-string "add item"))
	 (pb-2 (create-widget :push-button app-rc 
			      :label-string "add item unselected"))
	 (pb-3 (create-widget :push-button app-rc 
			      :label-string "delete item"))
	 (pb-4 (create-widget :push-button app-rc 
			      :label-string "delete item at position"))
	 (pb-5 (create-widget :push-button app-rc 
			      :label-string "deselect all items"))
	 (pb-6 (create-widget :push-button app-rc 
			      :label-string "deselect item"))
	 (pb-7 (create-widget :push-button app-rc 
			      :label-string "deselect pos"))
	 (pb-8 (create-widget :push-button app-rc 
			      :label-string "select item"))
	 (pb-9 (create-widget :push-button app-rc 
			      :label-string "select pos"))
	 (pb-10 (create-widget :push-button app-rc 
			       :label-string "set bottom item"))
	 (pb-11 (create-widget :push-button app-rc 
			       :label-string "set bottom position"))
	 (pb-12 (create-widget :push-button app-rc 
			       :label-string "set horizontal position"))
	 (pb-13 (create-widget :push-button app-rc :label-string "set item"))
	 (pb-14 (create-widget :push-button app-rc :label-string "set pos"))
	 (pb-15 (create-widget :push-button app-rc
			       :label-string "get-selected items"))
	 ;;(sw (create-widget :scrolled-window app-rc))
	 (listw (create-widget :list app-rc
			       :selection-policy :multiple
			       :visible-item-count 6))
	 (client-data (list item-t count-t listw)))
	(set-items listw "111" "222" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j")
	(add-callback listw :multiple-selection #'multi-selection item-t)
	(add-callback count-t :modify-verify 'test-number nil)
	(add-callback pb-1 :activate #'add-item client-data)
	(add-callback pb-2 :activate #'add-item-unsel client-data)
	(add-callback pb-3 :activate #'delete-item client-data)
	(add-callback pb-4 :activate #'delete-item-pos client-data)
	(add-callback pb-5 :activate #'deselect-all client-data)
	(add-callback pb-6 :activate #'deselect-item client-data)
	(add-callback pb-7 :activate #'deselect-pos client-data)
	(add-callback pb-8 :activate #'select-item client-data)
	(add-callback pb-9 :activate #'select-pos client-data)
	(add-callback pb-10 :activate #'set-bottom-item client-data)
	(add-callback pb-11 :activate #'set-bottom-pos client-data)
	(add-callback pb-12 :activate #'set-horiz-pos client-data)
	(add-callback pb-13 :activate #'set-item client-data)
	(add-callback pb-14 :activate #'set-pos client-data)
	(add-callback pb-15 :activate #'do-get-selected client-data)
	(realize-widget app-shell)))

(defun int-from-widget (widget)
  (read-from-string (first (get-values widget :value))))

(defun set-pos (widget client-data &rest call-data)
  (declare (ignore call-data))
  (setf pos (int-from-widget (second client-data)))
  (list-set-top-pos (third client-data) pos))

(defun do-get-selected (widget client-data &rest call-data)
  (declare (ignore call-data))
  (format t "~a~%" (get-selected-items (third client-data))))

(defun set-item (widget client-data &rest call-data)
  (declare (ignore call-data))
  (setf value (first (get-values (first client-data) :value)))
  (list-set-top-item (third client-data) value))

(defun set-horiz-pos (widget client-data &rest call-data)
  (declare (ignore call-data))
  (setf pos (int-from-widget (second client-data)))
  (list-set-horiz-pos (third client-data) pos))

(defun set-bottom-pos (widget client-data &rest call-data)
  (declare (ignore call-data))
  (setf pos (int-from-widget (second client-data)))
  (list-set-bottom-pos (third client-data) pos))

(defun set-bottom-item (widget client-data &rest call-data)
  (declare (ignore call-data))
  (setf value (first (get-values (first client-data) :value)))
  (list-set-bottom-item (third client-data) value))

(defun select-pos (widget client-data &rest call-data)
  (declare (ignore call-data))
  (setf pos (int-from-widget (second client-data)))
  (list-select-pos (third client-data) pos nil))

(defun select-item (widget client-data &rest call-data)
  (declare (ignore call-data))
  (setf value (first (get-values (first client-data) :value)))
  (list-select-item (third client-data) value nil))

(defun deselect-pos (widget client-data &rest call-data)
  (declare (ignore call-data))
  (setf pos (int-from-widget (second client-data)))
  (list-deselect-pos (third client-data) pos))

(defun deselect-item (widget client-data &rest call-data)
  (declare (ignore call-data))
  (setf value (first (get-values (first client-data) :value)))
  (list-deselect-item (third client-data) value))

(defun deselect-all (widget client-data &rest call-data)
  (declare (ignore client-data call-data))
  (list-deselect-all-items (third client-data)))

(defun delete-item-pos (widget client-data &rest call-data)
  (declare (ignore call-data))
  (setf pos (int-from-widget (second client-data)))
  (list-delete-pos (third client-data) pos))

(defun delete-item (widget client-data &rest call-data)
  (declare (ignore call-data))
  (setf value (first (get-values (first client-data) :value)))
  (list-delete-item (third client-data) value))

(defun add-item-unsel (widget client-data &rest call-data)
  (declare (ignore call-data))
  (setf value (first (get-values (first client-data) :value)))
  (setf pos (int-from-widget (second client-data)))
  (list-add-item-unselected (third client-data) value pos))

(defun add-item (widget client-data &rest call-data)
  (declare (ignore call-data))
  (setf value (first (get-values (first client-data) :value)))
  (setf pos (int-from-widget (second client-data)))
  (list-add-item (third client-data) value pos))

(defun test-number (widget client-data &rest call-data)
  (declare (ignore client-data widget))
  (or (equal "" (fifth call-data))
      (numberp (read-from-string (fifth call-data)))))

(defun multi-selection (widget client-data &rest call-data)
  (declare (ignore client-data))
  (when (cdr call-data)
	(set-values client-data :value (second call-data))))
