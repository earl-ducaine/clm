;;; -*- Mode:LISP;Syntax: Common-Lisp;Package: GINA ;Base:10-*-
;;;
;;; Copyright 1990 GMD (German National Research Center for Computer Science)
;;;
;;; Permission to use, copy, modify, distribute, and sell this software and its
;;; documentation for any purpose is hereby granted without fee, provided that
;;; the above copyright notice appear in all copies and that both that
;;; copyright notice and this permission notice appear in supporting
;;; documentation, and that the name of GMD not be used in advertising or
;;; publicity pertaining to distribution of the software without specific,
;;; written prior permission.  GMD makes no representations about the
;;; suitability of this software for any purpose.  It is provided "as is"
;;; without express or implied warranty.
;;;
;;; GMD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL GMD
;;; BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
;;; OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
;;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;;
;;; Authors: Project GINA (spenke@gmd.de)
;;;          P.O. Box 1316
;;;          D-5205 Sankt Augustin 1
;;;

(in-package :GINA)
(setq *sccs-id* "@(#)finder.lisp	1.8  11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class finder
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass finder (application)
  (;; overrides
   (name          :initform "Finder"          :allocation :class)
   (document-type :initform 'finder-document  :allocation :class)
   (signature     :initform "finder"          :allocation :class)
   (file-type     :initform "directory"       :allocation :class)
   ;;(idle-timeout  :initform 10                :allocation :class)
   )
  (:documentation "a simple menu oriented finder"))

(eval-when (eval load)
  (export 'make-finder))

(defun make-finder (&key (display-host *default-display-host*)
		         (toolkit-host *default-toolkit-host*)
			 (document-pathname (current-directory-wildcard)))
  "start the finder application"
  (make-application :display-host display-host
		    :toolkit-host toolkit-host
		    :document-pathname document-pathname
		    :class 'finder))

(eval-when (eval load)
  (export 'ac))

(defun AC (&optional (display-host *default-display-host*)
	             (toolkit-host *default-toolkit-host*))
  "just a synonym for the finder"
  (make-finder :display-host display-host
	       :toolkit-host toolkit-host))

;; impossible to find out if a directory has changed on Symbolics!!!
;(defmethod idle-action ((finder finder))
;  "periodically refresh the folders"
;  (loop for folder in (document-list finder)
;	do (refresh-file-list folder)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class finder-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass finder-document (document)
  (;; related pathnames
   ;; the slot file-pathname holds a wildcard like e.g. "w1:>gina>documents>*.*.newest
   (finderinfo       :accessor finderinfo) ;; e.g."w1:>gina>documents>finderinfo.text"
   (parent-dir       :accessor parent-dir :initform nil) ;; e.g. "w1:>gina.directory"
   ;; overrides
   (shell-width  :initform 250)
   (shell-height :initform 300))
  (:documentation "the internal representaion of a folder"))

(defmethod initialize-instance :after ((doc finder-document) &rest initargs)
  "some special inits"
  (declare (ignore initargs))
  ;;the name of a finder document is constructed in a different way
  (setf (name doc) (directory-namestring (file-pathname doc))))

(defmethod create-windows ((doc finder-document))
  "create a window containig a scrollable list of files and some buttons"
  (with-slots (main-shell parent-dir) doc
    ;; create finder-shell containing the various widgets as slots
    (setq main-shell (make-finder-shell doc))

    ;; set callback of parent button
    (setf (activate-callback (parent-button main-shell))
	  (make-callback #'start-file parent-dir))
    (set-motif-resources (parent-button main-shell)
			 :sensitive (when parent-dir t))

    ;; set the callback of the start-button
    (setf (default-action-button (file-list main-shell)) 
      (start-button main-shell))
    (setf (activate-callback (start-button main-shell))
	  (make-callback #'start-selected-file doc))

    ;; set the callback of the refresh-button
    (setf (activate-callback (refresh-button main-shell))
	  (make-callback #'refresh-file-list doc))

    ;; remove or modify some main menu entries
    (remove-menu-entry (main-menu main-shell) "File" "New")
    (remove-menu-entry (main-menu main-shell) "File" "Open..")
    (remove-menu-entry (main-menu main-shell) "File" "Minifinder")

    ;; saving a documnet means storing its size
    (setf (label-string (save-menu-entry doc)) "Save Window Size")
    (setf (sensitive    (save-menu-entry doc)) t)
    
    (remove-menu-entry (main-menu main-shell) "File" "Save as..")
    (remove-menu-entry (main-menu main-shell) "File" "Revert")

    (remove-menu-entry (main-menu main-shell) "Edit" "Undo")
    (remove-menu-entry (main-menu main-shell) "Edit" "Redo")
    (remove-menu-entry (main-menu main-shell) "Edit" "Replay History")
    (remove-menu-entry (main-menu main-shell) "Edit" "History Scroller")
    
    ))

(defmethod read-from-file ((doc finder-document) wildcard-pathname 
			   &key (create-windows t))
  "list the directory instead of normally reading the file"
  (declare (ignore create-windows))
  ;; the parameter is a wildcard rather than a specific file !!
  ;; wildcard-pathname is of the form "w1:>gina>documents>*.*.newest"
  (with-slots (finderinfo parent-dir main-shell) doc

    ;; something like "w1:>gina>documents>finderinfo.text"
    (setq finderinfo (make-pathname :name "finderinfo" :type "text"
				    :defaults wildcard-pathname))

    ;; something like "w1:>gina.directory"
    (when (not (root-directory-p wildcard-pathname))
      (setq parent-dir (parent-directory-wildcard wildcard-pathname)))

    ;; read the finderinfo file placed in the directory if any
    (when (probe-file finderinfo)
      (with-open-file (stream finderinfo :direction :input)
	(read-header-from-stream doc stream)))

    ;; now that we know the size, we can create the windows
    (create-windows doc)

    ;; show the wildcard in the label
    (setf (label-string (directory-label main-shell))
	  (directory-namestring wildcard-pathname))

    ;; make a view containing the filenames and put it into the scroller
    (refresh-file-list doc)))

(defmethod write-to-file ((doc finder-document) pathname)
  "write the finderinfo to the directory"
  (declare (ignore pathname))
  (with-open-file (stream (finderinfo doc) 
		   :direction :output :if-exists :new-version)
    (write-header-to-stream doc stream)))

(defmethod start-selected-file ((doc finder-document))
  "start the file which has been selected in the file-list"
  (with-clock-cursor
    (start-file (value (file-list (main-shell doc))) :from-document doc)))

(defmethod refresh-file-list ((doc finder-document) &aux files items)
  "read the directory again and build a new file-list"
  (with-slots (file-pathname main-shell) doc
    (with-clock-cursor
      ;; make an item-list of the filenames and put it into the scroller
      (setq files (sort (directory file-pathname) #'pathname-name<))
      (setq items
	    (append
	      (loop for file in files
		    when (directoryp file)
		      collect (list (concatenate 'string "-> " 
						 (name-and-type file))
				    (subdirectory-wildcard file)))
	      (loop for file in files
		    when (not (directoryp file))
		      collect (list (name-and-type file) file))))

      ;; change the item list
      (set-item-list (file-list main-shell) items)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; main program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(register-application "finder" 'finder "directory")
'(make-finder)
