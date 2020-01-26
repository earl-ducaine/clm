;;;-*-Mode:LISP;Syntax: Common-Lisp;Package:ib;Base:10-*-
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
;;;          Germany

(in-package :ib)

(setq *sccs-id* "@(#)info-cmds.lisp	1.13	11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class clear-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass clear-command (command)
  (;; overrides
   (name :initform "Delete Widgets" :allocation :class)
   ;; instance-parameters
   (selected-plates :accessor selected-plates :initarg :selected-plates))
  (:documentation "a command to delete all selected widget-plates and widgets"))

(defun make-clear-command (document)
  "create a new command object with appropriate parameters"
  (make-command document
		:class 'clear-command
		;; Methode sensitive-plates bei view statt lambda-Ausdruck
		:initargs 
		(list :selected-plates
		      (loop for plate in (view-objects (main-view document))
			    when (mouse-sensitive plate) collect plate))))

(defmethod doit ((cmd clear-command))
  "delete all selected-plates from the view"
  (loop for plate in (selected-plates cmd) do
    (deinstall plate)
    (remove-child (main-info (document cmd)) (info plate))
    (unmanage (gina-widget plate))))

(defmethod undoit ((cmd clear-command))
  "reinstall all selected-plates in the view"
  (loop for plate in (selected-plates cmd) do 
    (install plate (main-view (document cmd)) (x-pos plate) (y-pos plate))
    (add-child (main-info (document cmd)) (info plate))
    (manage (gina-widget plate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class clear-all-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass clear-all-command (clear-command)
  (;; overrides
   (name :initform "Delete All" :allocation :class))
  (:documentation "a command to delete all selected-plates and widgets"))

(defun make-clear-all-command (document)
  "create a new command object with appropriate parameters"
  (make-command document
		:class 'clear-all-command
		:initargs 
		(list :selected-plates (view-objects (main-view document)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class dialog-type-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass dialog-type-command (command)
  (;; overrides
   (name :initform "Change Dialog Type" :allocation :class)
   ;; instance-parameters
   (old-info  :accessor old-info :initarg :old-info)
   (old-type  :accessor old-type :initarg :old-type)
   (new-type  :accessor new-type :initarg :new-type))
  (:documentation "a command to change the dialog-type of the document"))

(defun make-dialog-type-command (document new-type old-type)
  "create a new command object with appropriate parameters"
  (unless (equal new-type old-type)
    (make-command document
	          :class 'dialog-type-command
		  :initargs (list :old-info (main-info document)
                                  :old-type old-type
                                  :new-type new-type))))

(defmethod doit ((cmd dialog-type-command))
  "create main info with new type"
  (destroy-resource-dialog (old-info cmd))
  (setf (main-info (document cmd))
        (case (new-type cmd)
          ((:main)    (make-document-shell-info (document cmd)))
          ((:tool)    (make-tool-dialog-box-info (document cmd)))
          ((:dialog)  (make-modeless-dialog-box-info (document cmd)))
          ((:modal)   (make-modal-dialog-box-info (document cmd)))))
  (loop for child in (children (old-info cmd))
        do (reparent child (main-info (document cmd)))))

(defmethod undoit ((cmd dialog-type-command))
  "reinstall old info"
  (loop for child in (children (main-info (document cmd)))
        do (reparent child (old-info cmd)))
  (setf (main-info (document cmd)) (old-info cmd))
  ;;update
  (setf (value (class-option (main-shell (document cmd)))) (old-type cmd)))

(defmethod redoit ((cmd dialog-type-command))
  (doit cmd)
  ;;update
  (setf (value (class-option (main-shell (document cmd)))) (new-type cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class change-resource-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass change-resource-command (command)
  (;; overrides
   ;; name is instance-specific
   (name        :initarg :name       :allocation :instance)
   ;; instance-parameters
   (info        :accessor info       :initarg :info)
   (resource    :accessor resource   :initarg :resource)
   (new-value   :accessor new-value  :initarg :new-value)
   (old-value   :accessor old-value  :initform nil))
  (:documentation "a command to change resource values"))

(defun make-change-resource-command (document info resource value
                                     &key (class 'change-resource-command))
  (let ((prev (previous-command document)))
    (if (and prev (same-info-and-resource prev info resource))
        (append-changes prev value)
        (make-command document
                      :class class
                      :initargs (list :info info
                                      :name (format nil "Change ~a"
                                                    (resource-name resource))
                                      :resource resource
                                      :new-value value)))))

(defmethod previous-command ((doc document))
  "returns the last command executed"
  (when (and (undo-commands doc) (modified doc))
      (first (undo-commands doc))))

(defmethod same-info-and-resource ((cmd command) info resource)
  "return nil for all other command classes"
  (declare (ignore info resource))
  nil)

(defmethod same-info-and-resource ((cmd change-resource-command) info resource)
  "check whether last resource change was for same info"
  (and (equal (info cmd) info)
       (eql (resource cmd) resource)))

(defmethod apply-new-value ((cmd change-resource-command) value)
  (setf (resource-value (resource cmd) (info cmd)) value)
  (when (plate (info cmd))
    (handle-new-resource-value (resource cmd) (plate (info cmd)) value)))

(defmethod append-changes ((cmd change-resource-command) value)
  "append new resource change to existing command"
  (setf (new-value cmd) value)
  (apply-new-value cmd value)
  cmd)

(defmethod doit ((cmd change-resource-command))
  "apply the resource changes"
  (setf (old-value cmd) (resource-value (resource cmd) (info cmd)))
  (apply-new-value cmd (new-value cmd)))

(defmethod undoit ((cmd change-resource-command))
  "reset resource to old value"
  (apply-new-value cmd (old-value cmd))
  (update-resource-widget (resource cmd) (info cmd) (old-value cmd)))

(defmethod redoit ((cmd change-resource-command))
  (apply-new-value cmd (new-value cmd))
  (update-resource-widget (resource cmd) (info cmd) (new-value cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class change-multi-resource-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass change-multi-resource-command (change-resource-command)
  ())

(defun make-change-multi-resource-command (document info resource value)
  (make-change-resource-command document info resource value
                                :class 'change-multi-resource-command))

(defmethod apply-new-value ((cmd change-multi-resource-command) value)
  (set-resource-value-recursive cmd (info cmd) value))

(defun set-resource-value-recursive (cmd infos value)
  (loop for info in infos
        do (setf (resource-value (resource cmd) info) value)
           (when (plate info)
              (handle-new-resource-value (resource cmd) (plate info) value))
           (set-resource-value-recursive cmd (children info) value)))

(defmethod doit ((cmd change-multi-resource-command))
  "apply the resource changes"
  ;; hack for old-value
  (setf (old-value cmd) (resource-value (resource cmd) (first (info cmd))))
  (apply-new-value cmd (new-value cmd)))

(defmethod undoit ((cmd change-multi-resource-command))
  "reset resource to old value"
  (apply-new-value cmd (old-value cmd)))

(defmethod redoit ((cmd change-multi-resource-command))
  (apply-new-value cmd (new-value cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class prog-lang-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass prog-lang-command (command)
  (;; overrides
   (name :initform "Change Save Language" :allocation :class)
   ;; instance-parameters
   (new-lang-is-lisp  :accessor new-lang-is-lisp :initarg :new-lang-is-lisp))
  (:documentation "a command to change the save language of the document"))

(defun make-prog-lang-command (document not-lisp)
  "create a new command object with appropriate parameters"
  (unless (eql not-lisp (active (cplus-details document)))
    (make-command document
	          :class 'prog-lang-command
		  :initargs (list :new-lang-is-lisp (not not-lisp)))))

(defmethod doit ((cmd prog-lang-command))
  "set new language"
  (setf (show-lisp (coder-tool (main-shell (document cmd))))
        (new-lang-is-lisp cmd))
  (if (new-lang-is-lisp cmd)
      (progn
        (setf (active (lisp-details (document cmd))) t)
        (setf (active (cplus-details (document cmd))) nil))
      (progn
        (setf (active (lisp-details (document cmd))) nil)
        (setf (active (cplus-details (document cmd))) t))))

(defmethod undoit ((cmd prog-lang-command))
  "reinstall old info"
  (setf (show-lisp (coder-tool (main-shell (document cmd))))
        (not (new-lang-is-lisp cmd)))
  (if (new-lang-is-lisp cmd)
      (progn
        (setf (active (lisp-details (document cmd))) nil)
        (setf (active (cplus-details (document cmd))) t))
      (progn
        (setf (active (lisp-details (document cmd))) t)
        (setf (active (cplus-details (document cmd))) nil)))
  ;;update
  (setf (value (lisp-toggle (coder-tool (main-shell (document cmd)))))
        (new-lang-is-lisp cmd)))

(defmethod redoit ((cmd prog-lang-command))
  (doit cmd)
  ;;update
  (setf (value (lisp-toggle (coder-tool (main-shell (document cmd)))))
        (not (new-lang-is-lisp cmd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class change-package-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass change-package-command (command)
  (;; overrides
   (name        :initform "Change Package"  :allocation :class)
   ;; instance-parameters
   (new-value   :accessor new-value  :initarg :new-value)
   (old-value   :accessor old-value  :initarg :old-value))
  (:documentation "a command to change the code package"))

(defun make-change-package-command (document value)
  (let ((prev (previous-command document)))
    (if (and prev (equal (name prev) "Change Package"))
        (append-changes prev value)
        (make-command document
                      :class 'change-package-command
                      :initargs (list 
                              :old-value (what-package (lisp-details document))
                              :new-value value)))))

(defmethod append-changes ((cmd change-package-command) value)
  "append change to existing command"
  (setf (new-value cmd) value)
  (setf (what-package (lisp-details (document cmd))) value)
  cmd)

(defmethod doit ((cmd change-package-command))
  (setf (what-package (lisp-details (document cmd))) (new-value cmd)))

(defmethod undoit ((cmd change-package-command))
  (setf (what-package (lisp-details (document cmd))) (old-value cmd))
  ;; update
  (let ((coder-window (coder-tool (main-shell (document cmd)))))
    (when coder-window
      (update-window coder-window (document cmd)))))

(defmethod redoit ((cmd change-package-command))
  (setf (what-package (lisp-details (document cmd))) (new-value cmd))
  ;; update
  (let ((coder-window (coder-tool (main-shell (document cmd)))))
    (when coder-window
      (update-window coder-window (document cmd)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class change-doc-class-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass change-doc-class-command (command)
  (;; overrides
   (name        :initform "Change Document Class"  :allocation :class)
   ;; instance-parameters
   (new-value   :accessor new-value  :initarg :new-value)
   (old-value   :accessor old-value  :initarg :old-value))
  (:documentation "a command to change the C++ doc-class"))

(defun make-change-doc-class-command (document value)
  (let ((prev (previous-command document)))
    (if (and prev (equal (name prev) "Change Document Class"))
        (append-changes prev value)
        (make-command document
                      :class 'change-doc-class-command
                      :initargs (list 
                                 :old-value (doc-class (cplus-details document))
                                 :new-value value)))))

(defmethod append-changes ((cmd change-doc-class-command) value)
  "append change to existing command"
  (setf (new-value cmd) value)
  (setf (doc-class (cplus-details (document cmd))) value)
  cmd)

(defmethod doit ((cmd change-doc-class-command))
  (setf (doc-class (cplus-details (document cmd))) (new-value cmd)))

(defmethod undoit ((cmd change-doc-class-command))
  (setf (doc-class (cplus-details (document cmd))) (old-value cmd))
  ;; update
  (let ((coder-window (coder-tool (main-shell (document cmd)))))
    (when coder-window 
      (update-window coder-window (document cmd)))))

(defmethod redoit ((cmd change-doc-class-command))
  (setf (doc-class (cplus-details (document cmd))) (new-value cmd))
  ;; update
  (let ((coder-window (coder-tool (main-shell (document cmd)))))
    (when coder-window
      (update-window coder-window (document cmd)))))

