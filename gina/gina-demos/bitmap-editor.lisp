;;;-*-Mode:LISP;Syntax: Common-Lisp;Package: bitedit;Base:10-*-
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
(defginapackage :bitedit)
(in-package :bitedit)
(setq *sccs-id* "@(#)bitmap-editor.lisp	1.8  11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; class bitmap-editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass bitmap-editor (application)
  (;; overrides
   (name                 :initform "Bitmap Editor"  :allocation :class)
   (document-type        :initform 'bitmap-document :allocation :class)
   (signature            :initform "bitedit"        :allocation :class)
   (file-type            :initform "bitmap"         :allocation :class))
  (:documentation "a simple bitmap editor application"))

(defun make-bitmap-editor ()
  "start the bitmap-application"
  (make-application :class 'bitmap-editor))

'(make-bitmap-editor)
		    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; class bitmap-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass bitmap-document (document)
  ((bitmap     :accessor bitmap     :initform nil)
   (no-rows    :accessor no-rows    :initform 32)
   (no-columns :accessor no-columns :initform 32)
   
   (mini-view     :accessor mini-view)
   (factor-scale  :accessor factor-scale)
   (params-box    :accessor params-box    :initform nil)
   (rows-scale    :accessor rows-scale    :initform nil)
   (columns-scale :accessor columns-scale :initform nil)
   
   (shell-width  :initform 370)
   (shell-height :initform 420)))

;; forward declaration 
(defclass bitmap-view (view)
  ((factor  :accessor factor  :initarg :factor
	    :documentation "size of a single pixel on the screen")
   (gap     :accessor gap     :initarg :gap
	    :documentation "white space between pixels")))

(defmethod create-windows ((doc bitmap-document)
			   &aux form scroller frame mini-frame)
  "create the windows belonging to this document"
  (with-slots (main-shell main-view mini-view factor-scale no-columns no-rows) doc
    (setq main-shell (make-document-shell doc))
    (setq form       (make-form main-shell))
    (setq scroller   (make-scroller form))
    (setq frame      (make-frame scroller :shadow-type :etched-out))
    (setq main-view (make-bitmap-view frame doc
				      :factor 10
				      :gap 1
				      :columns no-columns
				      :rows no-rows))
    (setq mini-frame (make-frame form :shadow-type :etched-out))
    (setq mini-view (make-bitmap-view mini-frame doc
				      :factor 1
				      :gap 0
				      :columns no-columns
				      :rows no-rows))
       
    (setq factor-scale
	  (make-scale form
		      :orientation :horizontal
		      :title-string "Pixel Size"
		      :value (factor (main-view doc))
		      :minimum 2
		      :maximum 30
		      :value-changed-callback
		      (make-callback #'change-params doc :factor)))

    (define-form-constraint factor-scale
			    :top-attachment :form
			    :left-attachment :form
			    :right-attachment :form)
    (define-form-constraint mini-frame
			    :top-attachment :widget :top-widget factor-scale
			    :left-attachment :form)
    (define-form-constraint scroller
			    :left-attachment :widget :left-widget mini-frame
			    :top-attachment :widget :top-widget factor-scale
			    :right-attachment :form
			    :bottom-attachment :form)
    
    ;; add some menu commands
    (add-menu-command (main-menu main-shell)
		      "Edit" "Clear All" (make-callback #'make-clear-all-command doc))
    (add-menu-command (main-menu main-shell)
		      "Edit" "Set All"
		      (make-callback #'make-clear-all-command doc :color :black))
    (add-menu-command (main-menu main-shell)
		      "Bitmap" "Set Size ..."
		      (make-callback #'display-parms-box doc))
    (add-menu-command (main-menu main-shell)
		      "Bitmap" "Adapt Shell Size"
		      (make-callback #'adapt-shell-size doc))
    ))

(defmethod pixel-on ((doc bitmap-document) x y)
  "look up pixel value in data structure"
  (when (member (list x y) (bitmap doc) :test #'equal)
    t))

(defmethod set-pixel ((doc bitmap-document) x y &key (redisplay t)
		      &allow-other-keys
		      &aux coordinates)
  "set a single pixel in the data structure and display in the view"
  (with-slots (main-view mini-view bitmap) doc
    (setq coordinates (list x y))
    (setq bitmap (remove coordinates bitmap :test #'equal))
    (push coordinates bitmap)

    (when redisplay
      (set-pixel main-view x y)
      (set-pixel mini-view x y))))

(defmethod clear-pixel ((doc bitmap-document) x y &key (redisplay t)
			&allow-other-keys
			&aux coordinates)
  "clear a single pixel in the data structure and display in the view"
  (with-slots (main-view mini-view bitmap) doc
    (setq coordinates (list x y))
    (setq bitmap (remove coordinates bitmap :test #'equal))
    (when redisplay
      (clear-pixel main-view x y)
      (clear-pixel mini-view x y))))
	 
(defmethod toggle-pixel ((doc bitmap-document) x y &key (redisplay t)
			 &aux coordinates)
  "toggle a single pixel in the data structure and display in the view"
  (setq coordinates (list x y))
  (if (member coordinates (bitmap doc) :test #'equal)
      (clear-pixel doc x y :redisplay redisplay)
      (set-pixel doc x y :redisplay redisplay)))

(defmethod write-to-stream ((doc bitmap-document) stream)
  "write the document to the specified stream"
  (format stream "~d ~d ~d~%"
	  (no-rows doc) (no-columns doc) (factor (main-view doc)))
  (format stream "(")
  (loop for pair in (bitmap doc)
	when (and (<= (first  pair) (no-columns doc))
		  (<= (second pair) (no-rows doc)))
	  do (prin1 pair stream))
  (format stream ")~%"))

(defmethod read-from-stream ((doc bitmap-document) stream) 
  "read the document from the specified stream"
  (with-slots (no-rows no-columns main-view mini-view
		       factor-scale rows-scale columns-scale bitmap) doc
    (setq no-rows (read stream))
    (setq no-columns (read stream))
    (setf (factor main-view) (read stream))
    (setq bitmap (read stream))
    
    (when rows-scale
      (setf (value rows-scale) no-rows))
    (when columns-scale
      (setf (value columns-scale) no-columns))
    (setf (value factor-scale) (factor main-view))
    (resize main-view
	    (* (factor main-view) no-columns)
	    (* (factor main-view) no-rows))
    (resize (mini-view doc) no-columns no-rows)
    ))

(defmethod change-params ((doc bitmap-document)
			  &key (no-rows (no-rows doc))
			       (no-columns (no-columns doc))
			       (factor (factor (main-view doc))))
  "adapt to new layout params"
  (setf (no-rows doc) no-rows)
  (setf (no-columns doc) no-columns)
  (when (not (= factor (factor (main-view doc))))
    (setf (factor (main-view doc)) factor)
    ;; even the parts of the view that remain visible have to be redrawn
    (force-redraw (main-view doc))
    (force-redraw (mini-view doc)))

  ;; resize => expose-events for new areas
  (resize (main-view doc) (* factor no-columns) (* factor no-rows))
  (resize (mini-view doc) no-columns no-rows)
    (resize (main-shell doc) (+ (width (main-shell doc)) 1)
                             (height (main-shell doc)))
    (resize (main-shell doc) (- (width (main-shell doc)) 1)
                             (height (main-shell doc)))
  (setf (modified doc) t))

(defmethod display-parms-box ((doc bitmap-document))
  "bring up modeless dialog to set parms"
  (when (not (params-box doc))
    (setf (params-box doc)
	  (make-modeless-dialog-box "Bitmap Size" :document doc
				    :resize t
				    :motif-resources '(:width 250)))
    (setf (rows-scale doc)
	  (make-scale (params-box doc)
		      :orientation :horizontal 
		      :title-string "Rows"
		      :value (no-rows doc)
		      :minimum 1
		      :maximum 200
		      :value-changed-callback
		      (make-callback #'change-params doc :no-rows)))
    (setf (columns-scale doc)
	  (make-scale (params-box doc)
		      :orientation :horizontal 
		      :title-string "Columns"
		      :value (no-columns doc)
		      :minimum 1
		      :maximum 200
		      :value-changed-callback
		      (make-callback #'change-params doc :no-columns)))
    (define-form-constraint (rows-scale doc)
			    :top-attachment :form
			    :left-attachment :form
			    :right-attachment :form)
    (define-form-constraint (columns-scale doc)
			    :top-attachment :widget :top-widget (rows-scale doc)
			    :left-attachment :form
			    :right-attachment :form))
  (pop-up (params-box doc)))

(defmethod adapt-shell-size ((doc bitmap-document))
  "resize shell so that complete bitmap is shown"
  (with-slots (no-columns no-rows main-shell main-view) doc
    (resize main-shell
	    (max 270 (+ 20 no-columns (* no-columns (factor main-view))))
	    (+ 100 (* no-rows (factor main-view))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; class bitmap-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass bitmap-view (view)
  ((factor  :accessor factor  :initarg :factor
	    :documentation "size of a single pixel on the screen")
   (gap     :accessor gap     :initarg :gap
	    :documentation "white space between pixels")))

(defun make-bitmap-view (parent doc &key (factor 1) (rows 10) (columns 10) (gap 0))
  (make-view parent
	     :width  (* factor columns)
	     :height (* factor rows)
	     :document doc
	     :backing-store t
	     :class 'bitmap-view
	     :initargs (list :factor factor :gap gap)))

(defmethod button-press ((view bitmap-view) code repetition x y)
  "react to button-press event in the window"
  (declare (ignore repetition))
  (case code ;; which button      
    (:select (make-set-pixel-command    (document view) view x y))
    (:extend (make-clear-pixel-command  (document view) view x y))
    (:menu (make-toggle-pixel-command (document view) view x y)))
  )

(defmethod set-pixel ((view bitmap-view) x y &key &allow-other-keys)
  "set a single pixel"
  (with-slots (factor gap) view
    (draw-rectangle view (* (1- x) factor) (* (1- y) factor)
		    (- factor gap) (- factor gap) t)))

(defmethod clear-pixel ((view bitmap-view) x y &key &allow-other-keys)
  "clear a single pixel"
  (with-slots (factor) view 
    (clear-area view (* (1- x) factor) (* (1- y) factor)
		factor factor)))

(defmethod draw ((view bitmap-view) count x y width height)
  "draw bits into view"
  (declare (ignore x y width height))
  (when (zerop count)
    (loop for (x y) in (bitmap (document view))
	  do (set-pixel view x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; mouse-commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; class set-pixel-command ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass set-pixel-command (mouse-down-command)
  (;; overrides
   (name       :initform "Set Pixel" :allocation :class)
   (call-doit  :initform nil)
   ;; new slots
   (last-coordinates :accessor last-coordinates :initform nil)
   (coordinate-list  :accessor coordinate-list  :initform nil)))

(defun make-set-pixel-command (document view x y)
  "create command object storing all touched x/y coordinates"
  (make-mouse-down-command document view x y
			   :class 'set-pixel-command
			   :cursor :hand))

(defmethod draw-feedback ((cmd set-pixel-command) x y &key clear)
  "no feedback"
  (declare (ignore x y clear)))

(defmethod track-mouse ((cmd set-pixel-command) x y
			&key (started nil) (finished nil)
			&aux pixel-x pixel-y coordinates)
  "process each touched pixel"
  (declare (ignore started finished))
  (with-slots (last-coordinates coordinate-list document call-doit view) cmd
    (setq pixel-x (ceiling (/ x (factor view))))
    (setq pixel-y (ceiling (/ y (factor view))))
    (setq coordinates (list pixel-x pixel-y))
    (when (and (not (equal coordinates last-coordinates))
	       (not (pixel-on document pixel-x pixel-y)))
      (setq call-doit t)
      (setq last-coordinates coordinates)
      (push coordinates coordinate-list)
      (set-pixel document pixel-x pixel-y))
    ))

(defmethod undoit ((cmd set-pixel-command))
  "clear all pixels again"
  (with-slots (coordinate-list document) cmd
    (loop for coordinates in coordinate-list
	  do (clear-pixel document (first coordinates) (second coordinates)))
    ))

(defmethod redoit ((cmd set-pixel-command))
  "set all pixels again"
  (with-slots (coordinate-list document) cmd
    (loop for coordinates in coordinate-list
	  do (set-pixel document (first coordinates) (second coordinates)))
    ))

;; class clear-pixel-command ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clear-pixel-command (mouse-down-command)
  (;; overrides
   (name       :initform "Clear Pixel" :allocation :class)
   (call-doit  :initform nil)
   ;; new slots
   (last-coordinates :accessor last-coordinates :initform nil)
   (coordinate-list  :accessor coordinate-list  :initform nil)))

(defun make-clear-pixel-command (document view x y)
  "create command object storing all touched x/y coordinates"
  (make-mouse-down-command document view x y
			   :class 'clear-pixel-command
			   :cursor :hand))

(defmethod draw-feedback ((cmd clear-pixel-command) x y &key clear)
  "no feedback"
  (declare (ignore x y clear)))

(defmethod track-mouse ((cmd clear-pixel-command) x y
			&key (started nil) (finished nil)
			&aux pixel-x pixel-y coordinates)
  "process each touched pixel"
  (declare (ignore started finished))
  (with-slots (last-coordinates coordinate-list document call-doit view) cmd
    (setq pixel-x (ceiling (/ x (factor view))))
    (setq pixel-y (ceiling (/ y (factor view))))
    (setq coordinates (list pixel-x pixel-y))
    (when (and (not (equal coordinates last-coordinates))
	       (pixel-on document pixel-x pixel-y))
      (setq call-doit t)
      (setq last-coordinates coordinates)
      (push coordinates coordinate-list)
      (clear-pixel document pixel-x pixel-y))
    ))

(defmethod undoit ((cmd clear-pixel-command))
  "set all pixels again"
  (with-slots (coordinate-list document) cmd
    (loop for coordinates in coordinate-list
	  do (set-pixel document (first coordinates) (second coordinates)))
    ))

(defmethod redoit ((cmd clear-pixel-command))
  "add new pixel to bitmap"
  "clear all pixels again"
  (with-slots (coordinate-list document) cmd
    (loop for coordinates in coordinate-list
	  do (clear-pixel document (first coordinates) (second coordinates)))
    ))

;; class toggle-pixel-command ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass toggle-pixel-command (mouse-down-command)
  (;; overrides
   (name        :initform "Toggle Pixel" :allocation :class)
   ;; new slots
   (last-coordinates :accessor last-coordinates :initform nil)
   (coordinate-list  :accessor coordinate-list  :initform nil)))

(defun make-toggle-pixel-command (document view x y)
  "create command object storing the x/y coordinates of the mouse-click"
  (make-mouse-down-command document view x y
			   :class 'toggle-pixel-command
			   :cursor :hand))

(defmethod draw-feedback ((cmd toggle-pixel-command) x y &key clear)
  "no feedback"
  (declare (ignore x y clear)))

(defmethod track-mouse ((cmd toggle-pixel-command) x y
			&key (started nil) (finished nil)
			&aux pixel-x pixel-y coordinates)
  "process each touched pixel"
  (declare (ignore started finished))
  (with-slots (last-coordinates coordinate-list document call-doit view) cmd
    (setq pixel-x (ceiling (/ x (factor view))))
    (setq pixel-y (ceiling (/ y (factor view))))
    (setq coordinates (list pixel-x pixel-y))
    (when (not (equal coordinates last-coordinates))
      (setq last-coordinates coordinates)
      (push coordinates coordinate-list)
      (toggle-pixel document pixel-x pixel-y))
    ))

(defmethod undoit ((cmd toggle-pixel-command))
  "toggle all pixels again"
  (with-slots (coordinate-list document) cmd
    (loop for coordinates in coordinate-list
	  do (toggle-pixel document (first coordinates) (second coordinates)))
    ))

(defmethod redoit ((cmd toggle-pixel-command))
  "toggle all pixels again"
  (undoit cmd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class clear-all-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clear-all-command (command)
  (;; instance-parameters
   (name :initarg :name) ;; turn class slot into an instance slot
   (color  :accessor color  :initarg :color)
   (bitmap :accessor bitmap :initarg :bitmap))
  (:documentation "a command to delete all pixels"))

(defun make-clear-all-command (document &key (color :white))
  "create a new command object with appropriate parameters"
  (make-command document
		:class 'clear-all-command
		:initargs `(:bitmap ,(bitmap document)
			    :color ,color
			    :name ,(if (eq color :white) "Clear All" "Set All"))))

(defmethod doit ((cmd clear-all-command))
  "construct coordinate list and redraw"
  (with-slots (bitmap main-view mini-view no-rows no-columns) (document cmd) 
    (setq bitmap
	  (if (eq (color cmd) :white)
	      nil
	      ;; else set all to black
	      (loop for x from 0 to no-columns
		    append
		      (loop for y from 0 to no-rows
			    collect (list x y)))))
    (force-redraw main-view)
    (force-redraw mini-view)))

(defmethod undoit ((cmd clear-all-command))
  "reinstall old coordinate list and redraw"
  (with-slots (bitmap main-view mini-view) (document cmd)
    (setq bitmap (bitmap cmd))
    (force-redraw main-view)
    (force-redraw mini-view)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; main program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(register-application "bitedit" 'bitmap-editor "bitmap")
'(make-bitmap-editor) 
