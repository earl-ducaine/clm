;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: chess ;Base: 10 -*-
;;;
;;; copyright 1991 GMD (German National Research Center for Computer Science)
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
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING
;;; OUT OF OR INCONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;;
;;; Authors: Project GINA (spenke@gmd.de)
;;;          P.O. Box 1316
;;;          D-5205 Sankt Augustin 1
;;;

(in-package :gina)
(defginapackage :chess)
(in-package :chess)
(setq *sccs-id* "@(#)chess.lisp	1.17 11/9/92")

(defvar *gnuchess-pathname*)
(defvar *square-size*) ;; size of one chess field  (adjust to chesspiece)
(defvar *top-margin* ) ;; size of margin top & bot (where letters are shown)
(defvar *left-margin*) ;; size of margin left & ri (where numbers are shown)

(setq *gnuchess-pathname*      "/home/cici/Gina/Gnu/gnuchessr")
(setq *square-size* 90)
(setq *top-margin*  55)
(setq *left-margin* 30)

(defvar *shadow-offset-down*)
(defvar *shadow-offset-up*)
(defvar *icon-offset-down*)
(defvar *icon-offset-up*)

(setq *shadow-offset-down* 4)
(setq *shadow-offset-up*  10)
(setq *icon-offset-down*   5)
(setq *icon-offset-up*     0)

;; Load images while loading file into lisp
(defun read-icon (name)
  (xlib:read-bitmap-file (find-bitmap (format nil "~a.bitmap" name))))
(defun read-mask (name)
  (xlib:read-bitmap-file (find-bitmap (format nil "~a_mask.bitmap" name))))

(defvar *pawn-icon-image*)   (setq *pawn-icon-image*   (read-icon "pawn"))
(defvar *pawn-mask-image*)   (setq *pawn-mask-image*   (read-mask "pawn"))
(defvar *rook-icon-image*)   (setq *rook-icon-image*   (read-icon "rook"))
(defvar *rook-mask-image*)   (setq *rook-mask-image*   (read-mask "rook"))
(defvar *knight-icon-image*) (setq *knight-icon-image* (read-icon "knight"))
(defvar *knight-mask-image*) (setq *knight-mask-image* (read-mask "knight"))
(defvar *bishop-icon-image*) (setq *bishop-icon-image* (read-icon "bishop"))
(defvar *bishop-mask-image*) (setq *bishop-mask-image* (read-mask "bishop"))
(defvar *queen-icon-image*)  (setq *queen-icon-image*  (read-icon "queen"))
(defvar *queen-mask-image*)  (setq *queen-mask-image*  (read-mask "queen"))
(defvar *king-icon-image*)   (setq *king-icon-image*   (read-icon "king"))
(defvar *king-mask-image*)   (setq *king-mask-image*   (read-mask "king"))

;; abbreviation characters for naming commands and use within files:
(defvar *king-char*)  (setq *king-char*   "K") 
(defvar *queen-char*) (setq *queen-char*  "D")
(defvar *rook-char*)  (setq *rook-char*   "T")
(defvar *bishop-char*)(setq *bishop-char* "L")
(defvar *knight-char*)(setq *knight-char* "S")
(defvar *pawn-char*)  (setq *pawn-char*   " ")

(defun play (sound-name &key (volume 40))
   "submit a play command to unix"
   ;; 1<=volume<=100
   #-genera
   (shell-command
    (format nil "/usr/demo/SOUND/play -v~d /usr/demo/SOUND/sounds/~a.au &"
	    volume sound-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class chess-application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass chess-application (application)
  (;; overrides
   (name          :initform "Chess"               :allocation :class)
   (document-type :initform 'chess-document       :allocation :class)
   (signature     :initform "chess"               :allocation :class)
   (file-type     :initform "chess"               :allocation :class))
  (:documentation "a simple chess board"))

(defun make-chess-application ()
  "start the chess-application"
  (make-application :class 'chess-application))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; these forward declarations are necessary because of PCL only !!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass chesspiece-mover (object-mover)
  ;; allocate call-doit as :allocation instance for using it indiviually
  ((call-doit :allocation :instance)
   (name :initform "move")
   ;; animation wanted for automatic move by computer
   (animate-move :accessor animate-move :initform nil)
   ;; remember pairs of coordinates for source and target
   (from-line :accessor from-line)
   (from-column :accessor from-column)
   (to-line :accessor to-line)
   (to-column :accessor to-column)
   ;; remember which piece taken by this move:
   (taken-piece :accessor taken-piece :initform nil)
   ;; remember which piece was exchanged at promotion:
   (promote-to :accessor promote-to :initform nil)
   ;; for gnu: flag if called undoit by gnu:
   (undoit-called-by-gnu :accessor undoit-called-by-gnu :initform nil)))

(defclass chess-view (view)
  ((top-margin  :accessor top-margin
		:initform *top-margin*)
   (left-margin :accessor left-margin
		:initform *left-margin*)
   (size :accessor size :initform *square-size*
	 :documentation "size of a square i.e. one chess field")
   (bottom-color :accessor bottom-color :initform :white)
   (whos-turn :accessor whos-turn :initform :white)
   ;; :allocation :class for gray-pixmap leads to problems when
   ;; running this application from same lisp world on different displays!
   (gray-pixmap :accessor gray-pixmap)
   )
   (:documentation "a view with chess board and movable chess pieces"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class chess-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass chess-document (document)
  (;; instance-variables
   ;; needs width +15 and height +45 against chess-view without srolling
   (shell-width  :initform (+ 35 *left-margin* (* *square-size* 8)))
   (shell-height :initform (+ 65 *top-margin*  (* *square-size* 8)))
   (promote-dialog-box :initform nil)
   (unix-input-stream   :accessor unix-input-stream :initform nil)
   (unix-output-stream  :accessor unix-output-stream :initform nil)
   (computer-play-toggle :accessor computer-play-toggle)
   (computer-animation-toggle :accessor computer-animation-toggle)
   (status-display :accessor status-display)
   (my-move-display :accessor my-move-display)
   (level-group :accessor level-group)
   ;; for use within revert command - see method irreversible-change:
   (suppress-clear-commands :accessor suppress-clear-commands :initform nil))
  (:documentation "chess application dependent document type"))

(defmethod write-to-stream ((doc chess-document) stream
			    &aux (moves-done (length (undo-commands doc))))
  "write all moves in chess notation to the specified stream"

  ;; first write number of full moves (i.e. number of lines following):
  (format stream "~d~%" (floor (+ (length (undo-commands doc))
				  (length (redo-commands doc))
				  1)
			       2))

  ;; write moves in chess notation - using command names:
  (loop for command in (append (reverse (undo-commands doc))
			       (redo-commands doc))
    for half-move-number from 1
    for move-number from 1 by 0.5
   do
    (if (oddp half-move-number)
	;; white move
	(format stream "~3d.  ~10a~a" (round move-number) (name command)
		;; mark end of done moves with *
		(if (= moves-done half-move-number) "*" " "))
        ;; black move
        (format stream " ~10a~a~%" (name command)
		;; mark end of done moves with *
		(if (= moves-done half-move-number) "*" " "))))
  (format stream "~%"))

(defun number-of-char (char)
  (+ 1 (- (char-code char) (char-code #\a))))

(defmethod read-from-stream ((doc chess-document) stream
			     &aux number-of-lines the-line line-length
			     (last-done 0)
			     from-column from-line
			     to-line to-column)
  "read a game in chess notation from the specified stream"

  ;; clear and re-setup the board when reverting to file version:
  (when (undo-commands doc)
    (setf (undo-commands doc) nil)
    (clear-chessboard doc)
    (setup-chessboard doc)
    ;; this is for method irreversible-change; see there
    (setf (suppress-clear-commands doc) t))

  (setq number-of-lines (read stream))
  
  ;; read moves in following notation:
  ;; positions are fixed, surrounding text does not matter
  ;;          1         2
  ;; 12345678901234567890123456 (character-positions)
  ;;        d2-d4      Sb8-c6   (character for piece ignored)
  ;;        g1xf3       a2-a1:D (promote to one of *xxxx-char*)
  ;;        0-0-0        0-0    (digit 0 for castling; again: position!)

  (loop for i from 1 to number-of-lines
   do (setq the-line (read-line stream))
      (setq line-length (length the-line))
    ;;(format t ">~a~%" the-line)
    (loop as promote-to-char = nil
          for move in '(:white :black)
          for p    in '(7 19) ; character position minus one for white/black
     do (when (< line-length p) (return nil)) ;; no more input for move
      ;; first check for * as delimiter between done and undone moves:
      (when (and (> line-length 16) (eq #\* (char the-line 16)))
	(setq last-done (1- (* i 2))))
      (when (and (> line-length 28) (eq #\* (char the-line 28)))
	(setq last-done (* i 2)))

      (cond
       ((and (> (length the-line) (+ p 4))
	     (equal (subseq the-line  p (+ p 5)) "0-0-0")) ;; long castling
	(setq from-column 5)
	(setq to-column   3)
	(setq from-line (if (eq move :white) 1 8))
	(setq to-line from-line))
       ((and (> (length the-line) (+ p 3))
	     (equal (subseq the-line p (+ p 4)) " 0-0")) ;; short castling
	(setq from-column 5)
	(setq to-column   7)
	(setq from-line (if (eq move :white) 1 8))
	(setq to-line from-line))
       (t                                             ;; normal move: d2-d4
	(setq from-column (number-of-char (char the-line p))) (setq p (1+ p))
	(setq from-line   (parse-integer the-line :start p :end (1+ p)))
	(setq p (+ 2 p)) 
	(setq to-column   (number-of-char (char the-line p))) (setq p (1+ p))
	(setq to-line     (parse-integer the-line :start p :end (1+ p)))
	(setq p (+ 1 p))                              ;; promotion: a2-a1:D
	(when (and (> line-length p) (eq #\: (char the-line p)))
	  (setq p (1+ p)) (setq promote-to-char (subseq the-line p (1+ p))))))

      (submit-move-command doc from-line from-column to-line to-column
			   :promote-to-char promote-to-char)
      ))
  ;; undo number of commands minus last-done
  (loop repeat (- (length (undo-commands doc)) last-done)
    do (undo doc)))

(defmethod submit-move-command ((doc chess-document)
				from-line from-column to-line to-column
				&key (promote-to-char nil)
				     (animate nil)
				&aux moved-piece command)
  "called by read-from-stream and make-computer-move"
  (setq moved-piece
    (loop for piece in (view-objects (main-view doc))
     when (and (eq (my-line piece) from-line)
	       (eq (my-column piece) from-column))
     do (return piece)))
  
  (if (not moved-piece)
	(format t "~%Internal error: No piece found at ~a ~a!~%"
		from-column from-line)
	(progn
	  (setq command
	    (make-command doc :immediately-submit nil
			  :class `chesspiece-mover))

	  ;; set slots of command
	  (setf (move-object command) moved-piece)
	  (setf (from-line command) from-line)
	  (setf (from-column command) from-column)
	  (setf (to-line command) to-line)
	  (setf (to-column command) to-column)
	  (setf (animate-move command) animate)
	  
	  (when promote-to-char         ;; if there was a promotion:
	    (setf (promote-to command)  ;; generate a piece:
	      (generate-chesspiece promote-to-char
				   (if (eq to-line 1) :black :white))))
	    
	  ;; for redo to scroll to appropriate place
	  (setf (last-x command) 1)
	  (setf (last-y command) 1)

	  (submit command))))

(defmethod irreversible-change ((doc chess-document))
  "override to suppress clearing all command objects when reverting"
  ;; the flag suppress-clear-commands is set in this application
  ;; within read-from-stream when reverting to file contents
  ;; to prohibit the default clearing of all commands by the gina method
  ;; revert. Method revert calls irreversible-change.
  (with-slots (suppress-clear-commands) doc
    (if suppress-clear-commands
	;; then: do nothing but resetting the flag.
	(setq suppress-clear-commands nil)
        ;; else: (normal case) call original method.
        (call-next-method))))


(defmethod create-windows ((doc chess-document) &aux scroller column-widget)
  "create the windows belonging to this document"
  (with-slots (main-shell main-view computer-play-toggle
			  computer-animation-toggle level-group
	       status-display my-move-display) doc
    (setq main-shell (make-document-shell doc))
    (setq scroller   (make-scroller main-shell))
    (setq main-view  (make-chess-view scroller doc))

    ;; add application specific commands
    (add-menu-command (main-menu main-shell) "Chess" "Change Sides"
		      (make-callback 'change-sides main-view))
    (insert-menu-entry (main-menu main-shell) "Chess" "Button Animation"
		       (setq computer-animation-toggle
			 (make-toggle-entry "Animate Computer Moves"
					    nil :value t)))
    (insert-menu-entry (main-menu main-shell) "Chess" "Button Outline"
		       (make-toggle-entry
			"Drag as Outline"
			(make-callback 'toggle-draw-outline main-view)
                        :value nil))
    (insert-menu-entry (main-menu main-shell) "Chess" "Level Group"
		   (setq level-group
		       (make-radio-group-entry "Level (Time per Move)"
			  '(("0.1 sec" (600  1))
			    ("  5 sec" (60   5))
			    (" 15 sec" (60  15))
			    (" 30 sec" (60  30))
			    (" 45 sec" (40  30))
			    (" 90 sec" (40  60))
			    ("  3 min" (40 120))
			    ("  6 min" (40 240))
			    (" 15 min" ( 1  15))
			    (" 10 h"   ( 1 600)))
			    (make-callback 'set-gnus-level doc))))

    (setup-chessboard doc)

    ;; add displays and button for playing against computer
    (setq column-widget
      (make-row-column main-view :orientation :horizontal :spacing 6))
    (move column-widget 35 0)
    (setq my-move-display
      (make-labeled-text column-widget "My Move" :value ""
			 :editable nil :columns 12))
    (setq status-display
      (make-labeled-text column-widget "State" :value "Human/Human"
			 :editable nil :columns 20))
    (setq computer-play-toggle
      (make-toggle-button column-widget "Play Against Computer" :value nil 
	 :value-changed-callback (make-callback 'toggle-play-computer doc)))))

(defmethod setup-chessboard ((doc chess-document)
			  &aux (view (main-view doc))
			  piece)
  "Setup method for initial chess board"

  ;; Who is on the turn must be set for reverting from file:
  (setf (whos-turn view) :white)
  (loop
    for base-line in '(1 8)
    for pawn-line in '(2 7)
    for color in '(:white :black)                          ;; for each side:
   do
    (loop for column from 1 to 8
     as piece = (generate-chesspiece *pawn-char* color)    ;; 8 pawns
     do (install piece view 0 0 :redraw nil)
        (place piece pawn-line column))

    (loop for column in '(2 7)
     as piece = (generate-chesspiece *knight-char* color)  ;; two knights
     do (install piece view 0 0 :redraw nil)
        (place piece base-line column))

    (loop for column in '(3 6)
     as piece = (generate-chesspiece *bishop-char* color)  ;; two bishops
     do (install piece view 0 0 :redraw nil)
        (place piece base-line column))

    (loop for column in '(1 8)
     as piece = (generate-chesspiece *rook-char* color)    ;; two rooks
     do (install piece view 0 0 :redraw nil)
        (place piece base-line column))

    (setq piece (generate-chesspiece  *queen-char* color)) ;; one queen
    (install piece view 0 0 :redraw nil)
    (place piece base-line 4)

    (setq piece (generate-chesspiece  *king-char* color))  ;; one king
    (install piece view 0 0 :redraw nil)
    (place piece base-line 5)))

(defmethod clear-chessboard ((doc chess-document))
  (loop for piece in (view-objects (main-view doc))
    do (deinstall piece)))

(defmethod toggle-play-computer ((doc chess-document) value)
  "provides computer moves from gnu chesstools"
  (declare (ignore value))
  (if (unix-input-stream doc)
      (switch-gnu-off doc :send-quit t)
      (switch-gnu-on doc)))

(defmethod set-gnus-level ((doc chess-document) value
			   &optional (old-value nil))
  "sets gnu to desired value, a list of number of moves, within minutes"
  (declare (ignore old-value))
  (with-slots (unix-input-stream) doc
     (when unix-input-stream
     ;;(format t           "level ~a ~a 0~%" (first value) (second value))
       (format unix-input-stream "level ~a ~a 0~%" (first value) (second value))
       (force-output unix-input-stream))))

(defmethod switch-gnu-off ((doc chess-document) &key (send-quit nil))
  "sends quit to gnu when disired, closes the stream and sets vars"
  (with-slots (unix-input-stream unix-output-stream) doc
       (when send-quit
	 (format unix-input-stream "quit~%") (force-output unix-input-stream))
       (if (eq unix-input-stream unix-output-stream)
	   (close unix-input-stream)
	   (progn (close unix-input-stream) (close unix-output-stream)))
       (setq unix-input-stream nil)
       (setq unix-output-stream nil)
       
       (setf (value (status-display doc)) "Human/Human")
       (setf (value (computer-play-toggle doc)) nil)))
 
(defmethod switch-gnu-on ((doc chess-document) &aux line)
  "provides computer moves from gnu chesstools"
  #-(or excl lucid cmu)
  (warning-dialog
 "Sorry, connection to gnu-chess only implemented for Allegro, Lucid, and CMU"
   :document doc)
  #+(or excl lucid cmu)
  (if (not (file-exists *gnuchess-pathname*))
      (warning-dialog
       (format nil "Could not find ~a!" *gnuchess-pathname*)
       :document doc)
      (with-clock-cursor
       (with-slots (unix-input-stream unix-output-stream
		    status-display level-group main-view) doc
        (with-slots (whos-turn bottom-color) main-view

	 #+(or lucid excl)
	     (multiple-value-bind (io-stream ignore #+lucid ignore2 process-id)
	      #+excl (excl::run-shell-command
	       (format nil "exec ~a" *gnuchess-pathname*)
	       :wait nil :input :stream :output :stream :error-output nil)
	  
	      #+lucid (run-program *gnuchess-pathname*
	       :wait nil :input :stream :output :stream :error-output nil)
	       
	    (declare (ignore ignore #+lucid ignore2))
	    (format t "Process: ~a, Stream: ~a~%" process-id io-stream)
	    (setq unix-input-stream io-stream)
	    (setq unix-output-stream io-stream))
	 
	 #+cmu (let ((process-structure
		 (extensions:run-program *gnuchess-pathname* nil
	          :wait nil :input :stream :output :stream :error nil)))
	         (format t "Process: ~a~%"
		   (extensions:process-pid process-structure))
		 (setq unix-input-stream
		   (extensions:process-input process-structure))
		 (setq unix-output-stream
		   (extensions:process-output process-structure)))
	 
	(setq line (read-line unix-output-stream nil :eof))
	(format t "~a~%" line) ; says "Chess"
	(format unix-input-stream "beep~%")	; do not send beep character
	(force-output unix-input-stream)
	(set-gnus-level doc (value level-group))
	(when (undo-commands doc)
	  ;; there are already some movements done; set up new board:
	    (when (eq whos-turn bottom-color)
	      ;; its humans turn:
	      ;; we have to force a dummy move of gnu to set gnus state
	      (format unix-input-stream "~a~%"
		      (if (eq :white bottom-color) "black" "white"))
	      (force-output unix-input-stream)
	      (read-line unix-output-stream nil :eof)  ;; protocoll switch
	      (read-line unix-output-stream nil :eof)) ;; his move
	    
	    (format unix-input-stream "edit~%") (force-output unix-input-stream)
	    (read-line unix-output-stream nil :eof) ; . exit to main
	    (read-line unix-output-stream nil :eof) ; # clear board
	    (read-line unix-output-stream nil :eof) ; c change sides
	    (read-line unix-output-stream nil :eof) ; enter piece & location:
	    (format unix-input-stream "#~%") ;; clear board
	    (force-output unix-input-stream)
	    (loop for piece in (view-objects (main-view doc))
	     do (when (eq :white (color piece))
		  (setq line  (format nil "~a~a~a~%"
				(if (equal "knight" (name piece)) "n"
				  (char (name piece) 0))
				(char "*abcdefgh" (my-column piece))
				(my-line piece)))
		  ;;(format t "~a" line)
	          (format unix-input-stream "~a" line)
		  (force-output unix-input-stream))) ;; enter white pieces
	    
	    ;;(format t "c~%")
	    (format unix-input-stream "c~%") ;; change color for entering
	    (force-output unix-input-stream)
	    (loop for piece in (view-objects (main-view doc))
	      do (when (eq :black (color piece))
		   (setq line  (format nil "~a~a~a~%"
				(if (equal "knight" (name piece)) "n"
				  (char (name piece) 0))
				(char "*abcdefgh" (my-column piece))
				(my-line piece)))
	          ;;(format t "~a" line)
	          (format unix-input-stream "~a" line)
		  (force-output unix-input-stream))) ;; enter black pieces
	    ;;(format t ".~%")
	    (format unix-input-stream ".~%") ;; end edit mode
	    (force-output unix-input-stream)) 

	  (setf (value status-display) "Your Move")

	  (when (not (eq whos-turn bottom-color))
	    ;; it is computers turn. force a move of correct color:
	    ;; (format t "~a~%" (if (eq :white whos-turn) "white" "black"))
	    (format unix-input-stream "~a~%"
		    (if (eq :white whos-turn) "white" "black"))
	    (force-output unix-input-stream)
	    (read-line unix-output-stream nil :eof)
	    (make-computer-move doc)))))))


(defmethod make-computer-move ((doc chess-document)
			       &aux thinking-time)
  "expects move from gnu chesstools, reads it and submits a command"
  (with-slots (status-display my-move-display level-group) doc
     ;; update view before thinking:
     (xtk:with-immediate-update-enabled 
        (setf (value status-display) "My Move...")
        (setf (value my-move-display) "..."))
     (xlib:display-force-output *display*)
     (setq thinking-time (/ (* 60  (second (value level-group)))   ;; #minutes
			    (float (first  (value level-group))))) ;; #moves
     (if #-cmu (< thinking-time 6) #+cmu t ;; cmu has no background processes
	 (with-clock-cursor (wait-for-computer-move doc))
         #-cmu (with-progress-bar
	  (doc :title "Thinking" :message "Please Wait ..."
	   :abortable nil :centered nil :with-scale nil)
	  (wait-for-computer-move doc)))))


(defmethod wait-for-computer-move ((doc chess-document) 
			       &aux answer p
			            from-column from-line
			            to-column to-line)
  "expects move from gnu chesstools, reads it and submits a command"
  (with-slots (unix-output-stream status-display my-move-display
			   computer-animation-toggle main-view) doc
      (with-slots (bottom-color) main-view
	(multiple-value-bind (the-line eof)
	    (read-line unix-output-stream nil :eof)
	(when (or eof (eq answer :eof))
	      (format t "Internal error: unexpected eof after last move! ")
	      (if eof (format t "eof=TRUE~%")
                      (format t "answer=~a~%" answer))
	      (switch-gnu-off doc))
	(setq answer the-line))

     (xlib:bell *display*)
     ;; (format t "Computers Move: ~s (~a)~%" answer (length answer))
     ;; if we do not copy the string,
     ;; one mystic character at the end reaches the widget!
     (setf (value my-move-display) (copy-seq answer))
     ;;(format t "~a~%" (first (get-motif-resources my-move-display :value)))
     (setf (value status-display) "Your Move")

     (when unix-output-stream
       (when (> (length answer) 8)
	 ;; format of gnus move:                 "1. ... a6e2"
	 ;; if gnu can not move its protocoll is "30. ... "
	 (setq p (- (length answer) 4))
	 (setq from-column (number-of-char (char answer p))) (setq p (1+ p))
	 (setq from-line   (parse-integer answer :start p :end (1+ p)))
	 (setq p (+ 1 p)) 
	 (setq to-column   (number-of-char (char answer p))) (setq p (1+ p))
	 (setq to-line     (parse-integer answer :start p :end (1+ p)))
 
	 (submit-move-command doc from-line from-column to-line to-column
		:animate (value computer-animation-toggle)))

       ;; any more output from chess program means end of game:
       (setq answer (read-char-no-hang unix-output-stream nil :eof))
       ;;(format t "test: (~a) " answer)
       (when (and answer (not (eq answer :eof)))
	 (unread-char answer unix-output-stream)
	 (setq answer (read-line unix-output-stream nil :eof))
	 (when (or (eq answer :eof)
		   (equal answer "Black")
		   (equal answer "White"))

	   (switch-gnu-off doc)
	   ;; update view before displaying dialog
	   (xlib:display-force-output *display*)
	   (if (eq answer :eof)
	       (warning-dialog "Game Over!" :document doc :modal nil)
	       (if (or (and (equal answer "Black") (eq bottom-color :black))
		       (and (equal answer "White") (eq bottom-color :white)))
		   (progn (warning-dialog "Congratulation, you won!"
				   :document doc :modal nil)
			  (play "gong"))
		   (progn (warning-dialog "Sorry, I guess I won!"
				   :document doc :modal nil)
			  (play "laugh"))))))))))
 


(defmethod do-close ((doc chess-document))
  "calls do-close of document and closes unix-streams when left open"
  (when (and (not (eq :cancel (call-next-method)))
             (unix-input-stream doc))
    (switch-gnu-off doc :send-quit t)))

    
		     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class chess-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass chess-view (view)
  ((top-margin  :accessor top-margin
		:initform *top-margin*)
   (left-margin :accessor left-margin
		:initform *left-margin*)
   (size :accessor size :initform *square-size*
	 :documentation "size of a square i.e. one chess field")
   (bottom-color :accessor bottom-color :initform :white)
   (whos-turn :accessor whos-turn :initform :white)
   ;; :allocation :class for gray-pixmap leads to problems when
   ;; running this application from same lisp world on different displays!
   (gray-pixmap :accessor gray-pixmap)
   )
   (:documentation "a view with chess board and movable chess pieces"))

(defun make-chess-view (parent doc)
  "create a new chess-view"
  (make-view parent :document doc :class 'chess-view
	     :width  (+ 20 *left-margin* (* *square-size* 8))
	     :height (+ 20 *top-margin*  (* *square-size* 8))
	     :double-buffering t))


(defmethod draw ((view chess-view) count x y width height
		 &aux (filled nil) (size (size view))
		 (letters '("A" "B" "C" "D" "E" "F" "G" "H"))
		 (numbers '("1" "2" "3" "4" "5" "6" "7" "8")))
  "draw empty chess board"
  (declare (ignore count))
  ;; (format t "~%~a (~a,~a) (~a,~a): " count x y width height)

  ;; show whos-turn it is:
  (draw-arc view 10 10 20 20 0 (* 2 pi) (eq :black (whos-turn view)))

  ;; draw black fields of chess board
  (loop for line from 0 to 7
   do
    (loop for column from 0 to 7
     as rx1 = (+ (left-margin view) (* column size))
     as ry1 = (+ (top-margin view) (* line size))
     do (when (and filled
		   (rectangles-overlap rx1 ry1 size size x y width height))
	  (xlib:with-gcontext
	   ((gcontext view)
		:fill-style :stippled
	        :stipple (gray-pixmap view))
	    (draw-rectangle view rx1 ry1 size size filled)))
	(setq filled (not filled)))
      (setq filled (not filled)))
    ;;(xlib:display-force-output *display*)

  ;; draw frame around board:
   (draw-rectangle view (left-margin view) (top-margin view)
		   (* 8 size)(* 8 size))

   ;; draw letters
   (loop
     for column from 0 to 7
     for letter in (if (eq (bottom-color view) :white)
		       letters
		       (reverse letters))
      do
      (draw-glyphs view ;; letters on top
		   (+ (left-margin view) (round (/ size 2)) (* column size))
		   (- (top-margin view) 7) letter)
      (draw-glyphs view ;; letters on bottom
		   (+ (left-margin view) (round (/ size 2)) (* column size))
		   (+ (top-margin view) (* size 8) 15) letter))

   ;; draw numbers
    (loop
     for line from 0 to 7
     for number in (if (eq (bottom-color view) :black)
		       numbers
		       (reverse numbers))
      do
      (draw-glyphs view ;; numbers on left
		   (- (left-margin view) 15)
		   (+ (top-margin view) (round (/ size 2)) (* line size))
		   number)
      (draw-glyphs view ;; numbers on right
		   (+ (left-margin view)(* size 8) 10)
		   (+ (top-margin view) (round (/ size 2)) (* line size))
		   number)))

(defmethod toggle-whos-turn ((view chess-view))
  "set toggle slot whos-turn and invalidate indicator"
  (with-slots (whos-turn) view
     (setq whos-turn (if (eq :white whos-turn) :black :white))
     ;; indicator is circle in upper left corner of view:
     (invalidate-rectangle view 10 10 25 25)))

(defmethod toggle-draw-outline ((view chess-view) value)
  "set slot outlines in all chesspiece according to toggle-button"
  (loop for chesspiece in (view-objects view)
    do (setf (outlines chesspiece) value)))

(defmethod change-sides ((view chess-view) &aux (doc (document view)))
  "change positions of white and black chesmen"
  (setf (bottom-color view)
    (if (eq (bottom-color view) :white)
	:black
        :white))
  ;; call place with original coordinates to enforce
  ;; new placement of chessmen
  (loop for chesspiece in (view-objects view)
    do (place chesspiece
	      (my-line   chesspiece)
	      (my-column chesspiece)
	      :redraw nil))
  (force-redraw view)
  
  (with-slots (unix-input-stream unix-output-stream) doc
      (when unix-input-stream
	(format unix-input-stream "switch~%") (force-output unix-input-stream)
	(read-line unix-output-stream nil :eof)
	(make-computer-move (document view)))))


(defmethod determine-window-id :after ((view chess-view) &aux gcontext)
  "provide a gray pixmap"
  (with-slots (gray-pixmap) view
     (setq gray-pixmap
       (xlib:create-pixmap
	:width 2 :height 2
	:depth 1
	:drawable
	(x-window view)))
     (setq gcontext (xlib:create-gcontext :drawable gray-pixmap))
     ;; clear pixmap
     (xlib:draw-rectangle gray-pixmap gcontext 0 0 2 2 t)

     ;; set two points
     (setf (xlib:gcontext-foreground gcontext) 1)
     (xlib:draw-points gray-pixmap gcontext '(0 0 1 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;;  chessmen: king, queen, rook, queen, bishop, knight, pawn
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass chesspiece (movable-icon)
  (;; override class slots
   (facilities :allocation :class :initform :movable)

   ;; instance slots
   (name :accessor name :initarg :name)
   (abbreviation-character :accessor abbreviation-character
			   :initarg :abbreviation-character)
   (outlines :initform nil)
   (color :accessor color :initarg :color)
   (my-line   :accessor my-line)
   (my-column :accessor my-column)
   ))

(defmethod initialize-instance :after ((piece chesspiece) &rest initargs)
   (declare (ignore initargs))
   (setf (selected piece) (eq (color piece) :black)))

(defun make-chesspiece (name abbreviation-character color
			&key (icon-image nil) (mask-image nil)
			     (class 'chesspiece)
			&aux icon-filename-or-image mask-filename-or-image)

  ;; when images not given read from file
  (setq icon-filename-or-image
         (or icon-image
	     (format nil "~a.bitmap" name)))
  (setq mask-filename-or-image
         (or mask-image
	     (format nil "~a_mask.bitmap" name)))

  ;; (format t "icon-image-or-filename: ~a~%" icon-image-or-filename)
  (make-movable-icon icon-filename-or-image
		     :mask mask-filename-or-image
		     :shadow-offset *shadow-offset-down*
		     :top-margin    *icon-offset-down*
		     :left-margin   *icon-offset-down*
		     :right-margin  *shadow-offset-up*
		     :bottom-margin *shadow-offset-up*
		     :class class
		     :initargs `(:color ,color
				 :name  ,name
				 :abbreviation-character
				       ,abbreviation-character)))

(defun generate-chesspiece (its-character its-color)
  "make chesspiece with only abbreviation char and color given"
  (cond ((equal its-character *queen-char*)
	 (make-chesspiece "queen" *queen-char* its-color
			  :icon-image *queen-icon-image*
			  :mask-image *queen-mask-image*))
	((equal its-character *rook-char*)
	 (make-chesspiece "rook" *rook-char* its-color
			  :icon-image *rook-icon-image*
			  :mask-image *rook-mask-image*))
	((equal its-character *bishop-char*)
	 (make-chesspiece "bishop" *bishop-char* its-color
			  :icon-image *bishop-icon-image*
			  :mask-image *bishop-mask-image*))
	((equal its-character *knight-char*)
	 (make-chesspiece "knight" *knight-char* its-color
			  :icon-image *knight-icon-image*
			  :mask-image *knight-mask-image*))
	((equal its-character *king-char*)
	 (make-chesspiece "king" *king-char* its-color
			  :icon-image *king-icon-image*
			  :mask-image *king-mask-image*))
	((equal its-character *pawn-char*)
	 (make-chesspiece "pawn" *pawn-char* its-color
			  :icon-image *pawn-icon-image*
			  :mask-image *pawn-mask-image*))))

(defmethod place ((piece chesspiece) line column
		  &key (redraw t) (animate nil)
		  &aux (view (parent-view piece))
		       (size (size view))
		       (bottom-color (bottom-color view))
		       target-x target-y)
  "place the piece on board position given by (line column)"

  (setq target-x (if (eq bottom-color :white)
		     (+ (left-margin view) (* (1- column) size))
		     (+ (left-margin view) (* (- 8 column) size))))
  (setq target-y (if (eq bottom-color :white)
		     (+ (top-margin view)  (* (- 8 line) size))
		     (+ (top-margin view)  (* (1- line) size))))

  (when animate
    (rise piece)
    (let ((incr-x (/ (- target-x (x-pos piece)) 10))
	  (incr-y (/ (- target-y (y-pos piece)) 10)))
      (loop repeat 9
          for x from (+ (x-pos piece) incr-x) by incr-x
          for y from (+ (y-pos piece) incr-y) by incr-y
       do (move piece (round x) (round y) :redraw redraw)
	  (xlib:display-force-output *display*)
	  (active-wait 0.02))))

  (move piece target-x target-y :redraw redraw)
  (when animate (settle-down piece))
  (setf (my-line   piece) line)
  (setf (my-column piece) column))

(defmethod rise ((piece chesspiece))
  "increase shadow and drag icon to upper left corner"
  (with-slots (shadow-offset left-margin top-margin) piece
    (setq shadow-offset *shadow-offset-up*) ;; shadow on
    (setq left-margin   *icon-offset-up*)   ;; icon to upper left
    (setq top-margin    *icon-offset-up*)
    (force-redraw piece)))

(defmethod settle-down ((piece chesspiece) &key (redraw t))
  "set to small shadow and centered icon"
  (with-slots (shadow-offset left-margin  top-margin) piece
    (setq shadow-offset *shadow-offset-down*) ;; shadow off
    (setq left-margin   *icon-offset-down*)   ;; center icon
    (setq top-margin    *icon-offset-down*)
    (when redraw (force-redraw piece))))

(defmethod calc-line ((view chess-view) y
		      &aux (size (size view))
		      (top-margin (top-margin view))
		      (bottom-color (bottom-color view)))
  "returns the line of pixel at position y"
  (if (eq bottom-color :white)
      (- 8 (floor (- y top-margin) size))
      (+ 1 (floor (- y top-margin) size))))

(defmethod calc-column ((view chess-view) x
			&aux (size (size view))
			(left-margin (left-margin view))
			(bottom-color (bottom-color view)))
  "returns the column of pixel at position x"
  (if (eq bottom-color :white)
    (+ 1 (floor (- x left-margin) size))
    (- 8 (floor (- x left-margin) size))))

(defmethod button-press ((piece chesspiece) code repetition x y)
  "move chesspiece on button press"
  (if (and (= repetition 1)
	   (eq code :select)
	   (eq (color piece) (whos-turn (parent-view piece))))
      (make-chesspiece-mover piece x y)
    (xlib:bell *display*)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; undoable chesspiece-mover-command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


'(defclass chesspiece-mover (object-mover)
  ;; allocate call-doit as :allocation instance for using it indiviually
  ((call-doit :allocation :instance)
   (name :initform "move")
   ;; animation wanted for automatic move by computer
   (animate-move :accessor animate-move :initform nil)
   ;; remember pairs of coordinates for source and target
   (from-line :accessor from-line)
   (from-column :accessor from-column)
   (to-line :accessor to-line)
   (to-column :accessor to-column)
   ;; remember which piece taken by this move:
   (taken-piece :accessor taken-piece :initform nil)
   ;; remember which piece was exchanged at promotion:
   (promote-to :accessor promote-to :initform nil)
   ;; for gnu: flag if called undoit by gnu:
   (undoit-called-by-gnu :accessor undoit-called-by-gnu :initform nil)))

(defun make-chesspiece-mover (piece x y
			      &aux (view (parent-view piece)) the-mover)
  ;; put piece on top of all other:
  (with-slots (view-objects) view
	      (setf (view-objects view)
		(cons piece (delete piece view-objects))))

  (rise piece) ;; show feedback
  ;; launch object-mover command:
  ;; no initargs allowed for make-object-mover
  (setq the-mover (make-object-mover piece x y :class 'chesspiece-mover))
  (setf (from-line   the-mover) (my-line piece))
  (setf (from-column the-mover) (my-column piece)))

(defmethod track-mouse :after ((cmd chesspiece-mover) x y
			       &key started finished)
  "determine target fields of move"
  (declare (ignore x y started))
  (when finished
    ;; do not submit a command when chesspiece is placed on original field
    (with-slots
     (move-object new-x new-y call-doit view
		  from-line from-column to-line to-column) cmd
     (setq to-line (max 1 (min 8 (calc-line view (+ new-y 40)))))
     (setq to-column (max 1 (min 8 (calc-column view (+ new-x 40)))))
     ;(format t "(~a ~a)->(~a ~a)~%" from-line from-column to-line to-column)
     (when (and (= to-line from-line (my-line move-object))
		(= to-column  from-column (my-column move-object)))
       (place move-object to-line to-column) ; put chesspiece back on field
       (setq call-doit nil)))))

(defmethod not-submitted ((cmd chesspiece-mover))
  "switch off shadow when immediately chesspiece released"
  (settle-down (move-object cmd)))

(defun flicker-piece (piece)
       (loop repeat 2 do
	 (active-wait 0.1) 
	 (rise piece)
	 (xlib:display-force-output *display*)
	 (active-wait 0.1)  
	 (settle-down piece)
	 (xlib:display-force-output *display*)))

(defun active-wait (seconds &aux start-time)
  (setq start-time (get-internal-real-time))
  (loop while (< (get-internal-real-time) 
		 (+ start-time (* seconds internal-time-units-per-second)))))

(defmethod doit ((cmd chesspiece-mover) &aux (promote-to-char ""))
  (with-slots
   (move-object new-x new-y x-off y-off start-x start-y
		name view taken-piece promote-to animate-move
		from-column from-line to-line to-column document) cmd

   ;; switch off shadow when chesspiece is released
   (settle-down (move-object cmd))

   ;; do we take another chesspiece?
   (setq taken-piece
     (loop for chesspiece in (view-objects view)
      when (and (= to-line   (my-line   chesspiece))
		(= to-column (my-column chesspiece)))
      do (return chesspiece)))

   ;; test special move: en passant
   (when (and (equal "pawn" (name move-object))
	      (null taken-piece)
	      (not (= from-column to-column)))
     (setq taken-piece
       (loop for chesspiece in (view-objects view)
	 as taken-line = (if (= to-line 6) 5 4)
	when (and (= taken-line (my-line   chesspiece))
		  (= to-column  (my-column chesspiece)))
	do (return chesspiece))))

   ;; put moving piece on top of all other:
   (with-slots (view-objects) view
	       (setf (view-objects view)
		 (cons move-object (delete move-object view-objects))))

   ;; chesspiece should be placed correctly within field:
   (place move-object to-line to-column :animate animate-move)

   (when taken-piece
     (when (x-window view)
       (play "clink")
       (flicker-piece taken-piece))
     (deinstall taken-piece))

   ;; test special move: promotion
   (when (and (equal "pawn" (name move-object))
	      (or (= 1 to-line) (= 8 to-line)))
     (doit-promotion cmd))

   ;; set command name to something like "Kd2-d4" or "Ld2xf3" or "d7-d8:Q":
   (setq name (format nil "~a~a~a~a~a~a~a~a"
		      (abbreviation-character move-object)
		      (char "*abcdefgh" from-column) from-line
		      (if taken-piece "x" "-")
		      (char "*abcdefgh" to-column) to-line
		      (if promote-to ":" "")
		      (if promote-to (abbreviation-character promote-to) "")))

   ;; test special move: castling (modifies name of command)
   (when (and (equal "king" (name move-object))
	      (= 2 (abs (- to-column from-column))))
     (move-rook cmd :animate animate-move))

   ;; change who is on the turn:
   (toggle-whos-turn view)
   
   (with-slots (unix-input-stream unix-output-stream status-display) document
 
      (when (not unix-input-stream) ;; gnu off
	 ;; remove move information after next user move
	 (setf (value (my-move-display (document cmd))) ""))

       (when (and unix-input-stream (eq (bottom-color view)
				  (color move-object)))
	 ;; playing against gnu-chess
	 (when promote-to
	   (setq promote-to-char
	     (cond ((equal (name promote-to) "queen")  "q")
		   ((equal (name promote-to) "rook")   "r")
		   ((equal (name promote-to) "bishop") "b")
		   ((equal (name promote-to) "knight") "n"))))
	 (format unix-input-stream "~a~a~a~a~a~%"
		 (char "*abcdefgh" from-column) from-line
		 (char "*abcdefgh"   to-column)   to-line
		 promote-to-char)
	 (force-output unix-input-stream)
		 
	 (multiple-value-bind (answer eof)
	     (read-line unix-output-stream nil :eof) ;; Protocoll or "Illegal move"
	   ;;(format t "Answer: ~a~%" answer) 
	   (cond ((or eof (eq answer :eof))
		  (print "Internal error: Forced eof")
		  (switch-gnu-off document))
		 ((search "Illegal move" answer)
		  ;; after changing the time for gnu (e.g. before first move)
		  ;; the protocoll of my move contains a prompt
		  (xlib:bell *display*)
		  (setf (value status-display) "NO! Do Other Move!")
		  ;; prepare calling undo to make an undone move:
		  (setf (undoit-called-by-gnu cmd) t))))))))

(defmethod submit :after ((cmd chesspiece-mover)
			  &aux (document (document cmd)))
  "calls make-computer-move when its his turn"
  ;; not done inside doit because commands would appear in wrong order
  ;; in the history list (submit first calls doit and then register-command).

  ;; call undo of last command when gnu detected an illagal move
  ;; (flag undoit-called-by-gnu set within doit when command not yet submited)
  (when (undoit-called-by-gnu cmd)
    (undo document))

  (with-slots (unix-input-stream) document
   (when (and unix-input-stream 
	      (not (eq (whos-turn (view cmd))
		       (bottom-color (main-view document))))
	      (eq (bottom-color (main-view document))
		  (color (move-object cmd))))
     (make-computer-move document))))  

(defmethod doit-promotion ((cmd chesspiece-mover) &aux selection)
  "ask for piece to exchange and proceed the change"
  (with-slots (promote-to move-object
	       document view to-line to-column) cmd

     ;; update view before displaying dialog
     (xlib:display-force-output *display*)

     (when (not promote-to)
       (if (and (unix-input-stream document)
		(not (eq (bottom-color (main-view document))
			 (color move-object))))
	   ;; gnu always promotes to queen
	   (setq selection *queen-char*)
	   ;; else ask user:
	   (with-slots (promote-dialog-box) document
		       (when (null promote-dialog-box)
			 (setq promote-dialog-box
			   (make-promote-dialog document
						:class 'promote-dialog)))
		       (pop-up promote-dialog-box)
		       (setq selection (result promote-dialog-box))))
       (setq promote-to (generate-chesspiece selection (color move-object))))

     (when promote-to ;; (should be given at this point)
       ;; install new piece
       (install promote-to view 0 0 :redraw nil)
       (place promote-to to-line to-column)
       ;; de-install old pawn
       (deinstall move-object))))

(defmethod move-rook ((cmd chesspiece-mover)
		           &key (undoit nil) (animate nil)
			   &aux rook-from-column rook-to-column
			        new-name the-rook h)
  "move the rook on special move castling"
  (with-slots (from-line from-column to-line to-column view name) cmd
	      (cond ((= to-column 7) ;; short castling
		     (setq rook-from-column 8)
		     (setq rook-to-column 6)
		      (setq new-name "  0-0"))
		    ((= to-column 3) ;; long castling
		     (setq rook-from-column 1)
		     (setq rook-to-column 4)
		     (setq new-name " 0-0-0"))
		    (t (return-from move-rook nil)))
	      (if undoit
		  ;; swap from and to:
		  (progn (setq h rook-from-column)
			 (setq rook-from-column rook-to-column)
			 (setq rook-to-column h))
		  ;; else test whether there is a piece at rooks target:
		  (when
		    (loop for chesspiece in (view-objects view)
		     when (and (= from-line      (my-line   chesspiece))
			       (= rook-to-column (my-column chesspiece)))
		      do (return chesspiece))
		    ;; there is a piece; do not move the rook
		    (return-from move-rook nil)))

     (setq the-rook
       (loop for chesspiece in (view-objects view)
	when (and (= from-line        (my-line   chesspiece))
		  (= rook-from-column (my-column chesspiece)))
	do (return chesspiece)))

     (when the-rook
       (place the-rook to-line rook-to-column :animate animate)
       (setq name new-name))))

(defmethod undoit ((cmd chesspiece-mover))
  (when (and (unix-input-stream (document cmd))
	     (not (undoit-called-by-gnu cmd)))
    ;; invoced by  user: switch gnu-off when running
    (setf (value (status-display (document cmd))) "Human/Human")
    (setf (value (my-move-display (document cmd))) "")
    (switch-gnu-off (document cmd)))
  (setf (undoit-called-by-gnu cmd) nil)

  (with-slots (view move-object taken-piece promote-to
		    from-line from-column to-column name) cmd
    (with-slots (my-line my-column x-pos y-pos) move-object

       ;; deinstall promote-to and reinstall pawn at promotion
       (when promote-to
	 (deinstall promote-to)
	 (install move-object view 0 0 :redraw nil))

       (place move-object from-line from-column)

       ;; reinstall taken-piece:
       (when taken-piece
	 (install taken-piece view 0 0 :redraw nil)
	 (place taken-piece (my-line taken-piece) (my-column taken-piece)))

       ;; test special move: castling
       (when (or (equal name " 0-0-0")
		 (equal name "  0-0"))
	 (move-rook cmd :undoit t))

       ;; change who is on the turn:
       (toggle-whos-turn view))))

(defmethod redoit ((cmd chesspiece-mover))
  "place piece correct within field and deinstall taken piece"

  ;; when gnu running switch it off (redoing an illegal move):
  (when (unix-input-stream (document cmd))
    (setf (value (status-display (document cmd))) "Human/Human")
    (setf (value (my-move-display (document cmd))) "")
    (switch-gnu-off (document cmd)))

  (with-slots (move-object taken-piece promote-to
			   view to-line to-column from-column) cmd
    (if (not promote-to)
	;; normal case: just move piece to target position:
	(place move-object to-line to-column)
        ;; promotion: reinstall promote-to and deinstall pawn:
        (progn
	  (install promote-to view 0 0 :redraw nil)
	  (place promote-to to-line to-column)
	  ;; de-install old pawn
	  (deinstall move-object)))

    (when taken-piece
      (deinstall taken-piece :redraw t))

    ;; test special move: castling
    (when (and (equal "king" (name move-object))
	       (= 2 (abs (- to-column from-column))))
      (move-rook cmd))

    ;; change who is on the turn:
    (toggle-whos-turn view)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; main program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(register-application "chess" 'chess-application "chess")
'(make-chess-application)



;;
;; This code was generated by the GINA Interface Builder.
;;

(defclass promote-dialog
          (modal-dialog-box)
          ((row-column-1 :accessor row-column-1) (queen :accessor queen)
           (rook :accessor rook) (knight :accessor knight)
           (bishop :accessor bishop))
          (:documentation ""))

(defun make-promote-dialog (doc &key (initargs nil) (motif-resources nil)
                            (class 'promote-dialog) &aux box)
  (setq box
        (make-modal-dialog-box "Promote to..." :class class :initargs initargs
         :document doc :motif-resources motif-resources))
  (setf (row-column-1 box) (make-row-column box :orientation :horizontal))
  (setf (queen box)
        (make-push-button (row-column-1 box) "queen.bitmap"  :label-type :pixmap))
  (setf (rook box)
        (make-push-button (row-column-1 box) "rook.bitmap"   :label-type :pixmap))
  (setf (knight box)
        (make-push-button (row-column-1 box) "knight.bitmap" :label-type :pixmap))
  (setf (bishop box)
        (make-push-button (row-column-1 box) "bishop.bitmap" :label-type :pixmap))
  (define-form-constraint (row-column-1 box) :left-attachment :form
   :right-attachment :form :top-attachment :form :bottom-attachment :form)
  (setf (activate-callback (queen  box)) (make-callback 'set-queen  box))
  (setf (activate-callback (rook   box)) (make-callback 'set-rook   box))
  (setf (activate-callback (knight box)) (make-callback 'set-knight box))
  (setf (activate-callback (bishop box)) (make-callback 'set-bishop box))
  box)

;; callback methods for promote-dialog:
(defmethod set-queen  ((dialog promote-dialog))
  (setf (result dialog) *queen-char*) (pop-down dialog))
(defmethod set-rook   ((dialog promote-dialog))
  (setf (result dialog) *rook-char*) (pop-down dialog))
(defmethod set-knight ((dialog promote-dialog))
  (setf (result dialog) *knight-char*) (pop-down dialog))
(defmethod set-bishop ((dialog promote-dialog))
  (setf (result dialog) *bishop-char*) (pop-down dialog))
