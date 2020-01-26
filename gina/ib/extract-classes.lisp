;;; -*- Mode:LISP;Syntax: Common-Lisp;Package: USER ;Base:10-*-
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

(in-package :user)

#+excl
(eval-when (compile load eval)
  (require :loop))

(defun extract-ib-class-definitions (output-file &rest input-files &aux def)
  "extract class-definitions from input files and write them into output-file"
  (with-open-file (ostream output-file :direction :output :if-exists :supersede)
      (format ostream 
              ";;; -*- Mode:LISP;Syntax: Common-Lisp;Package: ib ;Base:10-*-~%")
      (format ostream
              "~%;;; Copyright 1990 GMD~%~%")
      (format ostream "(in-package :ib)~%~%")
      (loop for input-file in input-files
	    do (setf def nil)
               (format t "; ~a~%" input-file)
               (with-open-file (istream input-file :direction :input)
		 (loop as form = (read-line istream nil :eof)
		       while (not (eq form :eof))
                       for was-def = def
                       do (if def
                            (setf def (is-non-empty-line form)) 
                            (setf def (is-definition-line form)))
                          (if def
                            (write-line (subseq form def) ostream)
                            (if was-def
                              (write-line "" ostream))))))))
                            
(defun is-definition-line (form)
  (loop for prefix in '("'(defclass" "'(defginaclass" 
                        "'(defmacro" "'(defginamacro")
        for result = (string<= prefix form)
        when (and result
                  (= result (length prefix)))
        do (return-from is-definition-line 1))
  nil)

(defun is-non-empty-line (form)
  (if (equal (string-right-trim '(#\Tab #\Space #\Newline) form) "") nil 0))

'(is-definition-line "'(defclass hallo")
  
(extract-ib-class-definitions "classes.lisp"
    "documents.lisp"
    "counter.lisp"
    "infos.lisp"
    "resources.lisp"
    "constraints.lisp"
    "class-tree.lisp"
    "main-view.lisp"
    "proto-view.lisp"
    ;"sheet-view.lisp"
    ;"slot-objects.lisp"
    "plates.lisp"
    "info-cmds.lisp"
    "mouse-cmds.lisp"
    "tree-cmds.lisp"
    "lisp-code.lisp"
    "cplus-code.lisp"
    "test-code.lisp"
    "main-window.lisp"
    "coder-window.lisp"
    "tree-window.lisp"
    ;"sheet-window.lisp"
    "dialogs.lisp"
    "color.lisp"
    "widget-drag.lisp"
    "grabber.lisp"
    "interface.lisp"
    "form-view.lisp"
    "form-cmds.lisp"
    "prefs.lisp"
    )
