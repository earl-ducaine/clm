;;; -*- Mode:LISP;Syntax: Common-Lisp;Package: gina;Base:10-*-
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;     Global variables and of GINA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :gina)

(setq *sccs-id* "@(#)globals.lisp	1.20  11/8/93")

(ginachapter "Some Global Variables of GINA")

;; if you start a CLM-demon also set XBMLANGPATH, so that CLM can find bitmaps!
(defginavar *bitmap-directories*
  (description "list of directories where bitmaps are searched"
	       :default "DEPENDS ON YOUR SITE"
	       :modify "during the installation of GINA"))
#-genera
(setq *bitmap-directories* '("/vol/gina/bitmaps" "/vol/X11R5/include/bitmaps" "/usr/include/X11/bitmaps"))
#+genera
(setq *bitmap-directories* '("gina-host:bitmaps"))

(defginavar *default-toolkit-host*
  (description "name of host where toolkit server runs"
	       :default "automatically start server on local host"
	       :modify "if the server runs on a remote host"))
(setq *default-toolkit-host* nil)

'(setq *default-toolkit-host* "gina")

(defginavar *default-display-host*
  (description "name of host where X server runs"
	       :default "the local host"
	       :modify "if the server runs on a remote host"))
(setq *default-display-host* "localhost")

'(setq *default-display-host* "gina")

(defginavar *default-display-number*
  (description "number of display on host where X server runs"
	       :default "0"
	       :modify "if the server runs on a display other than 0"))
(setq *default-display-number* 0)

(defginavar *default-screen-number*
  (description "number of screen on host where X server runs"
	       :default "0"
	       :modify "if the server runs on a screen other than 0"))
(setq *default-screen-number* 0)



;; ---------------------------------------------------------------------------
;; these globals are needed to test routines from the editor
(defginavar *application*
  (description "in each process a pointer to its application object"
	       :default "the own application object"
	       :modify "inside the editor to focus on another application")
  (comment 
   "The macro with-application-stopped sets the global copy of this variable"))

(defginavar *document*
  (description "the currently focussed document"
	       :default nil
	       :modify "by calling 'Focus this application' in the Debug menu")
  (comment "Used for debugging only"))

(defginavar *display*
  (description "in each process a pointer to a CLX display structure"
	       :default "the own CLX display structure"
	       :modify "inside the editor to focus on another application")
  (comment 
   "The macro with-application-stopped sets the global copy of this variable"))

(defginavar *running-applications* 
  (description 
   "the list of all running applications, maybe on different displays"
	       :modify "using the function focus-application"))
(unless (boundp '*running-applications*)
  (setq *running-applications* nil))
'(print *running-applications*)

;; example value of *running-applications*:
;(#<Application Demo Dialog on tom> #<Application Hello2 on tom>
; #<Application Graphic Editor on tom> #<Application Finder on tom>
; #<Application Finder on lispm-7>)


(defginavar *inspect-click*
  (description "flag if inspect click is generally enabled"
	       :default "inspect click enabled"
	       :modify "to turn off this debugging facility"))
(setq *inspect-click* t)
'(setq *inspect-click* nil)

(defginavar *debug-menu*
  (description "flag if debug menu is generally present"
	       :default "debug menu is present"
	       :modify "to turn off this debugging facility"))
(setq *debug-menu* t)

(defginavar *clx-synchronous*
  (description "flag if CLX is generally run in synchronous mode"
	       :default "not synchronous"
	       :modify "to turn on this debugging facility"))
(setq *clx-synchronous* nil)
'(setq *clx-synchronous* t)

;(defginavar *xtk-synchronous*
;  (description "flag if toolkit connection is generally run in synchronous mode"
;	       :default "not synchronous"
;	       :modify "to turn on this debugging facility"))
;(setq *xtk-synchronous* nil)
;'(setq *xtk-synchronous* t)


(defginavar *drag-n-drop-transfer-value*
  (description
   "value to be transferred between applications by drag-and-drop"))

(defginafun make-object (class more-inits &rest init-plist)
  (description "call make-instance with a list of additional inits"
	       :called-by-gina "whenever any object is created"
	       :called-by-application 
	       "in the constructor of a new class with no superclasses"
	       :result "the newly created object")
  (apply #'make-instance class (append init-plist more-inits)))

;; allow for conditonal compilation using #+gina2 or #+gina2.3
(pushnew :gina2.3 *features*)
(pushnew :gina2 *features*)
