;;; -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-
(in-package :user)

(setq *sccs-id* "@(#)load.lisp	1.7 11/8/93")

#+lucid
(setf (symbol-function 'chdir)
      (symbol-function 'cd))

#+excl
(let ((excl::*print-nicknames* t))
   (declare (special excl::*print-nicknames*))
   #-(or allegro-v4.0 (and excl (version>= 4 1))) (require :pcl))

;; load all files for the GINA-System

'(load "load")

;;#+excl
;;(gc t)

(#-(or lucid cmu) progn 
 #+lucid with-deferred-warnings
 #+cmu with-compilation-unit #+cmu ()
  (xtk::ld "package")
  (xtk::ld "metainfo")
  (xtk::ld "globals")
  (xtk::ld "OS-dependent"  "metainfo")
  (xtk::ld "classes"       "metainfo")
  (xtk::ld "framework"     "metainfo" "classes")
  (xtk::ld "motif-widgets" "metainfo" "classes")
  (xtk::ld "dialogs"       "metainfo" "classes")
  (xtk::ld "background"    "metainfo" "classes")
  (xtk::ld "views"         "metainfo" "classes")
  (xtk::ld "commands"      "metainfo" "classes")
  (xtk::ld "browser-shell")                      ;; new in Gina 2.2
  (xtk::ld "browser-subclass-dialog" )           ;; new in Gina 2.2
  (xtk::ld "browser"       "metainfo" "classes"))






