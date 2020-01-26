;;; -*- Syntax: Common-Lisp; Package: USER; Base: 10; Mode: LISP -*-
(in-package :user)

(setq *sccs-id* "@(#)load.lisp	1.1	9/10/90")

#+lucid
(setf (symbol-function 'chdir)
      (symbol-function 'cd))

(xtk::ld "finder-shell")
(xtk::ld "finder")
(xtk::ld "graphic-editor")
(xtk::ld "clock")
(xtk::ld "graphic-output")
(xtk::ld "hello")
(xtk::ld "lisp-widgets")
(xtk::ld "micky")
(xtk::ld "text-editor")
(xtk::ld "bitmap-editor")
(xtk::ld "tetris")
(xtk::ld "hyper")
(xtk::ld "spreadsheet")
#-cmu (xtk::ld "pacmen")     ;; no light weight processes in CMU-CL 16e
(xtk::ld "chess")
(xtk::ld "drag-and-drop")
(xtk::ld "mandelbrot")
#+(or allegro-v4.0 allegro-v4.1) (xtk::ld "lisp-listener")
(#-(or lucid cmu) progn 
 #+lucid with-deferred-warnings
 #+cmu with-compilation-unit #+cmu ()
 (xtk::ld "calculator")
 (xtk::ld "calculator-shell"))
(xtk::ld "diagram-editor")   ;; new demo
