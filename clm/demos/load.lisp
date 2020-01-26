(in-package :user)

(defvar *sccsid* "@(#)load.lisp	1.2 1/29/92")

(#+lucid cd #+excl chdir "../lisp")
(load "load")
(#+lucid cd #+excl chdir "../demos")

(xtk::ld "demo")

;; old stuff
;(xtk::ld "xclock")
;(xtk::ld "xprocesses")
;(xtk::ld "performance")
;(xtk::ld "clm-test")
;(xtk::ld "alert")
;(xtk::ld "app")
;(xtk::ld "bitmaps")
;(xtk::ld "color")
;(xtk::ld "cursor")
;(xtk::ld "gina-view")
;(xtk::ld "list")
;(xtk::ld "list-func")
;(xtk::ld "pusht")
;(xtk::ld "raise")
;(xtk::ld "sample-app")
;(xtk::ld "scrollbar")
;(xtk::ld "text-sel")
;(xtk::ld "update")
