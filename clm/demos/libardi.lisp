;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:(user (lisp)); Base:10 -*-

(in-package 'user)
(use-package 'xtk)

(defvar *sccsid* "@(#)libardi.lisp	1.3 11/27/91")


#|
The following entries are required in the application's defaults file:
(e.g. $HOME/.Xdefaults or $XAPPLRESDIR'Menu')

menu*file_cascade.labelString: File
menu*file_menu.new.labelString: New
menu*file_menu.load.labelString: Load
menu*file_menu.save.labelString: Save

menu*edit_cascade.labelString: Edit
menu*edit_menu.search.labelString: Search
menu*edit_menu.replace.labelString: Replace

menu*search_menu.forward.labelString: Forward
menu*search_menu.backward.labelString: Backward
menu*search_menu.round.labelString: Round

|#

'(run-motif-application 'menu :application-name "menu"
 :application-class "Menu")

(defun menu ()
 (let* ((shell (create-application-shell))
        (main (create-main-window shell "main"))
        (mb (create-menu-bar main "menu_bar"))
        (ww (create-drawing-area Main "view" :height 400 :width 400))
        (file-menu (create-pulldown-menu mb "file_menu"))
        (file-cascade (create-cascade-button mb "file_cascade"
					     :sub-menu-id file-menu))
        (new-button (create-push-button file-menu "new"))
        (load-button (create-push-button file-menu "load"))
        (save-button (create-push-button file-menu "save"))
	
        (edit-menu (create-pulldown-menu mb "edit_menu"))
        (edit-cascade (create-cascade-button mb "edit_cascade"
					     :sub-menu-id edit-menu))
        (search-menu (create-pulldown-menu edit-menu "search_menu"))
        (search-cascade (create-cascade-button edit-menu "search"
					       :sub-menu-id search-menu))
        (forward-button (create-push-button search-menu "forward"))
        (backward-button (create-push-button search-menu "backward"))
        (round-button (create-push-button search-menu "round"))
        (replace-button (create-push-button edit-menu "replace")))

   (set-values main :menu-bar mb :work-window ww)

   (realize-widget shell)))



