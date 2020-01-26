(in-package 'user)

(use-package 'xtk)


(defvar *sccsid* "@(#)bitmaps.lisp	1.3 11/27/91")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Main program
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(run-motif-application 'bitmaps)

(defmacro get-bitmaps ()
  '(quote ("background" "25_foreground" "50_foreground" "75_foreground" 
		       "horizontal" "vertical" "slant_right" "slant_left"
		       "1x1" "2x2" "black" "boxes" "cntr_ptr" "cntr_ptrmsk"
		       "cross_weave" "dimple1" "dimple3" "dot" "flagdown" 
		       "flagup" 
		       "flipped_gray" "flipped_gray" "gray" "gray1" "gray3"
		       "icon" "left_ptr" "left_ptrmsk" "light_gray" 
		       "opendot" "right_ptr" "right_ptrmsk" 
		       "root_weave" "scales" "sipb" "star" "stipple" "target"
		       "tie_fighter" "weird_size" "wide_weave" "wingdogs" 
		       "woman"
		       "xfd_icon" "xlogo16")))

(defun bitmaps (&aux app-shell app-rc sw rc)
  (setf app-shell (create-application-shell)
	sw (create-widget :scrolled-window app-shell
			  :width 500 :height 500
			  :visual-policy :constant 
			  :scrolling-policy :automatic
			  :scroll-bar-display-policy :static)
	app-rc (create-widget :row-column sw
			      :packing :pack-column 
			      :num-columns 5))
  (dolist (bitmap (get-bitmaps))
	  (setf rc (create-widget :row-column app-rc))
	  (create-widget :label rc 
			 :label-type :pixmap
			 :alignment :center
			 :label-pixmap bitmap)
	  (create-widget :label rc 
			 :alignment :center
			 :label-string bitmap))
  (realize-widget app-shell))
