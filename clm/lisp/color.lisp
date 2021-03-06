;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:xtk; Base:10 -*-

(in-package :xtk)

(setq *sccsid* "@(#)color.lisp	1.9 9/11/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;; Return a list describing all available named colors
;;;;; (This function should ask the X Server for the available colors)
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXPORT (get-x-colors)
(defun get-x-colors ()
  (list
   '(112 219 147 "aquamarine")
   '(50 204 153 "MediumAquamarine")
   '(0 0 0 "black")
   '(0 0 255 "blue")
   '(95 159 159 "CadetBlue")
   '(66 66 111 "CornflowerBlue")
   '(107 35 142 "DarkSlateBlue")
   '(191 216 216 "LightBlue")
   '(143 143 188 "LightSteelBlue")
   '(50 50 204 "MediumBlue")
   '(127 0 255 "MediumSlateBlue")
   '(47 47 79 "MidnightBlue")
   '(35 35 142 "NavyBlue")
   '(35 35 142 "navy")
   '(50 153 204 "SkyBlue")
   '(0 127 255 "SlateBlue")
   '(35 107 142 "SteelBlue")
   '(255 127 0 "coral")
   '(0 255 255 "cyan")
   '(142 35 35 "firebrick")
   '(165 42 42 "brown")
   '(244 164 96 "sandybrown")
   '(204 127 50 "gold")
   '(219 219 112 "goldenrod")
   '(0 255 0 "green")
   '(47 79 47 "DarkGreen")
   '(79 79 47 "DarkOliveGreen")
   '(35 142 35 "ForestGreen")
   '(50 204 50 "LimeGreen")
   '(66 111 66 "MediumSeaGreen")
   '(127 255 0 "MediumSpringGreen")
   '(143 188 143 "PaleGreen")
   '(35 142 107 "SeaGreen")
   '(0 255 127 "SpringGreen")
   '(153 204 50 "YellowGreen")
   '(47 79 79 "DarkSlateGrey")
   '(47 79 79 "DarkSlateGray")
   '(84 84 84 "DimGrey")
   '(84 84 84 "DimGray")
   '(168 168 168 "LightGrey")
   '(168 168 168 "LightGray")
   '(192 192 192 "gray")
   '(192 192 192 "grey")
   '(159 159 95 "khaki")
   '(255 0 255 "magenta")
   '(142 35 107 "maroon")
   '(204 50 50 "orange")
   '(219 112 219 "orchid")
   '(153 50 204 "DarkOrchid")
   '(147 112 219 "MediumOrchid")
   '(188 143 143 "pink")
   '(234 173 234 "plum")
   '(255 0 0 "red")
   '(79 47 47 "IndianRed")
   '(219 112 147 "MediumVioletRed")
   '(255 0 127 "OrangeRed")
   '(204 50 153 "VioletRed")
   '(111 66 66 "salmon")
   '(142 107 35 "sienna")
   '(219 147 112 "tan")
   '(216 191 216 "thistle")
   '(173 234 234 "turquoise")
   '(112 147 219 "DarkTurquoise")
   '(112 219 219 "MediumTurquoise")
   '(79 47 79 "violet")
   '(159 95 159 "BlueViolet")
   '(216 216 191 "wheat")
   '(255 255 255 "white")
   '(255 255 0 "yellow")))
