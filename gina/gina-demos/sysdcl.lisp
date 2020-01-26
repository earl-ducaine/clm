;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: USER;-*-

(defsystem gina-demos
    (:pretty-name "Generic Interface Application Demos"
     :initial-status :released
     :patchable t
     :default-pathname "gina-host:gina-demos;"
     :bug-reports (:mailing-list "spenke@gmd.de")
     )
  (:module component-systems (gina) (:type :system))
  (:serial
    component-systems
    "demo-packages"
    "finder-shell"
    "finder"
    "graphic-editor"
    "clock"
    "graphic-output"
    "hello"
    "lisp-widgets"
    "micky"
    "text-editor"
    "bitmap-editor"
    "tetris"
    "hyper"
    "spreadsheet"
    "pacmen"
    "chess"
    "drag-and-drop"
    "mandelbrot"
    "calculator-shell"
    "calculator"))
