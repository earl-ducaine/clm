;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: USER;-*-


(defsystem gina-ib
    (:pretty-name "GINA Interface Builder"
     :initial-status :released
     :patchable t
     :default-pathname "gina-host:ib;"
     :bug-reports (:mailing-list "spenke@gmd.de")
     )
  (:module component-systems (gina) (:type :system))
  (:serial
    component-systems
    "pkg"
    "classes"
    "documents"
    "counter"
    "resources"
    "infos"
    "constraints"
    "class-tree"
    "main-view"
    "proto-view"
    "plates"
    "info-cmds"
    "mouse-cmds"
    "tree-cmds"
    "lisp-code"
    "cplus-code"
    "test-code"
    "main-window"
    "coder-window"
    "tree-window"
    "dialogs"
    "color"
    "widget-drag"
    "grabber"
    "interface"
    "form-view"
    "form-cmds"
    "prefs"))

