;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: USER;-*-



(defsystem gina
    (:pretty-name "Generic Interface Application"
     :initial-status :released
     :patchable t
     :default-pathname "gina-host:gina;"
     :bug-reports (:mailing-list "spenke@gmd.de")
     )
  (:module component-systems (clm) (:type :system))
  (:serial
    component-systems
   "package"
   "metainfo"
   "globals"
   "OS-dependent" 
   "classes" 
   "framework" 
   "motif-widgets" 
   "dialogs" 
   "background" 
   "views" 
   "commands" 
   "browser"
   ))
