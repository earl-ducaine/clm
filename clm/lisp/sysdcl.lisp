;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: USER;-*-

;; What to do for using CLM and GINA on Symbolix:

;; Create a toplevel directory holding the whole stuff
;;  :Create Directory Rainbow:>gina-stuff>
;;                    (my machine is called "Rainbow" --cla)
;; Copy the gina system files to directories called clm, gina, gina-demos, ib, and bitmaps
;;  :Copy File UNIX:/xyz/clm/lisp/*.lisp Rainbow:>gina-stuff>clm>*.lisp :Create Directories Yes
;;  :Copy File UNIX:/xyz/gina/*.lisp Rainbow:>gina-stuff>gina>*.lisp :C D Yes
;;  :Copy File UNIX:/xyz/gina-demos/*.lisp Rainbow:>gina-stuff>gina-demos>*.lisp :C D Yes
;;  :Copy File UNIX:/xyz/ib/*.lisp Rainbow:>gina-stuff>gina-ib>*.lisp :C D Yes
;;  :Copy File UNIX:/xyz/bitmaps/* Rainbow:>gina-stuff>bitmaps>*.bitmap :C D Yes
;;            ;; referenced by gina:*bitmap-directories*

;; In the distribution there are some documents you might want to try out...
;;  :Copy File UNIX:/xyz/documents/* Rainbow:>gina-stuff>documents>* :C D Yes

;; Copy the files for the defsystem facilitiy to your official place (often "SYS:SITE;")
;;  :Copy File UNIX:/xyz/clm/gina-host.translations SYS:SITE;
;;  :Copy File UNIX:/xyz/clm/clm.system SYS:SITE;
;;  :Copy File UNIX:/xyz/gina/gina.system SYS:SITE;
;;  :Copy File UNIX:/xyz/gina-demos/gina-demos.system SYS:SITE;
;;  :Copy File UNIX:/xyz/ib/gina-ib.system SYS:SITE;

;; Edit SYS:SITE;gina-host.translations, change "Rainbow" to your physical machine
;; and change the translation, if desired
;;  :Compile System clm (This will automatically load the system "CLX")
;;  :Compile System gina
;;  :Compile System gina-demos
;;  :Compile System gina-ib
;; Now you are ready to use gina

;; start the toolkit on your UNIX host (don't forget to set XBMLANGPATH)
;;  (setq gina:*default-display-host* "your UNIX-host"
;;        gina:*default-toolkit-host* "your UNIX-host")
;; call the gina finder
;;  (gina:make-finder)
;; go to the documents directory and click onto the documents ...

(defsystem clm
    (:pretty-name "Common Lisp Motif Interface"
     :default-pathname "gina-host:clm;"
     :initial-status :released
     :patchable t
     :bug-reports (:mailing-list "spenke@gmdzi.gmd.de")
     )
  (:module component-systems (clx) (:type :system))
  (:serial
    component-systems
    "pkg"
    "defs"
    "genera"
    "low" 
    "files" 
    "display" 
    "convenience" 
    "widgets" 
    "text" 
    "callbacks" 
    "events" 
    "transl" 
    "dialogs" 
    "cursor" 
    "color" 
    "listw" 
    "timers" 
    "graph"
    ))
