;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Package: USER;-*-

;; What to do for using CLM and GINA on Symbolix:

;; start the toolkit on your UNIX host (don't forget to set XBMLANGPATH)
;;  (setq gina:*default-display-host* "your UNIX-host"
;;        gina:*default-toolkit-host* "your UNIX-host")
;; call the gina finder
;;  (gina:make-finder)
;; go to the documents directory and click onto the documents ...

(asdf:defsystem :clm
  :depends-on (:cffi :clx)
  :components
  ((:file pkg)
   (:file defs)
   (:module clm-files
	    :depends-on (:defs)
	    :pathname ""
	    :components
	    ((:file "pkg")
	     (:file "trace-logging")
	     ;; (:file "genera")
	     (:file clm-client)
	     (:file "sbcl" :depends-on (clm-client))
	     (:file "low" :depends-on (sbcl))
	     (:file "files" )
	     (:file "display" )
	     (:file "convenience" )
	     (:file "widgets" )
	     (:file "text" )
	     (:file "callbacks" )
	     (:file "events" )
	     (:file "transl" )
	     (:file "dialogs" )
	     (:file "cursor" )
	     (:file "color" )
	     (:file "listw" )
	     (:file "timers" )
	     (:file "graph")))))

;; from cmucl.lisp
;; (defvar *clm-files*
;;   '(("pkg")
;;     ("defs")
;;     ("cmucl" "defs")
;;     ("low" "cmucl" "defs")
;;     ("files" "defs")
;;     ("display" "defs")
;;     ("convenience" "defs")
;;     ("widgets" "defs")
;;     ("text" "defs")
;;     ("callbacks" "defs")
;;     ("events" "defs")
;;     ("transl" "defs")
;;     ("dialogs" "defs")
;;     ("cursor" "defs")
;;     ("color" "defs")
;;     ("listw" "defs")
;;     ("timers" "defs")
;;     ("graph" "defs")))


;; (defsystem clm
;;     (:pretty-name "Common Lisp Motif Interface"
;;      :default-pathname "gina-host:clm;"
;;      :initial-status :released
;;      :patchable t
;;      :bug-reports (:mailing-list "spenke@gmdzi.gmd.de")
;;      )
;;   (:module component-systems (clx) (:type :system))
;;   (:serial
;;     component-systems
;;     (:file "pkg")
;;     (:file "defs")
;;     (:file "genera")
;;     (:file "low" )
;;     (:file "files" )
;;     (:file "display" )
;;     (:file "convenience" )
;;     (:file "widgets" )
;;     (:file "text" )
;;     (:file "callbacks" )
;;     (:file "events" )
;;     (:file "transl" )
;;     (:file "dialogs" )
;;     (:file "cursor" )
;;     (:file "color" )
;;     (:file "listw" )
;;     (:file "timers" )
;;     (:file "graph"))
;;     ))
