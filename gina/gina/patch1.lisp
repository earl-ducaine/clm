;;; -*- Mode:LISP;Syntax: Common-Lisp;Package: GINA ;Base:10-*-

(in-package :GINA)

;; Newest Allegro versions now support ANSI pathnames

(defginafun root-directory-p (pathname "pathname object or string")
  (description "check if directory component denotes the root directory"
	       :called-by-gina "in open/save dialogs"
	       :called-by-application :sometimes
	       :result "T or NIL")
  #+Genera (eq :root (pathname-directory pathname)) 
  #+lucid  (equal '(:root) (pathname-directory pathname))
  #+excl   (equal (pathname-directory "/") (pathname-directory pathname))
  #+cmu    (equal '(:absolute) (pathname-directory pathname))
  )
