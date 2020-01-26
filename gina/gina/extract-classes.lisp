;;; -*- Mode:LISP;Syntax: Common-Lisp;Package: user ;Base:10-*-
(in-package :user)

(defun extract-gina-class-definitions (output-file &rest input-files)
  "extract class-definitions from input files and write them into output-file"
  (with-open-file (ostream output-file :direction :output
		   :if-exists :supersede)
    (let ((*print-case* :downcase)
	  (*print-pretty* t))
      (format ostream ";;; -*- Mode:LISP;Syntax: Common-Lisp;Package: GINA ;Base:10-*-~%")
      (print '(in-package :GINA) ostream)

      ;; special forward declarations only necessary for CMU-CL 16e:
      (format ostream "~%~%#+cmu (defclass menu-bar () ()~%")
      (format ostream
	      "            (:documentation \"forward declaration for CMU-CL 16e\"))~%")
      (format ostream "#+cmu (defclass modeless-dialog-box () ()~%")
      (format ostream 
              "            (:documentation \"forward declaration for CMU-CL 16e\"))~%")

      (loop for input-file in input-files
	do (print input-file)
	   (with-open-file (istream input-file :direction :input)
	     (loop as form = (read istream nil :eof)
	           while (not (eq form :eof)) 
		   when (and (listp form)
			     (eq 'quote (first form))
			     (listp (second form))
			     (or (eq 'defclass (first (second form)))
				 (eq 'defginaclass (first (second form )))
				 (eq 'defmacro (first (second form)))
				 (eq 'defginamacro (first (second form)))))
	           do (format t "~%        Extract ~a" 
			      (second (second form)))
	              (print (second form) ostream))))
            (format ostream "~%"))))

'(extract-gina-class-definitions "classes.lisp"
				 "OS-dependent.lisp"
				 "framework.lisp"
				 "motif-widgets.lisp"
				 "dialogs.lisp"
				 "views.lisp"
				 "commands.lisp"
				 "background.lisp")
