;;;-*-Mode:LISP;Syntax: Common-Lisp;Package:ib;Base:10-*-
;;;
;;; Copyright 1990 GMD (German National Research Center for Computer Science)
;;;
;;; Permission to use, copy, modify, distribute, and sell this software and its
;;; documentation for any purpose is hereby granted without fee, provided that
;;; the above copyright notice appear in all copies and that both that
;;; copyright notice and this permission notice appear in supporting
;;; documentation, and that the name of GMD not be used in advertising or
;;; publicity pertaining to distribution of the software without specific,
;;; written prior permission.  GMD makes no representations about the
;;; suitability of this software for any purpose.  It is provided "as is"
;;; without express or implied warranty.
;;;
;;; GMD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL GMD
;;; BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
;;; OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
;;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;;
;;; Authors: Project GINA (spenke@gmd.de)
;;;          P.O. Box 1316
;;;          D-5205 Sankt Augustin 1
;;;          Germany

(in-package :ib)

(setq *sccs-id* "@(#)test-code.lisp	1.18	11/8/93")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; generate and test lisp code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ensure-package-exists (package)
  (let ((name (string-upcase package)))
    (unless (find-package name)
      (make-package name :use '(:gina #+genera :future-common-lisp
				      #-genera :lisp 
				      #+lucid :lcl)))))

(defmethod code-file-name ((detail general-code-details))
  (make-pathname :name (code-file detail) :type "lisp"
                 :defaults
                    #+genera (user-homedir-pathname)
                    #-genera (pathname "/tmp/x.lisp")))

(defmethod compiled-code-file-name ((detail general-code-details))
  (make-pathname :name (code-file detail) :type #+excl "fasl"
		                                #+lucid "sbin"
						#-(or excl lucid) "bin"
                 :defaults
                    #+genera (user-homedir-pathname)
                    #-genera (pathname "/tmp/x.lisp")))

(defmethod save-code ((doc ib-document) &key (save-as nil))
  (declare (ignore save-as))
  (with-clock-cursor
    (ensure-package-exists (what-package (lisp-details doc)))
    (write-code (lisp-details doc) doc (code-file-name (lisp-details doc))))
  t)

(defmethod load-code ((doc ib-document))
  (when (save-code doc)
    (ensure-package-exists (what-package (lisp-details doc)))
    (with-clock-cursor
      (load (code-file-name (lisp-details doc)) :verbose nil))))

(defmethod compile-code ((doc ib-document) &key (load nil))
  (when (save-code doc)
    (with-clock-cursor
     (ensure-package-exists (what-package (lisp-details doc)))
     (compile-file (code-file-name (lisp-details doc))
		   :output-file (compiled-code-file-name (lisp-details doc)))
     (when load
       (load (compiled-code-file-name (lisp-details doc)) :verbose nil)))))

(defvar *test-document* nil)

(defvar *test-application* nil)

(defmethod create-instance ((doc ib-document) 
                            &aux creator view-constructors)
  (load-code doc)
  (setq creator 
        (read-from-string
	     (format nil "~a::make-~a" 
                     (what-package (lisp-details doc)) 
                     (dialog-class (main-info doc)))))
  (unless *test-document*
      (setq *test-document* 
            (make-instance 'document :wildcard nil :file-pathname nil)))
  (setf (main-shell *test-document*) (main-shell doc))
  (unless *test-application*
      (setq *test-application* (make-instance 'application)))
  (setq view-constructors
        (loop for view-info in (find-all-of-a-class (main-info doc) 'view)
              collect (read-from-string 
                          (format nil ":~a-constructor" 
                                      (widget-name view-info)))
              collect 'ib::make-test-view))
  (let ((save-app *application*))
      (unwind-protect
        (progn
         ; (setq *application* *test-application*)
          (pop-up (apply creator
                         (append
                             (list *test-document*)
                             view-constructors))))
         (setq *application* save-app))))

