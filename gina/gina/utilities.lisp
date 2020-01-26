;;; -*- Syntax: Common-Lisp; Package: gina; Base: 10; Mode: LISP -*-

;; start all demos
(defun start-demos (&optional (from 0) (to  (length *loaded-applications*)))
  (loop for (signature app-class doc-type) 
   in (subseq *loaded-applications* from (1+ to))
   do (print (make-application :class app-class))))

(start-demos 0 5)

;; start all documents
(defun start-docs (&optional (from 0) (to nil))
  (with-application-stopped
   (loop for file 
             in (subseq (directory "/vol/gina/gina2/documents/") 
			from (when to(1+ to)))
    do (print file)
       (start-file file)
     )))

(start-docs 0 10)

;; quit all-demos
(defun quit-demos ()
  (with-application-stopped
   (loop for app in (reverse *running-applications*)
    do (send-message app (make-callback 'quit-app app)))))

(quit-demos)
