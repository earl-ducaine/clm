;;; -*- Mode:lisp; Syntax:Common-Lisp; Package:user; Base:10 -*-

(in-package :user)

'"@(#)load.lisp	1.17 9/10/93"

(defun initial-ld (file &aux (fun nil))
  (when (find-package :xtk) 
    (setq fun (fboundp (intern 'ld :xtk))))
  (if fun
      (funcall fun file)
    (progn
      (compile-file (concatenate 'simple-string file ".lisp"))
      (load file))))
  
(initial-ld "pkg")

(#+lucid with-deferred-warnings
 #+cmu with-compilation-unit #+cmu ()
 #-(or lucid cmu) progn

 (initial-ld "defs")

 #+excl
 (initial-ld "excl")

 #+lucid
 (initial-ld "lucid")

 #+lispworks
 (initial-ld "lispworks")

 #+cmu
 (initial-ld "cmucl")

 #+ibcl
 (initial-ld "ibuki")

 #+akcl
 (initial-ld "akcl")

 #+genera
 (error "You must use sysdcl.lisp for Genera!!")

 (mapcar #'(lambda (x) (apply 'xtk::ld x)) (cdddr xtk::*clm-files*))

)
