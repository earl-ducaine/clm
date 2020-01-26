;;; -*- Mode:LISP;Syntax: Common-Lisp;Package: GINA ;Base:10-*-

(in-package :GINA)
			  
(defginamethod pathname->image ((appl application)
				pathname-or-image
				"if an image is given, it is just returned" 
				&aux image ppm-format supports-24)
  (description "look up cached image for pathname or read file and cache it"
     :called-by-application "for mapping pathnames to images"
     :called-by-gina        "in methods of class movable-icon")

  (setf supports-24 (member 24 (xlib::screen-depths (first (xlib::display-roots *display*))) :key #'car))
  (when (typep pathname-or-image 'xlib:image)
    ;; Already an image. Nothing to do:
    (return-from pathname->image pathname-or-image))
  (setq image (gethash pathname-or-image (pixmap-table appl)))
  ;; (format t "~a: hash: ~a (~a)~%" *display* image supports-24)
  (when (not image)
    ;; read ppm or bitmap format
    (with-open-file (fstream (find-bitmap pathname-or-image) :direction :input)
      (setq ppm-format (equal #\P (read-char fstream))))
    (if ppm-format
	(if supports-24
	    (setq image (xlib::read-ppm-file (find-bitmap pathname-or-image)))
	  (progn ;; if the display does not support 24-bit pixmap we convert them to bitmaps:
	    (excl::run-shell-command 
	     (format nil "<~a /usr/local/bin/ppmtopgm | /usr/local/bin/pgmtopbm | /usr/local/bin/pbmtoxbm > /tmp/diva-~a"
		     (find-bitmap pathname-or-image)
		     (display-host *application*)))
	    (setq image (xlib:read-bitmap-file (format nil "/tmp/diva-~a" (display-host *application*))))
	    (excl::run-shell-command (format nil "rm /tmp/diva-~a" (display-host *application*)))))
        (setq image (xlib:read-bitmap-file (find-bitmap pathname-or-image))))
    ;(format t "xlib:read-bitmap-file ~a~%" pathname-or-image)
    (setf (gethash pathname-or-image (pixmap-table appl)) image)
    ;;(format t "~a: read: ~a (~a)~%" *display* image supports-24)
    )
  image)





