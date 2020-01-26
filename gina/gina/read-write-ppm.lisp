;;; -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-
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
;;;

(in-package :xlib)

(defun read-ppm-file (pathname &aux width height max data pixel raw)
  (with-open-file (fstream pathname :direction :input)
    (setq raw (read-line fstream))	
    (if (or (equal raw "P3") (equal raw "P6"))
	(progn
	  (setq raw (equal raw "P6"))
	  (setq width (read-next fstream))
	  (setq height (read-next fstream))
	  (setq max (read-next fstream))
	  (setq data (the pixarray (make-array (list height width) 
						     :element-type 'pixel)))
	  (loop for i from 0 to (1- height) do
		(loop for j from 0 to (1- width) do
		      (if raw
			  (setq pixel 
			    (* (+ (* (+ (* (char-int (read-char fstream)) 256) 
					(char-int (read-char fstream))) 256) 
				  (char-int (read-char fstream))) 256))
			(setq pixel 
			  (* (+ (* (+ (* (read-next fstream) 256) 
				      (read-next fstream)) 256) 
				(read-next fstream)) 256)))
		      (setf (aref data i j) pixel)))
	  (create-image :data data :depth 24 
			      :red-mask (* 255 256) 
			      :green-mask (* 65280 256) 
			      :blue-mask (* 16711680 256) 
			      :bits-per-pixel 32))
      nil)))

(defun write-ppm-file (pathname image &key (raw t) (creator "GINA") &aux width height data)
  (unless (typep image 'image-z)
    (setq image (copy-image image :result-type 'image-z)))
  (setq width (image-width image))
  (setq height (image-height image))
  (setq data (image-z-pixarray image))
  (with-open-file (fstream pathname :direction :output :if-exists :supersede)
    (if raw
	(format fstream "P6~%")
      (format fstream "P3~%"))
    (multiple-value-bind (seconds minutes hours day month year) 
	(decode-universal-time (get-universal-time))
      (format fstream "# created by ~A on ~d.~d.~d ~d:~d:~d~%" 
	      creator day month year hours minutes seconds))
    (format fstream "~d~%" width)
    (format fstream "~d~%" height)
    (format fstream "~d~%" 255)
    (loop for i from 0 to (1- height) do
	  (loop for j from 0 to (1- width) do
		(if raw
		    (progn
		      (write-char (int-char (floor (aref data i j) 16777216)) fstream)
		      (write-char (int-char (logand (floor (aref data i j) 65536) 255)) fstream)
		      (write-char (int-char (logand (floor (aref data i j) 256) 255)) fstream))
		(format fstream "~d ~d ~d~%" 
			(floor (aref data i j) 16777216)
			(logand (floor (aref data i j) 65536) 255)
			(logand (floor (aref data i j) 256) 255))
			)))))

(defun read-next (stream)
  "read next expression but skip comments"
  (loop while (equal #\# (peek-char t stream)) do
	(read-line stream))
  (read stream))
	      
(defun read-bitmap-file (pathname)
  ;; Creates an image from a C include file in standard X11 format
  ;; Patch: accept 32-bit bitmaps
  (declare (type (or pathname string stream) pathname))
  (declare (values image))
  (with-open-file (fstream pathname :direction :input)
    (let ((line "")
	  (properties nil)
	  (name nil)
	  (name-end nil))
      (declare (type string line)
	       (type stringable name)
	       (type list properties))
      ;; Get properties
      (loop
	(setq line (read-line fstream))
	(unless (char= (aref line 0) #\#) (return))
	(flet ((read-keyword (line start end)
		 (kintern
		   (substitute
		     #\- #\_
		     (#-excl string-upcase
		      #+excl correct-case
		      (subseq line start end))
		     :test #'char=))))
	  (when (null name)
	    (setq name-end (position #\_ line :test #'char= :from-end t)
		  name (read-keyword line 8 name-end))
	    (unless (eq name :image)
	      (setf (getf properties :name) name)))
	  (let* ((ind-start (index1+ name-end))
		 (ind-end (position #\Space line :test #'char=
				    :start ind-start))
		 (ind (read-keyword line ind-start ind-end))
		 (val-start (index1+ ind-end))
		 (val (parse-integer line :start val-start)))
	    (setf (getf properties ind) val))))
      ;; Calculate sizes
      (multiple-value-bind (width height depth left-pad)
	  (flet ((extract-property (ind &rest default)
		   (prog1 (apply #'getf properties ind default)
			  (remf properties ind))))
	    (values (extract-property :width)
		    (extract-property :height)
		    (extract-property :depth 1)
		    (extract-property :left-pad 0)))
	(declare (type (or null card16) width height)
		 (type image-depth depth)
		 (type card8 left-pad))
	(unless (and width height) (error "Not a BITMAP file"))
	(let* ((bits-per-pixel
		(cond ;; ((index> depth 24) 32) ;; removed cici.
		      ;; ((index> depth 16) 24) ;; changed cici.
		       ((index> depth 16) 32)
		       ((index> depth 8)  16)
		       ((index> depth 4)   8)
		       ((index> depth 1)   4)
		       (t                  1)))
	       (bits-per-line (index* width bits-per-pixel))
	       (bytes-per-line (index-ceiling bits-per-line 8))
	       (padded-bits-per-line
		 (index* (index-ceiling bits-per-line 32) 32))
	       (padded-bytes-per-line
		 (index-ceiling padded-bits-per-line 8))
	       (data (make-array (* padded-bytes-per-line height)
				 :element-type 'card8))
	       (line-base 0)
	       (byte 0))
	  (declare (type array-index bits-per-line
			 padded-bits-per-line padded-bytes-per-line
			 line-base byte)
		   (type buffer-bytes data))
	  (with-vector (data buffer-bytes)
	    (flet ((parse-hex (char)
		     (second
		       (assoc char
			      '((#\0  0) (#\1  1) (#\2  2) (#\3  3)
				(#\4  4) (#\5  5) (#\6  6) (#\7  7)
				(#\8  8) (#\9  9) (#\a 10) (#\b 11)
				(#\c 12) (#\d 13) (#\e 14) (#\f 15))
			      :test #'char-equal))))
	      (declare (inline parse-hex))
	      ;; Read data
	      ;; Note: using read-line instead of read-char would be 20% faster,
	      ;;       but would cons a lot of garbage...
	      (dotimes (i height)
		(dotimes (j bytes-per-line)
		  (loop (when (eql (read-char fstream) #\x) (return)))
		  (setf (aref data (index+ line-base byte))
			(index+ (index-ash (parse-hex (read-char fstream)) 4)
				(parse-hex (read-char fstream))))
		  (incf byte))
		(setq byte 0
		      line-base (index+ line-base padded-bytes-per-line)))))
	  ;; Compensate for left-pad in width and x-hot
	  (index-decf width left-pad)
	  (when (getf properties :x-hot)
	    (index-decf (getf properties :x-hot) left-pad))
	  (create-image
	    :width width :height height
	    :depth depth :bits-per-pixel bits-per-pixel
	    :data data :plist properties :format :z-pixmap
	    :bytes-per-line padded-bytes-per-line
	    :unit 32 :pad 32 :left-pad left-pad
	    :byte-lsb-first-p t :bit-lsb-first-p t))))))


