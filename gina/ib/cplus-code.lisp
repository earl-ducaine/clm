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

(setq *sccs-id* "@(#)cplus-code.lisp	1.16	11/9/92")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; details record for C++ code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass cplus-code-details (general-code-details)
  ((detail-name    :reader detail-name      :initform :cplus-code-details
                   :allocation :class)
   (header-name    :accessor header-name    :initform "GnNewDialog")
   (doc-class      :accessor doc-class      :initform "GnDocument"))
  (:documentation "contains information needed to generate C++ code"))

(defun make-cplus-code-details ()
  (make-instance 'cplus-code-details))

(defmethod write-code-details :after ((details cplus-code-details) stream)
  (format stream ":doc-class ~s ~s~%" 
                 (doc-class details) (header-name details)))

(defmethod read-code-details :after ((details cplus-code-details) stream)
  (setf (header-name details) (read stream))
  (when (eql (header-name details) :doc-class)
    (setf (doc-class details) (read stream))
    (setf (header-name details) (read stream))))

(defmethod write-code ((details cplus-code-details) doc default-pathname)
  (let ((header-path (make-pathname :type "h" :defaults default-pathname)))
    (with-open-file (stream 
                     header-path
                     :direction :output
                     :if-exists :new-version :if-does-not-exist :create)
      (write-code-for-dialog-class details doc stream)
      (terpri stream))
    (setf (header-name details) (pathname-name header-path)))
  (with-open-file (stream 
                   (make-pathname :type "C" :defaults default-pathname)
                   :direction :output
                   :if-exists :new-version :if-does-not-exist :create)
    (write-code-header details doc stream)
    (terpri stream)
    (write-code-for-constructor details doc stream)
    (terpri stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; writing the c++ source file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod write-code-header ((details cplus-code-details) doc stream)
  (declare (ignore doc))
  (format stream "/* !!! Do not edit !!! */~%")
  (format stream 
          "/* This code was generated by the GINA Interface Builder. */~%~%")
  (format stream "#include <Gina/Gina.h>~%~%")
  (format stream "#include \"~a.h\"~%" (header-name details)))

(defmethod write-code-for-dialog-class ((details cplus-code-details) doc stream)
  (format stream "/* !!! Do not edit !!! */~%")
  (format stream 
          "/* This code was generated by the GINA Interface Builder. */~%~%")
  (format stream "#ifndef _~a_H_~%" 
                 (capitalize (dialog-class (main-info doc))))
  (format stream "#define _~a_H_~%~%" 
                 (capitalize (dialog-class (main-info doc))))
  ;; print declarations for all view classes
  (loop for info in (find-all-of-a-class (main-info doc) 'view)
        do (format stream "class ~a;~%"
                   (capitalize (string-downcase (dialog-class info)))))
  (format stream 
      "class ~a : public ~a {~%  public:~%    ~a (GnDocument *doc~a);~%~%"
          (capitalize (dialog-class (main-info doc)))
          (cplus-widget-class (main-info doc))
          (capitalize (dialog-class (main-info doc)))
          (if (eql (widget-class (main-info doc)) 'document-shell)
              ", int width, int height" ""))
  (loop for info in (find-all-widget-infos (main-info doc)) do
      (format stream "    ~a   ~a;~%"
              (cplus-widget-class info)
              (underscore (widget-name info))))
  (format stream "~%  protected:~%")
  (format stream "    GnDocument    *document;~%~%")
  (format stream "    virtual void before_create (GnWidget *);~%")
  (format stream "    virtual void after_create ();~%};~%")
  (format stream "~%#endif~%"))
  
(defmethod write-code-for-constructor ((details cplus-code-details) doc stream)
  (format stream "~a ::~%~a (GnDocument *doc~a)~%"
          (capitalize (dialog-class (main-info doc)))
          (capitalize (dialog-class (main-info doc)))
          (if (eql (widget-class (main-info doc)) 'document-shell)
              ", int width, int height" ""))
  (let ((view-names 
            (loop for info in (find-all-of-a-class (main-info doc) 'view)
                  collect (underscore (widget-name info)))))
    (when (eql (widget-class (main-info doc)) 'document-shell)
        (setq view-names (push "GnDocumentShell" view-names)))
    (when view-names
        (format stream "  :~a (doc~a)~{, ~a (doc)~}~%" (first view-names)
                       (if (eql (widget-class (main-info doc)) 'document-shell)
                           (if (named-resource-value (main-info doc) :with-menu)
                               ", width, height" ", width, height, False")
                           "")
                       (rest view-names))))
  (format stream "  {document = doc;}~%~%")
  (format stream "void ~a :: before_create (GnWidget *)~%{~%"
          (capitalize (dialog-class (main-info doc))))
  ;; here set main-shell of doc if :is-main-shell resource set
  ;; ...
  (print-resource-settings (main-info doc) "" stream)
  (format stream "}~%~%")
  (format stream "void ~a :: after_create ()~%{~%"
          (capitalize (dialog-class (main-info doc))))
  ;(when (eql (widget-class (main-info doc)) 'document-shell))
  (format stream "    ~a::after_create();~%~%"
                 (cplus-widget-class (main-info doc)))
  (loop for child in (children (main-info doc))
        do (write-code-for-widget child stream))
  (unless (eql (widget-class (main-info doc)) 'document-shell)
    (write-default-constraints (main-info doc) stream))
  (format stream "}~%~%"))

(defmethod write-default-constraints ((info widget-info) stream)
  (let ((single (= 1 (length (children info))))
        (tool (eql (widget-class info) 'tool-dialog-box))
        (size (bounding-box (children info))))
  (if single
    (let ((prefix (concatenate 'simple-string 
		         (underscore (widget-name (first (children info)))) 
		         ".")))
      (if tool
        (progn
          (format stream "    ~asetR_topAttachment (XmATTACH_WIDGET);~%" prefix)
	  (format stream "    ~asetR_topWidget (main_menu);~%" prefix))
        (format stream "    ~asetR_topAttachment (XmATTACH_FORM);~%" prefix))
      (format stream "    ~asetR_leftAttachment (XmATTACH_FORM);~%" prefix)
      (format stream "    ~asetR_rightAttachment (XmATTACH_FORM);~%" prefix)
      (format stream "    ~asetR_bottomAttachment (XmATTACH_FORM);~%" prefix)
      (format stream "    ~aSetValues();~%" prefix))
    (loop for child in (children info)
          for prefix = (concatenate 'simple-string 
		                    (underscore (widget-name child)) ".") do
      (if tool
	(progn
          (format stream "    ~asetR_topAttachment (XmATTACH_WIDGET);~%" prefix)
	  (format stream "    ~asetR_topWidget (main_menu);~%" prefix))
        (format stream "    ~asetR_topAttachment (XmATTACH_FORM);~%" prefix))
      (format stream "    ~asetR_topOffset (~d);~%" prefix
            (- (x-pos child) (first size)))
      (format stream "    ~asetR_leftAttachment (XmATTACH_FORM);~%" prefix)
      (format stream "    ~asetR_leftOffset (~d);~%" prefix
	    (- (y-pos child) (second size)))
      (format stream "    ~aSetValues();~%" prefix))
  )))

(defmethod write-code-for-widget ((info widget-info) stream)
  ;(format stream "    ~a = new ~a;~%"
  ;        (underscore (widget-name info))
  ;        (cplus-widget-class info))
  (print-resource-settings info
          (concatenate 'simple-string (underscore (widget-name info)) ".")
          stream)
  (format stream "    ~a.create (~a, ~s);~%"
          (underscore (widget-name info))
          (if (parent-info (parent-info info))
              (underscore (widget-name (parent-info info)))
              "this")
          (underscore (widget-name info)))
  (write-callback-definitions info 
          (concatenate 'simple-string (underscore (widget-name info)) ".")
          stream)
  (format stream "~%")
  (loop for child in (children info)
        do (write-code-for-widget child stream))
  (loop for child in (children info)
        do (write-constraints child stream)))

(defmethod print-resource-settings ((info widget-info) prefix stream)
  (loop for resource in (positional-resources info)
        do (print-one-resource info resource prefix stream))
  (loop for resource in (key-resources info)
        when (and (not (eql (access-mode resource) :ignore))
                  (or (resource-modified resource info)
                      (member (resource-name resource)
                              (required-key-names info))))
        do (print-one-resource info resource prefix stream))
  (loop for resource in (motif-resources info)
        when (or (resource-modified resource info)
		 (member (resource-name resource)
			 (required-key-names info)))
        do (print-one-resource info resource prefix stream)))

(defmethod print-one-resource ((info widget-info) (resource motif-resource) 
                               prefix stream)
  ;; C++ GINA does not switch packing-mode if columns > 1
  (when (and (eql (resource-name resource) :num-columns)
             (is-row-column-or-subclass info))
      (format stream "    ~asetR_packing (~a);~%"
          prefix
          (if (eql (named-resource-value info :num-columns) 1)
              "XmPACK_TIGHT"
              "XmPACK_COLUMN")))
  ;; end of hack
  (let ((rname (cplus-resource-name resource)))
    (when (and (eql (resource-name resource) :label-string)
               (find-resource info :label-type)
               (eql (named-resource-value info :label-type) :pixmap))
        (setq rname "labelPixmap"))
    (format stream "    ~asetR_~a (~a);~%"
            prefix rname
            (format nil "~s" (resource-value resource info)))))

(defmethod print-one-resource ((info widget-info) 
                               (resource enumerative-resource) 
                               prefix stream)
  (unless (eql (resource-name resource) :selection-policy)
      (format stream "    ~asetR_~a (~a);~%"
          prefix
          (cplus-resource-name resource)
          (concatenate 'simple-string 
                  (cplus-prefix resource) 
                  (underscore (symbol-name (resource-value resource info)))))))

(defmethod print-one-resource ((info widget-info) (resource boolean-resource)
                               prefix stream)
  (if (eql (resource-name resource) :resize)
      (format stream "    ~asetR_noResize (~a);~%"
              prefix
              (bool-translation (not (resource-value resource info))))
      (format stream "    ~asetR_~a (~a);~%"
              prefix
              (cplus-resource-name resource)
              (bool-translation (resource-value resource info)))))

(defmethod print-one-resource ((info widget-info) (resource item-list-resource)
                               prefix stream)
  (loop for item in (resource-value resource info) do
    (if (listp item)
      (format stream "    ~aadd (~s, ~s);~%" prefix (first item) (second item))
      (format stream "    ~aadd (~s);~%" prefix item))))

(defmethod write-callback-definitions ((info widget-info) prefix stream)
  (loop for resource in (callback-resources info)
        when (resource-modified resource info)
        do (let* ((res-value (resource-value resource info))
                  (param-list (cplus-callback-params res-value info))
                  (is-constructor (equal "<Constructor>" (second res-value))))
             (format stream "    ~aadd_~a~%" 
                            prefix 
                            (cplus-resource-name resource))
             (if is-constructor
               (format stream "        (SIMPLE_CALLBACK~a (~a~{~{, ~a~}~}));~%"
                              (if (zerop (length param-list)) 
                                  "" (format nil "~d" (length param-list)))
                              (underscore (first res-value))
                              param-list)
               (format stream "        (CALLBACK~a (~a, ~a, ~a~{~{, ~a~}~}));~%"
                              (if (zerop (1- (length param-list))) 
                                  "" (format nil "~d" (1- (length param-list))))
                              (first (first param-list))
                              (underscore (first res-value))
                              (second (first param-list))
                              (rest param-list))))))

(defun cplus-callback-params (res-value info &aux doc)
  (setq doc (document info))
  (loop for item in (rest res-value)
        unless (equal item "<Constructor>")
        collect (if (equal item (widget-name (main-info doc)))
                    (list (format nil "~a" 
                                  (capitalize (dialog-class (main-info doc))))
                          "this")
                    (if (equal item "<Document>") 
                        (list (doc-class (cplus-details doc)) "document")
                        (if (position (char item 0) "0123456789+-")
                            (list "int" item)
                            (if (eq (char item 0) #\")
                                (list "char *" item)
                                (if (eq (char item 0) #\:)
                                    (list "caddr_t"
                                          (underscore (subseq item 1)))
                                (widget-as-callback-param doc item))))))))

(defun widget-as-callback-param (doc item &aux info)
  (setq info (find-widget-info (main-info doc) item))
  (list
     (if info 
         (format nil "~a" (cplus-widget-class info))
         "GnWidget")
     (concatenate 'simple-string "&" (underscore item))))

(defun capitalize (lisp-name &optional (start-low nil))
  (let ((result (if start-low
                    (subseq lisp-name 0 1)
                    (string-upcase (subseq lisp-name 0 1))))
        (minus-pos 1))
    (loop for new-pos = (position #\- lisp-name :start minus-pos)
	  while new-pos
          do (setq result 
	       (concatenate 'simple-string result
			    (subseq lisp-name minus-pos new-pos)
			    (string-upcase (subseq lisp-name 
						   (1+ new-pos)
						   (+ 2 new-pos)))))
	     (setq minus-pos (+ 2 new-pos)))
    (concatenate 'simple-string result (subseq lisp-name minus-pos))))

(defun underscore (lisp-name)
  (substitute #\_ #\- lisp-name))

(defun cplus-widget-class (info)
  (let ((wc (widget-class info)))
    (case wc
      ((view)
          (capitalize (string-downcase (dialog-class info))))
      ((modeless-dialog-box) "GnFormDialog")
      ((modal-dialog-box) "GnFormDialog")
      ((radio-button-group) "GnRadioGroup")
      ((toggle-button-group) "GnToggleGroup")
      ((option-menu) "GnOptionGroup")
      ((scroller) "GnVariableScrolledWindow")
      ((scrollbar) "GnScrollBar")
      ((selection-list) 
          (case (named-resource-value info :selection-policy)
             ((:single) "GnSingleSelectionList")
             ((:multiple) "GnMultipleSelectionList")
             ((:browse) "GnBrowseSelectionList")
             ((:extended) "GnExtendedSelectionList")))
      ((scrollable-selection-list) 
          (case (named-resource-value info :selection-policy)
             ((:single) "GnScrollableSingleSelectionList")
             ((:multiple) "GnScrollableMultipleSelectionList")
             ((:browse) "GnScrollableBrowseSelectionList")
             ((:extended) "GnScrollableExtendedSelectionList")))
      (t (concatenate 'simple-string "Gn"
		      (capitalize (string-downcase (symbol-name wc))))))))

(defun cplus-resource-name (resource)
  (let ((nn (resource-name resource)))
    (case nn
      ((:title) "dialogTitle")
      (t       (capitalize (string-downcase (symbol-name nn)) t)))))

(defun bool-translation (value)
  (if value "True" "False"))

(defmethod write-constraints ((info widget-info) stream)
  (when (and (constraints info) (is-constraint-child info))
      (print-constraint-code (constraints info) stream
            (concatenate 'simple-string (underscore (widget-name info)) "."))))

(defmethod print-constraint-code ((constraints ib-form-constraints) stream
                                   prefix)
  (format stream "~%")
  (print-attachment-code (top constraints) "top" stream prefix)
  (print-attachment-code (left constraints) "left" stream prefix)
  (print-attachment-code (right constraints) "right" stream prefix)
  (print-attachment-code (bottom constraints) "bottom" stream prefix)
  (format stream "    ~aSetValues ();~%" prefix))

(defmethod print-attachment-code ((attachment attachment) direction
                                  stream prefix)
  (format stream "    ~asetR_~aAttachment (XmATTACH_~a);~%" prefix direction
                       (underscore (symbol-name (atype attachment))))
  (case (atype attachment)
    ((:none)
        )
    ((:position :self)
        (format stream "    ~asetR_~aPosition (~d);~%" prefix direction
                       (form-pos attachment)))
    ((:widget :opposite-widget)
        (format stream "    ~asetR_~aWidget (~a);~%" prefix direction
                       (underscore (widget-name (widget attachment))))
        (format stream "    ~asetR_~aOffset (~d);~%" prefix direction
                       (offset attachment)))
    (t
        (format stream "    ~asetR_~aOffset (~d);~%" prefix direction
                       (offset attachment)))))

(defmethod print-constraint-code ((constraints ib-pane-constraints) stream
                                   prefix)
  (format stream "    ~asetR_minimum (~d);~%" prefix (minimum constraints))
  (format stream "    ~asetR_maximum (~d);~%" prefix (maximum constraints))
  (format stream "    ~asetR_skipAdjust (~d);~%" prefix
                 (bool-translation (skip-adjust constraints)))
  (format stream "    ~asetR_allowResize (~d);~%" prefix
                 (bool-translation (allow-resize constraints)))
  (format stream "    ~aSetValues ();~%" prefix))