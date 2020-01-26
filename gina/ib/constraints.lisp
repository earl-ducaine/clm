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

(setq *sccs-id* "@(#)constraints.lisp	1.14	11/8/93")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; constraint resource classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(defclass attachment ()
  ((atype     :accessor atype      :initform :none)
   (offset    :accessor offset     :initform 0)
   (form-pos  :accessor form-pos   :initform 0)
   (widget    :accessor widget     :initform nil))
  (:documentation "Attachments for one direction"))

(defun make-attachment ()
  (make-instance 'attachment))

'(defclass ib-constraints ()
  ((constraint-name   :accessor constraint-name   :initform "General"
                      :allocation :class))
  (:documentation "Superclass for widget constraints"))

'(defclass ib-form-constraints (ib-constraints)
  ((constraint-name   :accessor constraint-name   :initform "Form"
                      :allocation :class)
   (bottom   :accessor bottom    :initform   (make-attachment))
   (left     :accessor left      :initform   (make-attachment))
   (right    :accessor right     :initform   (make-attachment))
   (top      :accessor top       :initform   (make-attachment)))
  (:documentation "Form constraints for a widget in the interface builder"))

(defun make-form-constraints ()
  (make-instance 'ib-form-constraints))

'(defclass ib-pane-constraints (ib-constraints)
  ((constraint-name  :accessor constraint-name  :initform "Paned Window"
                     :allocation :class)
   (minimum          :accessor minimum          :initform 1)
   (maximum          :accessor maximum          :initform 1000)
   (skip-adjust      :accessor skip-adjust      :initform nil)
   (allow-resize     :accessor allow-resize     :initform nil))
  (:documentation "Pane constraints for a widget in the interface builder"))

(defun make-pane-constraints ()
  (make-instance 'ib-pane-constraints))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Constraints ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod new-constraints ((info widget-info) constraints siblings)
   "set new constraints for a child, use existing if possible"
   (if (and (constraints info)
            (eql (constraint-name constraints)
                    (constraint-name (constraints info))))
       (check-constraints (constraints info) info siblings)
       (setf (constraints info) constraints)))

(defun calculate-rows-and-columns (list-of-infos)
  "sort children of a form into rows and columns"
  (let ((rows nil)
        (columns nil)
        (children (sort (copy-list list-of-infos)
                        #'(lambda (c1 c2) (< (y-pos c1) (y-pos c2))))))
    (loop while children
          do (setq rows
                   (push (one-row-of children) rows))
             (loop for removed-child in (first rows)
                   do (setq children (delete removed-child children))))
    (setq children (sort (copy-list list-of-infos)
                         #'(lambda (c1 c2) (< (x-pos c1) (x-pos c2)))))
    (loop while children
          do (setq columns
                   (push (one-column-of children) columns))
             (loop for removed-child in (first columns)
                   do (setq children (delete removed-child children))))
    (setq rows
      (sort rows 
            #'(lambda (r1 r2) (< (y-pos (first r1)) (y-pos (first r2))))))
    (setq columns
      (sort columns 
            #'(lambda (c1 c2) (< (x-pos (first c1)) (x-pos (first c2))))))
    ;(format t "Rows:~%")
    ;(loop for r in rows do 
    ;   (loop for i in r do (format t "~a " (widget-name i)))
    ;   (format t "~%"))
    ;(format t "Columns:~%")
    ;(loop for c in columns do 
    ;   (loop for i in c do (format t "~a " (widget-name i)))
    ;   (format t "~%"))
    (values rows columns)))

(defun determine-row-column-layout (rows columns &aux (r0 nil) (c0 nil))
  "return rows and columns with fixed/variable annotation"
  ;(loop for aa in rows do (format t "~{~s ~}~%" aa))
  ;(format t "~%")
  ;(loop for aa in columns do (format t "~{~s ~}~%" aa))
  (loop as r1 = (list (cons :fixed 
                            (cons (which-is-variable (first rows) t) 
                                  (first rows))))
        as c1 = (list (cons :fixed 
                            (cons (which-is-variable (first columns) nil)
                                  (first columns))))
        while (or rows columns) ;; as must be before while (CLtL2 26.6). cici.
        do (setq rows (rest rows))
           (setq columns (rest columns))
           (loop as r2 = nil
                 as c2 = nil
                 do (loop for r in rows
                          for pos = (overlaps r c1)
                          for var = (which-is-variable r t)
                          for more-than-one = (> (length r) 1)
                          when pos
                          do (when (or more-than-one var)
                               (push (cons pos 
                                           (cons (unless (and (eql pos var) 
                                                              more-than-one)
                                                     var) r))
                                     r2))
                             (setq rows (remove r rows)))
                    (loop for c in columns
                          for pos = (overlaps c r1)
                          for alt-pos = (overlaps c r2)
                          for var = (which-is-variable c nil)
                          for more-than-one = (> (length c) 1)
                          when pos
                          do (when (or more-than-one var)
                               (when (and (eql pos var)
                                          alt-pos
                                          (not (prefers-variable-size
                                                  (nth alt-pos c) nil)))
                                  (setq pos alt-pos))
                               (push (cons pos
                                           (cons (unless (and (eql pos var)
                                                              more-than-one)
						   var) c))
                                     c2))
                             (setq columns (remove c columns)))
                    (setq r0 (append r0 r1))
                    (setq c0 (append c0 c1))
                    (setq c1 c2)
                    (setq r1 r2)
                 until (and (not r2) (not c2))))
  (values r0 c0))                    

(defun overlaps (list list-of-lists)
  "return number of first element that overlaps with orthogonal row/column"
  (loop for el in list
        for i from 0
        do (loop for other-list in list-of-lists
              when (member el other-list)
              do (return-from overlaps i)))
  nil)

(defun which-is-variable (list horizontal)
  "determine the variable element"
  (let ((res nil))
    (loop for item in list
          for i from 0
          when (prefers-variable-size item horizontal)
          do (setq res i))
    res))

;;; TEST
'(multiple-value-bind (a b)
  (determine-row-column-layout 
    (sort (child-rows *inspected-object*)
          #'(lambda (r1 r2) (< (y-pos (first r1)) (y-pos (first r2)))))
    (sort (child-columns *inspected-object*)
          #'(lambda (c1 c2) (< (x-pos (first c1)) (x-pos (first c2))))))
  (loop for aa in a do (format t "~{~s ~}~%" aa))
  (loop for bb in b do (format t "~{~s ~}~%" bb))
  (loop for cc in (selected-plates *inspected-object*)
        do (format t "~%---~s~%" (info cc))
           (calculate-matrix-form-constraints (info cc) a b 0 0)))

(defun one-row-of (list-of-children)
  "collect one row starting with first child"
  (let ((element (first list-of-children)))
    (sort (cons element
                (loop for child in (rest list-of-children)
                      when (and (in-one-row element child)
                                (not (row-overlapping child the-row)))
                      collect child into the-row
                      finally (return the-row)))
          #'(lambda (c1 c2) (< (x-pos c1) (x-pos c2))))))

(defun one-column-of (list-of-children)
  "collect one column starting with first child"
  (let ((element (first list-of-children)))
    (sort (cons element
                (loop for child in (rest list-of-children)
                      when (and (in-one-column element child)
                                (not (col-overlapping child the-col)))
                      collect child into the-col
                      finally (return the-col)))
          #'(lambda (c1 c2) (< (y-pos c1) (y-pos c2))))))

(defun in-one-row (c1 c2)
  (< (abs (- (y-pos c1) (y-pos c2))) (height c1)))

(defun row-overlapping (child row)
  (loop for other in row
        when (and (< (x-pos child) (+ (x-pos other) (width other)))
                  (> (+ (x-pos child) (width child)) (x-pos other)))
          do (return t)))

(defun in-one-column (c1 c2)
  (< (abs (- (x-pos c1) (x-pos c2))) (width c1)))

(defun col-overlapping (child col)
  (loop for other in col
        when (and (< (y-pos child) (+ (y-pos other) (height other)))
                  (> (+ (y-pos child) (height child)) (y-pos other)))
          do (return t)))

(defmethod calculate-matrix-form-constraints ((info widget-info) rows columns
                                              min-x min-y)
  "calculate form constraints based on annotated rows and columns"
  (let ((constraints (make-form-constraints))
        (left-col nil)
        (left-pos nil)
        (left-fixed nil)
        (left-var 0)
        (left-offset (- (x-pos (plate info)) min-x))
        (top-row nil)
        (top-pos nil)
        (top-fixed nil)
        (top-var 0)
        (top-offset (- (y-pos (plate info)) min-y)))
    (loop for row in rows
          when (member info row)
          do (setq top-row row)
             (setq top-pos (- (position info row) 2))
             (setq top-fixed (eql (first row) :fixed))
             (setq top-var (second row))
             (when top-fixed
               (setq top-offset (- (y-pos (plate (third row))) min-y -10))))
    (loop for col in columns
          when (member info col)
          do (setq left-col col)
             (setq left-pos (- (position info col) 2))
             (setq left-fixed (eql (first col) :fixed))
             (setq left-var (second col))
             (when left-fixed
               (setq left-offset (- (x-pos (plate (third col))) min-x -10))))
    ;; left attachment
    ;(format t "~%LEFT:   ")
    (if left-fixed
        (new-attachment (left constraints) :form 
                        left-offset ;(+ (if (zerop left-pos) left-offset 0) 10)
                        nil)
        (if (and left-col
                 (not (eql left-pos (first left-col))))
            (new-attachment (left constraints) :opposite-widget 0
                            (nth (+ (first left-col) 2) left-col))
            (if (and top-row
                     (or (not top-var)
                         (<= top-pos top-var)))
                (if (zerop top-pos)
                  (new-attachment (left constraints) :form 
                                                     (+ left-offset 10) nil)
                  (new-attachment (left constraints) :widget
                                  (- (x-pos (plate info))
                                     (x-pos (plate (nth (1+ top-pos) top-row)))
                                     (width (plate (nth (1+ top-pos) top-row)))
                                     -10)
                                  (nth (1+ top-pos) top-row)))
                (new-attachment (left constraints) :none nil nil))))
    ;; right attachment
    ;(format t "~%RIGHT:  ")
    (if (and top-row
             top-var
             (>= top-pos top-var))
        (if (= (+ top-pos 3) (length top-row)) ;; last one
            (new-attachment (right constraints) :form 10 nil)
            (new-attachment (right constraints) :widget 
                            (- (x-pos (plate (nth (+ top-pos 3) top-row)))
                               (x-pos (plate info))
                               (width (plate info))
                               -10)
                            ;10
                            (nth (+ top-pos 3) top-row)))
        (new-attachment (right constraints) :none nil nil))
    ;; top attachment
    ;(format t "~%TOP:    ")
    (if top-fixed
        (new-attachment (top constraints) :form 
                        top-offset; (+ (if (zerop top-pos) top-offset 0) 10)
                        nil)
        (if (and top-row
                 (not (eql top-pos (first top-row))))
            (new-attachment (top constraints) :opposite-widget 0
                            (nth (+ (first top-row) 2) top-row))
            (if (and left-col
                     (or (not left-var)
                         (<= left-pos left-var)))
              (if (zerop left-pos)
                (new-attachment (top constraints) :form 
                                                  (+ top-offset 10) nil)
                (new-attachment (top constraints) :widget 
                                (- (y-pos (plate info))
                                   (y-pos (plate (nth (1+ left-pos) left-col)))
                                   (height (plate (nth (1+ left-pos) left-col)))
                                   -10)
                                (nth (1+ left-pos) left-col)))
              (new-attachment (top constraints) :none nil nil))))
    ;; bottom attachment
    ;(format t "~%BOTTOM: ")
    (if (and left-col
             left-var
             (>= left-pos left-var))
        (if (= (+ left-pos 3) (length left-col)) ;; last one
            (new-attachment (bottom constraints) :form 10 nil)
            (new-attachment (bottom constraints) :widget 
                            (- (y-pos (plate (nth (+ left-pos 3) left-col)))
                               (y-pos (plate info))
                               (height (plate info))
                               -10)
                            ;10
                            (nth (+ left-pos 3) left-col)))
        (new-attachment (bottom constraints) :none nil nil))
    constraints
    ))

(defun new-attachment (attachment atype offset widget-info)
  "fill an attachment object"
  ;(format t "~s " type)
  (setf (atype attachment) atype)
  (when widget-info
     ;(format t "~s " widget-info)
     (setf (widget attachment) widget-info))
  (when offset
     ;(format t "~d " offset)
     (setf (offset attachment) offset)))

;(defmethod calculate-matrix-form-constraints ((info widget-info) rows columns
;                                              min-x min-y)
;  (let ((constraints (make-form-constraints))
;        (column-leader nil)
;        (column-members 0)
;        (row-leader nil)
;        (row-members 0))
;    ;(loop for a in rows do (format t "~{~s ~}~%" a))
;    ;(loop for a in columns do (format t "~{~s ~}~%" a))
;    (loop for row in rows
;          when (member info row)
;          do (setq row-leader (first row))
;             (setq row-members (length row)))
;    (loop for column in columns
;          when (member info column)
;          do (setq column-leader (first column))
;             (setq column-members (length column)))
;    (if (and (eql info row-leader) (eql info column-leader))
;        (progn
;          (setf (atype (left constraints)) :form)
;          (setf (offset (left constraints)) (- (x-pos info) min-x))
;          (setf (atype (top constraints)) :form)
;          (setf (offset (top constraints)) (- (y-pos info) min-y)))
;        (progn
;          (if (or (eql info row-leader)
;                  (and (= row-members 2) (not (eql info column-leader))
;                       (not (member row-leader (loop for col in columns
;                                                        collect (first col))))))
;            (let ((anc (ancestor info columns)))
;              (setf (atype (top constraints)) :widget)
;              (setf (offset (top constraints)) (min (- (y-pos info)
;                                                        (+ (y-pos anc)
;                                                           (height anc)))
;                                                    100))
;              (setf (widget (top constraints)) anc))
;            (progn
;              (setf (atype (top constraints)) :opposite-widget)
;              (setf (offset (top constraints)) 0)
;              (setf (widget (top constraints)) row-leader)))
;          (if (eql info column-leader)
;            (let ((anc (ancestor info rows)))
;              (setf (atype (left constraints)) :widget)
;              (setf (offset (left constraints)) (min (- (x-pos info)
;                                                        (+ (x-pos anc)
;                                                           (width anc)))
;                                                     100))
;              (setf (widget (left constraints)) anc))
;            (progn
;              (setf (atype (left constraints)) :opposite-widget)
;              (setf (offset (left constraints)) 0)
;              (setf (widget (left constraints)) column-leader)))))
    ;(format t "~s ~{~s ~}~%" info
    ;    (loop for attachment in (list (left constraints) (top constraints))
    ;          collect (atype attachment) collect (offset attachment)
    ;          collect (widget attachment)))
;    constraints))

;(defun ancestor (info list-of-rulers)
;  (loop for ruler in list-of-rulers
;        do (loop for prev in ruler
;                 for follow in (rest ruler)
;                 when (eql follow info)
;                 do (return-from ancestor prev))))

;(defmethod calculate-initial-form-constraints ((info widget-info) infos 
;                                               &aux constraints x0 y0 x1 y1)
;  (setq constraints (make-form-constraints))
;  (setq x0 1000 y0 1000 x1 0 y1 0)
;  (loop for p in infos
;        do (setq x0 (min (x-pos p) x0))
;           (setq y0 (min (y-pos p) y0))
;           (setq x1 (max (+ (x-pos p) (width p)) x1))
;           (setq y1 (max (+ (y-pos p) (height p)) y1)))
;  (setf (atype (left constraints)) :position)
;  (setf (form-pos (left constraints)) 
;        (round (/ (* (- (x-pos info) x0) 100) (- x1 x0))))
;  (setf (atype (top constraints)) :position)
;  (setf (form-pos (top constraints)) 
;        (round (/ (* (- (y-pos info) y0) 100) (- y1 y0))))
;  constraints)

(defmethod fix-none-form-constraints ((info widget-info) infos
                                               &aux x0 y0)
  (setq x0 1000 y0 1000)
  (loop for p in infos
        do (setq x0 (min (x-pos p) x0))
           (setq y0 (min (y-pos p) y0)))
  (when (and (eql (atype (left (constraints info))) :none)
             (eql (atype (right (constraints info))) :none))
        (setf (atype (left (constraints info))) :form)
        (setf (offset (left (constraints info))) (- (x-pos info) x0)))
  (when (and (eql (atype (top (constraints info))) :none)
             (eql (atype (bottom (constraints info))) :none))
        (setf (atype (top (constraints info))) :form)
        (setf (offset (top (constraints info))) (- (y-pos info) y0))))

(defmethod update-constraints ((constraints ib-form-constraints) plate)
  (when (eq (widget-class (parent-info (info plate))) 'form)
    (apply 'define-form-constraint 
	 (append (list (gina-widget plate))
	         `(:bottom-attachment ,(atype (bottom constraints)))
	         `(:bottom-offset ,(offset (bottom constraints)))
	         `(:bottom-position ,(form-pos (bottom constraints)))
	         (when (widget (bottom constraints)) 
	               `(:bottom-widget 
                         ,(gina-widget (plate (widget (bottom constraints))))))
	         `(:left-attachment ,(atype (left constraints)))
	         `(:left-offset ,(offset (left constraints)))
	         `(:left-position ,(form-pos (left constraints)))
	         (when (widget (left constraints)) 
	               `(:left-widget 
                         ,(gina-widget (plate (widget (left constraints))))))
	         `(:right-attachment ,(atype (right constraints)))
	         `(:right-offset ,(offset (right constraints)))
	         `(:right-position ,(form-pos (right constraints)))
	         (when (widget (right constraints)) 
	               `(:right-widget 
                         ,(gina-widget (plate (widget (right constraints))))))
	         `(:top-attachment ,(atype (top constraints)))
	         `(:top-offset ,(offset (top constraints)))
	         `(:top-position ,(form-pos (top constraints)))
	         (when (widget (top constraints)) 
	               `(:top-widget 
                         ,(gina-widget (plate (widget (top constraints))))))))))

(defmethod constraints-from-stream ((constraints ib-form-constraints) stream)
  (setf (atype (bottom constraints)) (read stream))
  (setf (offset (bottom constraints)) (read stream))
  (setf (form-pos (bottom constraints)) (read stream))
  (setf (widget (bottom constraints)) (read stream))
  (setf (atype (left constraints)) (read stream))
  (setf (offset (left constraints)) (read stream))
  (setf (form-pos (left constraints)) (read stream))
  (setf (widget (left constraints)) (read stream))
  (setf (atype (right constraints)) (read stream))
  (setf (offset (right constraints)) (read stream))
  (setf (form-pos (right constraints)) (read stream))
  (setf (widget (right constraints)) (read stream))
  (setf (atype (top constraints)) (read stream))
  (setf (offset (top constraints)) (read stream))
  (setf (form-pos (top constraints)) (read stream))
  (setf (widget (top constraints)) (read stream)))

(defmethod check-constraints ((constraints ib-form-constraints) info siblings)
  (loop for attachment in `(,(bottom constraints) ,(left constraints)
                            ,(right constraints) ,(top constraints))
        do (when (stringp (widget attachment))
                 (find-attached-widget attachment siblings
                                       (widget attachment)))
           (unless (member (widget attachment) siblings)
                   (setf (widget attachment) nil))
           (unless (widget attachment)
                   (when (member (atype attachment) 
                                  '(:widget :opposite-widget))
                         (setf (atype attachment) :none))))
  (fix-none-form-constraints info siblings))

(defmethod side-attachment ((info widget-info) side)
  "return attachment object for this side"
  (case side 
    ((:left) (left (constraints info)))
    ((:right) (right (constraints info)))
    ((:top) (top (constraints info)))
    ((:bottom) (bottom (constraints info)))))

(defmethod find-attached-widget ((a attachment) siblings name)
  "set attached widget to <name> in siblings"
  (setf (widget a) nil)
  (loop for sibling in siblings
        when (equal (widget-name sibling) name)
        do   (setf (widget a) sibling)))
          
(defmethod constraints-to-stream ((constraints ib-form-constraints) stream)
  (format stream "ib::ib-form-constraints ")
  (format stream "~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s~%"
                  (atype (bottom constraints))
                  (offset (bottom constraints))
                  (form-pos (bottom constraints))
                  (widget-name-as-string (widget (bottom constraints)))
                  ;(when (widget (bottom constraints))
                  ;      (widget-name (widget (bottom constraints))))
                  (atype (left constraints))
                  (offset (left constraints))
                  (form-pos (left constraints))
                  (widget-name-as-string (widget (left constraints)))
                  ;(when (widget (left constraints))
                  ;      (widget-name (widget (left constraints))))
                  (atype (right constraints))
                  (offset (right constraints))
                  (form-pos (right constraints))
                  (widget-name-as-string (widget (right constraints)))
                  ;(when (widget (right constraints))
                  ;      (widget-name (widget (right constraints))))
                  (atype (top constraints))
                  (offset (top constraints))
                  (form-pos (top constraints))
                  (widget-name-as-string (widget (top constraints)))
                  ;(when (widget (top constraints))
                  ;      (widget-name (widget (top constraints))))
  ))

(defun widget-name-as-string (string-or-info)
  "always return a string from the widget attached to"
  (when string-or-info
     (if (stringp string-or-info)
         string-or-info
         (widget-name string-or-info))))

(defmethod update-constraints ((constraints ib-pane-constraints) plate)
  (when (eq (widget-class (parent-info (info plate))) 'paned-window)
    (apply 'define-pane-constraint
           (append (list (gina-widget plate))
                   `(:minimum ,(minimum constraints))
                   `(:maximum ,(maximum constraints))
                   `(:skip-adjust ,(skip-adjust constraints))
                   `(:allow-resize ,(allow-resize constraints))))))

(defmethod constraints-from-stream ((constraints ib-pane-constraints) stream)
  (setf (minimum constraints) (read stream))
  (setf (maximum constraints) (read stream))
  (setf (skip-adjust constraints) (read stream))
  (setf (allow-resize constraints) (read stream)))

(defmethod check-constraints ((constraints ib-pane-constraints) info siblings)
  "empty"
  (declare (ignore info siblings)))

(defmethod constraints-to-stream ((constraints ib-pane-constraints) stream)
  (with-slots (minimum maximum skip-adjust allow-resize) constraints
    (format stream "ib::ib-pane-constraints ~s ~s ~s ~s~%"
                   minimum maximum skip-adjust allow-resize)))

