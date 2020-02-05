

(in-package :xtk)


;; type is a cffi c type :float or :int
(defun check-number-serialization (number type)
  (let ((size (cffi:foreign-type-size type))
	(number (coerce number (if (eq type :float) 'float 'integer))))
    (cffi:with-foreign-object (chars :unsigned-char size)
      (multiple-value-bind (to-bytes from-bytes)
	  (if (eq type :float)
	      (values #'float-to-bytes #'bytes-to-float)
	      (values #'integer-to-bytes #'bytes-to-integer))
	(funcall to-bytes number chars)
      (assert (= number (funcall from-bytes chars)))))))

(defun check-float-serialization ()
  (flet ((check-float-signed (seed)
	   (do* ((i seed (* i 2)))
		;; note, max float is the same as min float except for
		;; sign, unlike signed ints (b/c twos compliment).
		((> (if (minusp i) (* i -1) i)
		    (max-float)))
	     (check-number-serialization i :float)
	     (format t "Checking i(~s)~%" i))))
    (check-float-signed 1)
    (check-float-signed -1)))

(defun check-int-serialization ()
  (flet ((check-int-signed (seed)
	   (do* ((i seed (funcall (if (plusp seed) #'ceiling #'floor)
				  (* i 1.1))))
		;; note, max float is the same as min float except for
		;; sign, unlike signed ints (b/c twos compliment).
		((if (plusp seed) (> i (max-int))
		     (< i (min-int))))
	     (check-number-serialization i :int)
	     (format t "Checking i(~s)~%" i))))
    (check-int-signed 1)
    (check-int-signed -1)))

(defun run-tests ()
  (check-int-serialization)
  (check-float-serialization)
