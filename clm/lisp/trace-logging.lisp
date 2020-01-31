

(in-package :xtk)

(defparameter *logging-enabled* t)
(defparameter *logging-destination* t)

(defmacro log-format (format-string &rest args)
  (when *logging-enabled*
    `(format *logging-destination* ,format-string ,@args)))
