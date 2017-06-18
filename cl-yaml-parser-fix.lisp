;;; cl-yaml picks up some of the base16 scheme values as floats instead of strings
;;; Until this is fixed patch the function so that it only parses strings

(in-package :yaml.scalar)

(defun parse-scalar (string &optional (style :plain-scalar-style))
  "Return the string for all values."
  string)
