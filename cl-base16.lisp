;;;; cl-base16.lisp
;;; Contains main functions related to building a theme
(in-package #:cl-base16)

(defparameter *base-dir* (pathname (directory-namestring (asdf:system-relative-pathname :cl-base16 "package.lisp")))
  "Base directory to expect input files from.
This is set to the base directory of the source files when loaded with asdf.
*base-dir* has to be of type pathname")

(defparameter *source-dir* (merge-pathnames "sources/" *base-dir*)
  "Source directory.
This is set to *base-dir*/sources/ when loaded with asdf.
*source-dir* has to be of type pathname")

;;; Theme application and saving

(defun get-template-dir (template)
  "Get the directory of template."
  (merge-pathnames (format nil "templates/~a/" template) *source-dir*))

(flet ((open-yaml-file-or-nil (path)
	  "Open yaml file specified by path or return nil"
	  (if (probe-file path)
	      (yaml:parse path)
	      nil)))
  
  (defun open-template-or-nil (template)
    "Open template yaml file"
    (open-yaml-file-or-nil (merge-pathnames
			     #p"templates/config.yaml" (get-template-dir template)))))

(defun make-keyword (key)
  "Make string key into a keyword."
  (multiple-value-bind (keyword ret) (intern (string-upcase key) "KEYWORD")
    (declare (ignore ret))
    keyword))

(defun load-scheme (scheme)
  "Load Scheme scheme as a cons list."
  (uiop:if-let ((scheme-yaml (yaml:parse scheme)))
    ;; cl-mustache wants its input in a cons list
    ;; build a cons list from the scheme yaml file
    (let ((scheme-cons '()))
      (flet ((add-to-scheme (key value)
	       (setf scheme-cons (append scheme-cons (list (cons
							    (make-keyword key)
							    value))))))
	(add-to-scheme :scheme-author (gethash "author" scheme-yaml))
	(add-to-scheme :scheme-name (gethash "scheme" scheme-yaml))
	(add-to-scheme :scheme-slug (slug:slugify (pathname-name scheme)))
	(loop for base in '("00" "01" "02" "03"
			    "04" "05" "06" "07"
			    "08" "09" "0A" "0B"
			    "0C" "0D" "0E" "0F")
	      do (let ((hex-value (let ((temp (gethash (concatenate'string "base" base) scheme-yaml)))
				    ;; Some scheme like dracula use #
				    ;; At the start of their values
				    ;; Remove this from the value
				    (if (= (length temp) 7)
					(subseq temp 1 7)
					temp))))
		   ;; Full hex string
		   (add-to-scheme (concatenate 'string "base" base "-hex")
				  hex-value)
		   ;; Hex string split into rgb values
		   (add-to-scheme (concatenate 'string "base" base "-hex-r")
				  (subseq hex-value 0 2))
		   (add-to-scheme (concatenate 'string "base" base "-hex-g")
				  (subseq hex-value 2 4))
		   (add-to-scheme (concatenate 'string "base" base "-hex-b")
				  (subseq hex-value 4 6))
		   ;; RGB values
		   (add-to-scheme (concatenate 'string "base" base "-rgb-r")
				  (parse-integer (subseq hex-value 0 2) :radix 16))
		   (add-to-scheme (concatenate 'string "base" base "-rgb-g")
				  (parse-integer (subseq hex-value 2 4) :radix 16))
		   (add-to-scheme (concatenate 'string "base" base "-rgb-b")
				  (parse-integer (subseq hex-value 4 6) :radix 16))
		   ;; Dec Values
		   (add-to-scheme (concatenate 'string "base" base "-dec-r")
				  (/ (parse-integer (subseq hex-value 0 2) :radix 16) 255))
		   (add-to-scheme (concatenate 'string "base" base "-dec-g")
				  (/ (parse-integer (subseq hex-value 2 4) :radix 16) 255))
		   (add-to-scheme (concatenate 'string "base" base "-dec-b")
				  (/ (parse-integer (subseq hex-value 4 6) :radix 16) 255)))))
      scheme-cons)
    (error "Cannot load scheme ~a" scheme)))

(defun apply-scheme (scheme template mustache-filename)
  "Apply Scheme scheme to Template template and return theme as a string."
  (let ((template-path
	  (merge-pathnames (concatenate 'string "templates/" mustache-filename ".mustache") (get-template-dir template))))
    (if (probe-file template-path)
	(mustache:render* template-path (load-scheme scheme))
	(error "Cannot find template: ~a" template))))

(defun save-theme (scheme template &optional (out nil))
  "Save theme specified by scheme after being applied to template.
Optionally specify:
The output path out to use"
  (uiop:if-let ((template-yaml (open-template-or-nil template)))
    (maphash (lambda (mustache-filename template-config)
	       (if (not out)
		   (setf out (merge-pathnames (pathname (concatenate 'string
								     (gethash "output" template-config) "/"
								     (uiop:split-name-type (file-namestring scheme))
								     (gethash "extension" template-config)))
					      (get-template-dir template))))
	       (uiop:ensure-all-directories-exist (list out))
	       (with-open-file (output-file out
					    :direction :output
					    :if-exists :supersede
					    :if-does-not-exist :create)
		 (format output-file "~a" (apply-scheme scheme template mustache-filename))))
	     template-yaml)
    (error "Could not find template: ~a" template)))
