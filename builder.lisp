;;; Functions for updating scheme and template files
(in-package #:cl-base16)

(defun clone-or-update-git-repo (repo url &optional (dir *source-dir*))
  "As it says on the tin."
  (trivial-shell:shell-command (format nil "git clone ~a \"~a/~a\"" url dir repo)))

(defun fetch-list-contents (source-name)
  "Fetch the contents of source source-name and clone th git repo's it specifies."
  (maphash (lambda (repo url) (clone-or-update-git-repo repo url
							(merge-pathnames source-name *source-dir*)))
	   (yaml:parse (merge-pathnames (format nil "~a/list.yaml" source-name) *source-dir*))))

(defun update-sources ()
  "Update sources
If sources is nil update *base-dir*/sources.yaml
If it has a value convert a string to yaml"
  (let ((sources (merge-pathnames "sources.yaml" *base-dir*)))
    (maphash (lambda (repo url)
	       (clone-or-update-git-repo repo url)
	       (fetch-list-contents repo))
	     (yaml:parse sources))))

(defun update ()
  (update-sources)
  ;; There has to be a better way todo this
  ;; Its cleaner then the original attmpt in loop but not great
  (maphash (lambda (key value)
	     (declare (ignore value))
	     (mapc (lambda (variant)
		     (maphash (lambda (template value)
				(declare (ignore value))
				(save-theme variant template))
			      (yaml:parse (merge-pathnames "templates/list.yaml" *source-dir*))))
		   (directory (merge-pathnames (concatenate 'string "schemes/" key "/*.yaml") *source-dir*))))
	   (yaml:parse (merge-pathnames "schemes/list.yaml" *source-dir*))))
