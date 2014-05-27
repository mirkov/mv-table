(defvar mv-table-org-publish nil
  "Rules for publishing mv-table org-documents into html

org-documents are in mv-table/doc/, while the published documents
are in mv-table/documentation")

(defun update-mv-table-org-publish ()
  "Update the value of mv-table-org-publish

This is a user (developer) interface."
  (interactive "P")
  (let* ((project "mv-table")
	 (project-directory "~/my-software-add-ons/my-lisp/mv-table/")
	 (doc-directory (concat project-directory "doc/"))
	 (documentation-directory (concat project-directory "documentation/"))
	 (recursive t)
	 (publishing-function 'org-html-publish-to-html))
    (let ((mv-table-org-publish
	   `((,project
	      :base-directory ,doc-directory
	      :publishing-directory ,documentation-directory
	      :recursive ,recursive
	      :publishing-function ,publishing-function))))
      (setq org-publish-project-alist mv-table-org-publish)))

