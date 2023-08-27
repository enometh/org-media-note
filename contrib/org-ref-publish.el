;; -*- lexical-binding: t -*-

;;madhu 230826
;; modified from org-publish-org-to to call org-ref-process-buffer before calling org-export-to-file
(defun org-ref-publish-org-to (backend filename extension plist &optional pub-dir)
  (unless (or (not pub-dir) (file-exists-p pub-dir)) (make-directory pub-dir t))
  ;; Check if a buffer visiting FILENAME is already open.
  (let* ((org-inhibit-startup t)
	 (visiting (find-buffer-visiting filename))
	 (work-buffer (or visiting (find-file-noselect filename))))
    (unwind-protect
	(with-current-buffer work-buffer
	  (let* ((output (org-export-output-file-name extension nil pub-dir)))
	    (org-export-with-buffer-copy
	     (org-export-expand-include-keyword)
	     (goto-char (point-min))
	     (org-ref-process-buffer backend nil) ;no subtreep
	     (goto-char (point-min))
	     (org-export-to-file backend output
	       nil nil nil (plist-get plist :body-only)
	       ;; Add `org-publish--store-crossrefs' and
	       ;; `org-publish-collect-index' to final output filters.
	       ;; The latter isn't dependent on `:makeindex', since we
	       ;; want to keep it up-to-date in cache anyway.
	       (org-combine-plists
		plist
		`(:crossrefs
		  ,(org-publish-cache-get-file-property
		    ;; Normalize file names in cache.
		    (file-truename filename) :crossrefs nil t)
		  :filter-final-output
		  (org-publish--store-crossrefs
		   org-publish-collect-index
		   ,@(plist-get plist :filter-final-output))))))))
      ;; Remove opened buffer in the process.
      (unless visiting (kill-buffer work-buffer)))))

;; modified from org-html-publish-to-html to call org-ref-publish-org-to instead of org-publish-org-to
(defun org-ref-publish-to-html (plist filename pub-dir)
  (org-ref-publish-org-to 'html filename
		      (concat (when (> (length org-html-extension) 0) ".")
			      (or (plist-get plist :html-extension)
				  org-html-extension
				  "html"))
		      plist pub-dir))


;; modified from org-latex-publish-to-latex to call org-ref-publish-org-to instead of org-publish-org-to
(defun org-ref-publish-to-latex (plist filename pub-dir)
  (org-ref-publish-org-to 'latex filename ".tex" plist pub-dir))

;; modified from org-latex-publish-to-pdfto call org-ref-publish-org-to instead of org-publish-org-to
(defun org-ref-publish-to-pdf (plist filename pub-dir)
  (org-publish-attachment
   plist
   (let ((default-directory (file-name-directory filename)))
     (org-latex-compile
      (org-ref-publish-org-to
       'latex filename ".tex" plist (file-name-directory filename))))
   pub-dir))

(provide 'org-ref-publish)