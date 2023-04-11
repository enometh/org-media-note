(require 'transient)
(require 'org-ref-core)

;; (org-ref-insert-link-hydra/body)
(transient-define-prefix or-insert-link-transient ()
  [:description "Insert an org-ref link" ""]
 [["org-ref"
   ("[" (lambda ()
	  (interactive)
	  (funcall org-ref-insert-cite-function))
    :transient nil			;XXX
    :description "Citation")
   ("]" (lambda ()
	  (interactive)
	  (funcall org-ref-insert-ref-function))
    :transient t
    :description "Cross-reference")
   ("\\" (lambda ()
	   (interactive)
	   (funcall org-ref-insert-label-function))
    :transient t
    :description "Label" )
   ]

  ["Bibliography"
   ("bs" (lambda ()
	   (interactive)
	   (insert (org-ref-bibliographystyle-complete-link)))
    :description "Bibliographystyle"
    :transient nil)
   ("bf" (lambda ()
	   (interactive)
	   (insert (org-ref-bibliography-complete)))
    :description "Bibliography"
    :transient nil)
   ("nb" (lambda ()
	   (interactive)
	   (insert (org-ref-nobibliography-complete)))
    :description "Bibliography"
    :transient nil)
   ]

  ["Glosssary"
   ("g" org-ref-insert-glossary-link
    :transient nil
    :description "Glossary link")
   ("a" org-ref-insert-acronym-link
    :transient nil
    :description "Acronym link")
   ("ng" (lambda ()
	   (interactive)
	   (progn
	     (org-mark-ring-push)
	     (goto-char (point-min))
	     (if (re-search-forward "#\\+name: glossary" nil t)
		 (progn
		   (goto-char (org-element-property :contents-end (org-element-context)))
		   (backward-char)
		   (org-table-insert-row '(4)))
	       ;; no table found
	       (goto-char (point-max))
	       (insert "\n\n#+name: glossary
| label | term    | definition                    |
|-------+---------+-------------------------------|
|       |         |                               |")
	       (beginning-of-line)
	       (forward-char))))
    :transient t
    :description "New glossary term" )

   ("na" (lambda ()
	   (interactive)
	   (progn
	     (org-mark-ring-push)
	     (goto-char (point-min))
	     (if (re-search-forward "#\\+name: acronym" nil t)
		 (progn
		   (goto-char (org-element-property :contents-end (org-element-context)))
		   (backward-char)
		   (org-table-insert-row '(4)))
	       ;; no table found
	       (goto-char (point-max))
	       (insert "\n\n#+name: acronyms
| label | abbreviation | full form                  |
|-------+--------------+----------------------------|
|       |              |                            |")
	       (beginning-of-line)
	       (forward-char))))
    :transient t
    :description "New acronym term" )
   ]
  ]
 [

  ["Bibtex"
   ("bd" doi-add-bibtex-entry
    :transient t
    :description "Add bibtex entry from a DOI")
   ("bc" crossref-add-bibtex-entry
    :transient t
    :description "Add bibtex entry from Crossref" )
   ("bo" (lambda ()
	   (interactive)
	   (find-file (completing-read "Bibliography: " (org-ref-find-bibliography))))
    :transient t
    :description "Open bibtex file")]

  ["Misc"
   ("t" (lambda ()
	  (interactive)
	  (insert "[[list-of-tables:]]\n"))
    :transient t
    :description "List of tables" )
   ("f" (lambda ()
	  (interactive)
	  (insert "[[list-of-figures:]]\n"))
    :transient t
    :description "List of figures" )
   ("i" (lambda ()
	  (interactive)
	  (insert (format "[[index:%s]]" (string-trim (read-string "Index entry: ")))))
    :transient t
    :description "Index entry")
   ("pi" (lambda ()
	   (interactive)
	   (insert "[[printindex:]]"))
    :transient t
    :description "Print index" )
   ("pg" (lambda ()
	   (interactive)
	   (insert "[[printglossaries:]]"))
    :transient t
    :description "Print glossary" )
   ]
  ])

;(or-insert-link-transient)

(when nil
(defun wrap-in-lambda ()
  (interactive)
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'sexp)
    (save-excursion
      (let ((str (buffer-substring beg end)))
	(delete-region beg end)
	(insert (format "(lambda ()
 (interactive)
 %s)"
			str))
	(indent-region beg end)))))
(my-replace-regexp ":column \"[^\"]+\"" "")
)

(provide 'or-transient)