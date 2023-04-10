;; ;madhu 230409 org-ref-citation-links-transient.el
(require 'transient)
(require 'org-ref-citation-links)

;;(org-ref-citation-hydra/body)
(transient-define-prefix org-ref-citation-transient ()
  [:description  "Citation actions" ""]
  [["open"
    ("o" org-ref-open-citation-at-point :description "Bibtex")
    ("p" org-ref-open-pdf-at-point :description "PDF")
    ("n" org-ref-open-notes-at-point :description "Notes")
    ("u" org-ref-open-url-at-point :description "URL")]

   ["WWW"
    :pad-keys t
    ;; WWW actions
    ("ww" org-ref-wos-at-point :description "WOS")
    ("wr" org-ref-wos-related-at-point :description "WOS related")
    ("wc" org-ref-wos-citing-at-point :description "WOS citing")
    ("wg" org-ref-google-scholar-at-point :description "Google Scholar")
    ("wp" org-ref-pubmed-at-point :description "Pubmed")
    ("wf" org-ref-crossref-at-point :description "Crossref")
    ("wb" org-ref-biblio-at-point :description "Biblio")
    ("e" org-ref-email-at-point :description "Email")]

   ["Copy"
    ;; Copyish actions
    ("K" (lambda ()
	   (interactive)
	   (save-window-excursion
	     (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
	       (bibtex-completion-show-entry (list (org-ref-get-bibtex-key-under-cursor)))
	       (bibtex-copy-entry-as-kill)
	       (kill-new (pop bibtex-entry-kill-ring)))))
     :description "Copy bibtex")
    ("k" (lambda ()
	   (interactive)
	   (kill-new (car (org-ref-get-bibtex-key-and-file))))
     :description "Copy key")
    ("f"  (lambda ()
	    (interactive)
	    (kill-new (bibtex-completion-apa-format-reference
		       (org-ref-get-bibtex-key-under-cursor))))
     :description "Copy formatted")
    ("h"  (lambda ()
	    (interactive)
	    (kill-new
	     (format "* %s\n\n cite:&%s"
		     (bibtex-completion-apa-format-reference
		      (org-ref-get-bibtex-key-under-cursor))
		     (car (org-ref-get-bibtex-key-and-file)))))
     :description  "Copy org heading")]

   ["Edit"
    :pad-keys t
    ;; Editing actions
    ("S-<left>" org-ref-cite-shift-left :description "Shift left" :transient t)
    ("S-<right>" org-ref-cite-shift-right :description "Shift right" :transient t)
    ("S-<up>" org-ref-sort-citation-link :description "Sort by year")
    ("i" (lambda ()
	   (interactive)
	   (funcall org-ref-insert-cite-function))
     :description "Insert cite")
    ("t" org-ref-change-cite-type :description "Change cite type")
    ("d" org-ref-delete-citation-at-point :description "Delete at point")
    ("r" org-ref-replace-citation-at-point :description "Replace cite")
    ("P" org-ref-edit-pre-post-notes :description "Edit pre/suffix")]

   ["Navigation"
    ;; Navigation
    ("[" org-ref-previous-key :description "Previous key" :transient t)
    ("]"  org-ref-next-key :description "Next key" :transient t)
    ("v" org-ref-jump-to-visible-key :description "Visible key" :transient t)]])

;(org-ref-citation-transient)

(provide 'orc-transient)