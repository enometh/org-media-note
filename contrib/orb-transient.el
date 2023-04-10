;; ;madhu 230409 org-ref-bibtext-transient.el -*- lexical-binding: t -*-
(require 'transient)
(require 'org-ref-bibtex)

;;(org-ref-bibtex-hydra/body)
(transient-define-prefix orb-transient ()
  [:description "Bibtex actions:" ""]
  [["Open"
    ;; Open-like actions
    ("p" org-ref-open-bibtex-pdf :description "PDF")
    ("n" org-ref-open-bibtex-notes :description "Notes")
    ("b" org-ref-open-in-browser :description "URL")]

   ["Edit"
    :pad-keys t
    ;; edit/modify
    ("K" (lambda ()
           (interactive)
           (org-ref-set-bibtex-keywords
            (read-string "Keywords: "
			 (bibtex-autokey-get-field "keywords"))
            t))
     :description "Keywords")
    ("a" org-ref-replace-nonascii :description "Replace nonascii")
    ("s" org-ref-sort-bibtex-entry :description "Sort fields")
    ("T" org-ref-title-case-article :description  "Title case")
    ("S" org-ref-sentence-case-article :description  "Sentence case")
    ("U" (lambda ()
	   (interactive)
	   (doi-utils-update-bibtex-entry-from-doi (org-ref-bibtex-entry-doi)))
     :description  "Update entry"
     :transient t)
    ("u" doi-utils-update-field  :description "Update field"
     :transient t)
    ("<backspace>" (lambda ()
		     (interactive)
		     (cl--set-buffer-substring (line-beginning-position)
					       (+ 1 (line-end-position)) ""))
     :description "Delete line"
     :transient t)
    ("d" bibtex-kill-entry  :description "Kill entry")
    ("L" org-ref-clean-bibtex-entry  :description "Clean entry")
    ("A" org-ref-bibtex-assoc-pdf-with-entry :description "Add pdf")
    ("r" (lambda ()
	   (interactive)
           (bibtex-beginning-of-entry)
           (bibtex-kill-entry)
           (find-file (completing-read
                       "Bibtex file: "
		       (append bibtex-completion-bibliography
			       (f-entries "." (lambda (f) (f-ext? f "bib"))))))
           (goto-char (point-max))
           (bibtex-yank)
           (save-buffer)
           (kill-buffer))
     :description "Refile entry")]

   ["www"
    ;; www
    ("P" org-ref-bibtex-pubmed :description "Pubmed")
    ("w" org-ref-bibtex-wos :description "WOS")
    ("c" org-ref-bibtex-wos-citing :description "WOS citing")
    ("a" org-ref-bibtex-wos-related :description "WOS related")
    ("R" org-ref-bibtex-crossref :description "Crossref")
    ("g" org-ref-bibtex-google-scholar :description "Google Scholar")
    ("e" org-ref-email-bibtex-entry :description "Email")]]
  [
   ["Copy"
    ;; Copy
    ("o" (lambda ()
	   (interactive)
	   (bibtex-copy-entry-as-kill)
	   (message "Use %s to paste the entry"
		    (substitute-command-keys (format "\\[bibtex-yank]"))))
     :description    "Copy entry")

    ("y" (lambda ()
	   (interactive)
	   (save-excursion
	     (bibtex-beginning-of-entry)
	     (when (looking-at bibtex-entry-maybe-empty-head)
	       (kill-new (bibtex-key-in-head)))))
     :description "Copy key")

    ("f" (lambda ()
	   (interactive)
	   (save-excursion
	     (bibtex-beginning-of-entry)
	     (kill-new (bibtex-completion-apa-format-reference
			(cdr (assoc "=key=" (bibtex-parse-entry t)))))))
     :description "Formatted entry")]

  ["Navigation"
    :pad-keys t
    ;; Navigation
    ("[" org-ref-bibtex-next-entry :description "Next entry"
     :transient t)
    ("]" org-ref-bibtex-previous-entry :description "Previous entry"
     :transient t)
    ;;madhu 230409 add Shift so we can still scroll the transient screen
    ("S-<down>" next-line :description "Next line"
     :transient t)
    ("S-<up>" previous-line :description "Previous line"
     :transient t)
    ("S-<next>" scroll-up-command :description "Scroll up"
     :transient t)
    ("S-<prior>" scroll-down-command :description "Scroll down"
     :transient t)
    ("v" org-ref-bibtex-visible-entry :description "Visible entry"
     :transient t)
    ("V" org-ref-bibtex-visible-field :description "Visible field"
     :transient t)]

   ["Misc"
    ;; Miscellaneous
    ("F" orb-file :description "File")
    ("N" orb-new-entry :description "New entry")]]
  ["\n" "\n"]
  )

;;(orb-transient)

;;(org-ref-bibtex-new-entry/body)
(transient-define-prefix orb-new-entry ()
  ["New Bibtex entry:"
   ["Automatic"
    ("d" doi-insert-bibtex :description "from DOI")
    ("c" crossref-add-bibtex-entry :description "from Crossref")
    ("a" (lambda () (interactive)(arxiv-add-bibtex-entry))
     :description "From Arxiv")
    ("b" biblio-lookup :description "From biblio")]
   ;; Bibtex types
   ["Manual"
    ("ma" bibtex-Article :description "Article")
    ("mb" bibtex-Book :description "Book")
    ("mi" bibtex-InBook :description "In book")
    ("ml" bibtex-Booklet :description "Booklet")
    ("mP" bibtex-Proceedings :description "Proceedings")
    ("mp" bibtex-InProceedings :description "In proceedings")
    ("mm" bibtex-Misc :description "Misc.")
    ("mM" bibtex-Manual :description "Manual")
    ("mT" bibtex-PhdThesis :description "PhD Thesis")
    ("mt" bibtex-MastersThesis :description "MS Thesis")
    ("mR" bibtex-TechReport :description "Report")
    ("mu" bibtex-Unpublished :description "unpublished")
    ("mc" bibtex-InCollection :description "Article in collection")
    ]])

;;(orb-new-entry)

;;(org-ref-bibtex-file/body)
(transient-define-prefix orb-file ()
  ["Bibtex file functions: "
   ("v" bibtex-validate :description "Validate entries")
   ("s" bibtex-sort-buffer :description "Sort entries")
   ("r" bibtex-reformat :description "Reformat entries")
   ("c" bibtex-count-entries :description "Count entries")
   ("p" org-ref-build-full-bibliography :description "PDF bibliography")])

;(orb-file)

;; ;madhu 230409 notes -
;; (wrap forms in (lambda () (interactive))
;; (rep-reg " :column.*$" ")" )
;; (rep-reg " \"" " :description \"")
;; (rep-reg ":color red" ":transient t")

(provide 'orb-transient)