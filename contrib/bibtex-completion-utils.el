;;; -*- Mode: emacs-lisp; lexical-binding: t -*-
;;;
;;; org-media-note/contrib/bibtex-completion-utils.el.  extra utils
;;; for bibtex-completion.el from
;;; https://github.com/tmalsburg/helm-bibtex (checked commit 6064e8625b295
;;; from 2024-11-16)
;; Package-Requires: ((parsebib "6.0"))
;;(require 'parsebib)
(require 'bibtex-completion)

(defun bibtex-completion-map-entries (func)
  "Call func with 2 args the entry-key and entry"
  (let ((bib (bibtex-completion-normalize-bibliography 'bibtex)))
    (with-temp-buffer
      (mapc #'insert-file-contents bib)
      (goto-char (point-min))
      (while
	  (re-search-forward (concat "^[ \t]*@\\(" parsebib--bibtex-identifier
                                     "\\)[[:space:]]*[\(\{][[:space:]]*"
                                     ;; "\\(.+?\\)" entry-key pre 6.0
				     "\\("  parsebib--bibtex-key-regexp "\\)"
				     "[[:space:]]*,")
                             nil t)
        (goto-char (match-beginning 0))	;not for pre 6.0
        (let* ((_entry-type (match-string 1))
	       (entry-key (match-string 2))
	       (entry ;; (parsebib-read-entry _entry-type (point)) pre 6.0
		(parsebib-read-entry nil bibtex-completion-string-hash-table)
		))
	  (funcall func entry-key entry))))))

(defun my-inveigle-biblatex ()
  "When visiting a bibtex file, set bibtex-completion-bibliography
variable to include the current file, so org-ref bibtex functions like
`org-ref-bibtex-entry-menu' can work on it. e.g. as a file local:
variable:

% -*- mode:bibtex; eval: (my-inveigle-biblatex) -*-
"
  (interactive)
  (when (eq major-mode 'bibtex-mode)
    (bibtex-set-dialect 'biblatex t)
    (let ((path (expand-file-name (buffer-file-name))))
      (unless (cl-find path bibtex-completion-bibliography :test #'equal)
	(setq-local bibtex-completion-bibliography
		    (cons path bibtex-completion-bibliography))))))

(provide 'bibtex-completion-utils)
