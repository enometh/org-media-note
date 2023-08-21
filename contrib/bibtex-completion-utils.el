;;; -*- Mode: emacs-lisp; lexical-binding: t -*-
;;;
;;; org-media-note/contrib/bibtex-completion-utils.el.  extra utils
;;; for bibtex-completion.el from
;;; https://github.com/tmalsburg/helm-bibtex (checked commit 8ebf50d5b
;;; from 2022-11-04)

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
                                     "\\(.+?\\)" ;; entry-key
				     "[[:space:]]*,")
                             nil t)
          (let* ((entry-type (match-string 1))
		 (entry-key (match-string 2))
		 (entry (parsebib-read-entry entry-type (point))))
	    (funcall func entry-key entry))))))

(provide 'bibtex-completion-utils)
