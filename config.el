;; -*- lexical-binding: nil -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sun Mar 03 08:25:36 2024 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2024 Madhu.  All Rights Reserved.
;;;
;;
;; configure via use-package: org ox bibtex bibtex-completion org-ref
;; mpv org-media-note org-ref-publish

;;(cl-assert (featurep 'bibtex))
(use-package bibtex
  :init
  (setq bibtex-dialect 'biblatex)
  (setq bibtex-files nil)
  (setq bibtex-search-entry-globally nil)
  (setq bibtex-file-path nil)
  ;;(setq bibtex-unify-case-function 'identity)
  (setq bibtex-unify-case-function 'downcase)
  ;;(setq bibtex-entry-format '(opts-or-alts required-fields numerical-fields))
  (setq bibtex-entry-format '(
			      last-comma
			      numerical-fields
			      opts-or-alts
			      realign
			      ;; required-fields
			      unify-case
			      whitespace
			      sort-fields
			      )))

;;(cl-assert (featurep 'bibtex-completion))
(locate-library "bibtex-completion")

(use-package bibtex-completion
  :init
  (setq bibtex-completion-bibliography nil) ;XXX
  (setq bibtex-completion-library-path nil)
  (setq bibtex-completion-notes-path nil) ;XXX
  (setq bibtex-completion-pdf-field "file"))

;; bibtex-completion-library-path
;; bibtex-completion-notes-path
;; bibtex-completion-notes-template-multiple-files
;; bibtex-completion-additional-search-fields '(keywords)

;;(cl-assert (featurep 'ol-bibtex))
;;(locate-library "ol-bibtex")
(use-package ol-bibtex
  :init
  (setq org-bibtex-file nil)		;XXX
  (setq org-bibtex-autogen-keys t)
  (setq org-bibtex-prefix "BIB_")
  (setq org-bibtex-export-arbitrary-fields t))

;;(cl-assert (featurep 'ob-core))
(use-package ob-core
  :init
  (setq org-confirm-babel-evaluate t))

;;(locate-library "ox")
;;(cl-assert (featurep 'ox))
(use-package ox
  :init
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-sub-superscripts t)
  (setq org-export-with-toc nil)
  ;;(setq org-export-with-broken-links nil)
  (setq org-export-with-broken-links 'mark))

;;(cl-assert (featurep 'ox-html))
(use-package ox-html
  :init
  (setq org-html-doctype "html5")
  (setq org-html-toplevel-hlevel 2)
  ;;(setq org-html-htmlize-output-type 'css)
  (setq org-html-html5-fancy t))

;;(cl-assert (featurep 'ox-publish))
;;(locate-library "ox-publish")
(use-package ox-publish
  :init
  (setq org-publish-use-timestamps-flag t) ;nil
  (setq org-export-async-debug t))

;; org-export-before-parsing-functions
(use-package ox
  :config
  (add-hook 'org-export-before-parsing-functions 'org-ref-acronyms-before-parsing)
  (add-hook 'org-export-before-parsing-functions 'org-ref-glossary-before-parsing)
  ;;(add-hook 'org-export-before-parsing-functions 'org-ref-process-buffer)
  )

;;(cl-assert (featurep 'ox-latex))
;;(locate-library "ox-latex")
(use-package ox-latex
  :init
  (setq org-latex-pdf-process '("latexmk -verbose -f -pdf -%latex -interaction=nonstopmode -bibtex -output-directory=%o %f"))
  (setq org-latex-pdf-process
	'("pdflatex -interaction nonstopmode -output-directory %o %f"
	  "bibtex %b"
	  "makeglossaries %b"
	  "pdflatex -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-compiler "pdflatex")
  (setq org-latex-bib-compiler "biber"))

;;(cl-assert (featurep 'org-ref-publish))
;; enometh/org-media-note/contrib
(use-package org-ref-publish)

;;(locate-library "mpv")
;;(cl-assert (featurep 'mpv))
(use-package mpv
  :config
  (cl-pushnew "--player-operation-mode=pseudo-gui"
	      mpv-default-options
	      :test #'equalp)
  (setq mpv-default-options nil))

;;(cl-assert (featurep 'org-media-note))
;;(cl-assert (featurep 'org-media-note-org-ref))
(use-package org-media-note
  :init
  (setq org-media-note-use-org-ref t)
  :hook (org-mode . org-media-note-mode)
  :config
  (setq org-media-note-pause-after-insert-link nil)
  (setq org-media-note-use-refcite-first t))

(use-package org-media-note-org-ref
  :config
  (setq org-media-note-bibtex-files nil))

;;(locate-library "org")
(use-package org
  :init
  (setq org-adapt-indentation nil)
  (setq org-agenda-files nil)
  (setq org-agenda-remove-times-when-in-prefix 'beg) ; parashiot
  (setq org-descriptive-links nil)
  ;;(setq org-export-allow-bind-keywords t)
  (setq org-insert-mode-line-in-empty-file t)
  (setq org-src-preserve-indentation t)
  (setq org-src-window-setup 'other-window)
  (setq org-startup-folded 'nofold)
  (setq org-startup-truncated nil)
  (setq org-time-stamp-formats
	'("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M:%S %Z>"))
  :config
  (cl-pushnew '("el" . "src emacs-lisp") org-structure-template-alist
	      :test (lambda (a b) (cl-equalp (car a) (car b))))
  (cl-pushnew '("sh" . "src sh") org-structure-template-alist
	      :test (lambda (a b) (cl-equalp (car a) (car b)))))


