;;; -*- lexical-binding: t -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Thu Mar 28 10:50:58 2023 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2023 Madhu.  All Rights Reserved.
;;;


;; https://emacs.stackexchange.com/questions/30344/how-to-link-and-open-a-pdf-file-to-a-specific-page-skim-adobe
;; ;madhu 230328

(org-add-link-type "pdf" 'org-pdf-open nil)


(defun org-pdf-open-default (pdf-file)
  (interactive)
  (let ((ext (downcase (file-name-extension pdf-file))))
    (if (or (cl-some (lambda (x) (string-match x ext)) org-media-note--video-types)
	    (cl-some  (lambda (x) (string-match x ext)) org-media-note--audio-types))
	(mpv-play pdf-file)
      (if current-prefix-arg
	  (start-process "view-pdf" nil "evince"  pdf-file)
	(start-process "view-pdf" nil "mupdf-x11" pdf-file)
	))))


(setq bibtex-completion-pdf-open-function 'org-pdf-open-default)

(defun org-pdf-open (link)
  "Where page number is 105, the link should look like:
   [[pdf:/path/to/file.pdf#page=105][My description.]]
;madhu 230328  with a prefix arg open mupdf otherwise open evince
"
  (interactive "P")
  (let* ((path+page (split-string link "#page="))
         (pdf-file (car path+page))
         (page (car (cdr path+page))))
    (if current-prefix-arg
	(start-process "view-pdf" nil "evince" "--page-index" (or page "0") pdf-file)
        (start-process "view-pdf" nil "mupdf-x11" pdf-file (or page "0"))
    )))

(require 'org-media-note-org-ref)

(org-add-link-type "pdfcite" 'org-pdf-cite-open nil)

(defun org-pdf-cite-open (link)
  (let* ((bibtex-completion-bibliography (org-ref-find-bibliography))
	 (pos (cl-position ?# link))
	 (ref-cite-1 (substring link 0 pos))
	 (key (org-media-note-ref-parse-path ref-cite-1))
	 (file (car (bibtex-completion-find-pdf key))))
    (when file
      (org-pdf-open (concat file (and pos (substring link pos)))))))

(org-link-set-parameters "pdfcite" :help-echo #'org-pdf-cite-help-echo)

(defun org-pdf-cite-help-echo  (_window _object position)
  (let ((org-media-note-link-types
	 (cl-adjoin "pdfcite" org-media-note-link-types :test #'equal)))
    (org-media-note-help-echo _window _object position)))

(provide 'org-pdf-open)

;; Local Variables:
;; byte-compile-warnings: (not docstrings docstrings-wide)
;; End:
