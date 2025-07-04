;;  -*- lexical-binding: t; -*-
;; ;madhu 250704 - reinstate support for org-ref removed from org-media-note in 8d0d03a8
(require 'org-ref-core)

(defun org-media-note-ref-parse-path (path)
  "try to handle org-ref  v3 syntax"
  (let* ((cite (org-ref-parse-cite-path path))
	 (references (plist-get cite :references))
	 (keys (cl-loop for ref in references collect
			(plist-get ref :key))))
    ;; XXX return the first key
    (car keys)))

(defun org-media-note-open-ref-cite-function ()
  "Open a ref-cite link."
  (interactive)
  (let* ((object (org-element-context))
         (media-note-link (if (eq (org-element-type object) 'link)
			      (org-media-note-ref-parse-path
			       (org-element-property :path object))))
         (ref-cite-key (car (split-string media-note-link "#"))))
    (with-temp-buffer
      (org-mode)
      ;; insert bibliography in order to find entry in org-ref
      (insert (mapconcat (mapcar (lambda (bib)
                                (format "bibliography:%s" bib))
                              org-media-note-cite-bibliography)
			  "\n"))
      (insert (format "\ncite:%s" ref-cite-key))
      (funcall org-ref-cite-onclick-function nil))))

(defcustom org-media-note-cite-keymap
  (let ((map (copy-keymap org-mouse-map)))
    (define-key map (kbd "s-o") 'org-media-note-open-ref-cite-function)
    map)
  "Keymap for cite links."
  :type 'symbol
  :group 'org-media-note)

(defun org-media-note-ref-cite (ref-cite-key)
  (if (fboundp 'org-ref-format-entry)
      (funcall 'org-ref-format-entry ref-cite-key)
    ;; ;madhu 230410 copied from org-ref/org-ref-citation-links.el:
    ;; (org-ref-cite-tooltip). FIXME refactor in org-ref.
    (let* ((bibtex-completion-bibliography (org-ref-find-bibliography))
           (has-pdf (when (bibtex-completion-find-pdf ref-cite-key)
                      bibtex-completion-pdf-symbol))
           (has-notes
            (when (cl-some #'identity
                           (mapcar (lambda (fn)
                                     (funcall fn ref-cite-key))
                                   bibtex-completion-find-note-functions))
              bibtex-completion-notes-symbol)))
      (format "%s%s %s" (or has-pdf "") (or has-notes "")
              (bibtex-completion-apa-format-reference ref-cite-key)))))

(provide 'org-media-note-org-ref)