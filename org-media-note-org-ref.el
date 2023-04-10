;;  -*- lexical-binding: t; -*-
;; ;madhu 250704 - reinstate support for org-ref removed from org-media-note in 8d0d03a8
(require 'org-ref-core)


(defun org-media-note-open-ref-cite-function ()
  "Open a ref-cite link."
  (interactive)
  (let* ((object (org-element-context))
         (media-note-link (if (eq (org-element-type object) 'link)
			      (org-element-property :path object)))
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

(provide 'org-media-note-org-ref)