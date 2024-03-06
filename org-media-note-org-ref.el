;;  -*- lexical-binding: t; -*-
;; ;madhu 250704 - reinstate support for org-ref removed from org-media-note in 8d0d03a8
(require 'org-ref-core)

(defun org-media-note-ref-parse-path (path)
  "try to handle org-ref  v3 syntax"
  (let* ((pos (cl-position ?\# path))
	 (cite (org-ref-parse-cite-path path))
	 (references (plist-get cite :references))
	 ;; XXX return the first key
	 (ref (car references)))
    (if (eql (plist-get cite :version) 3)
	(if (and pos org-media-note-working-with-kluged-org-ref-p)
	    (concat (plist-get ref :key) (plist-get ref :suffix))
	  (plist-get ref :key))
      (concat (plist-get ref :key) (and pos (cl-subseq path pos))))))

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
    (let* ((ref-cite-key (org-media-note-ref-parse-path ref-cite-key)) ;bad
	   (bibtex-completion-bibliography (org-ref-find-bibliography))
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

(defun org-media-note--org-ref-key-from-cite ()
  (let* ((object (org-element-context))
	 (type (org-element-property :type object))
	 (media-note-link (org-media-note-ref-parse-path
			   (org-element-property :path object)))
	 (ref-cite-key (car (split-string media-note-link "#"))))
    (and ;; (cl-member type '("cite") :test #'equal)
     ref-cite-key)))

;;; org activation: activate font-lock for org-ref citation links
;;;
;; ;madhu 240318 to version 2
;; modify org-ref-cite-activate in   org-ref/org-ref-citation-links.el; in the version 2 branch to set
;;	 (setq substrings (cl-loop for key in substrings for p = (cl-position ?\# key) if p collect (cl-subseq key 0 p) else collect p))

;; ;madhu 240318 - unconditionally modify the way org-ref activates
;; citation keys to include `#' this only works for version 3. for
;; version 2 add an advice.

(setq org-ref-citation-key-re
  (rx-to-string
   ;; '(seq "&" (group-n 1 (one-or-more (any word "-.:?!`'/*@+|(){}<>&_^$#%~"))))
   '(seq "&" (group-n 1 (one-or-more (any word "-.:?!`'/*@+|(){}<>&_^$%~"))))))

(defun org-media-note-ref--org-ref-arse-cite-path--around-advice (orig-function path)
  (let* ((ret (funcall orig-function path))
	 (keys (plist-get ret :references)))
    (when (eql (plist-get ret :version) 2)
      (cl-loop for elt in keys
	       for key = (plist-get elt :key)
	       for p = (cl-position ?\# key)
	       if p do (setf (plist-get elt :key) (cl-subseq key 0 p))))
    ret))

(defvar org-media-note-working-with-kluged-org-ref-p
  (cl-every (lambda (re) (not (cl-find ?\# re)))
	 (list org-ref-citation-key-re
	       ;; org-ref-label-re
	       ;;org-ref-label-link-re
	       ))
  "Non-NIL if org-ref has been modified to reject `#' as a part of a path")

(when org-media-note-working-with-kluged-org-ref-p
  (advice-add 'org-ref-parse-cite-path :around
	      'org-media-note-ref--org-ref-arse-cite-path--around-advice))

(provide 'org-media-note-org-ref)