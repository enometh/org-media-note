;;; org-media-note-org-ref.el --- Integrate org-media-note with org-ref -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Yuchen Lea

;; Author: Yuchen Lea <yuchen.lea@gmail.com>
;; URL: https://github.com/yuchen-lea/org-media-note
;; Package-Requires: ((emacs "27.1") (org-ref "1.1.1"))


;;; Commentary:

;;; Code:
;;;; Requirements
(require 'seq)
(require 'org-ref-core)

(require 'org-media-note-core)

;;;; Customization
(defcustom org-media-note-bibtex-files bibtex-files
  "List of BibTeX files that are searched for entry keys."
  :type '(repeat (choice (const :tag "bibtex-file-path" bibtex-file-path)
                         directory file)))
;;;; Variables
;;;; Commands
;;;;; Help echo
(defun org-media-note-help-echo (_window _object position)
  "Return help-echo for ref link at POSITION."
  (save-excursion
    (goto-char position)
    (let ((s (org-media-note-media-cite-link-message)))
      (when s
        (with-temp-buffer
          (insert s)
          (fill-paragraph)
          (buffer-string))))))

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

;; compat
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

(defvar org-media-note-link-types '("videocite" "audiocite"))

(defun org-media-note-media-cite-link-message ()
  "Print a minibuffer message about the link that point is on."
  (interactive)
  ;; the way links are recognized in org-element-context counts blank spaces
  ;; after a link and the closing brackets in literal links. We don't try to get
  ;; a message if the cursor is on those, or if it is on a blank line.
  (when (not (or (looking-at " ") ;looking at a space
                 (looking-at "^$") ;looking at a blank line
                 (looking-at "]") ;looking at a bracket at the end
                 (looking-at "$"))) ;looking at the end of the line.
    (save-restriction (widen)
                      (when (eq major-mode 'org-mode)
                        (let* ((object (org-element-context))
                               (type (org-element-property :type object)))
                          (save-excursion
                            (cond
                             ((cl-find type org-media-note-link-types :test #'equal)
                              (let* ((media-note-link (org-media-note-ref-parse-path
						       (org-element-property :path object)))
                                     (ref-cite-key (car (split-string media-note-link "#")))
                                     (hms (cdr (split-string media-note-link "#"))))
                                (format "%s @ %s"
                                        (or (and ref-cite-key (org-media-note-ref-cite ref-cite-key))
                                            "??")
                                        hms))))))))))

(defun org-media-note-display-media-cite-link-message-in-eldoc (&rest _)
  "Display media's cite link message when `eldoc' enabled."
  (org-media-note-media-cite-link-message))

;;;;; Keymap
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
      (insert (s-join "\n"
                      (mapcar (lambda (bib)
                                (format "bibliography:%s" bib))
                              org-media-note-bibtex-files)))
      (insert (format "\ncite:%s" ref-cite-key))
      (funcall org-ref-cite-onclick-function nil))))

(defcustom org-media-note-cite-keymap
  (let ((map (copy-keymap org-mouse-map)))
    (define-key map (kbd "s-o") 'org-media-note-open-ref-cite-function)
    map)
  "Keymap for cite links."
  :type 'symbol
  :group 'org-media-note)

;;;;; Link Follow
(defun org-media-note-media-cite-link-follow (link)
  "Open videocite and audiocite LINKs, supported formats:
1. videocite:course.104#0:02:13: jump to 0:02:13
2. videocite:course.104#0:02:13-0:02:20: jump to 0:02:13 and loop between 0:02:13 and 0:02:20"
  (let* ((splitted (split-string link "#"))
         (key-1 (nth 0 splitted))
	 (key (org-media-note-ref-parse-path key-1))
         (file-path-or-url (or (org-media-note-get-media-file-by-key key) (org-media-note-get-url-by-key key)))
         (timestamps (if (nth 1 splitted)
			 (split-string (nth 1 splitted)
                                   "-")
		       '("0")))
         (time-a (int-to-string (org-timer-hms-to-secs (nth 0 timestamps))))
         (time-b (if (= (length timestamps) 2)
                     (int-to-string (org-timer-hms-to-secs (nth 1 timestamps))))))
    (cond
     ((not file-path-or-url)
      (error "Cannot find media file for this Key"))
     (t (org-media-note--follow-link file-path-or-url time-a time-b)))))

(defvar org-media-note-bypass-realpath t
  "Don't use file-truename to resolve media file locations, as this may resolve
to a filename which is not identical to the file field in the bibliography,
and reverse lookups may fail.")

(defun org-media-note-get-media-file-by-key (key)
  "Get media file by KEY."
  (let* ((bibtex-completion-bibliography (org-ref-find-bibliography))
	 (files (bibtex-completion-find-pdf key))
         (video-files (seq-filter (lambda (elt)
                                    (s-matches-p (rx (eval (cons 'or org-media-note--video-types))
                                                     eos)
                                                 elt))
                                  files))
         (audio-files (seq-filter (lambda (elt)
                                    (s-matches-p (rx (eval (cons 'or org-media-note--audio-types))
                                                     eos)
                                                 elt))
                                  files)))


    (cl-letf (((symbol-function 'file-truename)
	       (if org-media-note-bypass-realpath
		   #'identity #'file-truename)))
    (cond
     ;; TODO when multiple media files?
     (video-files (file-truename (nth 0 video-files)))
     (audio-files (file-truename (nth 0 audio-files)))
     (t nil)))))

(defun org-media-note-get-url-by-key (key)
  "Get URL by KEY."
  (if key
      (let ((entry (bibtex-completion-get-entry1 key t)))
        (bibtex-completion-get-value "url" entry))))
;;;;; Setup

;;  (plist-get (cdr (assoc "audiocite" org-link-parameters)) :activate-func)
;;  (plist-get (cdr (assoc "videocite" org-link-parameters)) :activate-func)
;;  (plist-get (cdr (assoc "videocite" org-link-parameters)) :export)

;;;###autoload
(defun org-media-note-setup-org-ref ()
  "Set org link parameters for video/audiocite links."
  (dolist (link '("videocite" "audiocite"))
    (org-link-set-parameters link :follow 'org-media-note-media-cite-link-follow
                             :keymap org-media-note-cite-keymap
                             :help-echo #'org-media-note-help-echo)
    (cl-pushnew (cons link
		      (list "org-media-note extensions via org-media-note"))
		org-ref-cite-types
		:test (lambda (a b) (equal (car a) (car b))))
    (setf (plist-get (cdr (assoc link org-link-parameters)) :activate-func)
	  #'org-ref-cite-activate))

  ;; Display media link description in minibuffer when cursor is over it.
  (when (fboundp 'org-eldoc-documentation-function) ;;madhu 230422 gonn on FIXME
    (advice-add #'org-eldoc-documentation-function
		:before-until #'org-media-note-display-media-cite-link-message-in-eldoc)))


(defun org-media-note--org-ref-key-from-cite ()
  (let* ((object (org-element-context))
	 (type (org-element-property :type object))
	 (media-note-link (org-media-note-ref-parse-path
	       (org-element-property :path object)))
	 (ref-cite-key (car (split-string media-note-link "#"))))
    (and ;; (cl-member type '("cite") :test #'equal)
	 ref-cite-key)))

;;;; Footer
(provide 'org-media-note-org-ref)
;;; org-media-note-org-ref.el ends here
