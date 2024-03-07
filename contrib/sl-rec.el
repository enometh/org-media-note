;;; sl-rec.el  -*- lexical-binding: t -*-
;;
;;   Time-stamp: <>
;;   Touched: Sun Apr 23 12:27:42 AM IST 2023
;;   Bugs-To: enometh@net.meer
;;   Status: Experimental.  Do not redistribute
;;   Copyright (C) 2023 Madhu.  All Rights Reserved.
;;
;;
;; SYNOPSIS.  Generate bib files for (say media) files in a ZIP
;; archive, unpack them by key.  USAGE.  Subclass `sl-rec' and define
;; methods `sl-rec-describe-file-name' (eql specialized on BIB-KEY and
;; BIB-ENTRY), and `sl-rec-get-source-zip-pathname'.  Initialize an
;; instance by supplying the SOURCE-DIR for zip files, the TARGET-DIR
;; for unpacked files (which becomes the `file' bib field), the
;; KEY-TEMPLATE to produce a bibtex citation key from the BIB-KEY
;; desc, and the ENTRY-TEMPLATE to produce a bibtex entry from the
;; BIB-ENTRY desc.  You can then produce bib files for the zip-files
;; in SOURCE-DIR by calling `sl-rec-dump-bibs-file-for-zip'.  After
;; adding the paths of these bib files to
;; `bibtex-completion-bibliograhy' (and reloading the bibliographies
;; say via `bibtex-completion-init') the bibtex citation keys are
;; available to org-ref and org-media-note.  You can unpack a media
;; file (with the cursor on an org-ref citation) with
;; `sl-rec-extract-file-from-zip' after setting up `$sl-recs'

(defclass sl-rec ()
  ((target-dir :initarg :target-dir
	       :documentation "directory to extract the files from the zip")
   (source-dir :initarg :source-dir
	       :documentation "directory containing zips")
   (bib-key-template :initarg :key-template
		     :documentation
		     "mustache template for producing a bibtex citation key from a DESC")
   (bib-entry-template :initarg :entry-template
		       :documentation
		       "mustache template for producing a bibtex entry"))
  (:documentation "SL REC"))

(cl-defgeneric sl-rec-describe-file-name (sl-rec file-name type)
  "Return a DESC that uniquely identifies the the file-name.
DESC may be a plist or an alist of bindings which are used to fill a mustache
template of type TYPE . TYPE is either `bib-key' or `bib-entry'.
NOTE that the method specialised on `bib-key' is called to produce the DESC
for `bib-entry' since the entry will need a `key'.")

(cl-defgeneric sl-rec-get-source-zip-pathname (self file-name)
  "Return the path to the zip file from which file-name was extracted.")

;;;
;;; ad-hoc mustache for elisp
;;;
(defun mustache-lookup-binding (string-key bindings)
  (cond ((null bindings)
	 (eval (intern-soft string-key)))
	((consp (car bindings))
	 (cdr (cl-assoc (if (stringp (car (car bindings)))
			    string-key
			  (intern-soft string-key))
			bindings :test #'equal)))
	(t (cl-assert (symbolp (car bindings)))
	   (plist-get bindings (intern-soft string-key)))))

(cl-defun mustache (string &key readable bindings)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "{{\\([^}{]+?\\)}}" nil t)
      (let* ((string-key (match-string 1))
	     (val (mustache-lookup-binding string-key bindings))
	     (replacement (if readable (prin1-to-string val) "")))
	(replace-match replacement nil nil nil 0)
	(unless readable
	  (princ val (current-buffer)))))
    (buffer-substring (point-min) (point-max))))

;;;
;;;
;;;
(cl-defmethod sl-rec-produce-citation-key ((self sl-rec) bindings-desc)
  "Return a citation key from DESC
\(DESC is retrieved with `sl-rec-describe-file-name'.)"
  (with-slots (bib-key-template) self
    (mustache bib-key-template :bindings bindings-desc)))

(cl-defmethod sl-rec-produce-entry-for-file ((self sl-rec) file-name)
  (let ((desc (sl-rec-describe-file-name self file-name 'bib-entry)))
    (with-slots (bib-entry-template) self
      (mustache bib-entry-template :bindings desc))))

(defun sl-rec-get-file-list-from-zip (path)
  "Utility."
  (with-temp-buffer
    (call-process "/usr/bin/unzip" nil t nil "-l" path)
    (goto-char (point-min))
    (cl-assert (looking-at "^Archive:"))
    (forward-line 1)
    (cl-assert (looking-at "[ ]+Length"))
    (let* ((_pos (re-search-forward "Name" (line-end-position)))
	   (col (- (match-beginning 0) (line-beginning-position)))
	   (ret))
      (forward-line 1)
      (cl-assert (looking-at "^-+"))
      (forward-line 1)
      (while (not (looking-at "^-+"))
	(push (buffer-substring-no-properties
	       (+ (line-beginning-position) col)
	       (line-end-position))
	      ret)
	(forward-line 1))
      (nreverse ret))))

;; EXPORT
(cl-defmethod sl-rec-dump-bib-file-for-zip ((self sl-rec) output-file zip-file-name)
  (with-slots (source-dir) self
    (let ((file-list (sl-rec-get-file-list-from-zip
		      (file-name-concat source-dir zip-file-name))))
      (with-temp-buffer
	(mapc (lambda (f)
		(insert (sl-rec-produce-entry-for-file self f)))
	      file-list)
	(insert "\n")
	(write-region (point-min) (point-max) output-file)))))

(require 'org-media-note-org-ref)

(defvar $sl-recs nil
  "List of known sl-rec structures, used for looking up citation-keys")

;;; EXPORT
(defun sl-rec-extract-file-from-zip (&optional ref-key)
  "Public"
  (interactive (list (org-ref-read-key)))
  (unless ref-key
    (setq ref-key (org-media-note--org-ref-key-from-cite)))
  (message "org-ref-key = %s" ref-key)
  (when-let* ((entry-1 (bibtex-completion-get-entry1 ref-key t))
	      (file (bibtex-completion-get-value "file" entry-1))
	      ;; (year (bibtex-completion-get-value "year" entry-1))
	      (source-zip
	       (cl-some (lambda (self)
			  (sl-rec-get-source-zip-pathname self file))
			$sl-recs)))
    (message "file=%s" file)
    (cl-assert (file-exists-p source-zip))
    (unless (file-exists-p file)
      (shell-command-to-string (format "unzip %S %S -d %s"
				       source-zip
				       (file-name-nondirectory file)
				       (file-name-directory file))))))


(defun sl-rec--org-media-note-get-media-file-by-key-around-advice
    (orig-fn key)
  "This function is suitable as an `around advice' for
`org-media-note-get-media-file-by-key'.  This advice can be turned on through
`advice-add' after that function is loaded.  It extracts the media file
associated with the org-ref bibtex key if it does not exist at location NOTE
`$sl-recs' and `bibtex-completion-bibliography' have to be correctly defined
when this advice gets called."
  (let ((ret (funcall orig-fn key)))
    (cond (ret ret)
	  (t (message "will try to sl-rec-extract key %s" key)
	     (sl-rec-extract-file-from-zip key)
	     (message "trying again...")
	     (prog1 (setq ret (funcall orig-fn key))
	       (when ret (message "success")))))))

(when nil
(advice-remove 'org-media-note-get-media-file-by-key
	       #'sl-rec--org-media-note-get-media-file-by-key-around-advice)
(advice-add 'org-media-note-get-media-file-by-key :around
	    #'sl-rec--org-media-note-get-media-file-by-key-around-advice))

(provide 'sl-rec)
