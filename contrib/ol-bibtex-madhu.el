;;  -*- lexical-binding: t -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Fri Feb 13 08:37:33 2026 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2026 Madhu.  All Rights Reserved.
;;;
;;; ol-bibtex-madhu.el, some modifications to lisp/org/ol-bibtex.el


;;; madhu, Wed May 28 07:47:15 2025 +0530
;;; lisp/org/ol-bibtex.el: (org-bibtex-{types,fields}): add :file to misc
(dolist (entry org-bibtex-types)
  (when-let (elt (assoc :optional entry))
    (unless (find :file (cdr elt))
      (setf (cdr elt) (append (cdr elt) '(:file))))))

(cl-pushnew '(:file . "Local File") org-bibtex-fields :test #'equal)

;; madhu, Wed May 28 07:47:41 2025 +0530
;; work in an existing heading
(cl-defun org-bibtex-create-and-exit (&optional (prompt nil) (update-heading t))
  "Create a skeleton new entry at the given level without prompting
With a prefix arg, query for fields.
If UPDATE-HEADING is non-nil, add data to the headline of the entry at
point."
  (interactive "P")
  (let* ((type (completing-read
		"Type: " (mapcar (lambda (type)
				   (substring (symbol-name (car type)) 1))
				 org-bibtex-types)
		nil nil (when update-heading
		          (org-bibtex-get org-bibtex-type-property-name))))
	 (type (if (keywordp type) type (intern (concat ":" type))))
	 (org-bibtex-treat-headline-as-title (if update-heading nil t))
         (noprompt (not prompt))
	 required-fields optional-fields)
    (unless (assoc type org-bibtex-types)
      (error "Type:%s is not known" type))
    (if update-heading
	(org-back-to-heading)
      (org-insert-heading)
      (let ((title (if noprompt "" (org-bibtex-ask :title))))
	(insert title)
	(org-bibtex-put "TITLE" title)))
    (org-bibtex-put org-bibtex-type-property-name
		    (substring (symbol-name type) 1))
    ;; (org-bibtex-fleshout type arg)
    (cl-flet ((val  (key lst) (cdr (assoc key lst)))
	      (keyword (name) (intern (concat ":" (downcase name))))
	      (name  (keyword) (substring (symbol-name keyword) 1)))
      (setq required-fields
	    (if org-bibtex-treat-headline-as-title
			  (remove :title (val :required (val type org-bibtex-types)))
		        (val :required (val type org-bibtex-types))))
      (setq optional-fields (val :optional (val type org-bibtex-types)))
      (dolist (field (append required-fields optional-fields))
        (when (consp field) ; or'd pair of fields e.g., (:editor :author)
          (let ((present (nth 0 (remove
			         nil
			         (mapcar
				  (lambda (f)
				    (when (org-bibtex-get (name f)) f))
				  field)))))
            (setf field (or present (keyword
				     (completing-read
				      "Field: " (mapcar #'name field)))))))
        (let ((name (name field)))
          (unless (org-bibtex-get name)
            (let ((prop (if noprompt "" (org-bibtex-ask field))))
              (when prop (org-bibtex-put name prop)))))))
    (when (and type (assoc type org-bibtex-types)
	       (not (org-bibtex-get org-bibtex-key-property)))
      ;; (org-bibtex-autokey)
      (org-bibtex-put org-bibtex-key-property "FILLME")))
  (dolist (tag org-bibtex-tags) (org-toggle-tag tag 'on)))

;; patch org-bibtex-headline to skip empty fields, modified from emacs
;; master commit 4da38c63216186
(defvar org-bibtex-export-empty-fields nil
  "Used by org-bibtex-headline[-skip-empty-fields] to indicate
if empty fields are to be skipped when creating the bib-file with
org-bibtex.")

(defun org-bibtex-headline--skip-empty-fields ()
  "Return a bibtex entry of the given headline as a string. skips empty fields if `org-bibtex-export-empty-fields' is NIL."
  (letrec ((val (lambda (key lst) (cdr (assoc key lst))))
	   (to (lambda (string) (intern (concat ":" string))))
	   (from (lambda (key) (substring (symbol-name key) 1)))
	   (flatten (lambda (&rest lsts)
		      (apply #'append (mapcar
				       (lambda (e)
					 (if (listp e) (apply flatten e) (list e)))
				       lsts))))
	   (id (org-bibtex-get org-bibtex-key-property))
	   (type (org-bibtex-get org-bibtex-type-property-name))
	   (tags (when org-bibtex-tags-are-keywords
		   (delq nil
			 (mapcar
			  (lambda (tag)
			    (unless (member tag
					    (append org-bibtex-tags
						    org-bibtex-no-export-tags))
			      tag))
			  (if org-bibtex-inherit-tags (org-get-tags)
			    (org-get-tags nil t)))))))
    (when type
      (let ((entry (format
		    "@%s{%s,\n%s\n}\n" type id
		    (mapconcat
		     (lambda (pair)
                       (format "  %s={%s}" (car pair) (cdr pair)))
		     (remove-if (lambda (pair)
                                  (or (null pair)
                                      (and (not org-bibtex-export-empty-fields)
                                           (equal (cdr pair) ""))))
			     (if (and org-bibtex-export-arbitrary-fields
				      org-bibtex-prefix)
				 (mapcar
				  (lambda (kv)
				    (let ((key (car kv)) (val0 (cdr kv)))
				      (when (and
					     (string-match org-bibtex-prefix key)
					     (not (string=
						   (downcase (concat org-bibtex-prefix
								     org-bibtex-type-property-name))
						   (downcase key))))
					(cons (downcase (replace-regexp-in-string
							 org-bibtex-prefix "" key))
					      val0))))
				  (org-entry-properties nil 'standard))
			       (mapcar
				(lambda (field)
				  (let ((value (or (org-bibtex-get (funcall from field))
						   (and (eq :title field)
							(nth 4 (org-heading-components))))))
				    (when value (cons (funcall from field) value))))
				(funcall flatten
					 (funcall val :required (funcall val (funcall to type) org-bibtex-types))
					 (funcall val :optional (funcall val (funcall to type) org-bibtex-types))))))
		     ",\n"))))
	(with-temp-buffer
	  (insert entry)
	  (when tags
	    (bibtex-beginning-of-entry)
	    (if (re-search-forward "keywords.*=.*{\\(.*\\)}" nil t)
		(progn (goto-char (match-end 1)) (insert ", "))
	      (search-forward ",\n" nil t)
	      (insert "  keywords={},\n")
	      (search-backward "}," nil t))
	    (insert (mapconcat #'identity tags ", ")))
	  (buffer-string))))))

(when nil
  (advice-remove 'org-bibtex-headline
		 #'org-bibtex-headline@skip-empty-fields-around-advice))

(define-advice org-bibtex-headline (:around (_orig) skip-empty-fields-around-advice)
  (org-bibtex-headline--skip-empty-fields))


;;; ----------------------------------------------------------------------
;;;
;;;
;;;
(defvar $org-bibtex-autokey-vars
  '(bibtex-autokey-year-length
    bibtex-autokey-name-year-separator
    bibtex-autokey-year-title-separator
    bibtex-autokey-titleword-separator
    bibtex-autokey-titlewords
    bibtex-autokey-titlewords-stretch
    bibtex-autokey-titleword-length))

(defvar $org-bibtex-autokey-var-defaults
  '(2 "" ":_" "_" 5 2 5))

(when nil
  (mapcar (lambda (x) (eval (car (get x 'standard-value))))
	  $org-bibtex-autokey-vars))

(defvar $org-bibtex-autokey-var-alt
  '(4 "-" "-" "-" 2 1 5))

(when nil
  (let (,$org-bibtex-autokey-vars)
    (setq bibtex-autokey-year-length 4
	  bibtex-autokey-name-year-separator "-"
	  bibtex-autokey-year-title-separator "-"
	  bibtex-autokey-titleword-separator "-"
	  bibtex-autokey-titlewords 2
	  bibtex-autokey-titlewords-stretch 1
	  bibtex-autokey-titleword-length 5)
    (mapcar 'symbol-value $org-bibtex-autokey-vars))
(progv $org-bibtex-autokey-vars $org-bibtex-autokey-var-alt
   (mapcar 'symbol-value $org-bibtex-autokey-vars))
(progv $org-bibtex-autokey-vars $org-bibtex-autokey-var-defaults
  `(setq ,@(loop for k in $v append (list k (symbol-value k))))))

(cl-defmacro with-org-bibtex-autokey-defaults (&body body)
  `(progv $org-bibtex-autokey-vars  $org-bibtex-autokey-var-alt
     ,@body))

(defun org-bibtex-autokey-dry-run (&optional replace)
  "Generate an autokey for the current headline."
  (interactive "P")
  (with-org-bibtex-autokey-defaults
   (let ((new (let* ((entry (org-bibtex-headline))
		     (key
		      (with-temp-buffer
			(insert entry)
			(bibtex-generate-autokey))))
		(when (and
		       (equal org-bibtex-key-property "ID")
		       (featurep 'org-id)
		       (hash-table-p org-id-locations)
		       (gethash key org-id-locations))
		  (warn "Another entry has the same ID"))
		key)))
     (message "%S" new))))

(provide 'ol-bibtex-madhu)