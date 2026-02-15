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
    (setf (cdr elt) (append (cdr elt) '(:file)))))

(pushnew '(:file . "Local File") org-bibtex-fields :test #'equal)

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

