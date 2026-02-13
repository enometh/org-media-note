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

(when-let (elt (assoc :optional (assoc :misc org-bibtex-types)))
  (unless (find :file elt)
    (setf (cdr elt) (append (cdr elt) '(:file)))))

(if-let (elt (assoc :file org-bibtex-fields))
    nil
  (pushnew '(:file . "Local File") org-bibtex-fields))


;; madhu, Wed May 28 07:47:41 2025 +0530
(defun org-bibtex-create-and-exit (&optional prompt update-heading)
  "Create a skeleton new entry at the given level without prompting
With a prefix ARG, query for fields.
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
         (noprompt (not prompt)))
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
    (let ((val (lambda (key lst) (cdr (assoc key lst))))
	  (keyword (lambda (name) (intern (concat ":" (downcase name)))))
	  (name (lambda (keyword) (substring (symbol-name keyword) 1))))
      (dolist (field (append
		      (if org-bibtex-treat-headline-as-title
			  (remove :title (funcall val :required (funcall val type org-bibtex-types)))
		        (funcall val :required (funcall val type org-bibtex-types)))
                      ;; when optional
                      (funcall val :optional (funcall val type org-bibtex-types))))
        (when (consp field) ; or'd pair of fields e.g., (:editor :author)
          (let ((present (nth 0 (remove
			         nil
			         (mapcar
				  (lambda (f)
				    (when (org-bibtex-get (funcall name f)) f))
				  field)))))
            (setf field (or present (funcall keyword
					     (completing-read
					      "Field: " (mapcar name field)))))))
        (let ((name (funcall name field)))
          (unless (org-bibtex-get name)
            (let ((prop (if noprompt "" (org-bibtex-ask field))))
              (when prop (org-bibtex-put name prop)))))))
    (when (and type (assoc type org-bibtex-types)
               (not (org-bibtex-get org-bibtex-key-property)))
      (org-bibtex-autokey)))
  (dolist (tag org-bibtex-tags) (org-toggle-tag tag 'on)))