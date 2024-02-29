;;; -*- lexical-binding: t -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Thu Feb 29 19:32:39 2024 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2024 Madhu.  All Rights Reserved.
;;;
;;; contrib/omn-compile-media: org media note compile media:
;;; integrate org-media-note-ref and compile-media
;;; https://github.com/sachac/compile-media to compile media files ala
;;; https://github.com/sachac/subed-record, but use
;;; org-media-note-org-ref timestamps to specify intervals.
;;;
;;;
;;; wip. arguments, functions and variables need to be renamed
;;; sensibly.
;;;

(require 'org-media-note-org-ref)
(require 'compile-media)

;; (setq emacs-lisp-docstring-fill-column 72) ;;madhu 240307 who is
;; resetting this to 78 ??!

(defun org-media-note-get-media-link-refcites-in-region (beg end)
  "Internal.  Return an alist of \( \"audiocite\" | \"videocite | \"audio\"
| \"video\" | \"cite\" \).  of org-media-note org-ref refcite links in the
given region."
  ;; see `org-ref-get-bibtex-keys' (from org-ref-utils.el) for the
  ;; inspiration.
  (interactive "r")
  ;; If called interactively print the return value in the *Messages*
  ;; buffer for debugging
  (let* ((ret ())
	 (elements
	  (save-restriction
	    (narrow-to-region beg end)
	    (org-element-parse-buffer))))
    (org-element-map elements 'link
      (lambda (link)
        (when-let* ((plist (nth 1 link))
		    (cons (member (plist-get plist ':type)
				  (append
				   '("audio" "video" "cite")
				   org-media-note-link-types))))
	  (push (cons (car cons) (org-element-property :path link)) ret)))
      ;; set with-affiliated to get keys in captions
      nil nil nil t)
    (setq ret (nreverse ret))
    (if (called-interactively-p 'interactive) (message "%S" ret))
    ret))


(defun org-media-note-media-cite-split-path (link &optional plain)
  "Internal.  If LINK ia a recognizable (via bibtex-completion)
org-media-note ref-cite link with a timestamp interval, return an sexp
which represents the file and interval which is recognizable by the
`compile-media' package.

if PLAIN is non-NIL, do not go through org-ref.
"
  ;; see `org-media-note-media-cite-link-follow' and `org-media-note-media-link-follw' for the inspiration
  (let* ((splitted (split-string link "#"))
	 (key-1 (nth 0 splitted))
	 (key (if (not plain) (org-media-note-ref-parse-path key-1)))
	 (file-path-or-url (if plain key-1
			     (or (org-media-note-cite--file-path key)
				 ;; (org-media-note-cite--url key)
				 )))
	 (timestamps (if (nth 1 splitted)
			 (split-string (nth 1 splitted)
				       "-"))))
    (if (not (= (length timestamps) 2))
	(progn (message "org-media-note-cite-split-path: skipping refcite link without an interval: %s" key)
	       nil)
      (if-let* ((time-a (org-timer-hms-to-secs (nth 0 timestamps)))
		(time-b (if (= (length timestamps) 2)
			    (org-timer-hms-to-secs (nth 1 timestamps)))))
	  (cond
	   ((not file-path-or-url)
	    (error "Cannot find media file for this Key"))
	   (t `(:source ,file-path-or-url :start-ms ,(* time-a 1000)
			:stop-ms ,(* time-b 1000))))))))

(when nil
(equal (org-media-note-media-cite-split-path "/dev/shm/foo#0:28:58-0:29:00" t)
       '(:source "/dev/shm/foo" :start-ms 1738000 :stop-ms 1740000)))

(defun org-media-note-get-media-links-in-region (beg end)
  (interactive "r")
  (let (ret head)
    (cl-loop for (type . cite) in
	     (org-media-note-get-media-link-refcites-in-region beg end)
	     for plain-p = (member type '("audio" "video"))
	     for sym = (cond ((member type '("audiocite" "audio"))
			      'audio)
			     ((member type '("videocite" "video"))
			      'video)
			     (t (error "sanity a")))
	     for elt = (org-media-note-media-cite-split-path cite plain-p)
	     when elt do
	     (cond ((not ret) (push (list sym elt) ret))
		   ((eql (car (setq head (car ret))) 'audio)
		    (cond ((eql sym 'audio)
			   (push elt (cdr head)))
			  ((eql sym 'video)
			   (setcdr head (nreverse (cdr head)))
			   (push (setq head (list 'video elt)) ret))
			  (t (error "sanity b"))))
		   ((eql (car head) 'video)
		    (cond ((eql sym 'video)
			   (push elt (cdr head)))
			  ((eql sym 'audio)
			   (setcdr head (nreverse (cdr head)))
			   (push (setq head (list 'audio elt)) ret))
			  (t (error "sanity c"))))
		   (t (error "sanity d"))))
    (when (and head (cddr head))
      (setcdr head (nreverse (cdr head))))
    (setq ret (nreverse ret))
    (when (called-interactively-p 'interactive)
      (message "%S" ret))
    ret))

(when nil (equal (let ((str "
* compile media
[[audio:/dev/shm/foo.au#0:00:00-0:10:10]]
[[video:/dev/shm/bar.wmv#0:00:00-10:0:0]]
[[video:/dev/shm/bar.wmv#1:20:30-1:20:32]]
"))
   (with-temp-buffer
     (insert str)
     (org-mode)
     (org-media-note-get-media-links-in-region (point-min) (point-max)))
   )
'((audio (:source "/dev/shm/foo.au" :start-ms 0 :stop-ms 610000)) (video (:source "/dev/shm/bar.wmv" :start-ms 0 :stop-ms 0) (:source "/dev/shm/bar.wmv" :start-ms 4830000 :stop-ms 4832000)))))

(defun omn-compile-media (beg end outfile)
  "Call compile-media for the org-media-note links in the region that
have an timestamp interval."
  (interactive (progn
                 (let ((beg (mark))
                       (end (point)))
                   (unless (and beg end)
                     (user-error "The mark is not set now, so there is no region"))
                   (list beg end (read-file-name "Output file (webm) ")
			 ))))
  (let ((cmd (org-media-note-get-media-links-in-region beg end)))
    (compile-media-sync cmd outfile)))

(provide 'omn-compile-media)
