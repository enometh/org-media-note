;; -*- lexical-binding: nil
;;;
;;;   Time-stamp: <>
;;;   Touched: Sun Mar 03 08:25:47 2024 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2024 Madhu.  All Rights Reserved.
;;;

;; load-prereqs.el - org ox bibtex-completion org-ref mpv
;; org-media-note
;;
;; this file should set up load-paths (in an fresh emacs) for our
;; pre-reqs and/or load up our pre-reqs.  it is intended that it be
;; customised for the current specific site.  it is expected be called
;; when the file specified in `org-export-async-init-file' is loaded,
;; but before the file that configures our pre-reqs is loaded.


;;(locate-library "bibtex")
;;(require 'bibtex)

(defvar $pkg-root "~/elisp/pkg/")

(with-temp-load-paths
    ((file-name-concat $pkg-root "helm-bibtex")
     (file-name-concat $pkg-root "parsebib")
     (file-name-concat $pkg-root "s.el")
     (file-name-concat $pkg-root "f.el")
     (file-name-concat $pkg-root "magit/dash")
     (file-name-concat $pkg-root "biblio.el"))
  (require 'bibtex-completion))

;; add org-ref directory to load-path because because some code in
;; org-ref calls (locate-library "org-ref")
(add-to-list 'load-path (file-name-concat $pkg-root "org-ref"))

(with-temp-load-paths
    ((file-name-concat $pkg-root "citeproc-el")
     (file-name-concat $pkg-root "ht.el")
     (file-name-concat $pkg-root "hydra")
     (file-name-concat $pkg-root "ox-pandoc")
     (file-name-concat $pkg-root "org-ref")
     (file-name-concat "~/.emacs.d/elpa/queue-0.2/")
     (file-name-concat $pkg-root "avy"))
  (require 'org-ref-core))

(with-temp-load-paths ((file-name-concat $pkg-root "mpv.el"))
  (require 'mpv))

(with-temp-load-paths
    ((file-name-concat $pkg-root "org-media-note")
     (file-name-concat $pkg-root "org-media-note" "contrib"))
  (require 'org-media-note)
  (require 'org-media-note-org-ref)
  ;; contrib
  (require 'org-ref-publish))


;;;
;;; Other pre-reqs (not explicitly used for org-publish)
;;;
(when nil
(with-temp-load-paths ((file-name-concat $pkg-root "org-contrib/lisp/"))
  (locate-library "ox-bibtex"))

(with-temp-load-paths
    ((file-name-concat $pkg-root "emacs-htmlize"))
  (require 'htmlize))
)