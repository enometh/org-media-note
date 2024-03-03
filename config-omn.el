;; -*- lexical-binding: nil -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sun Mar 03 08:24:31 2024 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2024 Madhu.  All Rights Reserved.
;;;
;; config-omn.el
;;
;; load and configure org-media-note extras

(unless (locate-library "omn-transient")
  (let ((load-path load-path))
    (add-to-list 'load-path
		 (file-name-concat $pkg-root "org-media-note"))
    (add-to-list 'load-path
		 (file-name-concat $pkg-root "org-media-note" "contrib"))
    (require 'omn-transient)
    (require 'orc-transient)
    (require 'or-transient)
    (require 'org-pdf-open)))

(use-package omn-transient
  :bind ("s-v" . 'omn-transient))

(use-package or-transient
  :bind ("s-]" . or-insert-link-transient))

(use-package orb-transient
  :bind ("s-b" . orb-transient))

(use-package orc-transient
  :bind ("s-c" . org-ref-citation-transient))

