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

(let (($pkg-root (or $omn-alt-pkg-root $pkg-root)))
  (let ((load-path load-path))
    (add-to-list 'load-path
		 (file-name-concat $pkg-root "org-media-note" "contrib"))
    (require 'org-pdf-open)
    (require 'omn-mpv-playback-positions-transient)
    (require 'ol-bibtex-madhu)))

(use-package omn-mpv-playback-positions-transient
  :bind ("s-l" . 'omn-mpv-playback-positions-transient))


(when nil
(unless (locate-library "orc-transient")
  (let ((load-path load-path))
    (add-to-list 'load-path
		 (file-name-concat $pkg-root "org-media-note"))
    (add-to-list 'load-path
		 (file-name-concat $pkg-root "org-media-note" "contrib"))
      (require 'orc-transient)
      (require 'or-transient)
      (require 'orb-transient)))

;; obsolete ;;[Thu Oct 31 20:28:33 2024 +0530]
(use-package omn-transient
  :init
  :bind ("s-v" . omn-transient))

;; obsolete ;madhu 260716
(use-package or-transient
  :init
  (setq org-ref-insert-cite-function 'org-ref-insert-cite-link)
  (setq org-ref-insert-ref-function 'org-ref-insert-ref-link)
  (setq org-ref-insert-label-function 'org-ref-insert-label-link)
  (setq org-ref-cite-onclick-function (lambda(_) (org-ref-citation-transient)))
  :bind ("s-]" . or-insert-link-transient))

(use-package orb-transient
  :bind ("s-b" . orb-transient))

(use-package orc-transient
  :bind ("s-c" . org-ref-citation-transient)))