;;; -*- lexical-binding: t -*-
;;;
;;;
;;;   Time-stamp: <>
;;;   Touched: Fri Mar 22 14:39:30 2024 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2024 Madhu.  All Rights Reserved.
;;;
;;; omn-mpv-playback-positions-transient.el: - wrap org-media-note
;;; commands (org-media-note-next-playback-position
;;; org-media-note-previous-playback-position
;;; org-media-note-seek-to-playback-position
;;; org-media-note-insert-playback-position) in a transient.el user
;;; interface

(require 'transient)
(require 'org-media-note-core)

(transient-define-prefix omn-mpv-playback-positions-transient ()
  "Insert and navigate between mpv playback positions (represented as mpv h:m:s
timestamps or org-timer items) in the current org buffer."
  ;; represent the emacs interactive "P" prefix arguments to the
  ;; underlying backend commands that we eventually call as a plist
  ;; with keys :arg1 and :arg2, and stash it the `scope' slot of the
  ;; transient.el prefix.  Note that COMMAND lambda bodies have to
  ;; access the scope slot via transient-current-prefix, but the
  ;; DESCRIPTION lambda body has to access it via transient--prefix.
  ["Set Prefix Args"
   ("A" ;; "toggle arg1 - if t insert org item else insert hms"
    (lambda ()
      (interactive)
      (setf (plist-get (oref transient-current-prefix scope) :arg1)
	    (not (plist-get (oref transient-current-prefix scope) :arg1))))
    :description (lambda ()
		   (format "arg1: %s: %s"
			   (plist-get (oref transient--prefix scope)
				      :arg1)
			   (if (plist-get (oref transient--prefix scope)
					  :arg1)
			       "insert timestamp as hms"
			     "insert timestamp as org-timer-item")))
    :transient t)
   ("B" ;; "toggle arg2 - if t when navigating between timestamps, do not
	;; also seek to the position in the playing media"
    (lambda ()
      (interactive)
      (setf (plist-get (oref transient-current-prefix scope) :arg2)
	    (not (plist-get (oref transient-current-prefix scope) :arg2))))
    :transient t
    :description (lambda ()
		   (format "arg2: %s: %sseek to the playback position when navigating between timestamps"
			   (plist-get (oref transient--prefix scope)
				      :arg2)
			   (if (plist-get (oref transient--prefix scope)
					  :arg2)
			       "don't " ""))))]
  ["Transient"
   ("." "Insert playback position"
    (lambda ()
      (interactive
       (org-media-note-insert-playback-position
	(plist-get (oref transient-current-prefix scope) :arg1))))
    :transient t)
   ("j" "Seek to current playback position"
    ;;org-media-note-seek-to-playback-position
    (lambda () (interactive) (org-media-note-seek-to-playback-position))
    ;;madhu 240323 transient.el EMACSBUG? if instead of a lambda we
    ;; have the symbol for the command, the :transient t below is
    ;; treated as :transient nil. why??
    :transient t)
   ("<" "Previous playback position"
    (lambda ()
      (interactive)
      (org-media-note-previous-playback-position
       (plist-get (oref transient-current-prefix scope) :arg2)))
    :transient t)
   (">" "Next playback position"
    (lambda ()
      (interactive)
      (org-media-note-next-playback-position
       (plist-get (oref transient-current-prefix scope) :arg2)))
    :transient t)]
  ["Playback Positions"
   ("i" "insert-playback-position as timestamp (org timer item format)"
    org-media-note-insert-playback-position)
   ("I" "insert-playback-position as timestamp with prefix arg (hms)"
    (lambda () (interactive) (org-media-note-insert-playback-position t)))
   ("a" "mpv seek to position at point"
    org-media-note-seek-to-playback-position)
   ("n" "next-playback-position (and seek)"
    org-media-note-next-playback-position)
   ("p" "previous-playback-position (and seek)"
    org-media-note-previous-playback-position)
   ("N" "next-playback-position with prefix arg (don't seek)"
    (lambda () (interactive (org-media-note-next-playback-position t))))
   ("P" "prev-playback-position with prefix arg (don't seek)"
    (lambda () (interactive (org-media-note-previous-playback-position t))))
    ("q" "quit" (lambda () (interactive)) :transient transient--do-exit)]
  (interactive)
  (transient-setup 'omn-mpv-playback-positions-transient nil nil
		   :scope (list :arg1 nil :arg2 t)))


(when nil
(keymap-unset org-mode-map "s-l")
(keymap-unset global-map "s-l")
(keymap-set global-map "s-l" 'omn-mpv-playback-positions-transient))

(provide 'omn-mpv-playback-positions-transient)

;; Local Variables:
;; byte-compile-warnings: (not docstrings docstrings-wide)
;; End:
