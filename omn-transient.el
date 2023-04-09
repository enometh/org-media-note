(require 'transient)
(require 'org-media-note)
;;; ;madhu 230407 because pretty-hydra won't fit into 80x25 try to
;;; use transient

(transient-define-prefix omn-transient ()
  "org media note"
  [
   :description org-media-note--hydra-title
    ("t m" org-media-note-mode
     :transient t
     :description (lambda () (format "Auto insert media item: %s" org-media-note-mode)))
    ("t c" org-media-note-toggle-refcite
     :transient t
     :description (lambda () (format "Use ref key instead of absolute path: %s" org-media-note-use-refcite-first)))
    ("t p" org-media-note-toggle-pause-after-insertion
     :transient t
     :description (lambda () (format "Pause media after insert link: %s" org-media-note-pause-after-insert-link)))
    ("t s" org-media-note-toggle-save-screenshot
     :transient t
     :description (lambda () (format "Auto save screenshot: %s"  org-media-note-save-screenshot-p)))
    ("t S" org-media-note-toggle-screenshot-with-sub
     :transient t
     :description (lambda () (format "Screenshot with subtitles: %s" org-media-note-screenshot-with-sub)))
    ("t t" org-media-note-toggle-timestamp-pattern
     :transient t
     :description (lambda () (format "Timestamp format: %s"
				     (cond
				      ((eq org-media-note-timestamp-pattern 'hms)
				       "hh:mm:ss")
				      ((eq org-media-note-timestamp-pattern 'hmsf)
				       "hh:mm:ss.fff")))))
    "Suffix"
   ("f" "File" omn-transient-file)
   ("p" "Playback" omn-transient-playback)
   ("v" "Volume" omn-transient-volume)
   ("n" "Note" omn-transient-note)
   ("i" "Import" omn-transient-import)])

; (omn-transient)

(transient-define-prefix omn-transient-file ()
  "OMN File"
  [("o" org-media-note-mpv-smart-play
    :description (lambda ()
		   (if (org-media-note-ref-cite-p)
		       (format "Open %s"
			       (org-media-note--current-org-ref-key))
		     "Open file")))
   ("q" (lambda () (interactive) (mpv-quit t))
    :description "watch later")
   ("j" (lambda () (interactive) (mpv-cycle-property "sub"))
    :description "toggle subtitles"
   ("T" (lambda() (interactive) (mpv-cycle-property "ontop"))
    :description  "toggle ontop"))
   ("c" (lambda () (interactive) (org-media-note-change-speed-by 0.1))
    :description "increase speed")
   ("x" (lambda () (interactive) (org-media-note-change-speed-by -0.1))
    :description "decrease speed")
   ("z" (lambda () (interactive)  (org-media-note-mpv-toggle-speed "reset speed"))
    :description "reset speed")])

; (omn-transient-file)

(transient-define-prefix omn-transient-playback ()
  "OMN Playback"
  [("<SPC>" "Play/Pause" mpv-pause)
   ("l"
    (lambda () (interactive) (mpv-run-command "ab-loop"))
    :description
    (lambda ()
      (let ((time-a (mpv-get-property "ab-loop-a"))
            (time-b (mpv-get-property "ab-loop-b")))
	(if (org-media-note--ab-loop-p)
            (format "Clear A-B loop (%s - %s)"
                    (org-media-note--seconds-to-timestamp time-a)
                    (org-media-note--seconds-to-timestamp time-b))
	  (if (numberp time-a)
              (format "Set B of A-B loop (%s - )"
                      (org-media-note--seconds-to-timestamp time-a))
            "Set A of A-B loop")))))
   ("g" "Jump to the timestamp" org-media-note-goto-timestamp)
   ("<left>" "Back 5s" mpv-seek-backward)
   ("<right>" "Forward 5s" mpv-seek-forward)
   ("C-<left>" "Previous subtitle"
    (lambda () (interactive) (mpv-run-command "sub-seek" -1)))
   ("C-<right>" "Next subtitle"
    (lambda () (interactive) (mpv-run-command "sub-seek" 1)))])

;; (omn-transient-playback)

(transient-define-prefix omn-transient-volume ()
  "OMN Volume"
  [("+" (lambda () (interactive) (org-media-note-change-volume-by 5))
    :description "Up")
   ("-" (lambda () (interactive) (org-media-note-change-volume-by -5))
    :description "Down")
   ("0" org-media-note-mpv-toggle-volume
    :description "toggle")
   ("m" (lambda () (interactive) (mpv-cycle-property "mute"))
    :description "(un)mute")])

;; (omn-transient-volume)

(transient-define-prefix omn-transient-note ()
  "OMN Note"
  [("i" org-media-note-insert-link :description "Insert timestamp")
   ("a" org-media-note-adjust-timestamp-offset :description "Adjust timestamp")
   ("S" org-media-note-insert-screenshot :description "Insert Screenshot")
   ("s" org-media-note-insert-sub-text :description "Insert subtitle")])

;; (omn-transient-note)

(transient-define-prefix omn-transient-import ()
  "OMN Import"
  [("I p" org-media-note-insert-note-from-pbf
    :description "Import from pbf")
   ("I n" org-media-note-insert-note-from-noted
    :description "Import from Noted")
   ("I t" org-media-note-convert-from-org-timer
    :description "Import from org-timer")
   ("I s" org-media-note-insert-note-from-srt
    :description "Import from srt")])

;; (omn-transient-import)

(provide 'omn-transient)