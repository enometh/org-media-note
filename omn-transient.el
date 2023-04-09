(require 'transient)
(require 'org-media-note)
;;; ;madhu 230407 because pretty-hydra won't fit into 80x25 try to
;;; use transient

(transient-define-prefix omn-transient ()
  "org media note"

  [:description
   org-media-note--hydra-title ""]

  [["OMN File"
    ("o" org-media-note-mpv-smart-play
     :description (lambda ()
		    (if (org-media-note-ref-cite-p)
			(format "Open %s"
				(org-media-note--current-org-ref-key))
		      "Open file")))
    ("Q" (lambda () (interactive) (mpv-quit t))
     :description "watch later")
    ("j" (lambda () (interactive) (mpv-cycle-property "sub"))
     :description "toggle subtitles"
     :transient t)
    ("T" (lambda() (interactive) (mpv-cycle-property "ontop"))
     :description  "toggle ontop"
     :transient t)
    ("c" (lambda () (interactive) (org-media-note-change-speed-by 0.1))
     :description "increase speed"
     :transient t)
    ("x" (lambda () (interactive) (org-media-note-change-speed-by -0.1))
     :description "decrease speed"
     :transient t)
    ("z" (lambda () (interactive)  (org-media-note-mpv-toggle-speed "reset speed"))
     :description "reset speed"
     :transient t)]

   ["OMN Volume"
    ("+" (lambda () (interactive) (org-media-note-change-volume-by 5))
     :transient t
     :description "Up")
    ("-" (lambda () (interactive) (org-media-note-change-volume-by -5))
     :transient t
     :description "Down")
    ("0" org-media-note-mpv-toggle-volume
     :transient t
     :description "toggle")
    ("m" (lambda () (interactive) (mpv-cycle-property "mute"))
     :transient t
     :description "(un)mute")]

   ["OMN Note"
    ("i" org-media-note-insert-link :description "Insert timestamp")
    ("a" org-media-note-adjust-timestamp-offset :description "Adjust timestamp")
    ("S" org-media-note-insert-screenshot :description "Insert Screenshot")
    ("s" org-media-note-insert-sub-text :description "Insert subtitle")
    ]

   ["OMN Import from"
    ("I p" org-media-note-insert-note-from-pbf
     :description "pbf")
    ("I n" org-media-note-insert-note-from-noted
     :description "Noted")
    ("I t" org-media-note-convert-from-org-timer
     :description "org-timer")
    ("I s" org-media-note-insert-note-from-srt
     :description "srt")]]

  [["OMN Playback"
    :pad-keys t
    ("<SPC>" "Play/Pause" mpv-pause
     :transient t)
    ("l"
     (lambda () (interactive) (mpv-run-command "ab-loop"))
     :transient t
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
    ("g" "Jump to the timestamp" org-media-note-goto-timestamp
     :transient t)
    ("<left>" "Back 5s" mpv-seek-backward
     :transient t)
    ("<right>" "Forward 5s" mpv-seek-forward
     :transient t)
    ("C-<left>" "Previous subtitle"
     (lambda () (interactive) (mpv-run-command "sub-seek" -1))
     :transient t)
    ("C-<right>" "Next subtitle"
     (lambda () (interactive) (mpv-run-command "sub-seek" 1))
     :transient t)
    ]

   ["OMN Infix"
    ("t m" org-media-note-mode
     :transient t
     :description (lambda () (format "Auto insert media item: %s" org-media-note-mode)))
    ("t c" org-media-note-toggle-refcite
     :transient t
     :description (lambda () (format "Use ref key instead of abs path: %s" org-media-note-use-refcite-first)))
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
    ]
   ]
  ["\n" "\n"]
  )

(provide 'omn-transient)