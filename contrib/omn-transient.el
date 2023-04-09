;; -*- lexical-binding: t -*-
(require 'transient)
(require 'org-media-note)
;;; ;madhu 230407 because pretty-hydra won't fit into 80x25 try to
;;; use transient

(transient-define-prefix omn-transient ()
  "org media note"

  [:description
   org-media-note--hydra-title ""]

  [["OMN File"
    ("o" org-media-note-play-smart
     :description
     (lambda ()
       (cl-multiple-value-bind (link _ _)
	   (org-media-note--link-context)
	 (cl-multiple-value-bind (ref-mode key _ _)
	     (org-media-note--ref-context)
	   (cl-multiple-value-bind (_ media-files-in-attach-dir)
	       (org-media-note--attach-context)
	     (cond
	      (link "Open link")
	      (ref-mode (format "Open %s" key))
	      ((> (length media-files-in-attach-dir) 0) "Open attach")
	      (t "Open media")))))))
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
    ("z" (lambda () (interactive)  (org-media-note-mpv-toggle-speed) "reset speed")
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
    ("u" org-media-note-update-link-timestamp :description "Update Link timestamp")
    ("a" org-media-note-adjust-timestamp-offset :description "Adjust timestamp")
    ("S" (lambda ()
	   (interactive)
	   (if (org-media-note--ab-loop-p)
               (org-media-note-capture-ab-loop-and-insert)
	     (org-media-note-insert-screenshot)))
     :description
     (lambda ()
       (if (org-media-note--ab-loop-p)
	   "Insert ab-loop clip"
	 "Insert Screenshot")))
    ("s" org-media-note-insert-sub-text :description "Insert subtitle")
    ("m" org-media-note-merge-item :description "Merge items")
    ]

   ["OMN Import from"
    ("I p" org-media-note-insert-note-from-pbf
     :description "pbf")
    ("I n" org-media-note-insert-note-from-noted
     :description "Noted")
    ("I t" org-media-note-convert-from-org-timer
     :description "org-timer")
    ("I s" org-media-note-insert-note-from-subtitle
     :description "subtitle")
    ("I c" org-media-note-insert-note-from-chapter-list
     :description "chapters")]]

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
	 (if (and (numberp time-a) (numberp time-b)); (org-media-note--ab-loop-p)
             (format "Clear AB-loop (%s-%s)"
                     (org-media-note--seconds-to-timestamp time-a)
                     (org-media-note--seconds-to-timestamp time-b))
	   (if (numberp time-a)
	       (format "Set B of AB-loop (%s-)"
		       (org-media-note--seconds-to-timestamp time-a))
             "Set A of AB-loop")))))
    ("g" "Jump to timestamp" org-media-note-goto-timestamp
     :transient t)
    ("<left>" "Backward 5s" mpv-seek-backward
     :transient t)
    ("<right>" "Forward 5s" mpv-seek-forward
     :transient t)
    ("C-<left>" "Previous subtitle"
     (lambda () (interactive) (mpv-run-command "sub-seek" -1))
     :transient t)
    ("C-<right>" "Next subtitle"
     (lambda () (interactive) (mpv-run-command "sub-seek" 1))
     :transient t)
    ("<prior>"  "Previous Chapter"
     (lambda () (interactive) (mpv-run-command "add" "chapter" -1))
     :transient t)
    ("<next>" "Next Chapter"
     (lambda () (interactive) (mpv-run-command "add" "chapter" 1))
     :transient t)
    ]

   ["OMN Infix"
    ("t m" toggle-org-media-note-auto-insert-item
     :transient t
     :description (lambda () (format "Auto insert media item: %s" org-media-note-auto-insert-item)))
    ("t s" org-media-note-toggle-save-screenshot
     :transient t
     :description (lambda () (format "Auto insert screenshot: %s"  org-media-note-save-screenshot-p)))
    ("t l" org-media-note-config-ab-loop-capture-method
     :transient t
     :description
     (lambda ()
       (format "AB-loop Clip: %s"
	       (if org-media-note-capture-ab-loop-ask-each-time
		   "always ask" org-media-note-default-capture-ab-loop-function-name))))
    ("t S" org-media-note-toggle-screenshot-with-sub
     :transient t
     :description (lambda () (format "Screenshot with subtitles: %s" org-media-note-screenshot-with-sub)))
    ("t c" org-media-note-toggle-refcite
     :transient t
     :description (lambda () (format "Cite ref key instead of abs path: %s" org-media-note-use-refcite-first)))
    ("t p" org-media-note-toggle-pause-after-insertion
     :transient t
     :description (lambda () (format "Pause after insert link: %s" org-media-note-pause-after-insert-link)))
    ("t t" org-media-note-toggle-timestamp-pattern
     :transient t
     :description (lambda () (format "Timestamp format: %s"
				     (cond
				      ((eq org-media-note-timestamp-pattern 'hms)
				       "hh:mm:ss")
				      ((eq org-media-note-timestamp-pattern 'hmsf)
				       "hh:mm:ss.fff")))))
    ("t M" org-media-note-set-separator
     :transient t
     :description (lambda ()  (format "Separator when merge: %s"
				      org-media-note-separator-when-merge)))
    ("t <right>" org-media-note-set-seek-method
     :transient t
     :description (lambda ()  (format "Seek step: %s"
				      (org-media-note--seek-step t))))

    ]
   ]
  ["\n" "\n"]
  )

(provide 'omn-transient)

;; Local Variables:
;; byte-compile-warnings: (not docstrings docstrings-wide)
;; End:
