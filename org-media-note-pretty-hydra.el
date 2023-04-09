(require 'pretty-hydra)
(require 'org-media-note)

;;;; Commands
;;;;; Hydra

;; A quick menu for org-media-note
(pretty-hydra-define org-media-note-hydra
  (:color red
   :title (org-media-note--hydra-title)
   :hint nil)
  ("File"
   (("o" org-media-note-mpv-smart-play
     (if (org-media-note-ref-cite-p)
         (format "Open %s"
                 (org-media-note--current-org-ref-key))
       "Open file")
     :width 20)
    ("j"
     (mpv-cycle-property "sub")
     "toggle subtitles")
    ("T"
     (mpv-cycle-property "ontop")
     "toggle ontop")
    ("c"
     (org-media-note-change-speed-by 0.1)
     "increase speed")
    ("x"
     (org-media-note-change-speed-by -0.1)
     "decrease speed")
    ("z" org-media-note-mpv-toggle-speed "reset speed"))
   "Playback"
   (("<SPC>" mpv-pause "Play/Pause")
    ("l"
     (mpv-run-command "ab-loop")
     (let ((time-a (mpv-get-property "ab-loop-a"))
           (time-b (mpv-get-property "ab-loop-b")))
       (if (org-media-note--ab-loop-p)
           (format "Clear A-B loop (%s - %s)"
                   (org-media-note--seconds-to-timestamp time-a)
                   (org-media-note--seconds-to-timestamp time-b))
         (if (numberp time-a)
             (format "Set B of A-B loop (%s - )"
                     (org-media-note--seconds-to-timestamp time-a))
           "Set A of A-B loop")))
     :width 45)
    ("g" org-media-note-goto-timestamp "Jump to the timestamp")
    ("<left>" mpv-seek-backward "Back 5s")
    ("<right>" mpv-seek-forward "Forward 5s")
    ("C-<left>"
     (mpv-run-command "sub-seek" -1)
     "Previous subtitle")
    ("C-<right>"
     (mpv-run-command "sub-seek" 1)
     "Next subtitle"))
   "Volume"
   (("+"
     (org-media-note-change-volume-by 5)
     "Up")
    ("-"
     (org-media-note-change-volume-by -5)
     "Down")
    ("0" org-media-note-mpv-toggle-volume "toggle")
    ("m"
     (mpv-cycle-property "mute")
     "(un)mute"))
   "Note"
   (("i" org-media-note-insert-link "Insert timestamp")
    ("a" org-media-note-adjust-timestamp-offset "Adjust timestamp")
    ("S" org-media-note-insert-screenshot "Insert Screenshot")
    ("s" org-media-note-insert-sub-text "Insert subtitle"))
   "Import"
   (("I p" org-media-note-insert-note-from-pbf
     "Import from pbf")
    ("I n" org-media-note-insert-note-from-noted
     "Import from Noted")
    ("I t" org-media-note-convert-from-org-timer
     "Import from org-timer")
    ("I s" org-media-note-insert-note-from-srt
     "Import from srt"))
   "Toggle"
   (("t m" org-media-note-mode "Auto insert media item"
     :toggle t)
    ("t c" org-media-note-toggle-refcite "Use ref key instead of absolute path"
     :toggle org-media-note-use-refcite-first)
    ("t p" org-media-note-toggle-pause-after-insertion
     "Pause media after insert link" :toggle org-media-note-pause-after-insert-link)
    ("t s" org-media-note-toggle-save-screenshot
     "Auto save screenshot" :toggle org-media-note-save-screenshot-p)
    ("t S" org-media-note-toggle-screenshot-with-sub
     "Screenshot with subtitles" :toggle org-media-note-screenshot-with-sub)
    ("t t" org-media-note-toggle-timestamp-pattern
     (format "Timestamp format: %s"
             (cond
              ((eq org-media-note-timestamp-pattern 'hms)
               "hh:mm:ss")
              ((eq org-media-note-timestamp-pattern 'hmsf)
               "hh:mm:ss.fff")))))))

(provide 'org-media-note-pretty-hydra)