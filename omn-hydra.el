(require 'hydra)
(require 'org-media-note)
;;; ;madhu 230407 because pretty-hydra won't fit into 80x25 try to
;;; use ox-hydra

(defhydra omn-hydra-file (:color teal)
  "OMN File"
  ("o" org-media-note-mpv-smart-play
   (if (org-media-note-ref-cite-p)
       (format "Open %s"
	       (org-media-note--current-org-ref-key))
     "Open file"))
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
  ("z" org-media-note-mpv-toggle-speed "reset speed")
  ("b" omn-hydra/body "back")
  ("q" nil "quit"))

(defhydra omn-hydra-playback (:color teal)
  "OMN Playback"
  ("<SPC>" mpv-pause "Play/Pause")
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
         "Set A of A-B loop"))))
  ("g" org-media-note-goto-timestamp "Jump to the timestamp")
  ("<left>" mpv-seek-backward "Back 5s")
  ("<right>" mpv-seek-forward "Forward 5s")
  ("C-<left>"
   (mpv-run-command "sub-seek" -1)
   "Previous subtitle")
  ("C-<right>"
   (mpv-run-command "sub-seek" 1)
   "Next subtitle")
  ("b" omn-hydra/body "back")
  ("q" nil "quit"))

(defhydra omn-hydra-volume (:color teal)
  "OMN Volume"
  ("+"
   (org-media-note-change-volume-by 5)
   "Up")
  ("-"
   (org-media-note-change-volume-by -5)
   "Down")
  ("0" org-media-note-mpv-toggle-volume "toggle")
  ("m"
   (mpv-cycle-property "mute")
   "(un)mute")
  ("b" omn-hydra/body "back")
  ("q" nil "quit"))

(defhydra omn-hydra-note (:color teal)
  "OMN Note"
  ("i" org-media-note-insert-link "Insert timestamp")
  ("a" org-media-note-adjust-timestamp-offset "Adjust timestamp")
  ("S" org-media-note-insert-screenshot "Insert Screenshot")
  ("s" org-media-note-insert-sub-text "Insert subtitle")
  ("b" omn-hydra/body "back")
  ("q" nil "quit"))

(defhydra omn-hydra-import (:color teal)
  "OMN Import"
  ("I p" org-media-note-insert-note-from-pbf
   "Import from pbf")
  ("I n" org-media-note-insert-note-from-noted
   "Import from Noted")
  ("I t" org-media-note-convert-from-org-timer
   "Import from org-timer")
  ("I s" org-media-note-insert-note-from-srt
   "Import from srt")
  ("b" omn-hydra/body "back")
  ("q" nil "quit"))

(defmacro defyhdradio-omn-hydra ()
  `(defhydradio omn-hydra ()
     (org-media-note-mode "Auto insert media item" [t nil])
     (org-media-note-toggle-refcite
      "Use ref key instead of absolute path"
      ,(vector org-media-note-use-refcite-first
	       (not org-media-note-use-refcite-first)))
     (org-media-note-toggle-pause-after-insertion
      "Pause media after insert link" 
      ,(vector org-media-note-pause-after-insert-link
	       (not  org-media-note-pause-after-insert-link)))
     (org-media-note-toggle-save-screenshot
      "Auto save screenshot" 
      ,(vector org-media-note-save-screenshot-p
	       (not org-media-note-save-screenshot-p)))
     (org-media-note-toggle-screenshot-with-sub
      "Screenshot with subtitles"
      ,(vector org-media-note-screenshot-with-sub
	       (not org-media-note-screenshot-with-sub)))
     (org-media-note-toggle-timestamp-pattern
      (format "Timestamp format: %s"
              (cond
	       ((eq org-media-note-timestamp-pattern 'hms)
		"hh:mm:ss")
	       ((eq org-media-note-timestamp-pattern 'hmsf)
		)))
      ,(vector org-media-note-timestamp-pattern
	       (cond ((eq org-media-note-timestamp-pattern 'hms)
		      'hmsf)
		     ((eq org-media-note-timestamp-pattern 'hmsf)
		      'hms))))))

(defyhdradio-omn-hydra)


(defhydra omn-hydra (:exit nil :foreign-keys warn)
  "
[t m] Auto insert media item: % -10`omn-hydra/org-media-note-mode
[t c] Use ref key instead of absolute path: %`omn-hydra/org-media-note-toggle-refcite
[t p] Pause media after link: % -10`omn-hydra/org-media-note-toggle-pause-after-insertion ^^^ \
[t s] Auto save screenshot: %`omn-hydra/org-media-note-toggle-save-screenshot
[t S] Screenshot with subtitles: % -8`omn-hydra/org-media-note-toggle-screenshot-with-sub \
[t t] Toggle timestamp pattern: %`omn-hydra/org-media-note-toggle-timestamp-pattern

"

  ("t m" (omn-hydra/org-media-note-mode) nil)
  ("t c" (omn-hydra/org-media-note-toggle-refcite) nil)
  ("t p" (omn-hydra/org-media-note-toggle-pause-after-insertion) nil)
  ("t s" (omn-hydra/org-media-note-toggle-save-screenshot) nil)
  ("t S" (omn-hydra/org-media-note-toggle-screenshot-with-sub) nil)
  ("t t" (omn-hydra/org-media-note-toggle-timestamp-pattern) nil)

  ("f" omn-hydra-file/body "File" :exit t)
  ("p" omn-hydra-playback/body "Playback" :exit t)
  ("v" omn-hydra-volume/body "Volume" :exit t)
  ("n" omn-hydra-note/body "Note" :exit t)
  ("i" omn-hydra-import/body "Import" :exit t)
  ("q" nil "quit"))

(provide 'omn-hydra)