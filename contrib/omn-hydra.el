;; -*- lexical-binding: t -*-

(require 'hydra)
(require 'org-media-note)
;;; ;madhu 230407 because pretty-hydra won't fit into 80x25 try to
;;; use hydra

;;;madhu 240325 - this file is not guaranteed to be in sync with
;; org-media-note-pretty-hydra.el or provide all the functionality. It
;; contains wrong syntax.  I don't use hydra and I haven't been able
;; to keep up with tha changes to that file: the translation to hydra
;; syntax may be incorrect and incomplete.

(defhydra omn-hydra-config (:color teal)
  " OMN Toggle"
  ("t m" (omn-hydra/toggle-org-media-note-auto-insert-item))
  ("t s" (omn-hydra/org-media-note-config-save-screenshot))
  ("t S" (omn-hydra/org-media-note-config-screenshot-with-sub))
  ("t l" org-media-note-config-ab-loop-capture-method
   (format "AB-loop Clip: %s"
	   (if org-media-note-capture-ab-loop-ask-each-time
	       "always ask" org-media-note-default-capture-ab-loop-function-name))
   )
  ("t c" (omn-hydra/org-media-note-config-refcite))
  ("t p" (omn-hydra/org-media-note-config-pause-after-insertion))
  ("t t" (omn-hydra/org-media-note-config-timestamp-pattern))
  ("t M" (omn-hydra/org-media-note-set-separator))
  ("t <right>" (omn-hydra/org-media-note-set-seek-method)))

(defhydra omn-hydra-file (:color teal)
  "OMN File"
  ("o" org-media-note-play-smart
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
            (t "Open media"))))))
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
     (if (and (numberp time-a) (numberp time-b)); (org-media-note--ab-loop-p)
         (format "Clear AB-loop (%s-%s)"
                 (org-media-note--seconds-to-timestamp time-a)
                 (org-media-note--seconds-to-timestamp time-b))
       (if (numberp time-a)
           (format "Set B of AB-loop (%s-)"
                   (org-media-note--seconds-to-timestamp time-a))
         "Set A of AB-loop"))))
  ("g" org-media-note-goto-timestamp "Jump to timestamp")
    ("<left>" (org-media-note-seek 'backward) (format "Backward %s" (org-media-note--seek-step t)))
    ("<right>" (org-media-note-seek 'forward) (format "Forward %s" (org-media-note--seek-step t)))
  ("C-<left>"
   (mpv-run-command "sub-seek" -1)
   "Previous subtitle")
  ("C-<right>"
   (mpv-run-command "sub-seek" 1)
   "Next subtitle")
  ("<prior>"
   (mpv-run-command "add" "chapter" -1)
   "Previous Chapter")
  ("<next>"
   (mpv-run-command "add" "chapter" 1)
   "Next Chapter")
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
  ("u" org-media-note-update-link-timestamp "Update Link timestamp")
  ("a" org-media-note-adjust-timestamp-offset "Adjust timestamp")
  ("S" ("S"
	(if (org-media-note--ab-loop-p)
            (org-media-note-capture-ab-loop-and-insert)
	  (org-media-note-insert-screenshot))
	(if (org-media-note--ab-loop-p)
            "Insert ab-loop clip"
	  "Insert Screenshot"))
   "Insert Screenshot")
  ("s" org-media-note-insert-sub-text "Insert subtitle")
  ("H-m" org-media-note-merge-item "Merge items")
  ("b" omn-hydra/body "back")
  ("q" nil "quit"))

(defhydra omn-hydra-import (:color teal)
  "OMN Import"
  ("I p" org-media-note-insert-note-from-pbf
   "from pbf")
  ("I n" org-media-note-insert-note-from-noted
   "from Noted")
  ("I t" org-media-note-convert-from-org-timer
   "from org-timer")
  ("I s" org-media-note-insert-note-from-subtitle
   "from subtitle")
  ("I c" org-media-note-insert-note-from-chapter-list
   "from chapters")
  ("b" omn-hydra/body "back")
  ("q" nil "quit"))

(defmacro defyhdradio-omn-hydra ()
  `(defhydradio omn-hydra ()
     (toggle-org-media-note-auto-insert-item
      "Auto insert media item"
      ,(vector org-media-note-auto-insert-item
	       (not org-media-note-auto-insert-item)))
     (org-media-note-toggle-save-screenshot
      "Auto insert screenshot"
      ,(vector org-media-note-save-screenshot-p
	       (not org-media-note-save-screenshot-p)))
     (org-media-note-toggle-screenshot-with-sub
      "Screenshot with sub"
      ,(vector org-media-note-screenshot-with-sub
	       (not org-media-note-screenshot-with-sub)))
     (org-media-note-toggle-refcite
      "Use Cite key instead of absolute path"
      ,(vector org-media-note-use-refcite-first
	       (not org-media-note-use-refcite-first)))
     (org-media-note-toggle-pause-after-insertion
      "Pause after insert link"
      ,(vector org-media-note-pause-after-insert-link
	       (not  org-media-note-pause-after-insert-link)))
     (org-media-note-toggle-timestamp-pattern
      (format "Timestamp format: %s"
              (cond
	       ((eq org-media-note-timestamp-pattern 'hms) "hh:mm:ss")
	       ((eq org-media-note-timestamp-pattern 'hmsf) "hh:mm:ss.fff")))
      ,(vector org-media-note-timestamp-pattern
	       (cond ((eq org-media-note-timestamp-pattern 'hms)
		      'hmsf)
		     ((eq org-media-note-timestamp-pattern 'hmsf)
		      'hms))))
     (org-media-note-set-separator
      (format "Separator when merge: %s"
             org-media-note-separator-when-merge))
     (org-media-note-set-seek-method
      (format "Seek step: %s" (org-media-note--seek-step t)))
     ))

(defyhdradio-omn-hydra)

(defhydra omn-hydra (:exit nil :foreign-keys warn)
  ("f" omn-hydra-file/body "File" :exit t)
  ("p" omn-hydra-playback/body "Playback" :exit t)
  ("v" omn-hydra-volume/body "Volume" :exit t)
  ("n" omn-hydra-note/body "Note" :exit t)
  ("i" omn-hydra-import/body "Import" :exit t)
  ("c" omn-hydra-config/body "Import" :exit t)
  ("q" nil "quit"))

(provide 'omn-hydra)

;; Local Variables:
;; byte-compile-warnings: (not docstrings docstrings-wide)
;; End:
