;;; sl-rec-fzp.el -*- lexical-binding: t -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Thu Jun 1 20:23:45 2023 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2023 Madhu.  All Rights Reserved.
;;;
;;; An example that illustrates how sl-rec.el is to be used.

(require 'sl-rec)

;; subclass sl-rec to define defaults. the names sl-rec and sl-rec-fzp
;; are arbitrary

;; Say FZP-2020.zip contain mp3 files of the form
;; 01-FZP-001-01.01.20.mp3, where 01 is the lecture number, 001 is a
;; some `verse' and 01.01.20 is the date in dd.mm.yy form.
;;
;; defne a key-template for bibtex keys and an entry-template for
;; bibtex entries for individual files in the zip archive.

(defclass sl-rec-fzp (sl-rec)
  ()
  (:default-initargs
   :key-template "fzp:sl_{{no}}_{{verse}}_{{date}}"
   :entry-template  "@Misc{{{key}},
file = {{{filename}}},
year = {{{year}}},
title = {SL Lec {{no}}},
author = {NN Author},
publisher = {NN Publisher}
}
"))

;; define the protocol methods

(defun sl-rec-fzp-get-year-from-date (date)
  (when (string-match "[0-9][0-9]\\.[0-9][0-9]\\.\\([0-9][0-9]\\)" date)
    (concat "20" (match-string 1 date))))

(cl-defmethod sl-rec-describe-file-name ((_self sl-rec-fzp) file-name (_type (eql 'bib-key)))
  (when (string-match "\\([0-9][0-9]\\)[ -]FZP[- ]\\(.+\\)[- ]\\([0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9]\\)[ ]*\\.mp3" file-name)
    (list 'no  (match-string 1 file-name)
	  'verse (match-string 2 file-name)
	  'date (match-string 3 file-name))))

(cl-defmethod sl-rec-describe-file-name ((self sl-rec-fzp) file-name (_type (eql 'bib-entry)))
  (let* ((desc-key-bindings
	  (sl-rec-describe-file-name self file-name 'bib-key))
	 (key (sl-rec-produce-citation-key self desc-key-bindings))
	 (date (cl-getf desc-key-bindings 'date))
	 (year (sl-rec-fzp-get-year-from-date date))
	 (filename (with-slots (target-dir) self
		     (file-name-concat target-dir file-name))))
    (append desc-key-bindings
	    (list 'key key 'year year 'filename filename))))

(cl-defmethod sl-rec-get-source-zip-pathname ((self sl-rec-fzp) file-name)
  "Hardcoded."
  (when-let* ((desc-key-bindings
	       (sl-rec-describe-file-name self file-name 'bib-key))
	      (date (cl-getf desc-key-bindings 'date))
	      (year (sl-rec-fzp-get-year-from-date date)))
    (with-slots (source-dir) self
      (file-name-concat source-dir
			(format "FZP-%s.zip" year)))))

;; maybe use ert whatever that is?
(provide 'sl-rec-fzp)

;;; JUNK AT EOF
(when nil
(defvar $fzp
  (make-instance 'sl-rec-fzp
		 :target-dir "/dev/shm/fzp/unpacked/"
		 :source-dir "/dev/shm/fzp/zip/"
)
(equalp "2020"  (sl-rec-fzp-get-year-from-date "01.06.20"))
(equalp (sl-rec-describe-file-name $fzp "25-FZP-025-12.12.20.mp3" 'bib-key)
	(no "25" verse "025" date "12.12.20"))
(equal
 (sl-rec-describe-file-name $fzp "25-FZP-025-12.12.20.mp3" 'bib-entry)
 '(no "25" verse "025" date "12.12.20" key "fzp:sl_25_025_12.12.20" year "2020" filename "/dev/shm/fzp/unpacked/25-FZP-025-12.12.20.mp3"))

(equalp (sl-rec-produce-citation-key
	 $fzp
	 (sl-rec-describe-file-name $fzp "25-FZP-025-13.12.20.mp3" 'bib-key))
	"fzp:sl_25_025_13.12.20")
(equalp
 (sl-rec-produce-citation-key
  $fzp
  (sl-rec-describe-file-name $fzp "03-FZP-003-04.03.20.mp3"
			     'bib-key))
 "fzp:sl_03_003_04.03.20")
(equalp (sl-rec-produce-entry-for-file $fzp  "25-FZP-025-12.12.20.mp3")
"@Misc{fzp:sl_25_025_12.12.20,
file = {/dev/shm/fzp/unpacked/25-FZP-025-12.12.20.mp3},
year = {2020},
title = {SL Lec 25},
author = {NN Author},
publisher = {NN Publisher}
}
")
(equalp (sl-rec-get-source-zip-pathname $fzp  "01-FZP-001-20.02.20.mp3")
	"/dev/shm/fzp/zip/FZP-2020.zip")
;;
;; generate example objects under /dev/shm
;;
(shell-command-to-string
"
rm -rfv /dev/shm/fzp
mkdir -pv /dev/shm/fzp/{unpacked,zip,bib}/
touch /dev/shm/fzp/unpacked/01-FZP-001-01.01.20.mp3
touch /dev/shm/fzp/unpacked/02-FZP-002-03.02.20.mp3
touch /dev/shm/fzp/unpacked/03-FZP-003-04.03.20.mp3
touch /dev/shm/fzp/unpacked/04-FZP-004-05.05.20.mp3
cd /dev/shm/fzp/unpacked; zip -mu ../zip/FZP-2020.zip *FZP*mp3
")
;;
;; generate a bibtex file with `keys' of the form
;; "fzp:sl_01_001_01.01.20" and a `file' field pointing to the
;; unpacked mp3 file with `sl-rec-dump-bib-file-for-zip'.
;;
(sl-rec-dump-bib-file-for-zip $fzp "/dev/shm/fzp/bib/fzp-sl-2020.bib"
			      "FZP-2020.zip")
;;
;; once the bib files are known to bibtex-completion.el, we can
;; extract the files from the zip with
;; `sl-rec-extract-file-from-zip'.
;;
(let (($sl-recs (list $fzp))
      (bibtex-completion-bibliography (list "/dev/shm/fzp/bib/fzp-sl-2020.bib")))
  (bibtex-completion-init)
  (sl-rec-extract-file-from-zip "fzp:sl_03_003_04.03.20"))
))
