;;; -*- lexical-binding: t -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sun Mar 03 08:26:01 2024 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2024 Madhu.  All Rights Reserved.
;;;
;;; track calls to require that newly provide the required feature in
;;; a *track-requires* buffer
;;;

;;(mapc (lambda (sym) (trace-function-background sym)) '(load require load-file))
;;(mapc (lambda (sym) (untrace-function sym)) '(load require load-file))

(defvar $track-requires-stack nil)

(defun track-requires-around-advice (orig-fn feature &optional filename no-error)
  (push feature $track-requires-stack)
  (let ((messages-buffer-name "*track requires*")
	present-p ret pad)
    (unless (setq present-p (featurep feature))
      (message "%srequired %s"
	       (setq pad (make-string (1- (length $track-requires-stack)) 32))
	       feature))
    (unwind-protect
	(setq ret (funcall orig-fn feature filename no-error))
      (unless (eql ret (car $track-requires-stack))
	(message "require %s failed" feature))
      (pop $track-requires-stack)
      (when nil
	(unless present-p
	  (message "%sprovided %s" pad feature))))
    ret))

(define-minor-mode track-requires-mode
  "Track calls to `require' that newly provide the required feature in a
*track-requires* buffer."
  :lighter nil
  :global t
  :init-value nil
  (cond (track-requires-mode
	 (message "enabling track requires mode")
	 (advice-add 'require :around #'track-requires-around-advice))
	(t
	 (message "disabling track requires mode")
	 (advice-remove 'require #'track-requires-around-advice))))

(track-requires-mode 1)

(provide 'track-requires)

(when nil
(error ";; JUNK AT EOF")
(get-all-advice 'require)
(let (all (symbol 'require))
  (advice-mapc (lambda (advice props) (push (list :symbol symbol :advice advice :props props) all))
	       symbol)
  all)
(require 'barf))
