;; -*- lexical-binding:t -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Fri Mar 01 12:08:10 2024 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2024 Madhu.  All Rights Reserved.
;;;
;;;
;;
;; async-init.el - synopsis
;;
;; (setq org-export-async-init-file "/path/to/async-init.el")
;; (org-publish `("example" :base-directory ,default-directory :exclude ".*" :include ("README.org") :publishing-function (org-ref-publish-to-html org-ref-publish-to-pdf) :publishing-directory "/dev/shm/build" :section-numbers nil) nil 'async)
;; (org-html-export-to-html t)

;; (setq emacs-lisp-docstring-fill-column 72)

(require 'cl-lib)
(cl-defun maybe-load-colocated-files (&rest relative-paths)
  "Load items in the list RELATIVE-PATHS with LOAD-FILE if the relative
path is found.  Items are expected to be strings of relative pathnames.
If instead an item is T an error is thrown if a subsequent file in the
list is not found.  If instead an item is NIL, no error is thrown if a
subsequent file in the list is not found."
  (let ((if-does-not-exist nil))
    (cl-dolist (relative-path relative-paths)
      (cond ((eql relative-path t)
	     (setq if-does-not-exist 'error))
	    ((eql relative-path nil)
	     (setq if-does-not-exist nil))
	    (t (let ((cfg (file-name-concat (if load-true-file-name (file-name-directory load-true-file-name) "") relative-path)))
		 (cond ((file-exists-p cfg)
			(message "loading %s" cfg)
			(load-file cfg))
		       (if-does-not-exist
			(error "failed to load %s" cfg)))))))))

(cl-defmacro with-temp-load-paths ((&rest paths) &body body)
  (declare (indent 1))
  (unless (consp (car paths)) (setq paths (list paths)))
  `(let ((load-path load-path))
     ,@(mapcar (lambda (path)
		 `(add-to-list 'load-path ,path))
	       paths)
     ,@body))



;;; ----------------------------------------------------------------------
;;;
;;;
;;;
(maybe-load-colocated-files t "track-requires.el"
			    nil "load-prereqs.el"
			    t   "config.el"
			    nil "local-overrides.el")


