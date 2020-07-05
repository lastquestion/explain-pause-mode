;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Lin Xu

;; Author: Lin Xu <lin@lastquestion.org>
;; Version: 0.1
;; Created: May 18, 2020
;; Keywords: performance speed config
;; URL: https://github.com/lastquestion/explain-pause-mode

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; test process measurement

;; TODO this needs to move to a new folder for tests that need
;; explain-pause-mode loaded but not a full subemacs.
;; TODO needs to test the throw cases
(load-file "./explain-pause-mode.el")

(defun my-filter (process string)
  t)

(defun my-sentinel (process event)
  t)

(defun it-hides-process-filters ()
  (let ((passed 0))
    (setq proc (make-process
                :name "foo"
                :command '("cat")
                :buffer nil
                :filter nil))

    (set-process-filter proc 'my-filter)
    (set-process-sentinel proc 'my-sentinel)

    (message-assert
     (eq (process-filter proc) 'my-filter)
     "process-filter returns the original unwrapped filter")

    (message-assert
     (eq (process-sentinel proc) 'my-sentinel)
     "process-sentinel returns the original unwrapped sentinel")

    (delete-process proc)

    passed))

(defun it-works-when-nil-passed ()
  (let ((passed 0))
    (setq proc (make-process
                :name "foo"
                :command '("cat")
                :buffer nil
                :filter nil))

    (set-process-filter proc nil)
    (set-process-sentinel proc nil)

    (message-assert
     (eq (process-filter proc) 'internal-default-process-filter)
     "process-filter returns internal-default-process-filter when set with nil")

    (message-assert
     (eq (process-sentinel proc) 'internal-default-process-sentinel)
     "process-sentinel returns internal-default-process-sentinel when set with nil")

    (delete-process proc)

    passed))

;; driver code
(defun run-test ()
  (explain-pause-mode--install-hooks)
  (let ((passed
         (and (it-hides-process-filters)
              (it-works-when-nil-passed))))

    (kill-emacs passed)))
