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

;;; Verify that when a native cb is asked to be wrapped twice, it
;;; only generates one frame.

(defun before-test () t)

(defun double-trouble (&rest args) t)

(defun do-it ()
  (interactive)
  (add-hook 'post-command-hook 'double-trouble)
  (add-hook 'post-gc-hook 'double-trouble))

(defun after-test () t)

;; driver code
(defun run-test ()
  (let ((session (start-test)))
    (wait-until-ready session)
    (m-x-run session "do-it")
    (sleep-for 1.5)
    (call-after-test session)
    (wait-until-dead session)))

(defun finish-test (session)
  (let* ((stream (reverse event-stream))
         (double-call (span-func stream "double-trouble"))
         (remaining-call (span-func (cdr double-call) "double-trouble"))
         (passed 0))

    (message-assert
     double-call
     "double trouble called")

    (message-assert-not
     (car remaining-call)
     "double trouble not called twice")

    (kill-emacs passed)))
