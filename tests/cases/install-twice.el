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

;;; Test for #78. Installing twice should work.

(defun before-test ()
  t)

(defun after-test ()
  t)

(defun doit ()
  (interactive)
  (explain-pause-mode)
  (explain-pause-mode 1))

(defun run-test ()
  (setq session (start-test
                 nil
                 nil
                 '("-f" "setup-test")))

  (sleep-for 0.5)

  (m-x-run session "doit")

  (sleep-for 0.1)

  (send-key session "a")

  (sleep-for 0.5)

  (eval-expr session "(explain-pause-mode 1)")

  (call-after-test session)
  (wait-until-dead session))

(defun finish-test (session)
  (let* ((passed 0)
         (stream (reverse event-stream))
         (first-call (find-enabled stream))
         (second-call (find-enabled (cdr first-call))))
    (message-assert
     (equal (nth 5 session) "exit-test-quit-emacs")
     "mode installed correctly")
    (message-assert
     (and first-call
          (not second-call))
     "enabled only once")
    (kill-emacs passed)))
