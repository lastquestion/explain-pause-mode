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

;;; Test that when you start up interactively, and you're in a
;;; recursive edit, we wait until you exit before trying to install

(defun before-test ()
  t)

(defun start-mode ()
  (interactive)
  (run-with-idle-timer 0.5 nil 'explain-pause-mode)
  (recursive-edit))

(defun after-test ()
  t)

(defun run-test ()
  (setq session (start-test
                 nil
                 nil
                 '("-f" "setup-test")))

  (sleep-for 0.5)

  (m-x-run session "start-mode")

  ;; wait long enough for the idle timer to try
  (sleep-for 0.7)

  ;; more then 5, which is the give up number
  (send-key session "abcdef")

  ;; ok, get out of recursive
  (m-x-run session "abort-recursive-edit")

  ;; install
  (send-key session 'enter)

  (call-after-test session)
  (wait-until-dead session))

(defun finish-test (session)
  (let ((passed 0))
    (message-assert
     (equal (nth 5 session) "exit-test-quit-emacs")
     "mode installed correctly")
    (kill-emacs passed)))
