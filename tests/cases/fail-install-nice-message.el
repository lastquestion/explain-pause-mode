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

;;; Test that when you start up interactively, and we can't get a good
;;; post-command startup, we eventually give up and print a nice
;;; message.

(defun before-test ()
  t)

(defun after-test ()
  ;; check messages that there is a good message
  (with-current-buffer (messages-buffer)
    (goto-char 0)
    (send-value "message-index"
                (re-search-forward "Unable to install ‘explain-pause-mode’" nil t))))

(defun run-test ()
  (setq session (start-test
                 nil
                 nil
                 '("-f" "setup-test")))

  (sleep-for 0.5)

  (m-x-run session "recursive-edit")

  (m-x-run session "explain-pause-mode")

  ;; more then 5 which is the give up number
  (send-key session "abcdef")

  (call-after-test session)
  (wait-until-dead session))

(defun finish-test (session)
  ;; if we get here, the mode must have installed
  (let ((passed 0))
    (message-assert-not
     (nth 1 (find-ptr event-stream (find-by "value" "message-index")))
     "Unable to install message was not printed")
    (kill-emacs passed)))
