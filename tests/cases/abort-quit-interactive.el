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

;;; test case for #58
;;; Aborting out of a command before it is run during the interactive
;;; phase should work.
(defun before-test ()
  t)

(defun after-test ()
  t)

(defun test-func (buff)
  (interactive "bBuffer:")
  t)

;; driver code
(defun run-test ()
  (let ((session (start-test)))
    (wait-until-ready session)
    (m-x-run session "test-func")
    (sleep-for 0.1)
    (send-special-key session 'quit)
    (sleep-for 1)
    (call-after-test session)
    (wait-until-dead session)))

(defun finish-test (session)
  (let* ((stream (reverse event-stream))
         (passed 0))

    (message-assert
     (found-span-p (span-func stream "call-interactively-interactive"))
     "Call interactively interactive exists")

    (message-assert
     (not (found-span-p (span-func stream "test")))
     "Test frames do not exist as it was never called")

    (kill-emacs passed)))
