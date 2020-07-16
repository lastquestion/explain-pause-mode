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

;;; test case for #31, test sit for inside a timer. Some people do
;;; that. Evil.

(defun before-test ()
  (setq minibuffer-message-timeout 1))

(defun timer-func ()
  (minibuffer-message "a long message"))

(defun run-timer-non-interactively ()
  (setq sit-timer
        (run-with-timer 0.25 nil 'timer-func)))

(defun run-timer-interactively ()
  (interactive)
  (setq sit-timer
        (run-with-timer 0.25 nil 'timer-func)))

(defun after-test ()
  t)

;; driver code
(defun run-test ()
  (let ((session (start-test)))
    (wait-until-ready session)
    (eval-expr session "(run-timer-non-interactively)")
    (sleep-for 1.5)
    (m-x-run session "run-timer-interactively")
    (sleep-for 1.5)
    (m-x-run session "run-timer-interactively")
    (sleep-for 0.75)
    (send-key session "p")
    (sleep-for 0.75)
    (call-after-test session)
    (wait-until-dead session)))

(defun finish-test (session)
  (let* ((stream (reverse event-stream))
         (non-interactive-timer
          (span-func stream "timer-func"))
         (interactive-timer-1
          (span-func (cddr non-interactive-timer) "timer-func"))
         (interactive-timer-2
          (span-func (cddr interactive-timer-1) "timer-func"))
         (passed 0))

    (message-assert
     (< (exit-measured-time (cadr non-interactive-timer)) 2)
     "Timer non-interactive subtracted sit-for")

    (message-assert
     (< (exit-measured-time (cadr interactive-timer-1)) 2)
     "Timer interactive subtracted sit-for")

    (message-assert
     (< (exit-measured-time (cadr interactive-timer-2)) 2)
     "Timer interactive interrupted with keys subtracted sit-for")

    (kill-emacs passed)))
