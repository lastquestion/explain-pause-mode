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

;;; Regression test for part of #26.
;;; test that when a timer interrupts a minibuffer with y-or-n-p,
;;; time is subtracted from each other.

(defun before-test ()
  t)

(defun start-interrupt-timer ()
  (run-with-timer 1 nil 'timer))

(defun timer ()
  (sleep-for 0.1)
  (unless (y-or-n-p "question")
    (start-interrupt-timer)))

(defun reader ()
  (interactive)
  (read-from-minibuffer "Some input:")
  (sleep-for 0.5))

(defun after-test ()
  t)

;; driver code
(defun run-test ()
  (let ((session (start-test)))
    (wait-until-ready session)
    (eval-expr session "(start-interrupt-timer)")
    (sleep-for 0.1)
    (m-x-run session "reader")
    (sleep-for 1.5)
    ;; the yes or no should be open
    (send-key session "n")
    (sleep-for 2)
    (send-key session "y")
    ;; now type for the buffer
    (sleep-for 0.5)
    (send-key session "f!" 'enter)
    (sleep-for 0.5)
    (call-after-test session)
    (wait-until-dead session)))

(defun finish-test (session)
  (let* ((stream (reverse event-stream))
         (reader (span-func stream "reader"))
         (timer-1 (span-func-between reader "timer"))
         (timer-2 (span-func-between (cons (cddr timer-1) (cdr reader)) "timer"))
         (passed 0))

    (message-assert
     (< (exit-measured-time (cadr reader)) 510)
     "Minibuffer time subtracted timers and read")

    (message-assert
     (< (exit-measured-time (cadr timer-1)) 120)
     "timer time subtracted out y-or-n-p time")

    (message-assert
     (< (exit-measured-time (cadr timer-2)) 120)
     "timer time subtracted out y-or-n-p time")

    (kill-emacs passed)))
