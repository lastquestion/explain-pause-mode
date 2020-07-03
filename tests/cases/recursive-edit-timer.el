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

;;; test case for #47
;; run a timer that spans the recursive edit, and takes time.
;; make sure the time subtracted from the call for both sides.
;; make sure we unwind with both abort and exit.

(defun before-test ()
  t)

(defun after-test ()
  t)

(defun test-recurse ()
  (interactive)
  (let ((proc (run-with-timer 0.5 0.5 'timer)))
    (recursive-edit)
    (sleep-for 0.25)
    (cancel-timer proc)))

(defun timer ()
  (sleep-for 0.01))

;; driver code
(defun run-test ()
  (let ((session (start-test)))
    (wait-until-ready session)
    (m-x-run session "test-recurse")
    (sleep-for 1.5) ;; at least one run  inside recursive
    (send-key session "hi")
    (m-x-run session "exit-recursive-edit")
    (sleep-for 1)
    (m-x-run session "test-recurse")
    (sleep-for 1.5)
    (m-x-run session "abort-recursive-edit")
    (sleep-for 0.5)
    (call-after-test session)
    (wait-until-dead session)))

(defun finish-test (session)
  (let* ((stream (reverse event-stream))
         (first-recurse (span-func stream "test-recurse"))
         (second-recurse (span-func-between
                          (cons
                           (cddr first-recurse)
                           (cdr session))
                          "test-recurse"))
         (timers-first-recurse (span-func-between first-recurse "timer"))
         (timers-second-recurse (span-func-between second-recurse "timer"))
         (first-recurse-time (exit-measured-time (cadr first-recurse)))
         (second-recurse-time (exit-measured-time (cadr second-recurse)))
         (timers-first-time (exit-measured-time (cadr timers-first-recurse)))
         (timers-second-time (exit-measured-time (cadr timers-second-recurse)))
         (passed 0))

    (message-assert
     (and (< first-recurse-time 275)
          (> first-recurse-time 250))
     "recursive-edit time does not include editing or timers with exit")

    (message-assert
     (< timers-first-time 15)
     "timers were measured correctly inside recursive edit")

    (message-assert
     (and (< first-recurse-time 275)
          (> first-recurse-time 250))
     "recursive-edit time does not include editing or timers with quit")

    (message-assert
     (< timers-second-time 15)
     "timers were measured correctly inside recursive edit")

    (kill-emacs passed)))
