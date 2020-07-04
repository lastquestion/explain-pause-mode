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

;;; Test that when explain-pause-mode is installed, we wrap all
;;; existing timers and processes. Repro case for #50

(setq test-process nil)

(defun filter-test (proc string)
  (sit-for 0.25)
  (send-value "filter-test" "t"))

(defun sentinel-test (proc event)
  (sit-for 0.25)
  (send-value "sentinel-test" "t"))

(defun timer-sit-for (kind time)
  (sit-for time)
  (send-value kind "t"))

(defun before-test ()
  (setq test-process
        (make-process
         :name "test"
         :command '("cat")
         :filter 'filter-test))
  (set-process-sentinel test-process 'sentinel-test)
  (setq timer-1 (run-with-timer 1 2 'timer-sit-for 'timer 0.5))
  (setq timer-2 (run-with-idle-timer 0.5 0.5 'timer-sit-for 'idle-timer 1)))

(defun echo ()
  (interactive)
  (process-send-string test-process "HI\n"))

(defun end ()
  (interactive)
  (delete-process test-process))

(defun after-test ()
  (cancel-timer timer-1)
  (cancel-timer timer-2))

(defun run-test ()
  (setq session (start-test
                 nil
                 nil
                 '("-f" "setup-test" "-f" "before-test")))

  (sleep-for 0.5)

  (m-x-run session "explain-pause-mode")

  ;; wait until timer-1 has run, then timer-2
  (sleep-for 2.5)

  ;; hit the filter
  (m-x-run session "echo")

  (sleep-for 0.5)

  (m-x-run session "end")

  (sleep-for 1.0)

  (call-after-test session)
  (wait-until-dead session))

(defun finish-test (session)
  ;; if we get here, the mode must have installed
  (let* ((stream (reverse event-stream))
         (enabled (find-enabled stream))
         (enabled-span (cons enabled nil))
         (timer-after-enabled (get-value-between enabled-span "timer"))
         (idle-timer-after-enabled (get-value-between enabled-span "idle-timer"))
         (sentinel-fired-after (get-value-between enabled-span "sentinel-test"))
         (filter-fired-after (get-value-between enabled-span "filter-test"))
         (passed 0))

    (message-assert-not
     (nth 1 (find-ptr event-stream (find-by "value" "message-index")))
     "Unable to install message was not printed")

    (message-assert
     timer-after-enabled
     "Timer was wrapped after install and read-key worked")

    (message-assert
     idle-timer-after-enabled
     "Idle timer was wrapped after install and read-key worked")

    (message-assert
     sentinel-fired-after
     "Snetinel was wrapped after install and read-key worked")

    (message-assert
     filter-fired-after
     "Filter was wrapped after install and read-key worked")

    (kill-emacs passed)))
