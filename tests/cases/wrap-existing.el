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
;;; Also test that we wrap existing post-command-hooks both
;;; global and buffer-local. Repro case for #54

(setq test-process nil)

(defun filter-test (proc string)
  (sit-for 0.25)
  (send-value "filter-test" "t"))

(defun sentinel-test (proc event)
  (sit-for 0.25)
  (send-value "sentinel-test" "t"))

(defun timer-sit-for (kind time)
  (sit-for time)
  (message "timer %s" kind)
  (send-value kind "t"))

(defun my-post-command ()
  t)

(defun byte-compiled-post-command ()
  (when explain-pause-mode
    (send-value "byte-compiled-post-command" t)))

(defun before-test ()
  (byte-compile 'byte-compiled-post-command)

  (setq test-process
        (make-process
         :name "test"
         :command '("cat")
         :filter 'filter-test))
  (set-process-sentinel test-process 'sentinel-test)
  (setq timer-1 (run-with-timer 1 1.5 'timer-sit-for 'timer 0.25))
  (setq timer-2 (run-with-idle-timer 0.5 0.5 'timer-sit-for 'idle-timer 1))
  (add-hook 'post-command-hook 'my-post-command)
  (let ((avalue 4))
    (add-hook 'post-command-hook (lambda ()
                                   (send-value "lambda-hook" t)
                                   avalue)))
  (add-hook 'post-command-hook 'byte-compiled-post-command))

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

  (sleep-for 1)

  (m-x-run session "explain-pause-mode")

  ;; wait until timer-1 has run, then timer-2
  (sleep-for 4)

  ;; hit the filter
  (m-x-run session "echo")

  (sleep-for 0.5)

  (m-x-run session "end")

  (sleep-for 0.5)

  (send-key session "a") ;; force idle timer to run

  (sleep-for 2)

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
         (byte-compile-hook (span-func-between enabled-span "byte-compiled-post-command"))
         (func-hook (span-func-between enabled-span "my-post-command"))
         (lambda-hook (span-func-between enabled-span "<closure> (arg-list: nil)"))
         (passed 0))

    (message-assert-not
     (get-value event-stream "message-index")
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

    (message-assert
     (equal (nth 3 (caar byte-compile-hook)) "post-command-hook")
     "Byte compiled post command hook wrapped after install")

    (message-assert
     (equal (nth 3 (caar func-hook)) "post-command-hook")
     "Func hook post command hook wrapped after install")

    (message-assert
     (equal (nth 3 (caar lambda-hook)) "post-command-hook")
     "Lambda post command hook wrapped after install")

    (kill-emacs passed)))
