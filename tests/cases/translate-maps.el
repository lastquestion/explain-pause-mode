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

;;; More tests for #84. Verify that keymap translators of all kinds
;;; have frames both before and after

(defun before-test ()
  (define-key input-decode-map
    [?z]
    (lambda (&optional prompt)
      (sit-for 0.5)
      (send-value "z key" t)
      [?f])))

(defun more-keys ()
  (define-key key-translation-map
    [?d]
    (lambda (&optional prompt)
      (sit-for 0.1)
      (send-value "d key" t)
      [?d])))

(defun delete-keys ()
  (interactive)
  (define-key input-decode-map
    [?z]
    nil))

(defun after-test ()
  t)

(defun run-test ()
  (setq session (start-test
                 nil
                 nil
                 '("-f" "setup-test" "-f" "before-test")))

  (sleep-for 0.5)

  (m-x-run session "explain-pause-mode")
  (send-key session "d")
  (sleep-for 0.5)

  (send-key session "z")

  (sleep-for 1)

  (eval-expr session "(more-keys)")

  (sleep-for 0.5)
  (send-key session "d")

  (sleep-for 0.5)

  (m-x-run session "delete-keys")

  (sleep-for 0.5)

  (send-key session "z")

  (sleep-for 0.5)

  (call-after-test session)
  (wait-until-dead session))

(defun finish-test (session)
  (let* ((stream (reverse event-stream))
         (z-fired (get-value stream "z key"))
         (d-fired (get-value stream "d key"))
         (passed 0))

    (message-assert
     z-fired
     "pre-install map fired")

    (message-assert
     d-fired
     "post-install map fired")

    (message-assert
     (equal (nth 5 session) "exit-test-quit-emacs")
     "translate maps worked")

    (kill-emacs passed)))
