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

;;; test case for #14
;; read for key sequence and it's family should not be counted
;; TODO do all the other cases in #14
;; under test emacs code
(defun before-test ()
  t)

(defun after-test ()
  t)

(defun test-read-key-sequence ()
  (interactive)
  (let ((result (read-key-sequence nil)))
    (send-value "key" result)))

;; driver code
(defun run-test ()
  (let ((session (start-test)))
    (wait-until-ready session)
    (m-x-run session "test-read-key-sequence")
    (sleep-for 1)
    (send-key session "p")
    (call-after-test session)
    (wait-until-dead session)))

(defun finish-test (session)
  (let* ((stream (reverse event-stream))
         (call
          (span-func stream "test-read-key-sequence"))
         (passed 0))

    (message-assert
     (< (exit-measured-time (cadr call)) 10)
     "read-key-sequence time subtracted")

    (message-assert
     (equal (get-value-between call "key") "p")
     "read-key-sequence returned actual key")

    (kill-emacs passed)))
