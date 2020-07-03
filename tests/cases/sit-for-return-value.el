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

;;; test case for #6 and #7
;; sit-for did not keep the return value
;; check also the duration is not counted

;; under test emacs code
(defun before-test ()
  t)

(defun after-test ()
  t)

(defun test-sit-for ()
  (interactive)
  (send-value "waited" (sit-for 2)))

;; driver code
(defun run-test ()
  (let ((session (start-test)))
    (wait-until-ready session)
    (m-x-run session "test-sit-for")
    (sleep-for 3)
    (m-x-run session "test-sit-for")
    (sleep-for 0.2)
    (send-key session "t")
    (call-after-test session)
    (wait-until-dead session)))

(defun finish-test (session)
  (let* ((stream (reverse event-stream))
         ;; the first result:
         (first-call
          (span-func stream "test-sit-for"))

         ;; the second result
         (second-call
          (span-func (cdr first-call) "test-sit-for"))

         (passed 0))

    (message-assert
     (< (exit-measured-time (cadr first-call)) 10)
     "sit-for full-time did not subtract")

    (message-assert
     (eq (get-value-between first-call "waited") t)
     "sit for full-time did not return t")

    (message-assert
     (< (exit-measured-time (cadr second-call)) 10)
     "sit-for part-time did not subtract")

    (message-assert
     (eq (get-value-between second-call "waited") nil)
     "sit-for part time did not return nil")

    (kill-emacs passed)))
