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

;;; Regression test case for part of #26.
;;; Test that sit-for is accounted correctly inside a process filter.

(defun before-test ()
  (setq proc (make-process
              :name "test"
              :buffer "test"
              :command '("cat")
              :filter 'filter-func)))

(defun filter-func (process string)
  (sit-for 1)
  (sleep-for 0.1))

(defun cause-input ()
  (process-send-string proc "HI\n"))

(defun after-test ()
  (delete-process proc))

;; driver code
(defun run-test ()
  (let ((session (start-test)))
    (wait-until-ready session)
    (eval-expr session "(cause-input)")
    (sleep-for 1.5)
    (call-after-test session)
    (wait-until-dead session)))

(defun finish-test (session)
  (let* ((stream (reverse event-stream))
         (filter (span-func stream "filter-func"))
         (passed 0))

    (message-assert
     (< (exit-measured-time (cadr filter)) 110)
     "filter time subtracted out sit-for time")

    (kill-emacs passed)))
