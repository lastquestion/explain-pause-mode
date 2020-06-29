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

;;; test case for #44, nil dereference because nil is the default
;;; process handler

(defun before-test ()
  (setq proc (make-process
              :name "test"
              :buffer "test"
              :command '("cat")))

  (set-process-filter proc nil))

(defun cause-input ()
  (process-send-string proc "HI\n"))

(defun after-test ()
  (delete-process proc))

;; driver code
(defun run-test ()
  (let ((session (start-test)))
    (wait-until-ready session)
    (eval-expr session "(cause-input)")
    (sleep-for 1)
    (call-after-test session)
    (wait-until-dead session)))

(defun finish-test (session)
  (let ((passed 0))
    (message-assert
     (equal (nth 5 session) "exit-test-quit-emacs")
     "mode installed correctly")
    (kill-emacs passed)))
