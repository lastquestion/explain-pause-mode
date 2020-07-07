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

;;; negative test case for #64, when stderr is a process

(defun my-sentinel (proc event) t)
(defun a-filter (proc str) t)

(defun before-test ()
  (setq err-proc
        (make-pipe-process
         :name "test-err"
         :buffer "my-awesome-buffer"
         :filter 'a-filter))

  (setq proc (make-process
              :name "test"
              :buffer "test"
              :command '("cat")
              :stderr err-proc))

  (set-process-sentinel err-proc 'my-sentinel))

(defun after-test ()
  (delete-process proc))

;; driver code
(defun run-test ()
  (let ((session (start-test)))
    (wait-until-ready session)
    (sleep-for 1)
    (call-after-test session)
    (wait-until-dead session)))

(defun finish-test (session)
  (let ((passed 0))
    (message-assert
     (equal (nth 5 session) "exit-test-quit-emacs")
     "set-process-sentinel succeeded on stderr process created via elisp make-pipe-process.")
    (kill-emacs passed)))
