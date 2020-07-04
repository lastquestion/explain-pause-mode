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

;;; Further regression test on #50. Test that in init.el startup,
;;; if read-key or another advised function called after, it works.

(defun after-test ()
  t)

(defun run-test ()
  ;; TODO ...
  (let* ((filename (symbol-file 'run-test))
         (homedir (file-name-directory filename))
         (rootdir (expand-file-name homedir "../../"))
         (session nil))

    ;; save the default directory fully expanded because we're about
    ;; to reset HOME:
    (setq-local default-directory
                (expand-file-name default-directory))
    (setenv "HOME" homedir)
    (setenv "EMACSLOADPATH" (format "%s:" default-directory))
    (setenv "TESTREAD" "1")

    (setq session (start-test
                   filename
                   '("-nw" "--no-site-lisp" "--no-splash" "--no-x-resources"
                     "--debug-init")
                   '("-f" "setup-test" "-f" "check-buffers")))

    (sleep-for 0.5)

    (call-after-test session)
    (wait-until-dead session)))

(defun finish-test (session)
  (let ((passed 0))
    (message-assert
     (equal (nth 5 session) "exit-test-quit-emacs")
     "mode installed correctly")
    (kill-emacs passed)))
