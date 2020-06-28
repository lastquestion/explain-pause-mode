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

;;; Test that we can boot while in a init.el. Init.el is NOT the same
;;; as running with `-l`, unfortunately.

(defun check-buffers ()
  "Find any buffer with backtrace or explain-pause-mode-report-bug or
if messages buffer has error message."
  (cl-loop
   for buffer being the buffers
   do
   (let ((name (buffer-name buffer)))
     (when (or (string-match-p "backtrace" name)
               (string-match-p "explain-pause-mode-report-bug" name)
               (string-match-p "Warnings" name))
       (send-exit-record "exit-test-debugger-invoked")
       (kill-emacs 1)))))

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

    (setq session (start-test
                   filename
                   '("-nw" "--no-site-lisp" "--no-splash" "--no-x-resources")
                   '("-f" "setup-test" "-f" "check-buffers")))

    (sleep-for 0.5)

    (call-after-test session)
    (wait-until-dead session)))

(defun finish-test (session)
  ;; if we got here we didn't die during check-buffers
  (kill-emacs 0))
