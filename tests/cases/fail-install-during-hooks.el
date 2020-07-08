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

;;; Test that when you install, and the install fails for any reason,
;;; we catch and disply the report bug page.

(defun before-test ()
  ;; make an error happen during install
  (setf (symbol-function 'set-process-filter)
        (lambda ()
          (setq unbound (1+ unbound)))))

(defun after-test ()
  ;; check messages that there is a good message
  (let ((report-buffer (get-buffer "explain-pause-mode-report-bug")))
    (when report-buffer
      (with-current-buffer report-buffer
        (goto-char 0)
        (send-value "report-bug-displayed"
                    (re-search-forward "Installation of explain-pause-mode failed" nil t)))
      ;; if we don't kill it the default check-buffers will fail the test
      (kill-buffer report-buffer))))

(defun run-test ()
  (setq session (start-test
                 nil
                 nil
                 '("-f" "setup-test" "-f" "before-test")))

  (sleep-for 0.5)

  (m-x-run session "explain-pause-mode")

  (send-key session " ")

  (sleep-for 0.5)

  (call-after-test session)
  (wait-until-dead session))

(defun finish-test (session)
  (let ((passed 0))
    (message-assert
     (equal (nth 5 session) "exit-test-unclean")
     "mode not installed correctly, which is expected")

    (message-assert
     (get-value event-stream "report-bug-displayed")
     "report-bug was shown during failed install")

    (kill-emacs passed)))
