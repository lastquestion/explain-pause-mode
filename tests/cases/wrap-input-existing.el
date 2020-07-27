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

;;; In emacs 26 (but not 27), mouse-down is bound to a function that
;;; uses sit-for to test to see if it's a "double click" or not. This
;;; means that sit-for fires without a command-record, because
;;; translation-maps are called before command-execute. In emacs 27+,
;;; this was changed to record time
;;; https://github.com/emacs-mirror/emacs/commit/3d5e31eceb9dc1fb62b2b27bcab549df3bd04ce9
;;; Repro for #84

(defun before-test ()
  (with-current-buffer (get-buffer-create "test-buffer")
    (insert-text-button "Test"
                        'action 'test-click
                        'follow-link t)
    (goto-char 3))
  (switch-to-buffer "test-buffer")

  (setq debug-on-event nil)
  (setq mouse-1-click-follows-link 1)
  (define-key special-event-map [sigusr2] 'mouse-down))

(defun after-test ()
  t)

(defun mouse-down ()
  (interactive)
  (let ((mouse-down
         `(down-mouse-1
           (,(selected-window)
            ,(point)
            (0 . 0)
            0 ;; timestamp
            nil
            ,(point)
            (,(current-column) . ;; column
             ,(line-number-at-pos (point))) ;; line
            nil
            (0 . 0) ;; object-relative pixel
            (1 . 1)))))
    (setq unread-command-events
          (cons mouse-down unread-command-events))))

(defun blank ()
  t)

(defun run-test ()
  (setq session (start-test
                 nil
                 nil
                 '("-f" "setup-test" "-f" "before-test")))

  (sleep-for 0.5)

  (m-x-run session "explain-pause-mode")
  (eval-expr session "(blank)")
  (sleep-for 0.5)

  (send-signal session 'sigusr2)
  (sleep-for 2)

  (call-after-test session)
  (wait-until-dead session))

(defun finish-test (session)
  (let* ((stream (reverse event-stream))
         (passed 0))

    (message-assert
     (equal (nth 5 session) "exit-test-quit-emacs")
     "mouse click worked")

    (kill-emacs passed)))
