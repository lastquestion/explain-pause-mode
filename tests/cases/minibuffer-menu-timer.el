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

;;; test case for #19. Open a minibuffer, then open a menu, while
;;; letting a timer run. Verify that all interleaves do not count
;;; against each other.

(defun before-test ()
  (setq explain-pause-profile-enabled nil))

(defun timer-func ()
  (sleep-for 0.1))

(defun run-timer ()
  (setq sit-timer
        (run-with-timer 0.25 1 'timer-func)))

(defun after-test ()
  t)

;; driver code
(defun run-test ()
  (let ((session (start-test)))
    (wait-until-ready session)
    (eval-expr session "(run-timer)")
    (send-key session 'escape "x")
    (send-special-key session 'f10)
    (sleep-for 2)
    (send-special-key session 'quit)
    (sleep-for 0.5)
    (send-special-key session 'quit)
    (sleep-for 0.5)
    (call-after-test session)
    (wait-until-dead session)))

(defun finish-test (session)
  (let* ((stream (reverse event-stream))
         (m-x (span-func stream "execute-extended-command"))
         (menu-bar-open (span-func-between m-x "menu-bar-open"))
         (timer-within-menu-bar (span-func-between menu-bar-open "timer-func"))
         (timer-within-minibuffer (span-func-between
                                   (cons (cddr menu-bar-open)
                                         (cdr m-x))
                                   "timer-func"))
         (passed 0))

    (message-assert
     (< (exit-measured-time (cadr m-x)) 10)
     "minibuffer subtracted out all time")

    (message-assert
     (< (exit-measured-time (cadr menu-bar-open)) 10)
     "menu-bar subtracted out all time")

    (message-assert
     (< (exit-measured-time (cadr timer-within-menu-bar)) 110)
     "timer within menu bar was correctly measured")

        (message-assert
     (< (exit-measured-time (cadr timer-within-minibuffer)) 110)
     "timer within minibuffer was correctly measured")

    (kill-emacs passed)))
