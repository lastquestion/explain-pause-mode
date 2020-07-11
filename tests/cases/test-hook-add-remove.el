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

;;; test hook add/remove

;; TODO this needs to move to a new folder for tests that need
;; explain-pause-mode loaded but not a full subemacs.

(load-file "./explain-pause-mode.el")

(defun test-hook ()
  (setq test-hook-run t))

(defun it-adds-removes-symbols ()
  (let ((passed 0))

    (add-hook 'post-command-hook 'test-hook)
    (setq test-hook-run nil)
    (run-hooks 'post-command-hook)
    (message-assert
     test-hook-run
     "Test hook ran after adding")

    (remove-hook 'post-command-hook 'test-hook)
    (setq test-hook-run nil)
    (run-hooks 'post-command-hook)
    (message-assert
     (not test-hook-run)
     "Test hook did not run after removing")

    passed))

(defun it-adds-removes-lambda ()
  (let* ((passed 0)
         (ran nil)
         (hook (lambda ()
                 (setq ran t)))
         (compiled-hook (byte-compile
                         (lambda ()
                           (setq byte-ran t)))))

    (setq byte-ran nil)

    (add-hook 'post-command-hook hook)
    (add-hook 'post-command-hook compiled-hook)
    (run-hooks 'post-command-hook)

    (message-assert
     ran
     "Lambda hook ran after adding")

    (message-assert
     byte-ran
     "Lambda compiled hook ran after adding")

    (remove-hook 'post-command-hook hook)
    (remove-hook 'post-command-hook compiled-hook)
    (setq ran nil)
    (setq byte-ran nil)
    (run-hooks 'post-command-hook)
    (message-assert
     (not ran)
     "Lambda hook did not run after removing")
    (message-assert
     (not byte-ran)
     "Lambda compiled hook did not run after removing")

    passed))

;; driver code
(defun run-test ()
  (explain-pause-mode--install-hooks)

  (let ((passed
         (and (it-adds-removes-symbols)
              (it-adds-removes-lambda))))

    (kill-emacs passed)))
