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

;;; Test hook functions

(defun list-func ()
  (setq called t))

(defun single-function ()
  (setq called t))

(defvar hook-list-functions (list 'list-func))
(defvar hook-list-single 'single-function)
(defvar hook-list-lambda nil)

(describe
 "explain-pause--wrap-existing-hooks-in-list"

 (it
  "hooks lists"
  (let ((result
         (explain-pause--wrap-existing-hooks-in-list 'test-kind
                                                     hook-list-functions))
        (explain-pause-mode nil))
    (setq called nil)
    (expect result :to-be hook-list-functions)
    (run-hooks 'hook-list-functions)
    (expect called :to-be t)))

 (it
  "hooks single functions as a symbol"
  (let ((result
          (explain-pause--wrap-existing-hooks-in-list 'test-kind
                                                      hook-list-single))
         (explain-pause-mode nil))
    (expect
     result
     :to-be
     hook-list-single)

    (setq called nil)
    (run-hooks 'hook-list-single)
    (expect called :to-be t)))

 (it
  "hooks single functions as a lambda"
  (let ((hook-function (lambda ()
                         (setq called t)))
        (result nil))
    (setq hook-list-lambda hook-function)
    (setq result
          (explain-pause--wrap-existing-hooks-in-list 'test-kind
                                                      hook-list-lambda))
    (expect result :not :to-be nil)

    (setq called nil)
    (run-hooks 'hook-list-lambda)
    (expect called :to-be t)))

 (it
  "does nothing if nil"
  (expect
   (explain-pause--wrap-existing-hooks-in-list 'test-kind nil)
   :to-be nil)))
