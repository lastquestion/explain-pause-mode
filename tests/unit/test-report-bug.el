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

;;; Test report bug
(defmacro mock-and-call (&rest body)
  ;; https://github.com/jorgenschaefer/emacs-buttercup/issues/180
  `(let ((saved-explain-pause-mode (symbol-function 'explain-pause-mode))
         (saved-profiler-cpu-stop (symbol-function 'profiler-cpu-stop)))
     (setq explain-pause-mode-call nil)
     (setq profiler-cpu-stop-call nil)

     (unwind-protect
         (progn
           (setf (symbol-function 'explain-pause-mode)
                 (lambda (&rest args)
                   (setq explain-pause-mode-call (cons 'called args))))
           (setf (symbol-function 'profiler-cpu-stop)
                 (lambda (&rest args)
                   (setq profiler-cpu-stop-call (cons 'called args))))
           ,@body)

       (setf (symbol-function 'explain-pause-mode) saved-explain-pause-mode)
       (setf (symbol-function 'profiler-cpu-stop) saved-profiler-cpu-stop))))

(describe
 "explain-pause-report-measuring-bug"
 :var (the-body
       the-buffer
       explain-pause-mode-call
       profiler-cpu-stop-call)

 (describe
  "working case"
  (before-all
   ;; https://github.com/jorgenschaefer/emacs-buttercup/issues/179
   (condition-case err
       (progn
         (mock-and-call
          (explain-pause-report-measuring-bug
           "The place where we bugged"
           "some-record"
           (make-explain-pause-command-record
            :command 'bar
            :depth 0)
           "some-string"
           "a happy string"))

         (setq the-buffer (get-buffer "explain-pause-mode-report-bug"))

         (when the-buffer
           (with-current-buffer the-buffer
             (setq the-body (buffer-string)))))
     (error
      (message "before-all failed %s" err))))

  (after-all
   (kill-buffer the-buffer))

  (it
   "disables the mode and turns off profiling"
   (expect explain-pause-mode-call :to-equal '(called -1))
   (expect profiler-cpu-stop-call :to-equal '(called)))

  (it
   "creates the buffer"
   (expect the-buffer :not :to-be nil))

  (it
   "puts the explain-pause-version"
   (expect
    (string-match "^explain-pause.*version:\\s-+\\(.*\\)$" the-body)
    :not :to-be nil)

   (expect
    (match-string 1 the-body)
    :to-equal
    (format "%s" explain-pause-version)))

  (it
   "puts the emacs version"
   (expect
    (string-match "^emacs version:\\s-+\\(.*\\)$" the-body)
    :not :to-be nil)

   (expect
    (match-string 1 the-body)
    :to-equal
    (format "%s" emacs-version)))

  (it
   "prints a backtrace"
   (expect
    (string-match "Backtrace:" the-body) :not :to-be nil)

   ;; buttercup is in the backtrace
   (expect
    (string-match "buttercup" the-body) :not :to-be nil)

   ;; make sure the first line is the bug report function
   (expect
    (string-match "Backtrace:\n\\s-+explain-pause-report-measuring-bug" the-body)
    :not :to-be nil))

  (it
   "prints the additional args"
   (expect
    (string-match
     "some-record\n.*explain-pause-command-record.*\nsome-string\na happy string"
     the-body)
    :not :to-be nil)))

 (it
  "works even with no optional args"
  (ignore-errors
    (kill-buffer "explain-pause-mode-report-bug"))
  (mock-and-call
   (explain-pause-report-measuring-bug
    "wow"))
  (expect explain-pause-mode-call
          :to-equal '(called -1))
  (expect profiler-cpu-stop-call
          :to-equal '(called))
  (expect (get-buffer "explain-pause-mode-report-bug")
          :not :to-be nil)))
