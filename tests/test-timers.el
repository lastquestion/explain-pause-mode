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

;;; Test timer wrapper code.

(describe
 "explain-pause--generate-timer-parent"

 (it "returns a new frame in normal situations"
     (let ((parent
            (make-explain-pause-command-record
             :command 'bar
             :depth 0)))
       (expect
        (explain-pause--generate-timer-parent
         'foo
         parent
         'timer)
        :to-equal
        (make-explain-pause-command-record
         :command 'timer
         :depth 1
         :parent parent
         :native t))))

 (it "reuses a frame in classic tail recursion"
     (let* ((native
             (make-explain-pause-command-record
              :command 'timer
              :native t
              :depth 0))
            (parent
             (make-explain-pause-command-record
              :command 'bar
              :depth 1
              :parent native)))
       (expect
        (explain-pause--generate-timer-parent
         'bar
         parent
         'timer)
        :to-equal
        native)))

 (let* ((root
         (make-explain-pause-command-record
          :command 'root
          :depth 0))
        (root-native
         (make-explain-pause-command-record
          :command 'timer
          :native t
          :parent root
          :depth 1))
        (frame-1
         (make-explain-pause-command-record
          :command 'bar
          :depth 2
          :parent root-native))
        (child-native
         (make-explain-pause-command-record
          :command 'idle-timer
          :parent frame-1
          :native t
          :depth 3))
        (frame-2
         (make-explain-pause-command-record
          :command 'foo
          :parent child-native
          :depth 4)))

   (it "cuts off the frames if frame length > max of the right kind"
       (let ((explain-pause--timer-frame-max-depth 3))
         (expect
          (explain-pause--generate-timer-parent
           'zaz
           frame-2
           'timer)
          :to-equal
          root-native)

         (expect
          (explain-pause--generate-timer-parent
           'zaz
           frame-2
           'idle-timer)
          :to-equal
          child-native)))

   (it "cuts to emacs root if it can't find one that matches"
       (let ((explain-pause--timer-frame-max-depth 1))
         (expect
          (explain-pause--generate-timer-parent
           'zaz
           frame-2
           'idle-timer)
          :to-equal
          (make-explain-pause-command-record
           :command 'idle-timer
           :parent explain-pause-root-command-loop
           :depth 1
           :native t))))))
