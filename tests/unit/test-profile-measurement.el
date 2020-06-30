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

;;; Test profile measurement hook functions.

(defun assert-empty-statistics ()
  (expect (hash-table-count explain-pause-profile--profile-statistics)
          :to-be
          0))

(defun circular-to-list (record statistic)
  (let* ((size (explain-pause-profile--statistic-slow-length statistic))
         (start (explain-pause-profile--statistic-slow-index record)))

    (cl-loop
     with list = nil
     with idx = start
     do
     (let ((val
            (aref statistic (+ explain-pause-profile--statistic-slow-count-offset
                               idx))))
       (when val
         (setq list (cons val list)))

       (setq idx (% (1+ idx) size)))
     until (eq idx start)
     finally return list)))

(describe
 "explain-pause-profile--profile-statistics"

 (describe
  "explain-pause-profile-clear"

  (it "clears"
      (setq explain-pause-profile--profile-statistics (make-hash-table))
      (assert-empty-statistics)
      (puthash 'foo [] explain-pause-profile--profile-statistics)

      (explain-pause-profile-clear)

      (assert-empty-statistics)))

 (describe
  "explain-pause-profile--profile-measured-command"
  (let ((foo-record (make-explain-pause-command-record
                     :command 'foo))
        (saved-profile nil)
        (saved-threshold nil))

    (before-all
     (explain-pause-profile-clear)
     (setq saved-profiles explain-pause-profile-saved-profiles)
     (setq saved-threshold explain-pause-profile-slow-threshold)
     (setq explain-pause-profile-saved-profiles 2)
     (setq explain-pause-profile-slow-threshold 3))

    (after-all
     (setq explain-pause-profile-saved-profiles saved-profiles)
     (setq explain-pause-profile-slow-threshold saved-threshold))

    (before-each
     (explain-pause-profile-clear))

    (it
     "does nothing when the record is native"

     (explain-pause-profile--profile-measured-command
      (make-explain-pause-command-record
       :native t))

     (assert-empty-statistics))

    (it
     "increments count and saves slow records"

     (dotimes (i 2)
       (explain-pause-profile--profile-measured-command
        (make-explain-pause-command-record
         :command 'foo
         :executing-time (+ (* i 1000) 500)
         :too-slow t)))

     (expect (explain-pause-profile--statistic-slow-count
              foo-record)
             :to-be
             2)

     (expect (circular-to-list
              foo-record
              (gethash 'foo explain-pause-profile--profile-statistics))
             :to-equal
             '(1500 500)))

    (it
     "throws away oldest slow record but keeps count"

     (dotimes (i 4)
       (explain-pause-profile--profile-measured-command
        (make-explain-pause-command-record
         :command 'foo
         :executing-time (+ (* i 1000) 500)
         :too-slow t)))

     (expect (explain-pause-profile--statistic-slow-count
              foo-record)
             :to-be
             4)

     (expect (circular-to-list
              foo-record
              (gethash 'foo explain-pause-profile--profile-statistics))
             :to-equal
             '(3500 2500)))
    (it
     "marks something for profiling after enough slow times"

     (dotimes (i 3)
       (explain-pause-profile--profile-measured-command
        (make-explain-pause-command-record
         :command 'foo
         :executing-time 500
         :too-slow t)))

     (expect (explain-pause-profile--statistic-profile-p
              foo-record)
             :to-be
             t))

    (it
     "unmarks something for profiling after enough failed profiles"

     (dotimes (i 3)
       (explain-pause-profile--profile-measured-command
        (make-explain-pause-command-record
         :command 'foo
         :executing-time 500
         :too-slow t)))

     (expect (explain-pause-profile--statistic-profile-p
              foo-record)
             :to-be
             t)

     (explain-pause-profile--profile-measured-command
      (make-explain-pause-command-record
       :command 'foo
       :executing-time 50
       :too-slow nil
       :is-profiled t))

     (expect (explain-pause-profile--statistic-profile-p
              foo-record)
             :to-be
             t)

     (dotimes (i 2)
       (explain-pause-profile--profile-measured-command
        (make-explain-pause-command-record
         :command 'foo
         :executing-time 50
         :too-slow nil
         :is-profiled t)))

     (expect (explain-pause-profile--statistic-profile-p
              foo-record)
             :to-be
             nil))

    (it
     "stores a profile if given"

     (explain-pause-profile--profile-measured-command
      (make-explain-pause-command-record
       :command 'foo
       :executing-time 1000
       :too-slow t
       :is-profiled t
       :profile 'profile-1))

     (let ((profiles (explain-pause-profile--statistic-profiles foo-record)))
       (expect (length profiles)
               :to-be
               1)

       (expect (car profiles)
               :to-equal
               [1000 profile-1])))

    (it
     "stores profiles in the right order"
     (explain-pause-profile--profile-measured-command
      (make-explain-pause-command-record
       :command 'foo
       :executing-time 1000
       :too-slow t
       :is-profiled t
       :profile 'profile-1))

     (explain-pause-profile--profile-measured-command
      (make-explain-pause-command-record
       :command 'foo
       :executing-time 2000
       :too-slow t
       :is-profiled t
       :profile 'profile-2))

     (let ((profiles (explain-pause-profile--statistic-profiles foo-record)))
       (expect (length profiles)
               :to-be
               2)

       (expect profiles
               :to-equal
               '([2000 profile-2]
                 [1000 profile-1]))))

    (it
     "gets rid of older profiles if out of space"

     (dotimes (i 5)
       (explain-pause-profile--profile-measured-command
        (make-explain-pause-command-record
         :command 'foo
         :executing-time (* (+ i 1) 1000)
         :too-slow t
         :is-profiled t
         :profile (intern (format "profile-%d" i)))))

     (expect
      (explain-pause-profile--statistic-profiles foo-record)
      :to-equal
      '([5000 profile-4] [4000 profile-3]))))))

