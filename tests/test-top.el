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

;;; Test top related code

(describe
 "explain-pause-top--command-entry-number-sorter"
 (it "generates a set of lambdas for sorting"
     (let ((lhs (make-explain-pause-top--command-entry
                 :count 10))
           (rhs (make-explain-pause-top--command-entry
                 :count 5))
           (sorter (explain-pause-top--command-entry-number-sorter 'count)))
       (expect (funcall sorter lhs rhs)
               :to-be
               nil)
       (expect (funcall sorter rhs lhs)
               :to-be
               t))))

(describe
 "explain-pause-top---command-entry-command-set-sorter"
 (let ((entry-c
        (make-explain-pause-top--command-entry
         :command-set '(c)))
       (entry-ca
        (make-explain-pause-top--command-entry
         :command-set '(c a)))
       (entry-caa
        (make-explain-pause-top--command-entry
         :command-set '(c a a)))
       (entry-cab
        (make-explain-pause-top--command-entry
         :command-set '(c a b)))
       (entry-cad
        (make-explain-pause-top--command-entry
         :command-set '(c a d)))
       (entry-da
        (make-explain-pause-top--command-entry
         :command-set '(d a)))
       (entry-a
        (make-explain-pause-top--command-entry
         :command-set '(a)))
       (entry-d
        (make-explain-pause-top--command-entry
         :command-set '(d)))
       (sorter
        (explain-pause-top---command-entry-command-set-sorter nil)))
   (it "sorts when sets same size"
       (expect
        (funcall sorter
         entry-c
         entry-a)
        :to-be
        t)

       (expect
        (funcall sorter
         entry-c
         entry-d)
         :to-be
         nil)

        (expect
         (funcall sorter
          entry-c
          entry-c)
          :to-be
          nil))

   (it "sorts when lhs longer"
       (expect
        (funcall sorter
         entry-ca
         entry-a)
        :to-be
        t))

   (it "sorts when rhs longer"
       (expect
        (funcall sorter
         entry-c
         entry-da)
        :to-be
        nil))

   (it "sorts after some equal"
       (expect
        (funcall sorter
         entry-cab
         entry-cad)
        :to-be
        nil)

       (expect
        (funcall sorter
         entry-cab
         entry-caa)
        :to-be
        t)

     (expect
      (funcall sorter
       entry-cab
       entry-ca)
      :to-be
      t))))
