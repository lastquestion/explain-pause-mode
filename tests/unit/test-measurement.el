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

;;; Test measurement engine

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

(describe
 "explain-pause--interactive-form-needs-frame-p"

 (it
  "skips special chars"

  (expect (explain-pause--interactive-form-needs-frame-p "*p")
          :to-be
          nil)

  (expect (explain-pause--interactive-form-needs-frame-p "^P")
          :to-be
          nil)

  (expect (explain-pause--interactive-form-needs-frame-p "@e")
          :to-be
          nil)

  (expect (explain-pause--interactive-form-needs-frame-p "*^@P")
          :to-be
          nil))

 (it
  "skips until the new line"

  (expect (explain-pause--interactive-form-needs-frame-p "PnN\niM")
          :to-be
          nil))

 (it
  "works with no prompt"

  (expect (explain-pause--interactive-form-needs-frame-p "P\np")
          :to-be
          nil))

 (it
  "returns t for various interactive cases"

  (expect (explain-pause--interactive-form-needs-frame-p "Mprompt: ")
          :to-be
          t)))

(defun test-function () t)
(defun byte-compile-function () t)
(defun double-trouble () t)

(describe
 "explain-pause--advice-add-cb"

 (before-all
  (byte-compile 'byte-compile-function))

 (it
  "advises a symbol function and returns it"
  (expect
   (explain-pause--advice-add-cb 'test-function 'test-list)
   :to-be
   'test-function)

  (expect
   (advice--p (symbol-function 'test-function))
   :to-be
   t))

 (it
  "advises a bytecompiled function"
  (expect
   (explain-pause--advice-add-cb 'byte-compile-function 'test-list)
   :to-be
   'byte-compile-function)

  (expect
   (advice--p (symbol-function 'byte-compile-function))
   :to-be
   t))

 (it
  "advises a lambda"
  (let ((call-count 0)
        (orig-func (symbol-function 'explain-pause--lambda-cb-wrapper)))
    (setf (symbol-function 'explain-pause--lambda-cb-wrapper)
          (lambda (&rest args)
            (setq call-count (1+ call-count))))
    (setq test-lambda (lambda () t))
    (setq result-lambda (explain-pause--advice-add-cb test-lambda 'test-list))
    (expect result-lambda
            :not :to-equal
            test-lambda)
    (funcall result-lambda)
    (expect call-count :to-be 1)
    (setq call-count 0)
    (setq result2-lambda (explain-pause--advice-add-cb result-lambda 'test-list))
    (expect result2-lambda
            :to-equal
            result-lambda)
    (funcall result2-lambda)
    (expect call-count :to-be 1)
    (setf (symbol-function 'explain-pause--lambda-cb-wrapper) orig-func))))
