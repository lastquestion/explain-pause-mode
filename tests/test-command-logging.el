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

;;; Test command set as string.

(describe
 "explain-pause--command-as-string"

 (it "works for simple symbols"
     (expect (explain-pause--command-as-string
              'foo)
             :to-equal
             "foo"))

 (it "works for evil symbols"
     (expect (explain-pause--command-as-string
              'foo-%s)
             :to-equal
             "foo-%s"))

 (it "works for strings"
     (expect (explain-pause--command-as-string
              "foo")
              :to-equal
              "foo"))

 (it "prints closures with argument list only"
     (expect (explain-pause--command-as-string
              ;; note that we are in a lexcial binding file.
              ;; check manual-test-command-logging for closure.
              (lambda (arglist is long)))
              :to-equal
              "<closure> (arg-list: (arglist is long))"))

 (it "prints bytecode for bytecode lambdas"
     (expect (explain-pause--command-as-string
              (byte-compile (lambda ()
                              (with-no-warnings
                                ;; ignore the fact we have %s in the format
                                ;; specifier without args. test here that
                                ;; %s and \n is not in the message. Relates
                                ;; to issue #15.
                                (message "astring \n %s %%s")))))
             :to-equal
             "<bytecode> (references: (message))"))

 (it "prints unknown for random lists"
     (expect (explain-pause--command-as-string
              '(bar))
              :to-equal
              "Unknown (please file a bug)"))

 (it "prints unknown for values"
     (expect (explain-pause--command-as-string
              10)
              :to-equal
              "Unknown (please file a bug)")))

(describe
 "explain-pause--command-set-as-string"

 (it "adds commas in lists"
     (expect (explain-pause--command-set-as-string
              '(foo bar))
             :to-equal
             "foo, bar")))

(describe
 "explain-pause--sanitize-minibuffer"

 (it "deletes extra normal spaces"
     (expect (explain-pause--sanitize-minibuffer "  fo  g ")
             :to-equal
             " fo g "))

 (it "deletes newlines and tabs"
     (expect (explain-pause--sanitize-minibuffer "\nevil\twow\t\n\nso")
             :to-equal
             " evil wow so")))
