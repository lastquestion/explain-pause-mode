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
     ;; issue #15
     ;; %s is expected to remain, the printer needs to handle it
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
              (lambda (arglist this-is %s-crazy)))
              :to-equal
              "<closure> (arg-list: (arglist this-is %s-crazy))"))

 (it "prints closures with evil argument lists"
     ;; issue #15
     ;; see above
     (expect (explain-pause--command-as-string
              ;; note that we are in a lexical binding file.
              ;; check manual-test-command-logging for closure.
              (lambda (arglist is long)))
             :to-equal
              "<closure> (arg-list: (arglist is long))"))

 (it "prints bytecode for bytecode lambdas"
     (expect (explain-pause--command-as-string
              (byte-compile (lambda ()
                              (with-no-warnings
                                (message "astring")))))
             :to-equal
             "<bytecode> (references: (message))"))

 (it "prints unknown for random lists"
     (expect (explain-pause--command-as-string
              '(bar))
              :to-equal
              "Unknown (please file a bug) (bar)"))

 (it "prints unknown for values"
     (expect (explain-pause--command-as-string
              10)
              :to-equal
              "Unknown (please file a bug) 10"))

 (it "works for subrp"
     (expect (explain-pause--command-as-string
              (symbol-function 'read-char))
             :to-equal
             "#<subr read-char>")))

(describe
 "explain-pause--command-set-as-string"

 (it "adds commas in lists"
     (expect (explain-pause--command-set-as-string
              '(foo bar))
             :to-equal
             "foo, bar")))
