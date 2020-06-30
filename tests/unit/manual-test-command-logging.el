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

;;; Test command set as string, without lexical binding, manually.
;;; the only way to test lambdas without closures...

(let* ((result
        (explain-pause--command-as-string
         (lambda (x y z)
           (message "%s\nevilness"))))
       (compare
        "<lambda> (arg-list: (x y z))")
       (pass (equal result compare)))
  (if pass
      (message "\e[32mpassed\e[m")
    (message "\e[31mlambda is not working in explain-pause--command-as-string:
'%s' does not match '%s'.

check tests/manual-test-command-logging\e[m" result compare)
    (kill-emacs 1)))
