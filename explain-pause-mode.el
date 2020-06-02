;;; explain-pause-mode.el --- explain emacs pauses -*- lexical-binding: t; emacs-lisp-docstring-fill-column: 80; fill-column: 80; -*-
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

;;; Commentary:

;; `explain-pause-mode' is a minor mode that measures how long Emacs commands
;; take to run. `explain-pause-top' is like 'top', but for Emacs.
;;
;; When a command consistently takes a long time to run, `explain-pause-mode'
;; alerts the user and records the next time the command is slow. These "slow
;; events" can be viewed in `explain-pause-top' and investigated immediately,
;; or summarized to be sent to developers.

;; Please see README.md for commentary, documentation, etc. in the repository
;; above.

;;; Code:

(require 'seq)
(require 'profiler)

;; customizable behavior
(defgroup explain-pause nil
  "Explain pauses in Emacs"
  :prefix "explain-pause-"
  :group 'development)

(defgroup explain-pause-logging nil
  "Explain pause logging"
  :prefix "explain-pause-log-"
  :group 'explain-pause)

(defgroup explain-pause-alerting nil
  "Explain pause alerting"
  :prefix "explain-pause-alert-"
  :group 'explain-pause)

(defgroup explain-pause-profiling nil
  "Explain pause profiling"
  :prefix "explain-pause-profile-"
  :group 'explain-pause)

;; main behaviors

(defcustom explain-pause-slow-too-long-ms 40
  "How long must some activity take before explain-pause considers it slow, in ms?"
  :type 'integer
  :group 'explain-pause)

;; profiling behaviors
(defcustom explain-pause-profile-slow-threshold 3
  "Explain-pause will profile a slow activity once it has executed slowly this
many times."
  :type 'integer
  :group 'explain-pause-profiling)

(defcustom explain-pause-profile-enabled t
  "Should explain-pause profile slow activities at all?"
  :type 'boolean
  :group 'explain-pause-profiling)

(defcustom explain-pause-profile-cpu-sampling-interval 200000
  "The CPU sampling interval when the profiler is activated in microseconds.
The default value is 2ms."
  :type 'integer
  :group 'explain-pause-profiling)

(defcustom explain-pause-profile-saved-profiles 5
  "The number of CPU profiles to save, after which the oldest is removed.
If you change this number, run `explain-pause-profiles-clear' to adjust
the buffer size (but you will lose the current profiles)."
  :type 'integer
  :set (lambda (symbol val)
         (set-default symbol val)
         (explain-pause-profiles-clear))
  :initialize 'custom-initialize-default
  :group 'explain-pause-profiling)

(defcustom explain-pause-top-auto-refresh-interval 2
  "How often `explain-pause-top' mode buffers refresh themselves by default,
in seconds. This can be a fraction of a second. If this is nil, they
do not automatically refresh. You can control this on a per buffer basis
by calling `explain-pause-top-auto-refresh'."
  :type '(choice (number :tag "Interval (seconds)")
                 (const :tag "Never" nil))
  :group 'explain-pause)

;; developer logging behaviors
(defcustom explain-pause-log-all-input-loop nil
  "Should all command loop executions be logged? WARNING: Very noisy!"
  :type 'boolean
  :group 'explain-pause-logging)

(defcustom explain-pause-log-all-timers nil
  "Should all timer executions be logged? WARNING: Very noisy!"
  :type 'boolean
  :group 'explain-pause-logging)

(defcustom explain-pause-log-all-process-io nil
  "Should all process filter executions be logged? WARNING: Very noisy!"
  :type 'boolean
  :group 'explain-pause-logging)

;; public hooks
(defvar explain-pause-measured-command-hook nil
  "Functions(s) to call after a command has been measured. The functions are
called with arguments (ms, read-io-ms, command-set). Command-set is a list
of function symbols or strings.

These commands must be fast, because this hook is executed on every command,
not just slow commands.")

;; logging functions
(defun explain--as-ms-exact (time)
  "Returns the TIME object in exact milliseconds, ignoring picoseconds."
  (seq-let [high-seconds low-seconds microseconds] time
    (+ (* (+ (* high-seconds 65536) low-seconds) 1000) (/ microseconds 1000))))

(defun explain-pause--float-2-fixed (val)
  "Turn a floating point value into a fixed 2 digit string."
  (format "%.2f" val))

(let ((explain--log-buffer nil))
  (defun explain--get-log-buffer ()
    "Get the explain-pause-log buffer or create it if does not exist"
    (when (not (buffer-live-p explain--log-buffer))
      (setq explain--log-buffer (get-buffer-create "*explain-pause-log*"))
      (with-current-buffer explain--log-buffer
        (setq buffer-read-only 1)))
    explain--log-buffer))

(defun explain--write-to-log (str &optional newline)
  "Write a string STR to the log buffer, optionally inserting a NEWLINE."
  (with-current-buffer (explain--get-log-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format "%s - " (current-time-string)))
      (insert str)
      (when newline
        (insert "\n"))
      (insert "\n") ;; always add a new line between lines
      (goto-char (point-max)))))

(defun explain-pause-mode-change-alert-style (new-style)
  "Change the alerting style to NEW-STYLE. Note that this does not change the
customizable variable `explain-pause-alert-style'.

NEW-STYLE can be:
'developer, where all alerts are shown;
'normal, when alerts are shown when more then 5 have occurred, and not
within 15 minutes of the last time an alert was shown; or
'silent, aka never."
  (let ((kinds
         '((developer . explain-pause-mode--log-alert-developer)
           (normal . explain-pause-mode--log-alert-normal))))
    (dolist (kind kinds)
      (remove-hook 'explain-pause-measured-command-hook (cdr kind)))

    (let ((new-hook (assq new-style kinds)))
      (when new-hook
        (add-hook 'explain-pause-measured-command-hook (cdr new-hook))))))

(let ((notification-count 0)
      (last-notified (current-time))
      (alert-timer nil))
  (defun explain-pause-mode--log-alert-normal (ms read-ms command-set was-profiled)
    "Notify the user of alerts when at least `explain-pause-alert-normal-minimum-count'
alerts have occurred, AND the time since the last notification (or startup)
is greater then `explain-pause-alert-normal-interval' minutes."
    (when (> ms explain-pause-slow-too-long-ms)
      (setq notification-count (1+ notification-count))
      (when (and (>= notification-count explain-pause-alert-normal-minimum-count)
                 (> (float-time (time-subtract nil last-notified))
                    (* explain-pause-alert-normal-interval 60))
                 (not alert-timer))
        (setq alert-timer
              (run-with-idle-timer 1 nil
                                   #'explain-pause-mode--log-alert-normal-display)))))

  (defun explain-pause-mode--log-alert-normal-display ()
    "Display the normal alert to the user but only if the minibuffer is not
active. If it is open, do nothing; at some point later, the conditions will
fire again and this timer will be called again."
    (setq alert-timer nil)
    ;; if we are not actively in the minibuffer, display our message
    (when (not (minibufferp (current-buffer)))
      (message "Emacs was slow %d times recently. Run `explain-pause-top' or check `*explain-pause-log*' to learn more." notification-count)
      (setq notification-count 0)
      (setq last-notified (current-time)))))

(let ((notifications '())
      (profiled-count 0)
      (alert-timer nil))
  (defun explain-pause-mode--log-alert-developer (ms read-ms command-set was-profiled)
    "Log all slow and profiling alerts in developer mode. They are gathered until
run-with-idle-timer allows an idle timer to run, and then they are printed
to the minibuffer with a 2 second sit-for."
    (when (> ms explain-pause-slow-too-long-ms)
      (push ms notifications)
      (when was-profiled
        (setq profiled-count (1+ profiled-count)))
      (unless alert-timer
        (setq alert-timer
              (run-with-idle-timer 0.5 nil
                                   #'explain-pause-mode--log-alert-developer-display)))))

  (defun explain-pause-mode--log-alert-developer-display ()
    "Display the last set of notifications in the echo area when the minibuffer is
not active."
    (if (minibufferp (current-buffer))
        ;; try again
        (setq alert-timer
              (run-with-idle-timer 0.5 nil
                                   #'explain-pause-mode--log-alert-developer-display))
      ;; ok, let's draw
      (message "Emacs was slow: %s ms%s"
               (mapconcat #'number-to-string notifications ", ")
               (if (> profiled-count 0)
                   (format " of which %d were profiled. Run `explain-pause-profiles' to learn more."
                           profiled-count)
                 ". Run `explain-pause-top' or check `*explain-pause-log*' to learn more."))
      ;; reset so more notifications can pile up while we wait
      (setq notifications '())
      (setq profiled-count 0)
      (sit-for 2)
      (message nil)
      ;; don't let us get rescheduled until we're really done.
      (setq alert-timer nil))))

;; logging customization
;; depressingly can't define it at the top because `explain-pause-mode-change-alert-style
;; isn't defined yet...
(defcustom explain-pause-alert-style 'normal
  "How often should explain-pause alert you about slow pauses in the mini-buffer?

Changing this value immediately adjusts the behavior. You can do this manually by
calling `explain-pause-mode-change-alert-style' directly if you wish. Note that
calling that function does not change this value."
  :type '(choice (const :tag "Always" developer)
                 (const :tag "Every now and then" normal)
                 (const :tag "Never" silent))
  :group 'explain-pause
  :set (lambda (symbol val)
         (set-default symbol val)
         (explain-pause-mode-change-alert-style val)))

(defcustom explain-pause-alert-normal-interval 15
  "What is the minimum amount of time, in minutes, between alerts when
`explain-pause-alert-style' is normal? You can put a fractional value if you
wish."
  :type 'number
  :group 'explain-pause-alerting)

(defcustom explain-pause-alert-normal-minimum-count 5
  "How many slow events must occur before `explain-pause' alerts you when
`explain-pause-alert-style' is normal?"
  :type 'integer
  :group 'explain-pause-alerting)

;; TODO perhaps this should also display minor modes? probably. minor modes can be interact
;; weirdly and become slow.
(defun explain--buffer-as-string ()
  "Return a human readable string about the buffer (name + major mode)."
  (format "%s (%s)"
          (buffer-name)
          major-mode))

(defun explain--buffers-as-string (buffers)
  "Return a human readable string for all BUFFERS given."
  (mapconcat (lambda (buffer)
               (with-current-buffer buffer
                 (explain--buffer-as-string)))
           buffers ", "))

(defun explain-pause--sanitize-minibuffer (contents)
  "Sanitize the minibuffer contents so it does not contain extra whitespace
and especially newlines."
  (replace-regexp-in-string "[\n\t ]+" " " contents))

(defun explain-pause--command-as-string (cmd)
  "Generate a human readable string for a command CMD.

Normally this is a symbol, when we are in a command loop, but in timers, process
filters, etc. this might be a lambda or a bytecompiled lambda. In those cases,
also handle if the forms are wrapped by closure. For bytecompiled code, use the
references as the best information available. For lambdas and closures, hope
that the argument names are clarifying. We also allow strings for things that go
through minibuffer invocations. Note that in elisp, symbols may have %! So
e.g. this function may generate strings with format specifiers in them."
  (cond
   ((stringp cmd) cmd)
   ((symbolp cmd) (symbol-name cmd))
   ((byte-code-function-p cmd)
    ;; "The vector of Lisp objects referenced by the byte code. These include
    ;; symbols used as function names and variable names."
    ;; list only symbol references:
    (let ((filtered-args
           (seq-filter #'symbolp (aref cmd 2))))
      (format "<bytecode> (references: %s)" filtered-args)))
   ((not (listp cmd))
    ;; something weird. This should not happen.
    "Unknown (please file a bug)")
   ;; closure. hypothetically, this is defined as a implementation detail,
   ;; but we'll read it anyway...
   ((eq (car cmd) 'closure)
    (format "<closure> (arg-list: %s)" (nth 2 cmd)))
   ;; lambda. directly read the arg-list:
   ((eq (car cmd) 'lambda)
    (format "<lambda> (arg-list: %s)" (nth 1 cmd)))
   (t
    "Unknown (please file a bug)")))

(defun explain-pause--command-set-as-string (command-set)
  "Format a COMMAND-SET as a human readable string.

A command set is a list of commands that represent the context that lead to the
blocking execution (or we think so, anyway)."
  (mapconcat
   #'explain-pause--command-as-string
   command-set ", "))

(defun explain--alert-delays (ms-or-array)
  "Display an alert message of duration(s) MS-OR-ARRAY."
  (let ((ms-str
         (if (listp ms-or-array)
             (mapconcat #'prin1-to-string (reverse ms-or-array) ", ")
           (prin1-to-string ms-or-array))))
    (message "Emacs blocked for %s ms - check *explain-pause-log*" ms-str)))

(defun explain--log-pause (diff read-wait-ms command-set log-current-buffer buffer-difference)
  "Log the pause to the log.

DIFF is the ms duration of the pause.
READ-WAIT-MS is the ms duration of any read* functions.
COMMAND-SET is the command-set that paused.
if LOG-CURRENT-BUFFER or BUFFER-DIFFERENCE are not nil, they are logged.  These are buffer objects."
  ;; use sprintf, it's probably faster (...eh)
  (let ((read-wait-str
         (if (> read-wait-ms 0)
             (format " (read-wait %s ms)" read-wait-ms)
           ""))
        (buffer-difference-str
         (if buffer-difference
             (format " (new buffers [%s])" (explain--buffers-as-string buffer-difference))
           ""))
        (current-buffer-str
         (if log-current-buffer
             (format " [%s]" (explain--buffer-as-string))
           ""))
        ;; safe; this is formatted as "%s" in next line
        (commandset-str (explain-pause--command-set-as-string command-set)))

    (explain--write-to-log
                  (format "%d ms%s - %s%s%s\n"
                          diff
                          read-wait-str
                          commandset-str
                          current-buffer-str
                          buffer-difference-str))))

;; profiling functions
(defun explain--profile-report-click-profile (button)
  "Click-handler when profile BUTTON is clicked in event profile report view."
  (let ((profile (button-get button 'profile)))
    (profiler-report-profile-other-frame profile)))

(defun explain--profile-report-click-refresh (_)
  "Click-handler when refresh button is clicked in event profile report view."
  (explain-pause-profiles t))

(defun explain--profile-report-header (&optional msg)
  "Generate a header for the profile report with help text and refesh button.

The optional parameter MSG is additional flavor text."
  (when msg
    (insert msg))
  (insert "This buffer is not auto-updated! Run `explain-pause-profiles' or ")
  (insert-text-button "[ Refresh ]"
                      'action #'explain--profile-report-click-refresh)
  (insert "\n\n"))

(let ((candidates (make-hash-table
                   :test 'equal))
      (profiles (make-ring explain-pause-profile-saved-profiles)))

  (defun explain-pause-profiles-candidates ()
    "Return the candidate table of functions that may be profiled soon."
    candidates)

  (defun explain-pause-profiles-clear-candidates ()
    "Clear all entries from the profiling candidates."
    (interactive)
    (clrhash candidates))

  (defun explain-pause-profiles-ignore-command (command-set)
    "Ignore this command-set from ever being profiled."
    ;;TODO (interactive)
    (puthash command-set -2 candidates))

  (defun explain-pause-profiles-force-command (command-set)
    "Force this command-set to be profiled the next time it is run, after
which the normal profiling rules apply."
    ;;TODO (interactive)
    ;;TODO this probably should use regex match against the strings
    (puthash command-set -1 candidates))

  (defun explain-pause-profiles (&optional print-to-buffer)
    "Return a list of the saved profiles. Each element is a list of
(time-as-obj diff-in-ms command-set profile). When PRINT-TO-BUFFER is not
nil, the profiles are written human-readable into a temporary buffer, which
is returned. (This buffer is not updated automatically when profiles are
changed.)"
    (interactive "p")
    (cond
     (print-to-buffer
      (with-temp-buffer-window
       "*explain-pause-profiles*" 'display-buffer-reuse-window nil
       (with-current-buffer "*explain-pause-profiles*"
         (explain--profile-report-header
          (when (ring-empty-p profiles)
            "No slow profile entries yet."))
         (dolist (profile (ring-elements profiles))
           (seq-let [time-stamp diff command-set profile] profile
             ;; TODO maybe a nicer table or something? There's only a handful of items though.
             (insert (format "Slow profile report\n  Time: %s\n  Command: %s\n  Duration: %d ms\n\n"
                             (current-time-string time-stamp)
                             (explain-pause--command-set-as-string command-set)
                             diff))
             (insert-text-button "[ View profile ]"
                                 'action #'explain--profile-report-click-profile
                                 'profile profile)
             (insert "\n\n"))))))
     (t
      (ring-elements profiles))))

  (defun explain-pause-profiles-clear ()
    "Clear the saved profiles of blocking work."
    (interactive)
    (setq profiles (make-ring explain-pause-profile-saved-profiles)))

  (defun explain--profile-p (command-set)
    "Should this command be profiled?"
    (let ((count (gethash command-set candidates 0)))
      (and explain-pause-profile-enabled
           (or (eq count -1) ;; forced
               (>= count
                   explain-pause-profile-slow-threshold)))))

  (defun explain--store-profile (time diff command-set profile)
    "Store the profiling information and reset the profile counter."
    ;;TODO probably, we'd like to count how many times a function is profiled
    ;;and alert / ignore after some threshold. as it is, we'll continually
    ;;profile slow stuff forever.
    (puthash command-set 0 candidates)
    (ring-insert profiles
                 (list time diff command-set profile)))

  (defun explain--increment-profile (command-set)
    "Increment the profile count for this command-set."
    (let ((count (gethash command-set candidates 0)))
      ;; only increment if the counter is acting "normal" (-1, -2 special)
      (when (>= count 0)
        (setq count (1+ count))
        (puthash command-set count candidates)))))

;; table functions
;; I tried to use `tabulated-list' as well as `ewoc' but I decided to implement
;; something myself. This list/table implements some ideas that are from react/JS
;; like philosophies around optimizing drawing...
;; part of it is already abstracted out into something close to reusable, but
;; other parts are not yet.

;; don't type check. note this only applies when (cl--compiling-file) returns t
;; - e.g. when it's bytecompiled.
(cl-declaim (optimize (safety 0) (speed 3)))

(cl-defstruct explain-pause-top--table
  ;; the list of entries to display, in sorted order
  ;; to simplify list manipulation code, always have a head
  (entries (list nil))
  ;; the display entries bookkeeping; a list of explain-pause-top--table-display-entry
  (display-entries (list nil))
  ;; the sort. it must be set before any inserts or updates.
  (sorter nil)
  ;; the current width
  (width 0)
  ;; whether on next paint, we need to resize
  (needs-resize t)
  ;; A VECTOR of widths of every column
  column-widths
  ;; A VECTOR of widths of every header
  header-widths
  ;; A VECTOR of  header titles. must be set before we attempt to draw.
  (header-titles nil)
  ;; the full line format string
  display-full-line-format
  ;; A VECTOR of format strings for every column
  display-column-formats
  ;; A VECTOR of offsets of every column
  display-column-offsets
  ;; the header-line
  display-header-line)

(cl-defstruct explain-pause-top--table-display-entry
  begin-mark
  item-ptr
  prev-state
  total-length
  ;; A VECTOR of cached strings
  cached-strings
  ;; A VECTOR of cached string lengths
  cached-string-lengths)

(defun explain-pause-top--table-set-sorter (table new-sort &optional fast-flip)
  "Change the sort function. Does not re-render.

If fast-flip is set, simply reverse the entries. The new sort function
must actually implement the reversed order, it (and sort) are just not
called."
  ;; skip over the head
  (let* ((entry-ptrs (cdr (explain-pause-top--table-entries table)))
         (sorted-ptrs (if fast-flip
                          (reverse entry-ptrs)
                        (sort entry-ptrs
                              ;; the sort we do is flipped
                              (lambda (lhs rhs)
                                (not (funcall new-sort lhs rhs)))))))

    (setf (explain-pause-top--table-entries table)
          (cons nil sorted-ptrs))
    (setf (explain-pause-top--table-sorter table)
          new-sort)))

(defun explain-pause-top--table-refresh (table)
  "Refresh the table of items in the current buffer when requested. Note that
the width cannot be 0."
  ;; first, calculate the widths of all the columns.
  ;; To do this, now walk through all the entries, updating their current
  ;; items as needed, and ask them to prepare to draw.
  ;; after, insert new, un-base-marked entries to take care of any new
  ;; items.
  ;; walk both the display-order and the display-entries
  (let* ((display-order-ptr (cdr (explain-pause-top--table-entries table)))
         (display-entries-prev (explain-pause-top--table-display-entries table))
         (display-entries-ptr (cdr display-entries-prev))
         (display-column-widths (explain-pause-top--table-column-widths table))
         (column-count (length display-column-widths))
         (requested-widths (copy-sequence
                            (explain-pause-top--table-header-widths table)))
         (layout-changed nil))

    (while (and display-order-ptr
                display-entries-ptr)
      (let* ((current-entry (car display-entries-ptr))
             (to-draw-item (car display-order-ptr)))

        (setf (explain-pause-top--table-display-entry-item-ptr current-entry)
              to-draw-item)

        (explain-pause-top--table-prepare-draw current-entry requested-widths))

      (setq display-order-ptr (cdr display-order-ptr))
      (setq display-entries-prev display-entries-ptr)
      (setq display-entries-ptr (cdr display-entries-ptr)))

    ;; ok, now reconcile & add new items
    ;; prev points to the end now
    (while display-order-ptr
      (let* ((new-entry (make-explain-pause-top--table-display-entry
                         :begin-mark nil
                         :item-ptr (car display-order-ptr)
                         :prev-state nil
                         :total-length nil
                         :cached-strings (make-vector column-count nil)
                         :cached-string-lengths (make-vector column-count nil)))
             (new-list-entry (list new-entry)))

        (explain-pause-top--table-prepare-draw new-entry requested-widths)

        ;; insert at the tail
        (setcdr display-entries-prev new-list-entry)
        (setq display-entries-prev new-list-entry)
        (setq display-order-ptr (cdr display-order-ptr))))

    ;; at this point, the following invariants hold:
    ;; * every entry has a display-entry (but not all of them have begin-marks)
    ;; * columns holds the largest requested width.
    ;; * anything that we don't need anymore is starting at display-entries-ptr
    ;; check to see if the fixed columns have changed width, OR if our width
    ;; changed. If so, we'll set prev-state for every entry as we paint to nil,
    ;; to force a full refresh:
    ;; (TODO could we only paint things "after" the first change?)
    (when (or
           (cl-mismatch display-column-widths
                        requested-widths
                        :start1 1
                        :start2 1
                        :test 'eq)
           (explain-pause-top--table-needs-resize table))

      ;; if they are not equal, update the header, format strings, etc.
      (explain-pause-top--table-resize-columns
       table
       ;; convert to a list as resize-columns expects a list of fixed widths
       (cdr (append requested-widths nil)))

      (let ((header (explain-pause-top--table-display-header-line table)))
        (setq header-line-format
              `(:eval (explain-pause-top--generate-header-line
                       ,header
                       ,(length header)
                       (window-hscroll)
                       (- (window-total-width) 1)))))

      (force-mode-line-update)

      (setf (explain-pause-top--table-needs-resize table) nil)
      (setq layout-changed t))

    ;; now, we are prepared to draw:
    (let ((display-draw-ptr
           (cdr (explain-pause-top--table-display-entries table))))
      (while display-draw-ptr
        (let ((current-entry (car display-draw-ptr)))
          (when layout-changed
            (setf (explain-pause-top--table-display-entry-prev-state current-entry)
                  nil))

          (explain-pause-top--table-draw table current-entry))

        (setq display-draw-ptr (cdr display-draw-ptr))))

    ;; move to the beginning of the "no longer needed entries",
    ;; wipe, and clear:
    (when display-entries-ptr
      (let ((mark (explain-pause-top--table-display-entry-begin-mark
                     (car display-entries-ptr))))
        (delete-region mark (point-max))
        (setcdr display-entries-prev nil)))))

(defun explain-pause-top--table-find-and-insert (table item)
  "insert item into the entries, sorted by the current sort function. If the
item is found by the time insertion happens, return the prev item (whose cdr
points to the item). If it is not found, return the newly added item.
Comparison of items is by `eq'. If the new item would have been inserted at
the exact same place as the existing item, no insertion occurs, and nil is
returned."
  (let* ((ptr-entry nil) ;; don't allocate it unless we absolutely need it
         (display-order-prev (explain-pause-top--table-entries table))
         (display-order-ptr (cdr display-order-prev))
         (sort-function (explain-pause-top--table-sorter table))
         (saved-dup-item-entry nil))

    ;; insert and search the list at the same time
    (catch 'inserted
      (while display-order-ptr
        (let ((compare-item (car display-order-ptr)))
          ;; it is very common we only update a value without changing
          ;; the order of the list. check for that case here, so we
          ;; don't create objects just to throw them away in the update
          ;; function
          (if (eq compare-item item)
              ;; exactly equal; we've found the previous entry
              ;; would we have inserted the new item here?
              (let ((next-item (cdr display-order-ptr)))
                ;; if there is no next, then we are at the end anyway,
                ;; and certainly we would replace ourselves
                (when (or (not next-item)
                           (funcall sort-function (car next-item) item))
                  ;; yes: get outta here
                  (throw 'inserted nil))
                ;; otherwise, skip it
                (setq saved-dup-item-entry display-order-prev))
                ;; not equal - actual compare:
            (when (funcall sort-function compare-item item)
              ;; we can insert
              (setq ptr-entry (list item))
              (setcdr display-order-prev ptr-entry)
              (setcdr ptr-entry display-order-ptr)
              ;; finish early
              (throw 'inserted nil)))

          (setq display-order-prev display-order-ptr)
          (setq display-order-ptr (cdr display-order-ptr))))

      ;; at the end, and we didn't insert
      (setq ptr-entry (list item))
      (setcdr display-order-prev ptr-entry))

    (or saved-dup-item-entry
        ptr-entry)))

(defun explain-pause-top--table-insert (table item)
  "Insert an item into the entries. It will be inserted at the correct place
with the current sort function."
  (explain-pause-top--table-find-and-insert table item))

(defun explain-pause-top--table-update (table item)
  "Update an item in the entries. It will be moved to the correct place
with the current sort function. It is expected that the item is `eq' to
an already existing item in the entries."
  (let* ((prev
          (explain-pause-top--table-find-and-insert table item))
         (ptr (cdr prev)))
    ;; if prev is nil, we don't need to do anything at all.
    (when prev
      ;; otherwise, we have to clean up the old entry:
      (when (eq (car prev) item)
        ;; it was not found, and the entry returned is the newly inserted
        ;; continue searching for the old entry:
        (catch 'found
          (while ptr
            (when (eq (car ptr) item)
              ;; prev now points to us
              (throw 'found nil))
            (setq prev ptr)
            (setq ptr (cdr ptr)))))

      ;; ok, splice the old one out
      (setcdr prev (cdr ptr)))))

(defun explain-pause-top--table-clear (table)
  "Clear all items in the table"
  (setf (explain-pause-top--table-entries table) (list nil)))

(defun explain-pause-top--table-set-headers (table headers)
  "Initialize the headers for TABLE. Must be run in the buffer it is expected
to draw in, because it also initializes the header widths."
  (setf (explain-pause-top--table-header-titles table) headers)
  (setf (explain-pause-top--table-header-widths table)
        (cl-map 'vector #'string-width headers)))

(defun explain-pause-top--table-generate-offsets (fill-width widths)
  "Return a vector of offsets for FILL-WIDTH and then all the columns in list WIDTHS.
Columns in WIDTHS get one character padding in between each."
  (cl-loop
   for width in widths
   with accum = fill-width
   collect accum into offsets
   do (setq accum (+ accum 1 width))
   finally return (apply 'vector 0 offsets)))

(defun explain-pause-top--table-resize-columns (table fixed-widths)
  "Resize the columns within a table to new fixed widths given. Does NOT need to
be run within the current buffer, as it never runs `string-width'."
  (let*
      ((width (explain-pause-top--table-width table))
       (header-titles (explain-pause-top--table-header-titles table))
       (total-fixed (+ (apply #'+ fixed-widths)
                       ;; one space between every fixed column
                       (- (length fixed-widths) 1)))
       ;; the beginning of the fixed base, aka the width of the fill column
       (fill-width (- width total-fixed))
       (final-widths
        (apply 'vector fill-width fixed-widths))
       (column-offsets
        (explain-pause-top--table-generate-offsets
         fill-width fixed-widths))
       ;; now generate the fill format string; it's left justified:
       (fill-format-string
        (format "%%-%d.%ds" fill-width fill-width))
       ;; and the fixed format strings:
       (fixed-format-string-list
        (mapcar
         (lambda (width)
           ;; ask for the column to be padded to be right
           ;; justified, but also to limit the total characters
           ;; to the same width.
           (format "%%%d.%ds" width width))
         fixed-widths))
       ;; now generate the vector of format strings for every column
       (format-string-list
        (apply 'vector fill-format-string fixed-format-string-list))
       ;; now generate the full format line for use when inserting a full row
       ;; (and header line)
       (full-format-string
        (concat fill-format-string
                (mapconcat #'identity fixed-format-string-list " ")))
       ;; now generate the header line:
       (header-line
        (apply 'format full-format-string
               (append header-titles nil))))

    (setf (explain-pause-top--table-display-header-line table) header-line)

    (setf (explain-pause-top--table-display-full-line-format table)
          full-format-string)

    (setf (explain-pause-top--table-column-widths table)
          final-widths)

    (setf (explain-pause-top--table-display-column-offsets table)
          column-offsets)

    (setf (explain-pause-top--table-display-column-formats table)
          format-string-list)))

(defun explain-pause-top--table-resize-width (table width)
  "Resize the table by updating the width and setting the dirty width
flag. Does not draw, nor recalculate any widths."
  (setf (explain-pause-top--table-width table) width)
  (setf (explain-pause-top--table-needs-resize table) t))

(defun explain-pause-top--generate-header-line
    (header header-length window-scroll window-width)
  "Generate a truncated header line. The header scrolls with the text, and
adds '$' when there is more header either front or end."
  ;; TODO this should probably use the actual continuation glyph?
  ;; TODO these really need to be test cases:

  ;; text      |-------|

  ;; window        |--------|
  ;; window |-------|
  ;; window |--------------|
  ;; window      |---|
  ;; window               |-------|
  ;; window -|

  ;; first, calculate the window we "should" watch over:
  (let* ((start window-scroll)
         (end (+ start window-width))

         ;; next, if the window is outside the bounds, adjust the bounds to match
         (bounded-start (min (max 0 start) header-length))
         (bounded-end (min (max 0 end) header-length))

         ;; next, calculate if we need dots:
         (head-dots (> bounded-start 0))
         (end-dots (< bounded-end header-length))

         ;; the dot strings
         (head-dot-str (when head-dots "$"))
         (end-dot-str (when end-dots "$"))

         ;; the head padding, which only applies if we've negatively scrolled
         (head-padding (when (< start 0)
                         (make-string (- start) ? )))

         ;; deal with left margins fringes and so on. actually this is a constant
         ;; TODO: could we cache it?
         (margin-padding (propertize
                          " "
                          'display (cons 'space (list :align-to 0)))))

    (when head-dots
      (if (< bounded-start header-length)
          ;; we need dots at the front and we can move forward.
          (setq bounded-start (1+ bounded-start))
        ;; if the move forward would have moved the bounded start
        ;; beyond the string, set start and end to 0 and clear the
        ;; end dot str:
        (setq bounded-start 0)
        (setq bounded-end 0)
        (setq end-dot-str nil)))

    (when end-dots
      (let ((new-end (- bounded-end 1)))
        (if (>= new-end bounded-start)
            ;; if we can go backwards, all's ok
            (setq bounded-end new-end)
          ;; if not, this means bounded-start == bounded-end,
          ;; and we don't have space to insert a $. do nothing
          (setq end-dot-str nil))))

    (concat margin-padding
            head-padding
            head-dot-str
            (substring header bounded-start bounded-end)
            end-dot-str)))

(cl-defstruct explain-pause-top--command-entry
  (command-set
   nil
   :header-name "Command"
   :to-string explain-pause--command-set-as-string
   :sorter explain-pause-top---command-entry-command-set-sorter)
  (count
   0
   :header-name "calls"
   :to-string number-to-string
   :sorter explain-pause-top--command-entry-number-sorter)
  (slow-count
   0
   :header-name "slow"
   :to-string number-to-string
   :sorter explain-pause-top--command-entry-number-sorter)
  (avg-ms
   0
   :header-name "avg ms"
   :to-string explain-pause--float-2-fixed
   :sorter explain-pause-top--command-entry-number-sorter)
  (total-ms
   0
   :header-name "ms"
   :to-string number-to-string
   :sorter explain-pause-top--command-entry-number-sorter))

(defun explain-pause-top---command-entry-command-set-sorter (_)
  "Generate a special sort for command-sets that sorts alphabetically."
  ;; we know the fieldname, so ignore that.
  ;; to sort by command, we have to convert the objects into strings, which
  ;; is expensive. TODO we should build a string mapping...
  (lambda (lhs rhs)
    (catch 'finished
      (let ((lhs-ptr (explain-pause-top--command-entry-command-set lhs))
            (rhs-ptr (explain-pause-top--command-entry-command-set rhs)))
        (while (or lhs-ptr rhs-ptr)
          (let ((lhs-cmd (car lhs-ptr))
                (rhs-cmd (car rhs-ptr)))
            (when (not (eq lhs-cmd rhs-cmd))
              (let ((string-lhs (if lhs-cmd
                                    (explain-pause--command-as-string lhs-cmd)
                                  ""))
                    (string-rhs (if rhs-cmd
                                    (explain-pause--command-as-string rhs-cmd)
                                  "")))
              (throw 'finished (string-greaterp string-lhs string-rhs)))))
          (setq lhs-ptr (cdr lhs-ptr))
          (setq rhs-ptr (cdr rhs-ptr)))
        ;; both must be identical
        nil))))

(defun explain-pause-top--command-entry-number-sorter (field-name)
  "Generate a sorter for numbers for the given FIELD-NAME"
  (lambda (lhs rhs)
    (<
     (cl-struct-slot-value
      'explain-pause-top--command-entry
      field-name
      lhs)
     (cl-struct-slot-value
      'explain-pause-top--command-entry
      field-name
      rhs))))

(let ((meta (cl-struct-slot-info 'explain-pause-top--command-entry))
      (order  [command-set slow-count avg-ms total-ms count]))
  (defun explain-pause-top--command-entry-meta-map (func)
    "Map over the meta fields of the columns"
    (cl-map 'vector (lambda (field-name)
                      (funcall func field-name (assq field-name meta)))
            order))

  (defconst explain-pause-top--command-entry-headers
    (explain-pause-top--command-entry-meta-map
     (lambda (field-name opts)
       (plist-get opts :header-name)))
    "The header strings of a `explain-pause-top' table")

  (defconst explain-pause-top--command-entry-converters
    (explain-pause-top--command-entry-meta-map
     (lambda (field-name opts)
       (plist-get opts :to-string)))
    "The converter functions for each column of a `explain-pause-top'
table")

  (defconst explain-pause-top--command-entry-fields
    (explain-pause-top--command-entry-meta-map
     (lambda (field-name opts) field-name))
    "The field name for each column of a `explain-pause-top' table")

  (defconst explain-pause-top--command-entry-sorters
    (explain-pause-top--command-entry-meta-map
     (lambda (field-name opts)
       (let* ((sorter (plist-get opts :sorter))
              (sort-function (funcall sorter field-name)))
         (list
          sort-function
          (lambda (lhs rhs)
            (funcall sort-function rhs lhs))))))
    "The sorter functions for each column of a `explain-pause-top' table"))

(defun explain-pause-top--table-item-command-overflow
    (table column-widths command-string)
  "Return the truncated string for command in first row, and strings for
further lines, if needed."
  ;; This really is not very nice, breaking multiple abstraction
  ;; layers, but I'm really not convinced yet I want to properly
  ;; genericize this table code
  (let ((command-column-width (aref column-widths 0)))
    (if (< (length command-string)
           command-column-width)
        ;; it fits. return a polymorphic type because I don't want to
        ;; make lists all the time.
        command-string
      ;; ok, truncate and split:
      (let* ((table-width (explain-pause-top--table-width table))
             (index (- command-column-width 1))
             (first-line (concat (substring command-string 0 index) "\\"))
             (rest-parts (seq-partition (substring command-string index)
                                        (- table-width 3))) ;; 2 spaces + slash
             ;; TODO probably should do this via proper ident systems...
             (rest-lines (concat "\n  " (mapconcat #'identity rest-parts "\\\n  "))))
        (cons first-line rest-lines)))))

(defun explain-pause-top--table-draw (table item)
  "Redraw an item within it's bounds. If the item has a begin-mark, we exist.
If not, we're new. Move to EOB, set begin-mark. If prev-state exists, we
should update columns. If it not set, draw the entire line at once."
  (let* ((begin-mark (explain-pause-top--table-display-entry-begin-mark item))
         (item-ptr (explain-pause-top--table-display-entry-item-ptr item))
         (prev-state (explain-pause-top--table-display-entry-prev-state item))
         (cached-strings
          (explain-pause-top--table-display-entry-cached-strings item))
         (total-prev-length
          (explain-pause-top--table-display-entry-total-length item))
         (column-widths
          (explain-pause-top--table-column-widths table)))

    (unless begin-mark
      (setq begin-mark (point-max-marker))
      (setf (explain-pause-top--table-display-entry-begin-mark item) begin-mark))

    (cond
     (prev-state
      (let ((format-strings
             (explain-pause-top--table-display-column-formats table))
            (column-offsets
             (explain-pause-top--table-display-column-offsets table)))

        (cl-loop
         for column-index from 0
         for column-field across explain-pause-top--command-entry-fields
         ;;TODO DRY with prepare-draw
         do (let* ((old-val (cl-struct-slot-value
                             'explain-pause-top--command-entry
                             column-field
                             prev-state))
                   (new-val (cl-struct-slot-value
                             'explain-pause-top--command-entry
                             column-field
                             item-ptr)))
              (unless (eq old-val new-val)
                (let ((cached-val (aref cached-strings column-index))
                      (format-str (aref format-strings column-index)))
                  (cond
                   ((eq column-field 'command-set)
                    ;; cmd, is special cased due to overflow logic. this could
                    ;; be cleaned up and abstracted away, but I'm not sure I
                    ;; want to bother yet
                    (let* ((command-lines
                            (explain-pause-top--table-item-command-overflow
                             table column-widths cached-val))
                           (first-line
                            (if (stringp command-lines)
                                command-lines
                              (car command-lines)))
                           (extra-lines
                            (unless (stringp command-lines)
                              (cdr command-lines)))
                           (printed-first-line (format format-str first-line))
                           (width (explain-pause-top--table-width table)))

                      ;; TODO hardcoded offset 0
                      (goto-char begin-mark)
                      (delete-char (length printed-first-line))
                      (insert printed-first-line)

                      ;; now deal with extra lines. total-prev-length must
                      ;; exist. if the total-prev-length is > width then we
                      ;; already had extra lines; delete them, insert ours, if
                      ;; it exists, and update total-prev-lines
                      (let ((prev-extra-length (- total-prev-length width)))
                        (goto-char (+ begin-mark width))
                        (when (> prev-extra-length 0)
                          (delete-char prev-extra-length))
                        (when extra-lines
                          (insert extra-lines))

                        (let ((new-total-length (+ width (length extra-lines))))
                          (unless (eq new-total-length total-prev-length)
                            (setf (explain-pause-top--table-display-entry-total-length item)
                                  new-total-length))))))
                   (t
                    ;; normal field. don't do these lookups unless we have to
                    (let* ((new-str (format format-str cached-val))
                           (offset (aref column-offsets column-index)))
                      (goto-char (+ begin-mark offset))
                      (delete-char (length new-str))
                      (insert new-str))))))))))
     (t
      ;; draw everything in one shot
      (let* ((full-format-string
              (explain-pause-top--table-display-full-line-format table))
             ;; TODO special command-str handling here
             (command-str (aref cached-strings 0))
             (command-lines (explain-pause-top--table-item-command-overflow
                             table column-widths command-str))
             (first-line
              (if (stringp command-lines) command-lines
                (car command-lines)))
             (extra-lines
              (unless (stringp command-lines) (cdr command-lines)))
             ;; Hm. feels slow.
             (final-string (concat
                            (apply 'format full-format-string first-line
                                   (cdr (append cached-strings nil)))
                            extra-lines)))
        ;; go to the beginning of our region
        (goto-char begin-mark)

        (when total-prev-length
          ;; we already existed, remove the old
          (delete-char total-prev-length))

        (insert final-string)

        (setf (explain-pause-top--table-display-entry-total-length item)
              (length final-string))

        (unless total-prev-length
          ;; we didn't exist, add the newline
          (insert "\n")))))

    ;; update the prev state. this assumes no one is mutating deeply
    (setf (explain-pause-top--table-display-entry-prev-state item)
          (copy-explain-pause-top--command-entry item-ptr))))

(defun explain-pause-top--table-prepare-draw (item requested-widths)
  "Prepare to draw an item by generating the converted strings from the values,
and update REQUESTED-WIDTHS with their widths."
  (let ((cached-strings
         (explain-pause-top--table-display-entry-cached-strings item))
        (cached-string-lengths
         (explain-pause-top--table-display-entry-cached-string-lengths item))
        (item-ptr (explain-pause-top--table-display-entry-item-ptr item))
        (prev-state (explain-pause-top--table-display-entry-prev-state item)))

    ;; command-set is safe, all inputs are always formatted in specifiers
    (cl-loop
     for column-index from 0
     for column-field across explain-pause-top--command-entry-fields
     for column-width across requested-widths
     do (let* ((old-val (when prev-state
                          (cl-struct-slot-value
                           'explain-pause-top--command-entry
                           column-field
                           prev-state)))
               (new-val (cl-struct-slot-value
                         'explain-pause-top--command-entry
                         column-field
                         item-ptr))
               (compare-width 0))

          (if (eq old-val new-val)
              (setq compare-width (aref cached-string-lengths column-index))
            ;; set and update
            (let* ((converter (aref explain-pause-top--command-entry-converters
                                    column-index))
                   (new-str (funcall converter new-val))
                   (new-string-width (string-width new-str)))
              (setf (aref cached-strings column-index) new-str)
              (setf (aref cached-string-lengths column-index) new-string-width)
              (setq compare-width new-string-width)))

          (when (> compare-width column-width)
            (setf (aref requested-widths column-index) compare-width))))))

;; explain-pause-top-mode
;; buffer-local variables that should be always private
(defvar-local explain-pause-top--buffer-refresh-timer nil
  "The timer for the buffer. It is nil if auto-refresh is off for that buffer.")

(defvar-local explain-pause-top--buffer-refresh-interval nil
  "The refresh interval for the buffer. It is nil if auto-refresh is off for
that buffer.")

(defvar-local explain-pause-top--buffer-window-size-changed nil
  "The lambda hook added to `window-size-change-functions' for the buffer
to watch for resizes.")

(defvar-local explain-pause-top--buffer-table nil
  "The table for the buffer")

(defvar-local explain-pause-top--sort-column nil
  "The column currently sorted in the table")

(defvar explain-pause-top-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'explain-pause-top-sort)
    (define-key map "c" 'explain-pause-top-clear)
    (define-key map "a" 'explain-pause-top-auto-refresh)
    map)
  "Keymap for `explain-pause-top' major mode")

;; `explain-pause-top' major mode
(define-derived-mode explain-pause-top-mode special-mode
  "Explain Pause Top"
  "Major mode for listing the statistics generated by explain-pause for recently
run commands in emacs. The mode resizes the table inside the buffer to always be
the width of the largest window viewing the buffer. Reverting the buffer will
refresh the table. The buffer initially starts with the auto refresh interval
given in `explain-pause-top-auto-refresh-interval'. You can modify this interval
on a per buffer basis by calling `explain-pause-top-auto-refresh'."
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)

  (setq-local revert-buffer-function #'explain-pause-top--buffer-revert)

  (setq-local explain-pause-top--buffer-table
              (make-explain-pause-top--table))

  (explain-pause-top--table-set-headers
   explain-pause-top--buffer-table
   explain-pause-top--command-entry-headers)

  ;; default sort: slow count
  (setq-local explain-pause-top--sort-column 1)
  (explain-pause-top--table-set-sorter
   explain-pause-top--buffer-table
   (car (aref explain-pause-top--command-entry-sorters 1)))

  (let ((this-buffer (current-buffer)))
    (when explain-pause-top--buffer-window-size-changed
      (remove-hook 'window-size-change-functions
                   explain-pause-top--buffer-window-size-changed))

    (setq-local explain-pause-top--buffer-window-size-changed
                (lambda (_)
                  ;; ignore frame, and recalculate the width across all frames
                  ;; every time. we always need the biggest.
                  (explain-pause-top--buffer-update-width-from-windows
                   this-buffer)))

    (explain-pause-top--pipe-commands 'add this-buffer))

  (add-hook 'window-size-change-functions
              explain-pause-top--buffer-window-size-changed)

  (add-hook 'window-configuration-change-hook
            'explain-pause-top--buffer-window-config-changed nil t)

  (add-hook 'kill-buffer-hook 'explain-pause-top--buffer-killed nil t)

  ;; if the user changes major mode, act as if we are destroyed and clear
  ;; all timers, etc.
  (add-hook 'change-major-mode-hook 'explain-pause-top--buffer-killed nil t)

  (explain-pause-top-auto-refresh nil explain-pause-top-auto-refresh-interval)

  ;; immediately ask for a resize:
  (funcall explain-pause-top--buffer-window-size-changed nil))

(defun explain-pause-top--buffer-killed ()
  "Clean timers and hooks when the buffer is destroyed."
  (explain-pause-top-auto-refresh)
  (remove-hook 'window-size-change-functions
               explain-pause-top--buffer-window-size-changed)
  (explain-pause-top--pipe-commands 'remove (current-buffer)))

(defun explain-pause-top--buffer-revert (_ignoreauto _noconfirm)
  (explain-pause-top--buffer-refresh))

(defun explain-pause-top--buffer-resize (width)
  "resize the current table in the buffer to the WIDTH given, and then redraw it."
  (explain-pause-top--table-resize-width explain-pause-top--buffer-table width)
  (explain-pause-top--buffer-refresh))

(defun explain-pause-top--buffer-reschedule-timer ()
  "Reschedule the timer for this buffer if needed."
  (when explain-pause-top--buffer-refresh-interval
    ;; do not repeat any missed timers
    (let ((timer-max-repeats 0))
      (setq-local explain-pause-top--buffer-refresh-timer
                  (run-with-timer explain-pause-top--buffer-refresh-interval
                                  explain-pause-top--buffer-refresh-interval
                                  #'explain-pause-top--buffer-refresh-with-buffer
                                  (current-buffer))))))

(defun explain-pause-top--buffer-refresh-with-buffer (buffer)
  "Refresh the target BUFFER"
  (with-current-buffer buffer
    (explain-pause-top--buffer-refresh)))

(defun explain-pause-top--buffer-refresh ()
  "Refresh the current buffer - redraw the data at the current target-width"
  ;; It's possible a refresh timer ran before/after we calculated size, if so,
  ;; don't try to draw yet.
  (unless (eq (explain-pause-top--table-width explain-pause-top--buffer-table) 0)
    (let ((inhibit-read-only t))
      (save-excursion
        (explain-pause-top--table-refresh explain-pause-top--buffer-table)))))

(defun explain-pause-top--buffer-window-config-changed ()
  "Buffer-local hook run when window config changed for a window showing
this buffer. Call `explain-pause-top--buffer-update-width-from-windows'."
  (explain-pause-top--buffer-update-width-from-windows (current-buffer)))

(defun explain-pause-top--buffer-update-width-from-windows
    (buffer)
  "Update the width of the target BUFFER, with the widest width from all the
windows displaying it. Does not change buffer if width does not change."
  (let ((table (buffer-local-value 'explain-pause-top--buffer-table buffer))
        ;; include all frames, everywhere, but don't include minibuffer
        (windows (get-buffer-window-list buffer nil t)))
    (when windows
      ;; if there are no windows, don't bother changing the size. the buffer
      ;; still exists, just not been drawn. when it is shown again, sizes
      ;; will be recalculated.
      (let ((new-width
             (seq-reduce
              (lambda (accum window)
                (max accum
                     (window-max-chars-per-line window)))
              windows 0))
            (existing-width
             (explain-pause-top--table-width table)))
        ;; we can't just take the max, because the old largest window might have
        ;; been closed:
        (unless (eq new-width existing-width)
          (with-current-buffer buffer
            (explain-pause-top--buffer-resize new-width)))))))

(defun explain-pause-top--get-default-buffer ()
  "Get or recreate the buffer for displaying explain-pause-top"
  (let ((buffer (get-buffer "*explain-pause-top*")))
    (unless buffer
      (setq buffer (generate-new-buffer "*explain-pause-top*"))
      (with-current-buffer buffer
        (explain-pause-top-mode)))
    buffer))

(let ((piped-command-buffers nil)
      (command-statistics (make-hash-table
                            :test 'equal)))
  (defun explain-pause-top-clear (&optional refresh)
    "Clear the command statistics that `explain-pause-top' is tracking.

This data is shared amongst all `explain-pause-top' buffers, so this action will
clear their data even if the buffer is not auto-refreshing. Optionally, REFRESH
all buffers immediately (causes a switch to every buffer). In interactive mode,
this is true."
    (interactive (list t))
    (clrhash command-statistics)
    (dolist (buffer piped-command-buffers)
      (let ((table (buffer-local-value 'explain-pause-top--buffer-table buffer)))
        (explain-pause-top--table-clear table))
      (when refresh
        (explain-pause-top--buffer-refresh-with-buffer buffer))))

  (defun explain-pause-top--pipe-commands (action buffer)
    "Add or remove (ACTION) the buffer from the list of `explain-pause-top' major
mode buffers. If there are none, the hook is removed. If there is at least one,
the hook is added."
    (cond
     ((eq action 'add)
      (add-to-list 'piped-command-buffers buffer))
     ((eq action 'remove)
      (setq piped-command-buffers (delq buffer piped-command-buffers))))

    (if (eq (length piped-command-buffers) 0)
        (remove-hook 'explain-pause-measured-command-hook
                     #'explain-pause-top--consume-commands)
      (add-hook 'explain-pause-measured-command-hook
                #'explain-pause-top--consume-commands)))

  (defun explain-pause-top--consume-commands (ms read-ms command-set was-profiled)
    "Consume the event from the stream and add it into the shared store between
all `explain-pause-top' buffers."
    (let ((entry (gethash command-set command-statistics nil))
          (action 'explain-pause-top--table-update)
          (this-slow-count (if (> ms explain-pause-slow-too-long-ms) 1 0)))
      (if entry
          (let*
              ((old-count (explain-pause-top--command-entry-count entry))
               (old-ms (explain-pause-top--command-entry-total-ms entry))
               (slow-count (explain-pause-top--command-entry-slow-count entry))
               (new-count (1+ old-count))
               (new-slow-count (+ slow-count this-slow-count))
               (new-ms (+ ms old-ms))
               (new-avg (/ (float new-ms) (float new-count))))
            (setf (explain-pause-top--command-entry-count entry) new-count)
            (setf (explain-pause-top--command-entry-slow-count entry) new-slow-count)
            (setf (explain-pause-top--command-entry-total-ms entry) new-ms)
            (setf (explain-pause-top--command-entry-avg-ms entry) new-avg))
        (setq entry (make-explain-pause-top--command-entry
                     :command-set command-set
                     :count 1
                     :avg-ms ms
                     :total-ms ms
                     :slow-count this-slow-count))
        (puthash command-set entry command-statistics)
        (setq action 'explain-pause-top--table-insert))

      (dolist (buffer piped-command-buffers)
        (let ((table (buffer-local-value 'explain-pause-top--buffer-table buffer)))
          (funcall action table entry))))))

(defun explain-pause-top-sort (buffer column &optional refresh)
  "Sort top table in the BUFFER using COLUMN, which is the 0-based
index. Optionally, immediately refresh the buffer (causes a buffer switch). In
interactive mode, sort the current buffer's column under point, and refreshes
immediately. If the target buffer is not a `explain-pause-top' buffer, do
nothing. Sorting the same column inverts the order."
  (interactive
   (let* ((column-offsets (explain-pause-top--table-display-column-offsets
                           explain-pause-top--buffer-table))
          (next-bigger-index (seq-position column-offsets (current-column)
                                           #'>))
          (next-column (if next-bigger-index next-bigger-index
                           (length column-offsets))))
     (list (current-buffer) (- next-column 1) t)))
  (when (eq (buffer-local-value 'major-mode buffer) 'explain-pause-top-mode)
    (let ((current-sorted (buffer-local-value 'explain-pause-top--sort-column buffer))
          (table (buffer-local-value 'explain-pause-top--buffer-table buffer)))
      (if (eq current-sorted column)
          ;; flip ordering
          (let* ((current-sort-func
                  (explain-pause-top--table-sorter table))
                 (current-sorters
                  (aref explain-pause-top--command-entry-sorters current-sorted))
                 (next-sorter
                  (if (eq (car current-sorters) current-sort-func)
                      (cadr current-sorters)
                    (car current-sorters))))
            (explain-pause-top--table-set-sorter
             table
             next-sorter))
        ;; new sort column
        (setq-local explain-pause-top--sort-column column)
        (explain-pause-top--table-set-sorter
         table
         (car (aref explain-pause-top--command-entry-sorters column)))))
    (when refresh
      (explain-pause-top--buffer-refresh-with-buffer buffer))))

(defun explain-pause-top-auto-refresh (&optional buffer interval)
  "Turn on or off auto-refresh for the BUFFER, or the current buffer if
nil. Does nothing if the buffer's major mode is not `explain-pause-top'. If
INTERVAL is nil then auto-refresh is disabled, else the INTERVAL is seconds
between refreshes. In interactive mode, if the current buffer is not a
`explain-pause-top', and there is more then one buffer of that type, prompt the
user to pick which one."
  (interactive
   (let ((buffer t)
         (interval t))
     ;; if the current buffer is a explain-pause, just pick that.
     (if (eq major-mode 'explain-pause-top-mode)
         (setq buffer nil)
       ;; otherwise, make a list of top buffers
       (let ((mode-buffers
              (seq-filter
               (lambda (buffer)
                 (eq (buffer-local-value 'major-mode buffer) 'explain-pause-top-mode))
               (buffer-list))))
         ;; only one? pick it
         (if (eq (length mode-buffers) 1)
             (setq buffer (car mode-buffers))
           ;; else, ask user to select
           (while (eq buffer t)
             (let ((buffer-completions
                    (mapcar (lambda (buffer)
                              (cons (buffer-name buffer)
                                    buffer))
                            mode-buffers)))
               (setq buffer
                     (completing-read "Explain-Pause-Top buffer: " buffer-completions
                                      nil t nil nil t)))))))
     ;; pick interval
     (while (eq interval t)
       (let ((candidate
              (read-from-minibuffer
               "Refresh interval (secs) or empty to pause: "
               nil
               nil
               t
               nil
               "nil")))
         (cond
          ((numberp candidate)
           (setq interval candidate))
          ((eq candidate nil)
           (setq interval nil)))))
     (list buffer interval)))
  (unless buffer
    (setq buffer (current-buffer)))

  (with-current-buffer buffer
    (when (eq major-mode 'explain-pause-top-mode)
      (when explain-pause-top--buffer-refresh-timer
        (cancel-timer explain-pause-top--buffer-refresh-timer)
        (setq-local explain-pause-top--buffer-refresh-timer nil))

      (setq-local mode-name
                  (if interval
                      (format "Explain Pause Top (every %ss)" interval)
                    "Explain Pause Top (Paused)"))

      (force-mode-line-update)

      (when interval
        (setq-local explain-pause-top--buffer-refresh-interval interval)
        (explain-pause-top--buffer-reschedule-timer)))))

;; command loop hooks
(defun explain--excluded-command-p (command-set)
  "Should the COMMAND-SET be excluded from analysis?"
  ;;TODO support some defcustom here
  (equal '(suspend-frame) command-set))

(let
    ;; the current command context we are measuring
    ((executing-command nil)
     (before-command-snap nil)
     (before-buffer-list nil)
     (read-for-wait 0)
     ;; the command context that we were running when we entered each level of minibuffer
     (mini-buffer-enter-stack '())
     (profiling-command nil))

  (defun explain--command-loop-reset ()
    (setq executing-command nil)
    (setq mini-buffer-enter-stack '())
    (when profiling-command
      (profiler-cpu-stop)
      (setq profiling-command nil)))

  (defun explain--start-profiling ()
    (setq profiling-command t)
    (profiler-cpu-start explain-pause-profile-cpu-sampling-interval))

  (defun explain--save-and-stop-profiling ()
    "Stop profiling and save the profile data"
    ;; Note; it's a bug in the profile package that you can't call `profiler-cpu-profile'
    ;; after stopping, even though the documentation states that you can. Directly call
    ;; make-profile:
    (profiler-cpu-stop)
    (setq profiling-command nil)
    (profiler-make-profile
     :type 'cpu
     :timestamp (current-time)
     :log (profiler-cpu-log)))

  (defun explain--enter-command (commands)
    "Set the context so we can start a new measurement loop. Does not affect
 minibuffer context."
    (setq executing-command commands)
    (setq read-for-wait 0)
    (when (explain--profile-p commands)
      (explain--start-profiling))
    (setq before-command-snap (current-time)))

  (defun explain--exit-command (now-snap command-set)
    "Finish running a measurement loop."
    (let* ((diff (- (explain--as-ms-exact (time-subtract now-snap before-command-snap))
                    read-for-wait))
           (excluded (explain--excluded-command-p command-set))
           (too-long (and (> diff explain-pause-slow-too-long-ms)
                          (not excluded)))
           (was-profiled profiling-command))

      (when was-profiled
        (let ((profile (explain--save-and-stop-profiling)))
          ;; only save the profile if it was worth it
          (when too-long
            (explain--store-profile now-snap diff command-set profile))))

      (unless excluded
        (run-hook-with-args 'explain-pause-measured-command-hook
                            diff read-for-wait command-set
                            (and was-profiled too-long)))

      (when (or too-long
                explain-pause-log-all-input-loop)
        (explain--log-pause diff read-for-wait command-set t
                            (seq-difference (buffer-list) before-buffer-list #'eq))

        ;; only increment if it was actually too long, not if it was overriden
        (when too-long
          (explain--increment-profile command-set)))))

  (defun explain--pre-command-hook ()
    (setq before-buffer-list (buffer-list))
    (explain--enter-command (list real-this-command)))

  (defun explain--post-command-hook ()
    (when executing-command
      (explain--exit-command (current-time) executing-command)
      (setq executing-command nil)))

  (defun explain--enter-minibuffer ()
    (push executing-command mini-buffer-enter-stack))

  (defun explain--exit-minibuffer ()
    ;; at the moment this hook is run, we have finished the actual "minibuffer"
    ;; work, and whatever actually asked for this mini-buffer to get user input
    ;; will run.
    ;; 1. Treat everything up to now as belonging to the actual minibuffer
    ;; 2. Treat everything after as a consequence of the selection
    (let*
        ((now-snap (current-time))
         ;; unforuntately, at this point, this-command is not yet updated :'(
         ;; so there's no way to actually know what command is going to execute
         ;; instead, steal the actual content from the minibuffer so it's useful
         ;; special case when you quit the buffer as the minibuffer might contain
         ;; non-completed entries.
         (minibuffer-command (if (eq this-command 'minibuffer-keyboard-quit)
                                 "from quitting minibuffer"
                               (format "from mini-buffer (`%s`)"
                                       (explain-pause--sanitize-minibuffer
                                        (minibuffer-contents-no-properties)))))
         ;; pop off the command that started this minibuffer to begin with
         (exiting-minibuffer-command (pop mini-buffer-enter-stack))
         (prev-command-set (cons real-this-command exiting-minibuffer-command))
         (next-command-set (cons minibuffer-command exiting-minibuffer-command)))

      (explain--exit-command now-snap prev-command-set)
      (explain--enter-command next-command-set)))

  ;; it would be nice to wrap only the args, but we actually need to know exactly
  ;; how long we waited for...
  (defun explain--wrap-read-key-family (original-func &rest args)
    "Advise read key family functions and measure how long we actually sat for.
Increment the current read-for-time with this value."
    (let* ((before-snap (current-time))
           (return-value (apply original-func args))
           (diff (explain--as-ms-exact (time-subtract nil before-snap))))
      (setq read-for-wait (+ diff read-for-wait))
      return-value))

  (defun explain--generate-command-set (head)
    "Generate a new command-set based on the current executing command-set"
    (cons head executing-command))

  (defun explain--measure-function (measure-func args command-set diff-override)
    "Execute the function with the arguments, measuring it's time and logging
if necessary. diff-override is a SYMBOL representing whether to log even if the
diff is less then the threshold."
    ;; This function will still be called even if the mode is off if the callback
    ;; was wrapped when the mode was on. check and exit if so:
    (if (not explain-pause-mode)
        (apply measure-func args)
      ;; otherwise...
      ;; push the original (bound) execution context as the current execution context
      ;; around the run of the original callback.
      ;; if executing-command is already set, we're inside a command-loop, and someone
      ;; accept-process-output or sit-for'ed, which is why we need to save/restore
      (let ((original-execution-command executing-command)
            (was-profiled
             ;; only profile if we were not already profiling - we could be inside
             ;; a command already being profiled
             ;; TODO somehow alert the user of this case?
             (and (not profiling-command)
                  (explain--profile-p command-set))))
        (setq executing-command command-set)

        (when was-profiled
          (explain--start-profiling))

        (let ((before-snap (current-time)))
          (apply measure-func args)
          (let* ((now-snap (current-time))
                 (diff (explain--as-ms-exact (time-subtract now-snap before-snap)))
                 (too-long (> diff explain-pause-slow-too-long-ms)))

            (when was-profiled
              (let ((profile (explain--save-and-stop-profiling)))
                (when too-long
                  (explain--store-profile now-snap diff command-set profile))))

            (setq executing-command original-execution-command)

            (run-hook-with-args 'explain-pause-measured-command-hook
                                diff 0 command-set
                                (and was-profiled too-long))

            (when (or too-long
                      (symbol-value diff-override))
              (explain--log-pause diff 0 command-set nil nil)

              (when too-long
                (explain--increment-profile command-set)))))))))

;; timer or process io hooks
(defun explain--generate-wrapper (command-set original-callback)
  "Generate a wrapper for use in process wrappers.

COMMAND-SET should describe the execution context when this wrapper was
generated.  ORIGINAL-CALLBACK is the function to be wrapped."
  (let ((final-command-set (cons original-callback command-set)))
    (lambda (&rest callback-args)
      (explain--measure-function original-callback
                                 callback-args
                                 final-command-set
                                 'explain-pause-log-all-process-io))))

(defun explain--measure-idle-timer-callback (original-cb &rest args)
  "Wrap the callback of an idle timer ORIGINAL-CB, calling it with ARGS."
  (explain--measure-function original-cb args
                             (cons original-cb (explain--generate-command-set 'idle-timer))
                             'explain-pause-log-all-timers))

(defun explain--measure-timer-callback (original-cb &rest args)
  "Wrap the callback of a regular timer ORIGINAL-CB, calling it with ARGS."
  (explain--measure-function original-cb args
                             (cons original-cb (explain--generate-command-set 'timer))
                             'explain-pause-log-all-timers))

(defun explain--wrap-make-process-sentinel-filter-callback (args)
  "Wrap the sentinel and process arguments inside ARGS to `make-process', if any."
  (let* ((original-filter (plist-get args :filter))
         (original-sentinel (plist-get args :sentinel))
         (wrapped-filter
          (when original-filter
            (explain--generate-wrapper (explain--generate-command-set 'process-filter)
                                       original-filter)))
         (wrapped-sentinel
          (when original-sentinel
            (explain--generate-wrapper (explain--generate-command-set 'sentinel-filter)
                                       original-sentinel)))
         (new-args (copy-sequence args)))
    (when wrapped-filter
      (setq new-args (plist-put new-args :filter wrapped-filter)))
    (when original-sentinel
      (setq new-args (plist-put new-args :sentinel wrapped-sentinel)))
    new-args))

(defun explain--wrap-set-process-filter-callback (args)
  "Advise that modifies the arguments ARGS to `process-filter' by wrapping the callback."
  (seq-let [arg-process original-callback] args
    (list arg-process
          (explain--generate-wrapper (explain--generate-command-set 'process-filter) original-callback))))

(defun explain--wrap-set-process-sentinel-callback (args)
  "Advise that modifies the arguments ARGS to `process-sentinel' by wrapping the callback."
  (seq-let [arg-process original-callback] args
    (list arg-process
          (explain--generate-wrapper (explain--generate-command-set 'sentinel-filter) original-callback))))

(defun explain--wrap-idle-timer-callback (args)
  "Advise that modifies the arguments ARGS to `run-with-idle-timer' by wrapping the callback."
  (let ((original-callback (nth 2 args)))
    ;; TODO this can be removed once we get sit-for calculated for timers (#31)
    (if (eq original-callback #'explain-pause-mode--log-alert-developer-display)
        args
      (append (seq-take args 2)
              (cons #'explain--measure-idle-timer-callback
                    (seq-drop args 2))))))

(defun explain--wrap-timer-callback (args)
  "Advise that modifies the arguments ARGS to `run-with-timer' by wrapping the callback."
  (append (seq-take args 2)
          (cons #'explain--measure-timer-callback
                (seq-drop args 2))))

;;;###autoload
(define-minor-mode explain-pause-mode
  "Toggle whether to attempt to discover and explain pauses in emacs.

When enabled, explain-pause will attempt to time how long blocking activity
takes. If it measures blocking work that takes longer then a configurable
amount of time, explain-pause logs contextual information that can be used
to help diagnose and propose areas of elisp that might affect emacs
interactivity.

When blocking work takes too long many times, explain-mode profiles the
blocking work using the builtin Emacs profiler (`profiler' package). A fixed
number of these are saved.

This mode hooks the command cycle, both idle and regular timers, and process
filters and sentinels."
  :global t
  :init-value nil
  :lighter " explain-pause"
  :keymap nil
  (let
      ((hooks '((pre-command-hook . explain--pre-command-hook)
                (post-command-hook . explain--post-command-hook)
                (minibuffer-setup-hook . explain--enter-minibuffer)
                (minibuffer-exit-hook . explain--exit-minibuffer)))
       (advices '((run-with-idle-timer . explain--wrap-idle-timer-callback)
                  (run-with-timer . explain--wrap-timer-callback)
                  (set-process-filter . explain--wrap-set-process-filter-callback)
                  (set-process-sentinel . explain--wrap-set-process-sentinel-callback)))
       (read-key-family '(read-key-sequence read-key-sequence-vector read-char
                                            read-char-exclusive read-event)))
  (cond
   (explain-pause-mode
    (explain--command-loop-reset)
    (dolist (hook hooks)
      (add-hook (car hook) (cdr hook)))
    (dolist (advice advices)
      (advice-add (car advice) :filter-args (cdr advice)))
    (dolist (read-key-func read-key-family)
      (advice-add read-key-func :around #'explain--wrap-read-key-family))
    (dolist (func '(make-process make-pipe-process make-network-process))
      (advice-add func :filter-args #'explain--wrap-make-process-sentinel-filter-callback)))
   (t
    (dolist (func '(make-process make-pipe-process make-network-process))
      (advice-remove func #'explain--wrap-make-process-sentinel-filter-callback))
    (dolist (read-key-func read-key-family)
      (advice-remove read-key-func #'explain--wrap-read-key-family))
    (dolist (advice advices)
      (advice-remove (car advice) (cdr advice)))
    (dolist (hook hooks)
      (remove-hook (car hook) (cdr hook)))))))

;;;###autoload
(defun explain-pause-top ()
  "Show a top-like report of commands recently ran and their runtime. Returns
the buffer."
  (interactive)

  (let ((buffer (explain-pause-top--get-default-buffer)))
      ;; this will call resize and that will refresh as width is 0
      (display-buffer buffer)
      buffer))

(provide 'explain-pause-top)
(provide 'explain-pause-mode)

;;; explain-pause-mode.el ends here
