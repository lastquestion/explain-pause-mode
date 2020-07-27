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

(defconst explain-pause-version 0.1
  "Explain-pause version")

(require 'seq)
(require 'profiler)
(require 'subr-x)
(require 'nadvice)
(require 'cl-macs)

;; don't type check. note this only applies when (cl--compiling-file) returns t
;; - e.g. when it's bytecompiled.
(cl-declaim (optimize (safety 0) (speed 3)))

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

(defgroup explain-pause-top nil
  "Explain pause top major mode"
  :prefix "explain-pause-top-"
  :group 'explain-pause)

;; main behaviors

(defcustom explain-pause-slow-too-long-ms 40
  "How long must some activity take before explain-pause considers it slow, in ms?"
  :type 'integer
  :group 'explain-pause)

(defcustom explain-pause-top-auto-refresh-interval 2
  "How often `explain-pause-top' mode buffers refresh themselves by default,
in seconds. This can be a fraction of a second. If this is nil, they
do not automatically refresh. You can control this on a per buffer basis
by calling `explain-pause-top-auto-refresh'."
  :type '(choice (number :tag "Interval (seconds)")
                 (const :tag "Never" nil))
  :group 'explain-pause-top)

(defcustom explain-pause-top-click-profile-action #'switch-to-buffer-other-window
  "The function that is called when the user clicks on the profile button in
`explain-pause-top' buffers. The function is passed PROFILE-BUFFER, the buffer
which holds the generated profile output. You can customize this to change the
behavior if you wish. The default is to view the buffer using
`switch-to-buffer-other-window'."
  :type 'function
  :group 'explain-pause-top)

;; profiling behaviors
(defcustom explain-pause-profile-slow-threshold 3
  "Explain-pause will profile a slow activity once it has executed slowly this
many times."
  :type 'integer
  :group 'explain-pause-profiling)

(defcustom explain-pause-profile-cpu-sampling-interval 200000
  "The CPU sampling interval when the profiler is activated in microseconds.
The default value is 2ms."
  :type 'integer
  :group 'explain-pause-profiling)

(defcustom explain-pause-profile-saved-profiles 5
  "The number of CPU profiles to save for each command, after which the oldest
is removed. Changes to this number apply to new commands only. If you wish,
you may run `explain-pause-profile-clear' to clear all profiles, though
this will not clear statistics from individual `explain-top-mode' buffers."
  :type 'integer
  :group 'explain-pause-profiling)

(defcustom explain-pause-profile-enabled t
  "Should explain-pause profile slow activities at all?"
  :type 'boolean
  :group 'explain-pause-profiling)

;; public hooks
(defvar explain-pause-measured-command-hook nil
  "Functions(s) to call after a command has been measured. The functions are
called with an explain-pause-command-record argument.

These commands must be fast, because this hook is executed on every command,
not just slow commands. You cannot give up execution in these commands in
any way, e.g. do not call any family of functions that `sit-for', `read-key',
etc. etc.")

;; custom faces
(defface explain-pause-top-slow
  '((t (:foreground "red")))
  "The face used to highlight the slow count column when a command is slow
(e.g. > 1 hit)."
  :group 'explain-pause-top)

(defface explain-pause-top-profile-heading
  '((t (:inherit warning)))
  "The face used to highlight the profile heading for commands which have
profiles available to view."
  :group 'explain-pause-top)

(defface explain-pause-top-slow-heading
  '((t (:inherit warning)))
  "The face used to highlight the slow times heading for commands which have
slow times."
  :group 'explain-pause-top)

(defface explain-pause-top-changed
  '((t (:inherit bold)))
  "The face used to indicate that a value changed since the last refresh of the
buffer."
  :group 'explain-pause-top)

(defface explain-pause-top-active-column-header
  '((t (:inherit header-line-highlight)))
  "The face used to indicate the currently sorted column in the header line."
  :group 'explain-pause-top)

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

(defvar explain-pause-mode)

;; time lists are too expensive to create every single call
;; convert to a integer of ms.
(defsubst explain-pause--as-ms-exact (time)
  "Returns the TIME object in exact ms, ignoring picoseconds."
  (+ (* (+ (* (nth 0 time) 65536) (nth 1 time)) 1000)
     (/ (nth 2 time) 1000)))

;; TODO perhaps this should also display minor modes? probably. minor modes can be interact
;; weirdly and become slow.
;; TODO these aren't used right now
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

(defun explain-pause--command-as-string (cmd)
  "Generate a human readable string for a command CMD.

Normally this is a symbol, when we are in a command loop, but in timers, process
filters, etc. this might be a lambda or a bytecompiled lambda. In those cases,
also handle if the forms are wrapped by closure. For bytecompiled code, use the
references as the best information available. For lambdas and closures, hope
that the argument names are clarifying. Also subrp is allowed, as we can
generate native frames. We also allow strings for things that need special
representatinos. Note that in elisp, symbols may have %! So e.g. this function
may generate strings with format specifiers in them."
  (cond
   ((stringp cmd) cmd)
   ((symbolp cmd) (symbol-name cmd))
   ;; TODO is there nicer ways to get this?
   ((subrp cmd) (prin1-to-string cmd t))
   ((byte-code-function-p cmd)
    ;; "The vector of Lisp objects referenced by the byte code. These include
    ;; symbols used as function names and variable names."
    ;; list only symbol references:
    (format "<bytecode> (references: %s)"
            (seq-filter #'symbolp (aref cmd 2))))
   ((not (listp cmd))
    ;; something weird. This should not happen.
    (format "Unknown (please file a bug) %s" cmd))
   ;; closure. hypothetically, this is defined as a implementation detail,
   ;; but we'll read it anyway...
   ((eq (car cmd) 'closure)
    (format "<closure> (arg-list: %s)" (nth 2 cmd)))
   ;; lambda. directly read the arg-list:
   ((eq (car cmd) 'lambda)
    (format "<lambda> (arg-list: %s)" (nth 1 cmd)))
   (t
    (format "Unknown (please file a bug) %s" cmd))))

;; TODO not used right now...
(defun explain-pause--command-set-as-string (command-set)
  "Format a COMMAND-SET as a human readable string.

A command set is a list of commands that represent the context that lead to the
blocking execution (or we think so, anyway)."
  (mapconcat
   #'explain-pause--command-as-string
   command-set ", "))

;; the record of an command that we measured
;; theorywise, we are constructing a tree of records, all rooted at "emacs command
;; loop". Idealistically, we could maintain this tree and calculate the timings
;; by subtracting child times from our own. But because elisp actually executes
;; only one thing at a time, structure the graph as a stack and pause tracking
;; as we enter / exit by push/popping - we're traversing the graph as DFS
;; as we execute.
(cl-defstruct explain-pause-command-record
  ;; the command this tracked
  command
  ;; was this a native frame
  native
  ;; the parent
  parent

  ;; timing
  ;; the number of ms spent so far.
  (executing-time 0)
  ;; a TIME object as snap
  entry-snap
  ;; was this too slow
  too-slow

  ;; profiling:
  ;; was profiling was started FOR this command
  is-profiled
  ;; was profiling started when this command started
  under-profile
  ;; the profile if it was
  profile

  ;; depth of the callstack so far
  depth)

(defconst explain-pause-root-command-loop
  (make-explain-pause-command-record
   :command 'root-emacs
   :depth 0)
  "All command records that `explain-pause' tracks ultimately are rooted to this
command entry, which represents the top level command loop that begins in
`keyboard.c' when called from the initial `recursive_edit' from `emacs.c'.")

;; profiling and slow statistics functions
;; TODO :equal list command
(defvar explain-pause-profile--profile-statistics (make-hash-table)
  "A hash map of the slow commands and their statistics.

This data is always gathered and stored when `explain-pause-mode' is
active. When `explain-pause-profile-enabled' is true, profiling logs are also
stored. Each entry is a VECTOR of values. In an effort to optimize memory
allocations, store the slow counts inline with the rest of the object
instead of using a cl-struct with a field of a vector.")

(defconst explain-pause-profile--statistic-defaults
  [0   ;; profile-counter
   nil ;; should-profile-next
   0   ;; profile-attempts
   nil ;; list-of-profiles
   0   ;; slow-count
   nil];; slow-ms-idx
  "A constant vector of defaults used when upset to the statistics hashmap is
cnot required.")

(defconst explain-pause-profile--statistic-slow-count-offset
  6
  "The offset into the vector of statistic where the first slow ms is found.")

(defsubst explain-pause-profile--statistic-slow-length (statistic)
  "Return the number of slow counts available in this STATISTIC"
  (- (length statistic)
     explain-pause-profile--statistic-slow-count-offset))

(defsubst explain-pause-profile--statistic-profile-p (record)
  "Whether the command represented by RECORD should be profiled. Does not create
a new entry if the command has not been seen; in that case, returns nil."
  (aref (gethash (explain-pause-command-record-command record)
                 explain-pause-profile--profile-statistics
                 explain-pause-profile--statistic-defaults)
        1))

(defsubst explain-pause-profile--statistic-profiles (record)
  "Get the profiles for a command represented by RECORD."
  (aref (gethash (explain-pause-command-record-command record)
                 explain-pause-profile--profile-statistics
                 explain-pause-profile--statistic-defaults)
        3))

(defsubst explain-pause-profile--statistic-profile-attempts (record)
  "Get the attempts to profile for a command represented by RECORD."
  (aref (gethash (explain-pause-command-record-command record)
                 explain-pause-profile--profile-statistics
                 explain-pause-profile--statistic-defaults)
        2))

(defsubst explain-pause-profile--statistic-slow-index (record)
  "Get the current index of the circular list of slow times in RECORD."
  (aref (gethash (explain-pause-command-record-command record)
                 explain-pause-profile--profile-statistics
                 explain-pause-profile--statistic-defaults)
        5))

(defsubst explain-pause-profile--statistic-slow-count (record)
  "Get the current index of the circular list of slow times in RECORD."
  (aref (gethash (explain-pause-command-record-command record)
                 explain-pause-profile--profile-statistics
                 explain-pause-profile--statistic-defaults)
        4))

(defun explain-pause-profile-clear ()
  "Clear the profiling data. Note that this does not clear profiles already visible
in any `explain-pause-top' buffers."
  (interactive)
  (clrhash explain-pause-profile--profile-statistics))

(defun explain-pause-profiles-ignore-command (_command-set)
  "Ignore this command-set from ever being profiled."
  ;;TODO (interactive)
  t)

(defmacro explain-pause-profile--profile-get-statistic (record)
  ;; define this as a macro because a defsubst cannot inline before the owning
  ;; let has finished (e.g. this can't be inside the next closure and be used
  ;; in `explain-pause-profile--profile-measured-command'
  `(progn
     (setq command (explain-pause-command-record-command ,record))
     (setq statistic (gethash command explain-pause-profile--profile-statistics nil))

     (unless statistic
       (setq statistic (make-vector
                        (+ explain-pause-profile--statistic-slow-count-offset
                           explain-pause-profile-saved-profiles)
                        nil))
       (cl-loop
        for new-stat across-ref statistic
        for default-stat across explain-pause-profile--statistic-defaults
        do
        (setf new-stat default-stat))
       (puthash command statistic explain-pause-profile--profile-statistics))))

(eval-and-compile
  ;; for the mainline case, no profiles are stored but values are incremented
  ;; store these outside in a closure, so we don't need to create lets every call.
  (let ((profile nil)
        (statistic nil)
        (command nil)
        (slow-index nil))
    (defun explain-pause-profile--profile-measured-command (record)
      "Record the statistics for this command.

Always store the slowness. If profiling is on, store the profiling counts.
Store the profile if it was profiled."
      (unless (explain-pause-command-record-native record)
        (cond
         ;; did we try to profile but it was too fast? if this happens more
         ;; then threshold times, reset the counter back to 0
         ((and (explain-pause-command-record-is-profiled record)
               (not (explain-pause-command-record-too-slow record)))

          (explain-pause-profile--profile-get-statistic record)

          ;; reuse profile var for attempt counter
          (setq profile (aref statistic 2))
          (if (< profile explain-pause-profile-saved-profiles)
              (setf (aref statistic 2) (1+ profile))
            ;; give up TODO force?
            (setf (aref statistic 0) 0)
            (setf (aref statistic 1) nil)
            (setf (aref statistic 2) 0)))

         ((explain-pause-command-record-too-slow record)
          ;; otherwise, if we're too slow...
          (explain-pause-profile--profile-get-statistic record)
          (setq profile (explain-pause-command-record-profile record))

          ;; increment the slow count
          (setf (aref statistic 4) (1+ (aref statistic 4)))

          ;; save the ms into the circular list
          (setq slow-index (or (aref statistic 5) 0))
          (setf (aref statistic (+ slow-index
                                   explain-pause-profile--statistic-slow-count-offset))
                (explain-pause-command-record-executing-time record))
          ;; increment slow-ms-index to the next place
          (setf (aref statistic 5)
                (% (1+ slow-index)
                   ;; don't use `explain-pause-profile-saved-profiles' because the value
                   ;; might have changed
                   (explain-pause-profile--statistic-slow-length statistic)))

          (cond
           ;; add the profile if it exists.
           ;; we assume that profiles happen relatively rarely, so it's ok to use
           ;; a list so that 'eq comparisons work against head:
           (profile
            (let ((head (aref statistic 3))
                  (new-entry (vector
                              (explain-pause-command-record-executing-time record)
                              profile)))

              (setf (aref statistic 3)
                    (if (< (length head)
                           explain-pause-profile-saved-profiles)
                        (cons new-entry head)
                      ;; need to make a duplicate list
                      (cons new-entry
                            (seq-take head
                                      (- explain-pause-profile-saved-profiles 1))))))

            ;; reset for next time
            (setf (aref statistic 0) 0)
            (setf (aref statistic 1) nil))
           (t
            ;; reuse profile var for the counter here
            (setq profile (aref statistic 0))
            (when (>= profile 0) ;; only increment for "non-special" counts
              (setq profile (1+ profile))
              (setf (aref statistic 0) profile)
              (setf (aref statistic 1)
                    (>= profile explain-pause-profile-slow-threshold)))))))))))

;; table functions
;; I tried to use `tabulated-list' as well as `ewoc' but I decided to implement
;; something myself. This list/table implements some ideas that are from react/JS
;; like philosophies around optimizing drawing...
;; part of it is already abstracted out into something close to reusable, but
;; other parts are not yet.

(cl-defstruct explain-pause-top--table
  ;; the list of entries to display, in sorted order
  ;; (item prev-display-ptr)
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
  ;; the number of COLUMNS, for which each must have a HEADER.
  column-count
  ;; the number of fields. Fields after COLUMN-COUNT are printed as full lines.
  field-count
  ;; A VECTOR of widths of every column
  column-widths
  ;; A VECTOR of widths of every header
  header-widths
  ;; A VECTOR of header titles. must be set before we attempt to draw.
  (header-titles nil)
  ;; whether the header is dirty
  header-dirty
  ;; the full line format string
  display-full-line-format
  ;; A VECTOR of format strings for every column
  display-column-formats
  ;; A VECTOR of offsets of every column
  display-column-offsets
  ;; the index into the buffer vector representing which buffer we are rendering into
  buffer-index
  ;; the previous buffer index
  prev-buffer-index
  ;; the width of the buffer (1 + fields + columns)
  buffer-width
  ;; A scratch diff VECTOR so we don't have to reallocate every draw.
  current-diffs
  ;; A scratch diff VECTOR of requested widths for COLUMNS so we don't have to reallocate
  requested-widths)

(cl-defstruct explain-pause-top--table-display-entry
  ;; info about the entry in the emacs buffer
  begin-mark
  ;; the total display length of this item. begin-market + total-length => '\n'
  total-length

  ;; each entry holds a VECTOR of data, one set for each BUFFER
  ;; (not emacs buffers, double buffering)
  ;; [item-ptr string-vals (0-FIELD) string-lengths (O-COLUMN)]
  buffer

  ;; A VECTOR of the dirtiness of FIELDS (nil or t)
  dirty-fields)

(defun explain-pause-top--table-set-sorter (table new-sort &optional fast-flip)
  "Change the sort function. Does not re-render.

If fast-flip is set, simply reverse the entries. The new sort function
must actually implement the reversed order, it (and sort) are just not
called."
  ;; note that we do not need to copy or move around prev-display-ptr as
  ;; no item is added or removed.
  ;; skip over the head
  (let* ((entry-ptrs (cdr (explain-pause-top--table-entries table)))
         (sorted-ptrs (if fast-flip
                          (reverse entry-ptrs)
                        (sort entry-ptrs
                              ;; the sort we do is flipped
                              (lambda (lhs rhs)
                                (not (funcall new-sort
                                              (car lhs)
                                              (car rhs))))))))

    (setf (explain-pause-top--table-entries table)
          (cons nil sorted-ptrs))
    (setf (explain-pause-top--table-sorter table)
          new-sort)))

(defun explain-pause-top--table-find-and-insert (table item)
  "insert ITEM into the entries, sorted by the current sort function. If the
item is found by the time insertion happens, return the prev item (whose cdr
points to the item). If it is not found, return the newly added item.
Comparison of items is by `eq'. If the new item would have been inserted at the
exact same place as the existing item, no insertion occurs, and nil is
returned."
  (let* ((ptr-entry nil) ;; don't allocate it unless we absolutely need it
         (display-order-prev (explain-pause-top--table-entries table))
         (display-order-ptr (cdr display-order-prev))
         (sort-function (explain-pause-top--table-sorter table))
         (saved-dup-item-entry nil)
         (saved-prev-item nil))

    ;; insert and search the list at the same time
    (catch 'inserted
      (while display-order-ptr
        (let ((compare-item (caar display-order-ptr)))
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
                          (funcall sort-function (caar next-item) item))
                  ;; yes: get outta here.
                  (throw 'inserted nil))
                ;; otherwise, record where it is, and skip past it
                (setq saved-prev-item (cdar display-order-ptr))
                (setq saved-dup-item-entry display-order-prev))
                ;; not equal - actual compare:
            (when (funcall sort-function compare-item item)
              ;; we can insert.
              ;; did we find the item already? if so, copy the prev-ptr, as well
              (setq ptr-entry (cons
                               (cons item saved-prev-item)
                               display-order-ptr))
              (setcdr display-order-prev ptr-entry)
              ;; finish early
              (throw 'inserted nil)))

          (setq display-order-prev display-order-ptr)
          (setq display-order-ptr (cdr display-order-ptr))))

      ;; at the end, and we didn't insert
      (setq ptr-entry (cons
                       (cons item saved-prev-item)
                       nil))

      (setcdr display-order-prev ptr-entry))

    (or saved-dup-item-entry
        ptr-entry)))

(defun explain-pause-top--table-insert (table item)
  "Insert an item into the entries. It will be inserted at the correct place
with the current sort function.  It is expected that an item is ever only
inserted once."
  (explain-pause-top--table-find-and-insert table item))

(defun explain-pause-top--table-update (table item)
  "Update an item in the entries. It will be moved to the correct place
with the current sort function. It is more efficient to call
`explain-pause-top--table-insert' if you know the entry is not in the
table yet, but this will succeed even if this is not true."
  (let* ((prev
          (explain-pause-top--table-find-and-insert table item))
         (ptr (cdr prev)))
    ;; if prev is nil, we don't need to do anything at all;
    ;; it means that the place in the list did not change.
    (when prev
      ;; otherwise, we have to clean up the old entry:
      (when (eq (caar prev) item)
        ;; if the returned item is the item we just inserted, it means
        ;; that insert did not find the old item. keep on searching for it:
        (let ((new-item prev))
          (catch 'found
            (while ptr
              (when (eq (caar ptr) item)
                ;; prev now points to the old item to delete.
                ;; copy the prev-ptr to the new-item
                (setcdr (car new-item) (cdar ptr))
                (throw 'found nil))
              (setq prev ptr)
              (setq ptr (cdr ptr))))))

      ;; ok, splice the old one out
      (setcdr prev (cdr ptr)))))

(defun explain-pause-top--table-clear (table)
  "Clear all items in the table"
  ;; TODO delete all the other entries
  (setf (explain-pause-top--table-entries table) (list nil)))

(defun explain-pause-top--table-initialize
    (table headers field-count)
  "Initialize headers, field infformation, and scratch buffers for TABLE. Must
be run in the buffer it is expected to draw in, because it also initializes
header widths."
  (let* ((column-count (length headers))
         (buffer-width (+ 1 field-count column-count)))
    ;; field and column sizes
    (setf (explain-pause-top--table-column-count table) column-count)
    (setf (explain-pause-top--table-field-count table) field-count)
    (setf (explain-pause-top--table-buffer-width table) buffer-width)

    ;; scratch objects
    (setf (explain-pause-top--table-requested-widths table)
          (make-vector column-count nil))
    (setf (explain-pause-top--table-current-diffs table)
          (make-vector field-count nil))

    (setf (explain-pause-top--table-buffer-index table) 0)
    (setf (explain-pause-top--table-prev-buffer-index table) buffer-width)

    ;; header info
    (setf (explain-pause-top--table-header-dirty table) t)
    (setf (explain-pause-top--table-header-titles table) headers)
    (setf (explain-pause-top--table-header-widths table)
          (cl-map 'vector #'string-width headers))))

(defun explain-pause-top--table-set-header (table idx header)
  "Set one header to a new value. Must be run in the buffer it is expected to
draw in, as it needs to calculate the width."
  (setf (aref (explain-pause-top--table-header-titles table) idx) header)
  (setf (aref (explain-pause-top--table-header-widths table) idx)
        (string-width header))
  (setf (explain-pause-top--table-header-dirty table) t))

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
                (mapconcat #'identity fixed-format-string-list " "))))

    (setf (explain-pause-top--table-display-full-line-format table)
          full-format-string)

    (setf (explain-pause-top--table-column-widths table)
          final-widths)

    (setf (explain-pause-top--table-display-column-offsets table)
          column-offsets)

    (setf (explain-pause-top--table-display-column-formats table)
          format-string-list)

    (setf (explain-pause-top--table-header-dirty table) t)))

(defun explain-pause-top--table-resize-width (table width)
  "Resize the table by updating the width and setting the dirty width
flag. Does not draw, nor recalculate any widths."
  (setf (explain-pause-top--table-width table) width)
  (setf (explain-pause-top--table-needs-resize table) t))

(defconst explain-pause-top--header-left-alignment
  (propertize " " 'display (cons 'space (list :align-to 0)))
  ;; this is how we deal with left margins fringes and so on, as those are
  ;; pixel sized, so we can't print spaces.
  "The display property to left align the header to the beginning of the body")

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
                         (make-string (- start) ? ))))
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

    (concat explain-pause-top--header-left-alignment
            head-padding
            head-dot-str
            (substring header bounded-start bounded-end)
            end-dot-str)))

(defun explain-pause-top--concat-to-width (strings width separator)
  "Concat STRINGS together with a space until WIDTH is reached, and then insert
SEPARATOR. The width of separator counts towards the next group, not the prior
one. At least one item will always fit in each group, even if the item is wider
then WIDTH."
  (let* ((group-length 0)
         (reversed-final nil)
         (item-ptr strings))

    (while item-ptr
      (let* ((item (car item-ptr))
             (this-length (length item))
             ;; +1 for the space if it's not first item
             (try-length (+ group-length this-length
                            (if (eq group-length 0) 0 1))))
        (if (<= try-length width)
            (progn
              (setq reversed-final
                    (cons item
                          (if (eq group-length 0)
                              ;; first item
                              reversed-final
                            (cons " " reversed-final))))
              (setq group-length try-length)
              (setq item-ptr (cdr item-ptr)))
          ;; break
          (if (> group-length 0)
              ;; at least one item has been pushed;
              (setq reversed-final
                    (cons separator reversed-final))
            ;; only this item; push anyway
            (setq reversed-final
                  (cons separator
                        (cons item reversed-final)))
            (setq item-ptr (cdr item-ptr)))

          ;; clear group length for next round
          (setq group-length 0))))

    (apply 'concat (reverse reversed-final))))

(defun explain-pause-top--split-at-space (string max-lengths)
  "Split a string at max-lengths or less, if possible, at a space boundary. If
not possible, split at (car MAX-LENGTH) - 1 and add a \\ continuation. Use up
MAX-LENGTHS until only one remains, which becomes the final max-length for
the rest of the lines."
  (let* ((splits (split-string string " +" t))
         (current-line-length 0)
         (current-line nil)
         (results nil))
    (while splits
      (let* ((this-split (car splits))
             (this-length (length this-split))
             (try (+ current-line-length this-length (length current-line)))
             (this-max-length (car max-lengths)))
        (if (<= try this-max-length)
            ;; fits
            (progn
              (push this-split current-line)
              (setq splits (cdr splits))
              (setq current-line-length (+ current-line-length this-length)))
          ;; doesn't fit
          (if current-line
              ;; some stuff filled, start a new line and try again
              (push current-line results)
            ;; cut the string up
            (let* ((split-point (- this-max-length 1))
                   (first-half (substring this-split 0 split-point))
                   (second-half (substring this-split split-point)))
              (push (list (concat first-half "\\")) results)
              (setq splits (cons second-half (cdr splits)))))

          ;; clear the line
          (setq current-line nil)
          (setq current-line-length 0)

          ;; next max-length
          (when (cdr max-lengths)
            (setq max-lengths (cdr max-lengths))))))

    (when current-line
      (push current-line results))

    (cl-loop
     for line in (reverse results)
     collect (string-join (reverse line) " "))))

(defsubst explain-pause-top--table-item-command-overflow
  (command-column-width full-width command-string)
  "Return nil or the (first, rest) strings for COMMAND-STRING."
  ;; TODO this really should be renamed and moved to the command entry
  ;; area
  (if (< (length command-string)
         command-column-width)
      ;; it fits
      nil
    ;; ok, truncate and split:
    (let ((lines
           (explain-pause-top--split-at-space
            command-string
            (list command-column-width
                  (- full-width 2))))
          (indent-newline "\n  "))
      (cons (car lines)
            (concat indent-newline
                    (string-join (cdr lines) indent-newline))))))

(defsubst explain-pause-top--table-prepare-draw
  (entry new-data buffer-index prev-buffer-index
        column-count field-count requested-widths field-diffs)
  "Prepare to draw ENTRY by setting the item to draw to NEW-DATA, then
generating the converted strings from the values. Store the strings and their
lengths into the buffer at BUFFER-INDEX, using the old values at
PREV-BUFFER-INDEX if useful. Finally, update REQUESTED-WIDTHS and dirty-fields
within the item with their dirtiness. FIELD-DIFFS is a temporary vector used to
hold the difference of fields."
  (let* ((to-draw-item (car new-data))
         (prev-draw-entry (cdr new-data))
         (dirty-fields (explain-pause-top--table-display-entry-dirty-fields entry))

         (buffer (explain-pause-top--table-display-entry-buffer entry))
         (prev-entry-buffer
          (when prev-draw-entry
            (explain-pause-top--table-display-entry-buffer prev-draw-entry))))

    ;; given the inputs, ask the entry to fill in the new state with new strings
    (setf (aref buffer buffer-index)
          (explain-pause-top--command-entry-compare
           (aref buffer buffer-index)
           ;; the new thing we want to draw
           to-draw-item
           ;; the previous item drawn here
           (aref buffer prev-buffer-index)
           ;; the previous drawn of item
           (when prev-entry-buffer
             (aref prev-entry-buffer prev-buffer-index))
           field-diffs))

    ;; update the item-ptr's prev-ptr to point to entry. we've saved the
    ;; actual prev-ptr already.
    (setcdr new-data entry)

    ;; current item-ptr is now filled with the new values, and field-diffs
    ;; holds the new strings, or where to copy.
    (cl-loop
     for field-index from 0
     for buffer-field from (1+ buffer-index)
     for prev-buffer-field from 1
     for field-diff across field-diffs
     for dirty-field across-ref dirty-fields
     with copy-buffer = nil
     with is-field = nil
     with field-width = 0
     do
     (setq is-field (< field-index column-count))
     (cond
      ((eq field-diff 'explain-pause-top--table-prev-item)
       (setq copy-buffer buffer)
       (setq dirty-field nil))
      ((eq field-diff 'explain-pause-top--table-prev-drawn)
       (setq copy-buffer prev-entry-buffer)
       (setq dirty-field t))
      (t
       (setq copy-buffer nil)
       (setq dirty-field t)))

     (setf (aref buffer buffer-field)
           (if copy-buffer
               (aref copy-buffer (+ prev-buffer-index prev-buffer-field))
             field-diff))

     (when is-field
       ;; update stored length
       (setq field-width
             (if copy-buffer
                 (aref copy-buffer (+ prev-buffer-index field-count prev-buffer-field))
               (string-width field-diff)))

       (setf (aref buffer (+ buffer-field field-count)) field-width)

       ;; update the requested-width
       (when (> field-width
                (aref requested-widths field-index))
         (setf (aref requested-widths field-index) field-width))))))

(defun explain-pause-top--table-refresh (table)
  "Refresh the table of items in the current buffer when requested. Note that
the width cannot be 0."
  ;; this is relatively optimized never to allocate memory unless absolutely
  ;; needed. it tries to hoist lets out of tight loops and generally
  ;; preallocates memory in table-initialize.
  ;;
  ;; first, calculate the widths of all the columns.
  ;; To do this, now walk through all the entries, updating their current
  ;; items as needed, and ask them to prepare to draw.
  ;; after, insert new, un-base-marked entries to take care of any new
  ;; items.
  (let ((display-order-ptr (cdr (explain-pause-top--table-entries table)))
        (display-entries-prev (explain-pause-top--table-display-entries table))
        (display-entries-ptr (cdr (explain-pause-top--table-display-entries table)))

        (column-count (explain-pause-top--table-column-count table))
        (field-count (explain-pause-top--table-field-count table))

        (buffer-index (explain-pause-top--table-buffer-index table))
        (prev-buffer-index (explain-pause-top--table-prev-buffer-index table))
        (buffer-width (explain-pause-top--table-buffer-width table))

        (requested-widths (explain-pause-top--table-requested-widths table))
        (current-diffs (explain-pause-top--table-current-diffs table))

        (layout-changed nil))

    ;; initialize current-diffs with the original header widths
    ;; don't use copy-sequence as it creates a new object
    (cl-loop
     for header-width across (explain-pause-top--table-header-widths table)
     for requested-width across-ref requested-widths
     do (setf requested-width header-width))

    ;; walk both the display-order and the display-entries
    (while (and display-order-ptr
                display-entries-ptr)

      (explain-pause-top--table-prepare-draw
       (car display-entries-ptr)
       (car display-order-ptr)
       buffer-index
       prev-buffer-index
       column-count
       field-count
       requested-widths
       current-diffs)

      (setq display-order-ptr (cdr display-order-ptr))
      (setq display-entries-prev display-entries-ptr)
      (setq display-entries-ptr (cdr display-entries-ptr)))

    ;; ok, now reconcile & add new items
    ;; prev points to the end now
    ;; most of the time, we don't need to add new items, so
    ;; check before letting:
    (when display-order-ptr
      (let ((new-list-entry nil)
            (new-entry nil))
        (while display-order-ptr
          (setq new-entry
                (make-explain-pause-top--table-display-entry
                 :begin-mark nil
                 :total-length nil
                 :buffer (make-vector (* 2 buffer-width) nil)
                 :dirty-fields (make-vector field-count nil)))
          (setq new-list-entry (cons new-entry nil))

          (explain-pause-top--table-prepare-draw
           new-entry
           (car display-order-ptr)
           buffer-index
           prev-buffer-index
           column-count
           field-count
           requested-widths
           current-diffs)

          ;; insert at the tail
          (setcdr display-entries-prev new-list-entry)
          (setq display-entries-prev new-list-entry)
          (setq display-order-ptr (cdr display-order-ptr)))))

    ;; at this point, the following invariants hold:
    ;; * every entry has a display-entry (but not all of them have begin-marks)
    ;; * columns holds the largest requested width.
    ;; * anything that we don't need anymore is starting at display-entries-ptr
    ;; check to see if the fixed columns have changed width, OR if our width
    ;; changed. If so, we'll force-draw full lines
    (when (or
           (explain-pause-top--table-needs-resize table)
           (cl-mismatch (explain-pause-top--table-column-widths table)
                        requested-widths
                        :start1 1
                        :start2 1
                        :test 'eq))

      ;; if they are not equal, update the header, format strings, etc.
      (explain-pause-top--table-resize-columns
       table
       ;; convert to a list as resize-columns expects a list of fixed widths
       (cdr (append requested-widths nil)))

      (setf (explain-pause-top--table-needs-resize table) nil)
      (setq layout-changed t))

    ;; if the header is dirty, refresh it:
    (when (explain-pause-top--table-header-dirty table)
      (let ((header
             (apply 'format
                    (explain-pause-top--table-display-full-line-format table)
                    (append (explain-pause-top--table-header-titles table) nil))))
        (setq header-line-format
              `(:eval (explain-pause-top--generate-header-line
                       ,header
                       ,(length header)
                       (window-hscroll)
                       (- (window-total-width) 1)))))

      (force-mode-line-update)

      (setf (explain-pause-top--table-header-dirty table) nil))

    ;; now, we are prepared to draw:
    (let ((column-widths
           (explain-pause-top--table-column-widths table))
          (table-width (explain-pause-top--table-width table))
          (full-format-string
           (explain-pause-top--table-display-full-line-format table))
          (format-strings
           (explain-pause-top--table-display-column-formats table))
          (column-offsets
           (explain-pause-top--table-display-column-offsets table))

          (buffer-value-index (1+ buffer-index))

          (display-draw-ptr
           (cdr (explain-pause-top--table-display-entries table)))

          (item nil)
          (begin-mark nil)
          (total-prev-length nil)
          (buffer nil)
          (new-item nil)
          (dirty-fields nil))

      (while display-draw-ptr
        (setq item (car display-draw-ptr))
        (setq begin-mark (explain-pause-top--table-display-entry-begin-mark item))
        (setq total-prev-length
              (explain-pause-top--table-display-entry-total-length item))
        (setq buffer
              (explain-pause-top--table-display-entry-buffer item))
        (setq new-item nil)

        (unless begin-mark
          (setq begin-mark (point-max-marker))
          (setf (explain-pause-top--table-display-entry-begin-mark item) begin-mark)
          (setq new-item t))

        (cond
         ((or layout-changed
              new-item)
          ;; draw everything in one shot
          ;; TODO special command-str handling here
          (let* ((command-str (aref buffer (+ buffer-value-index 0)))
                 ;; when full line is being drawn, always regenerate the cmd
                 ;; line (TODO this could be optimized)
                 (cmd-lines
                  (explain-pause-top--table-item-command-overflow
                   (aref column-widths 0)
                   table-width
                   command-str))

                 (first-command-str (or (car cmd-lines)
                                        command-str))

                 (slow-lines (aref buffer (+ buffer-value-index 6)))
                 (profile-lines (aref buffer (+ buffer-value-index 7)))

                 (extra-lines (concat
                               (cdr cmd-lines)
                               ;;TODO clean up multiline handling
                               ;;TODO stop regenerating this every time (?)
                               (when slow-lines
                                 (explain-pause-top--concat-to-width
                                  slow-lines
                                  table-width
                                  "\n "))
                               (when profile-lines
                                 (explain-pause-top--concat-to-width
                                  profile-lines
                                  table-width
                                  "\n "))))

                 ;; Hm. feels slow.
                 (final-string (concat
                                (apply 'format
                                       full-format-string
                                       first-command-str
                                       (append
                                        (cl-subseq buffer
                                                   ;; TODO skip over first
                                                   (+ buffer-value-index 1)
                                                   (+ buffer-value-index
                                                      column-count))
                                        nil))
                                extra-lines)))

            ;; store the cached cmd-lines
            ;; TODO cmd line magic
            (setf (aref buffer (+ buffer-value-index 5)) cmd-lines)

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
              (insert "\n"))))
         (t
          ;; per column update using dirty
          ;; deal with the real columns first
          (setq dirty-fields (explain-pause-top--table-display-entry-dirty-fields item))
          (cl-loop
           for column-index from 0 below column-count
           for buffer-index from buffer-value-index
           for dirty-column across dirty-fields
           do (when dirty-column
                ;; the colunn is dirty; we need to draw
                (let ((cached-val (aref buffer buffer-index))
                      (format-str (aref format-strings column-index)))
                  (cond
                   ((eq column-index 0)
                    ;; cmd, is special cased due to overflow logic. this could
                    ;; be cleaned up and abstracted away, but I'm not sure I
                    ;; want to bother yet
                    ;; TODO hardcoded offset 0
                    (let ((command-str (aref buffer buffer-index))
                          (cmd-lines (aref buffer (+ buffer-value-index 5)))
                          (printed-first-line nil))

                      (when (eq cmd-lines 'explain-pause-top--table-generate)
                        ;; need to regenerate it
                        (setq cmd-lines
                              (explain-pause-top--table-item-command-overflow
                               (aref column-widths 0)
                               table-width
                               command-str))

                      ;; save it
                        (setf (aref buffer (+ buffer-value-index 5)) cmd-lines))

                      (setq printed-first-line
                            (format format-str
                                    (or (car cmd-lines)
                                        command-str)))

                      (goto-char begin-mark)
                      (delete-char (length printed-first-line))
                      (insert printed-first-line)))
                   (t
                    ;; normal field. don't do these lookups unless we have to
                    (let* ((new-str (format format-str cached-val))
                           (offset (aref column-offsets column-index)))
                      (goto-char (+ begin-mark offset))
                      (delete-char (length new-str))
                      (insert new-str)))))))

          ;; now deal with extra lines. only bother if at least is dirty.
          (when (or (aref dirty-fields 0)
                    (aref dirty-fields 6)
                    (aref dirty-fields 7))
            (let* ((extra-cmd-lines
                    (cdr (aref buffer (+ buffer-value-index 5)))) ;; TODO cmd handling
                   ;; TODO multline handling
                   (slow-lines (aref buffer (+ buffer-value-index 6)))
                   (profile-lines (aref buffer (+ buffer-value-index 7)))
                   (extra-lines
                    ;; TODO dry with full line gen?
                    (concat extra-cmd-lines
                            (when slow-lines
                              (explain-pause-top--concat-to-width
                               slow-lines
                               table-width
                               "\n "))
                            (when profile-lines
                              (explain-pause-top--concat-to-width
                               profile-lines
                               table-width
                               "\n "))))
                   (new-extra-length (length extra-lines))
                   (new-total-length (+ table-width new-extra-length))
                   (prev-extra-length (- total-prev-length table-width)))

              ;; total-prev-length must exist. if the total-prev-length is > width
              ;; then we already had extra lines; delete them, insert ours, if it
              ;; exists, and update total-prev-lines

              (goto-char (+ begin-mark table-width))
              (when (> prev-extra-length 0)
                (delete-char prev-extra-length))
              (when (> new-extra-length 0)
                (insert extra-lines))

              (unless (eq new-total-length total-prev-length)
                (setf (explain-pause-top--table-display-entry-total-length item)
                      new-total-length))))))

        (setq display-draw-ptr (cdr display-draw-ptr))))

    ;; move to the beginning of the "no longer needed entries",
    ;; wipe, and clear:
    (when display-entries-ptr
      (let ((mark (explain-pause-top--table-display-entry-begin-mark
                     (car display-entries-ptr))))
        (delete-region mark (point-max))
        (setcdr display-entries-prev nil)))

    ;; update the pointers for buffer to flip for next time
    (setf (explain-pause-top--table-buffer-index table) prev-buffer-index)
    (setf (explain-pause-top--table-prev-buffer-index table) buffer-index)))

;; explain-pause-top-mode
;; buffer-local variables that should be always private
(defvar-local explain-pause-top--buffer-refresh-timer nil
  "The timer for the buffer. It is nil if auto-refresh is off for that buffer.")

(defvar-local explain-pause-top--buffer-command-pipe nil
  "The hook lambda that is added to the `explain-pause-measured-command-hook'
for this buffer.")

(defvar-local explain-pause-top--buffer-statistics nil
  "The set of commands that have not yet been applied in a table update.")

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

;; command-entry
(cl-defstruct explain-pause-top--command-entry
  command-set
  count
  slow-count
  avg-ms
  total-ms
  ;; either nil for no, t for yes, 'new for yes and new.
  dirty
  ;; index into the slow circular list
  slow-index
  ;; pointer to the profiles
  profiles)

(defun explain-pause-top---command-entry-command-set-sorter (lhs rhs)
  "Sort command-sets alphabetically."
  ;; to sort by command, we have to convert the objects into strings, which
  ;; is expensive. TODO we should build a string mapping...
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
      nil)))

(defmacro explain-pause-top--command-entry-number-sorters (field-names)
  "Generate sorters for numbers for the given FIELD-NAMES"
  `(list ,@(cl-loop
       for field-name in field-names
       collect
       (let ((getter (intern (format "explain-pause-top--command-entry-%s" field-name))))
         `(cons (lambda (lhs rhs)
                  (< (,getter lhs)
                     (,getter rhs)))
                (lambda (lhs rhs)
                  (>= (,getter lhs)
                      (,getter rhs))))))))

(defmacro explain-pause-top--command-entry-column-fields-compare
    (state-to-fill new-item prev-item prev-drawn-item field-diffs cases)
  "Generate a list of statements one for each field (car) of CASES, skipping
over nil cases, which compares the PREV and CURRENT values of that field.

There are two kinds of CASES.

When (car) is a symbol, eq is used to check prev-val and current-val, including
checking dirtiness equalness. If they are not equal, then if dirtiness equals,
the prev-string is used. Otherwise, the (cdr) is called to generate the new
string.

When (car) is a list, then (cdr) of that list is used as the body, with no tests.
The body must return the value of the field-diff itself."
  `(let* ((dirty (explain-pause-top--command-entry-dirty ,new-item))
          (prev-item-dirty (and ,prev-item
                                (explain-pause-top--command-entry-dirty ,prev-item)))
          (prev-drawn-dirty (and ,prev-drawn-item
                                 (explain-pause-top--command-entry-dirty ,prev-drawn-item))))
     ,@(cl-loop
        for case in cases
        for index from 0
        when case
        collect
        (let* ((test (car case))
               (body (cdr case))
               (field-name (if (symbolp test)
                               test
                             (car test))))
          (if (not field-name)
              ;; direct just body
              `(setf (aref ,field-diffs ,index) ,@body)
            ;; regular field logic
            (let ((getter (intern (format "explain-pause-top--command-entry-%s"
                                         field-name))))
              `(let ((field-val (,getter ,new-item)))
                 (setf (aref ,field-diffs ,index)
                       ,(cond
                         ((symbolp test)
                          ;; simple case
                          `(cond
                            ;; is it same as what's drawn?
                            ((and ,prev-item
                                  (eq prev-item-dirty dirty)
                                  (eq (,getter ,prev-item) field-val))
                             ;; then we just need to copy
                             'explain-pause-top--table-prev-item)
                            ;; is it the same as what was drawn earlier?
                            ((and ,prev-drawn-item
                                  (eq prev-drawn-dirty dirty)
                                  (eq (,getter ,prev-drawn-item) field-val))
                             ;; then we just need to copy
                             'explain-pause-top--table-prev-drawn)
                            ;; nope, we need to draw
                            (t
                             ,@body)))
                         (t
                          ;; custom case
                          `(let ((prev-val
                                  (when ,prev-item
                                    (,getter ,prev-item)))
                                 (prev-drawn-val
                                  (when ,prev-drawn-item
                                    (,getter ,prev-drawn-item))))
                             ,@(cdr test)))))
                 ;; copy the value
                 (setf (,getter ,state-to-fill) field-val))))))))

(defsubst explain-pause-top--propertize-if-dirty (dirty str-expr)
  "If DIRTY is true, generate a propertized STR-EXPR with
explain-pause-top-changed face, otherwise just return STR-EXR"
  (if dirty
      (propertize str-expr 'face 'explain-pause-top-changed)
    str-expr))

(defun explain-profile-top--click-profile-report (button)
  "Click-handler when profile BUTTON is clicked in event profile report view."
  (let* ((profile (button-get button 'profile))
         (profile-buffer (profiler-report-setup-buffer profile)))
    (funcall explain-pause-top-click-profile-action profile-buffer)))

(defconst explain-pause-top--command-entry-headers
  ["Command" "slow" "avg ms" "ms" "calls"]
  "The header strings of a `explain-pause-top' table")

(defconst explain-pause-top--single-profile-header
  (propertize "\n ► Profile:" 'face 'explain-pause-top-profile-heading)
  "The heading used when there is one profile available.")

(defconst explain-pause-top--multiple-profile-header
  ;; these strings ought to be propertized, but format does not work correctly
  ;; for multibyte strings in emacs <27 (bug#38191). propertize after format
  ;; instead.
  "\n ► Last %d profiles:"
  "The heading used when there are multiple profiles available.")

(defconst explain-pause-top--single-slow-header
  (propertize "\n ► Slow:" 'face 'explain-pause-top-slow-heading)
  "The heading used when there is one slow time available.")

(defconst explain-pause-top--multiple-slow-header
  "\n ► Last %d slow:"
  "The heading used when there are multiple slow times available.")

(defconst explain-pause-top--n/a-value -1
  "The sentinel value that means no value is available yet for this number field.")

(defsubst explain-pause-top--value-or-n/a-default (input-value)
  "Return INPUT-VALUE or 0 if it is n/a-value"
  (if (eq input-value explain-pause-top--n/a-value)
      0
    input-value))

(defmacro explain-pause-top--value-or-n/a-string (input-value &rest body)
  "If INPUT-VALUE is `explain-pause-top--n/a-value', return 'N/A' or otherwise
BODY"
  `(if (eq ,input-value explain-pause-top--n/a-value)
       "N/A"
     ,@body))

(defun explain-pause-top--command-entry-compare
    (state-to-fill new-item prev-item prev-drawn-item field-diffs)
  "Update FIELD-DIFFS, a vector, with the new strings or where to copy if
nothing changed. Update STATE-TO-FILL, or create it if nil, with the new values
from NEW-ITEM.

For every column, check to see if the value in PREV-ITEM matches NEW-ITEM. If it
is, set `prev-item'. If it is not, check to see if the value in
`prev-drawn-item' matches. If so, set `prev-drawn'. If not, finally generate a
new string.

Values are considered the same only if their owning object dirtiness is also the
same."
  (unless state-to-fill
    (setq state-to-fill
          (make-explain-pause-top--command-entry)))

  (explain-pause-top--command-entry-column-fields-compare
   state-to-fill new-item prev-item prev-drawn-item field-diffs
   (((command-set
      ;; as the title doesn't change if it's dirty or not, ignore dirtiness
      ;; TODO DRY with profile?
      ;; TODO copy the 5 too and check nil in draw
      (cond
       ((and prev-item
             (eq prev-val field-val))
        'explain-pause-top--table-prev-item)
       ((and prev-drawn-item
             (eq prev-drawn-val field-val))
        'explain-pause-top--table-prev-drawn)
       (t
        (explain-pause--command-set-as-string field-val)))))
    (slow-count
     (let ((val-str (number-to-string field-val)))
       (if (> field-val 0)
           (if dirty
               (propertize val-str 'face
                           '(explain-pause-top-slow explain-pause-top-changed))
             (propertize val-str 'face 'explain-pause-top-slow))
         (explain-pause-top--propertize-if-dirty dirty val-str))))
    (avg-ms
     (explain-pause-top--value-or-n/a-string
      field-val
      (explain-pause-top--propertize-if-dirty dirty (format "%.2f" field-val))))
    (total-ms
     (explain-pause-top--value-or-n/a-string
      field-val
      (explain-pause-top--propertize-if-dirty dirty (number-to-string field-val))))
    (count
     (explain-pause-top--value-or-n/a-string
      field-val
      (explain-pause-top--propertize-if-dirty dirty (number-to-string field-val))))
    (nil ;; cmd lines
     ;; copy the value if we have it from previous / prev-drawn or else ask
     ;; draw to generate it
     (let ((cmd-diff (aref field-diffs 0)))
       (if (or (eq cmd-diff 'explain-pause-top--table-prev-item)
               (eq cmd-diff 'explain-pause-top--table-prev-drawn))
           cmd-diff
         'explain-pause-top--table-generate)))
    ((slow-index ;; slow ms
      ;; dirtiness doesn't matter, but the index PLUS object must be the same
      ;; nil represents no results
      (cond
       ((not field-val)
        nil)
       ((and prev-item
             (eq prev-val field-val)
             (eq (aref field-diffs 0) 'explain-pause-top--table-prev-item))
        'explain-pause-top--table-prev-item)
       ((and prev-drawn-item
             (eq prev-drawn-val field-val)
             (eq (aref field-diffs 0) 'explain-pause-top--table-prev-drawn))
        'explain-pause-top--table-prev-drawn)
       (t
        ;; TODO directly using cmd ... hm :/
        ;; TODO command-list
        ;; the circular list's "next" place is at field-val aka slow-index.
        ;; this represents the very oldest item, so we can build the list in
        ;; reverse by walking forwards in the circular list.
        (let* ((statistics
                (gethash (car (explain-pause-top--command-entry-command-set new-item))
                         explain-pause-profile--profile-statistics))
               (size (explain-pause-profile--statistic-slow-length statistics))
               (items-length 0)
               (items (cons "ms" nil))
               (index field-val)
               (slot nil))

          (cl-loop
           do
           (setq slot (aref statistics
                            (+ explain-pause-profile--statistic-slow-count-offset
                               index)))
           (when slot
             (setq items (cons (format
                                (if (eq items-length 0)
                                    "%s" ;; no comma for the last (aka first)
                                  "%s,")
                                slot)
                               items))
             (setq items-length (1+ items-length)))
           (setq index (% (+ index 1) size))
           until (eq index field-val))

          (cons
           (if (eq items-length 1)
               explain-pause-top--single-slow-header
             ;; emacs bug #38191, <27
             (propertize
              (format explain-pause-top--multiple-slow-header items-length)
              'face 'explain-pause-top-slow-heading))
           items))))))
    ((profiles
      ;; as the lists are different if any profile inside is changed, we don't need
      ;; to account for dirtiness for this field.
      (cond
       ((not field-val)
        nil)
       ((and prev-item
             (eq prev-val field-val))
        'explain-pause-top--table-prev-item)
       ((and prev-drawn-item
             (eq prev-drawn-val field-val))
        'explain-pause-top--table-prev-drawn)
       (t
        ;; ok, actually generate it:
        (let ((count (length field-val)))
          (cons
           (if (eq count 1)
               explain-pause-top--single-profile-header
             ;; emacs bug #38191, <27
             (propertize
              (format explain-pause-top--multiple-profile-header count)
              'face 'explain-pause-top-profile-heading))
           ;;TODO stop making these every time dirty column
           (mapcar (lambda (profile-info)
                     (make-text-button
                      (format "[%.2f ms]" (aref profile-info 0))
                      nil
                      'action #'explain-profile-top--click-profile-report
                      'profile (aref profile-info 1)
                      'follow-link t))
                   field-val)))))))))

  ;; copy the dirtiness separately as it's not covered in the field set
  (setf (explain-pause-top--command-entry-dirty state-to-fill)
        (explain-pause-top--command-entry-dirty new-item))

  ;; clear the actual entry's dirtiness for the next draw round
  (setf (explain-pause-top--command-entry-dirty new-item) nil)

  state-to-fill)

(defconst explain-pause-top--command-entry-sorters
  (vconcat
   (cons (cons #'explain-pause-top---command-entry-command-set-sorter
               (lambda (lhs rhs)
                 (not (explain-pause-top---command-entry-command-set-sorter lhs rhs))))
         (explain-pause-top--command-entry-number-sorters
          (slow-count avg-ms total-ms count))))
  "The sorter functions for each column of a `explain-pause-top' table")

;; logging functions
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

(eval-and-compile
  (let ((notification-count 0)
        (last-notified (current-time))
        (alert-timer nil))
    (defun explain-pause-mode--log-alert-normal (record)
      "Notify the user of alerts when at least `explain-pause-alert-normal-minimum-count'
alerts have occurred, AND the time since the last notification (or startup)
is greater then `explain-pause-alert-normal-interval' minutes."
      (when (and (not (explain-pause-command-record-native record))
                 (explain-pause-command-record-too-slow record))
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
        (message "Emacs was slow %d times recently. Run `explain-pause-top' to learn more." notification-count)
        (setq notification-count 0)
        (setq last-notified (current-time)))))

  (let ((notifications '())
        (profiled-count 0)
        (alert-timer nil))
    (defun explain-pause-mode--log-alert-developer (record)
      "Log all slow and profiling alerts in developer mode. They are gathered until
run-with-idle-timer allows an idle timer to run, and then they are printed
to the minibuffer with a 2 second sit-for."
      (when (and (not (explain-pause-command-record-native record))
                 (explain-pause-command-record-too-slow record))
        (push (explain-pause-command-record-executing-time record) notifications)
        (when (explain-pause-command-record-profile record)
          (setq profiled-count (1+ profiled-count)))
        (unless alert-timer
          (setq alert-timer
                (run-with-idle-timer
                 0.5 nil
                 #'explain-pause-mode--log-alert-developer-display)))))

    (defun explain-pause-mode--log-alert-developer-display ()
      "Display the last set of notifications in the echo area when the minibuffer is
not active."
      (if (minibufferp (current-buffer))
          ;; try again
          (setq alert-timer
                (run-with-idle-timer
                 (time-add (current-idle-time) 0.5)
                 nil
                 #'explain-pause-mode--log-alert-developer-display))
        ;; ok, let's draw
        (message "Emacs was slow: %s ms%s%s"
                 (mapconcat #'number-to-string notifications ", ")
                 (if (> profiled-count 0)
                     (format " of which %d were profiled" profiled-count)
                   "")
                 ". Run `explain-pause-top' to learn more.")

        ;; reset so more notifications can pile up while we wait
        (setq notifications '())
        (setq profiled-count 0)
        (sit-for 2)
        (message nil)
        ;; don't let us get rescheduled until we're really done.
        (setq alert-timer nil)))))

;; logging customization
;; depressingly can't define it at the top because `explain-pause-mode-change-alert-style'
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

;; `explain-pause-top' major mode
(define-derived-mode explain-pause-top-mode special-mode
  "Explain Pause Top"
  "Major mode for listing the statistics generated by explain-pause for recently
run commands in emacs. The mode resizes the table inside the buffer to always be
the width of the largest window viewing the buffer. Reverting the buffer will
refresh the table. The buffer initially starts with the auto refresh interval
given in `explain-pause-top-auto-refresh-interval'. You can modify this interval
on a per buffer basis by calling `explain-pause-top-auto-refresh'. When a buffer
is made `explain-pause-top-mode', `explain-pause-mode' is also enabled."
  (buffer-disable-undo)
  (font-lock-mode -1)

  (setq truncate-lines t)
  (setq buffer-read-only t)

  (setq-local revert-buffer-function #'explain-pause-top--buffer-revert)

  (setq-local explain-pause-top--buffer-table
              (make-explain-pause-top--table))

  (setq-local explain-pause-top--buffer-statistics
              (make-hash-table :test 'equal))

  (explain-pause-top--table-initialize
   explain-pause-top--buffer-table
   (copy-sequence explain-pause-top--command-entry-headers)
   ;; 3 extra slots
   ;; - cmd lines
   ;; - slow lines
   ;; - profile lines
   (+ (length explain-pause-top--command-entry-headers) 3))

  (setq-local explain-pause-top--sort-column nil)

  ;; default sort: slow count
  ;; TODO hardcoded col index
  (explain-pause-top--apply-sort 1 t)

  (when explain-pause-top--buffer-window-size-changed
    (remove-hook 'window-size-change-functions
                 explain-pause-top--buffer-window-size-changed))

  (setq-local explain-pause-top--buffer-window-size-changed
              (let ((this-buffer (current-buffer)))
                (lambda (_)
                  ;; ignore frame, and recalculate the width across all frames
                  ;; every time. we always need the biggest.
                  (explain-pause-top--buffer-update-width-from-windows
                   this-buffer))))

  (when explain-pause-top--buffer-command-pipe
    (remove-hook 'explain-pause-measured-command-hook
                 explain-pause-top--buffer-command-pipe))

  (setq-local
   explain-pause-top--buffer-command-pipe
   (let ((this-commands explain-pause-top--buffer-statistics)
         ;; store these in the closure so we don't reallocate every command
         (command-set nil)
         (entry nil)
         (new-count nil)
         (new-ms nil))
     (lambda (record)
       ;; this lambda is called ON EVERY SINGLE COMMAND is it is important
       ;; to not use a let and allocate the minimum required.
       ;; ignore native frames for now - TODO
       (unless (explain-pause-command-record-native record)
         ;; TODO command-list
         (setq command-set (list (explain-pause-command-record-command record)))
         (setq entry (gethash command-set this-commands nil))
         (cond
          (entry
           ;; update.
           (setq new-count (1+ (explain-pause-top--value-or-n/a-default
                                (explain-pause-top--command-entry-count entry))))

           (setq new-ms (+ (explain-pause-command-record-executing-time record)
                           (explain-pause-top--value-or-n/a-default
                            (explain-pause-top--command-entry-total-ms entry))))

           (setf (explain-pause-top--command-entry-count entry) new-count)
           (setf (explain-pause-top--command-entry-total-ms entry) new-ms)
           (setf (explain-pause-top--command-entry-avg-ms entry)
                 (/ (float new-ms) (float new-count)))

           (setf (explain-pause-top--command-entry-slow-count entry)
                 (+  (explain-pause-top--command-entry-slow-count entry)
                     (if (explain-pause-command-record-too-slow record) 1 0)))

           (setf (explain-pause-top--command-entry-profiles entry)
                 (explain-pause-profile--statistic-profiles record))

           (setf (explain-pause-top--command-entry-slow-index entry)
                 (explain-pause-profile--statistic-slow-index record))

           (setf (explain-pause-top--command-entry-dirty entry) t))
          (t
           ;; new.
           (puthash command-set
                    (make-explain-pause-top--command-entry
                     :command-set command-set
                     :count 1
                     :avg-ms
                     (explain-pause-command-record-executing-time record)
                     :total-ms
                     (explain-pause-command-record-executing-time record)
                     :slow-count
                     (if (explain-pause-command-record-too-slow record) 1 0)
                     :dirty 'new
                     :slow-index
                     (explain-pause-profile--statistic-slow-index record)
                     :profiles
                     (explain-pause-profile--statistic-profiles record))
                    this-commands)))))))

  (add-hook 'explain-pause-measured-command-hook
            explain-pause-top--buffer-command-pipe t)

  (add-hook 'window-size-change-functions
              explain-pause-top--buffer-window-size-changed)

  (add-hook 'window-configuration-change-hook
            'explain-pause-top--buffer-window-config-changed nil t)

  (add-hook 'kill-buffer-hook 'explain-pause-top--buffer-killed nil t)

  ;; if the user changes major mode, act as if we are destroyed and clear
  ;; all timers, etc.
  (add-hook 'change-major-mode-hook 'explain-pause-top--buffer-killed nil t)

  (explain-pause-top-auto-refresh nil explain-pause-top-auto-refresh-interval)

  ;; enable the minor mode if not enabled
  (unless explain-pause-mode
    (explain-pause-mode))

  ;; create entries for all slow commands
  (maphash (lambda (command statistic)
             ;; TODO abstraction for statistic?
             (when (> (aref statistic 4) 0)
               (let ((command-set (list command))) ;; TODO command-set list
                 (puthash command-set
                          (make-explain-pause-top--command-entry
                           :command-set command-set
                           :count explain-pause-top--n/a-value
                           :slow-count (aref statistic 4)
                           :avg-ms explain-pause-top--n/a-value
                           :total-ms explain-pause-top--n/a-value
                           :dirty 'new
                           :slow-index (aref statistic 5)
                           :profiles (aref statistic 3))
                          explain-pause-top--buffer-statistics))))
           explain-pause-profile--profile-statistics)

  ;; immediately ask for a resize:
  (funcall explain-pause-top--buffer-window-size-changed nil))

(defun explain-pause-top--buffer-killed ()
  "Clean timers and hooks when the buffer is destroyed."
  (remove-hook 'explain-pause-measured-command-hook
               explain-pause-top--buffer-command-pipe)

  (explain-pause-top-auto-refresh)

  (remove-hook 'window-size-change-functions
               explain-pause-top--buffer-window-size-changed))

(defun explain-pause-top--buffer-revert (_ignoreauto _noconfirm)
  (explain-pause-top--buffer-refresh))

(defun explain-pause-top--buffer-resize (width)
  "resize the current table in the buffer to the WIDTH given, and then redraw it."
  (explain-pause-top--table-resize-width explain-pause-top--buffer-table width)
  (explain-pause-top--buffer-refresh))

(defun explain-pause-top--buffer-reschedule-timer ()
  "Reschedule the timer for this buffer if needed."
  (when (and explain-pause-top--buffer-refresh-interval
             (not explain-pause-top--buffer-refresh-timer))
    ;; manually reschedule timers so we don't get repeat reruns after delays
    (setq-local explain-pause-top--buffer-refresh-timer
                (run-with-timer explain-pause-top--buffer-refresh-interval
                                nil
                                #'explain-pause-top--buffer-refresh-handler
                                (current-buffer)))))

(defun explain-pause-top--buffer-refresh-handler (buffer)
  "Refresh the target BUFFER and reschedule the timer."
  (with-current-buffer buffer
    ;; clear the timer as we just ran
    (setq-local explain-pause-top--buffer-refresh-timer nil)

    (explain-pause-top--buffer-refresh)
    (explain-pause-top--buffer-reschedule-timer)))

(defun explain-pause-top--buffer-refresh-with-buffer (buffer)
  "Refresh the target BUFFER and reschedule the timer."
  (with-current-buffer buffer
    (explain-pause-top--buffer-refresh)))

(defsubst explain-pause-top--buffer-upsert-entry (_ item)
  "Upsert an item from the map of entries in a buffer."
  ;; deliberately call aref twice instead of letting a new scope.
  ;; this is in a very tight loop.
  (cond
   ((eq (explain-pause-top--command-entry-dirty item) 'new)
    (explain-pause-top--table-insert explain-pause-top--buffer-table item))
   ((explain-pause-top--command-entry-dirty item)
    (explain-pause-top--table-update explain-pause-top--buffer-table item))))

(defun explain-pause-top--buffer-refresh ()
  "Refresh the current buffer - redraw the data at the current target-width"
  ;; first, insert all the items
  (maphash #'explain-pause-top--buffer-upsert-entry
           explain-pause-top--buffer-statistics)

  ;; It's possible a refresh timer ran before/after we calculated size, if so,
  ;; don't try to draw yet.
  (unless (eq (explain-pause-top--table-width explain-pause-top--buffer-table) 0)
    (let ((inhibit-read-only t)
          (point-in-entry (explain-pause-top--display-entry-from-point)))
      (save-match-data
        (explain-pause-top--table-refresh explain-pause-top--buffer-table)

        ;; move the cursor back
        (when point-in-entry
          (goto-char (+ (explain-pause-top--table-display-entry-begin-mark
                         (car point-in-entry))
                        (cdr point-in-entry))))))))

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

(defun explain-pause-top--apply-sort (column direction)
  "Undo the header adjustment for the current sorted column and then applies
the new header adjustment for COLUMN in DIRECTION."
  (when explain-pause-top--sort-column
    (explain-pause-top--table-set-header
     explain-pause-top--buffer-table
     explain-pause-top--sort-column
     (aref explain-pause-top--command-entry-headers explain-pause-top--sort-column)))

  (let ((sorters (aref explain-pause-top--command-entry-sorters column)))
    (explain-pause-top--table-set-sorter
     explain-pause-top--buffer-table
     (if direction (car sorters) (cdr sorters))))

  (explain-pause-top--table-set-header
   explain-pause-top--buffer-table column
   (propertize
    (concat
     (aref explain-pause-top--command-entry-headers column)
     ;; TODO perhaps make these glyphs and/or customizable
     (if direction "▼" "▲"))
    'face 'explain-pause-top-active-column-header))

  (setq-local explain-pause-top--sort-column column))

(defun explain-pause-top--column-from-point ()
  "Calculate the column of the table from the current point in the current
buffer."
  (let* ((column-offsets (explain-pause-top--table-display-column-offsets
                          explain-pause-top--buffer-table))
         (next-bigger-index (seq-position column-offsets (current-column)
                                          #'>)))
    (- (if next-bigger-index
           next-bigger-index
         (explain-pause-top--table-column-count
          explain-pause-top--buffer-table))
       1)))

(defun explain-pause-top--display-entry-from-point ()
  "Return the display entry and relative offset left that point is within in the
current buffer or nil if it is not within any (this can only happen if there are
no entries at all)."
  (let ((display-entry-ptr (cdr (explain-pause-top--table-display-entries
                                 explain-pause-top--buffer-table)))
        (search (point))
        (offset nil))

    (catch 'found
      (while display-entry-ptr
        (setq offset (- search
                        (explain-pause-top--table-display-entry-begin-mark
                         (car display-entry-ptr))))
        (when (and (>= offset 0)
                   (<= offset
                       ;; account for the new line
                       (1+ (explain-pause-top--table-display-entry-total-length
                            (car display-entry-ptr)))))
          (throw 'found (cons (car display-entry-ptr) offset)))

        (setq display-entry-ptr (cdr display-entry-ptr)))

      nil)))

(defun explain-pause-top-sort (buffer column &optional refresh)
  "Sort top table in the BUFFER using COLUMN, which is the 0-based
index. Optionally, immediately refresh the buffer (causes a buffer switch). In
interactive mode, sort the current buffer's column under point, and refreshes
immediately. If the target buffer is not a `explain-pause-top' buffer, do
nothing. Sorting the same column inverts the order."
  (interactive
   (list (current-buffer) (explain-pause-top--column-from-point) t))
  (when (eq (buffer-local-value 'major-mode buffer) 'explain-pause-top-mode)
    (let ((current-sorted (buffer-local-value 'explain-pause-top--sort-column buffer))
          (table (buffer-local-value 'explain-pause-top--buffer-table buffer))
          (direction t))
      (if (eq current-sorted column)
          ;; flip ordering
          (let ((current-sort-func
                 (explain-pause-top--table-sorter table))
                (current-sorters
                 (aref explain-pause-top--command-entry-sorters current-sorted)))
            (if (eq current-sort-func (car current-sorters))
                (setq direction nil))))
      (explain-pause-top--apply-sort column direction))
    (when refresh
      (explain-pause-top--buffer-refresh-with-buffer buffer))))

(defun explain-pause-top--pick-interactive-buffer ()
  "In interactive mode, prompt the user to pick a `explain-pause-top' buffer if
the current buffer is not already one. If there is exactly one
`explain-pause-top' nuffer, pick it."
  ;; if the current buffer is a explain-pause, just pick that.
  (if (eq major-mode 'explain-pause-top-mode)
      nil
    ;; otherwise, make a list of top buffers
    (let ((mode-buffers
           (seq-filter
            (lambda (buffer)
              (eq (buffer-local-value 'major-mode buffer) 'explain-pause-top-mode))
            (buffer-list))))
      ;; only one? pick it
      (if (eq (length mode-buffers) 1)
          (car mode-buffers)
        (let ((buffer t))
          ;; else, ask user to select
          (while (eq buffer t)
            (let ((buffer-completions
                   (mapcar (lambda (buffer)
                             (buffer-name buffer))
                           mode-buffers)))
              (setq buffer
                    (completing-read "Explain-Pause-Top buffer: " buffer-completions
                                     nil t nil nil t))))
          (get-buffer buffer))))))

(defun explain-pause-top-clear (buffer &optional refresh)
  "Clear the statistics in an `explain-pause-top-mode' buffer, BUFFER.
Optionally, REFRESH the buffer immediately, as well. if BUFFER is nil, the
current buffer is used. In interactive mode, this function checks to see if the
current buffer is an `explain-pause-top' buffer. If not, the user is prompted to
select such a buffer if more then one exists. Also, in interactive mode, the
buffer is refreshed immediately."
  (interactive
   (list (explain-pause-top--pick-interactive-buffer) t))

  (unless buffer
    (setq buffer (current-buffer)))
  (when (eq (buffer-local-value 'major-mode buffer) 'explain-pause-top-mode)
    (let ((table (buffer-local-value 'explain-pause-top--buffer-table buffer))
          (stats (buffer-local-value 'explain-pause-top--buffer-statistics buffer)))
      (clrhash stats)
      (explain-pause-top--table-clear table))
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
   (let ((buffer (explain-pause-top--pick-interactive-buffer))
         (interval t))
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

      (setq-local explain-pause-top--buffer-refresh-interval interval)
      (explain-pause-top--buffer-reschedule-timer))))


(defcustom explain-pause-logging-default-log-location
  (expand-file-name "explain-pause-log.socket"
                    temporary-file-directory)
  "The default file location for the UNIX socket that is used to send or receive
logs. This is used when `explain-pause-log-to-socket' is given no parameter.
If you change this value, the filename you specify must be writable by Emacs."
  :type 'string
  :group 'explain-pause-logging)

(defvar explain-pause-log--send-process nil
  "The process used to send logs to the UNIX socket.")

(defvar explain-pause-log--dgram-buffer-size 256
  "The dgram buffer size.")

(defvar explain-pause-log--dgram-buffer
  (make-vector (+ 3 explain-pause-log--dgram-buffer-size) 0)
  "The vector of temporary dgrams if the receiver is full. The firsw two items
represent the push and pop indices. Reserve one empty slot to differentiate
empty and full.")

(defun explain-pause-log--missing-socket-disable ()
  (explain-pause-log-off)
  (message "Explain-pause-mode stopped logging to socket. It got too full.")
  (sit-for 2)
  (message nil))

(defsubst explain-pause-log--send-dgram (str)
  "Write to the socket if it is enabled. The DGRAM code has its own special
branch in process.c which is synchronous (it doesn't block). If the buffer is
full on the other side, an error is raised."
  (condition-case err
      (progn
        (while (not (eq (aref explain-pause-log--dgram-buffer 0)
                        (aref explain-pause-log--dgram-buffer 1)))
          (process-send-string
           explain-pause-log--send-process
           (aref explain-pause-log--dgram-buffer
                 (+ (aref explain-pause-log--dgram-buffer 0) 2)))
          (setf (aref explain-pause-log--dgram-buffer 0)
                (% (1+ (aref explain-pause-log--dgram-buffer 0))
                   explain-pause-log--dgram-buffer-size)))

        (process-send-string
         explain-pause-log--send-process
         str))
    (file-error
     (cond
      ;; the file didn't exist; turn off logging...
      ((eq (car err) 'file-missing)
       (explain-pause-log--missing-socket-disable))
      ;; to avoid doing a grep over the string, assume it's just
      ;; buffer full. Try to push it onto the dgrams buffer.
      ((eq (car err) 'file-error)
       (let ((next (% (1+ (aref explain-pause-log--dgram-buffer 1))
                      explain-pause-log--dgram-buffer-size)))
         (if (eq (aref explain-pause-log--dgram-buffer 0) next)
             (explain-pause-log--missing-socket-disable)
           (setf (aref explain-pause-log--dgram-buffer
                       (+ (aref explain-pause-log--dgram-buffer 1) 2))
                 str)
           (setf (aref explain-pause-log--dgram-buffer 1) next))))))))

(defsubst explain-pause-log--send-command-entry (entry record)
  "Send the fact that we are entering RECORD from ENTRY to the send pipe."
     ;; try to be fast: use format directly, don't bother making an object
     ;; and call prin1-to-string, because though that is C code, we have
  ;; to allocate an list. try not to allocate memory instead.
  (when explain-pause-log--send-process
    (explain-pause-log--send-dgram
     (format "(\"enter\" \"%s\" \"%s\" \"%s\" %s %s %s %s %s %d)\n"
             (explain-pause--command-as-string
              (explain-pause-command-record-command record))
             (explain-pause--command-as-string
              (explain-pause-command-record-command entry))
             (explain-pause--command-as-string
              (explain-pause-command-record-command
               (explain-pause-command-record-parent record)))
             (explain-pause-command-record-native record)
             (explain-pause-command-record-executing-time record)
             (explain-pause-command-record-too-slow record)
             (explain-pause-command-record-is-profiled record)
             (explain-pause-command-record-under-profile record)
             (explain-pause-command-record-depth record)))))

(defsubst explain-pause-log--send-profile-start (record)
  "Send the fact that we are beginning profiling to the send pipe"
  (when explain-pause-log--send-process
    (explain-pause-log--send-dgram
     (format "(\"profile-start\" \"%s\" %s)\n"
             (explain-pause--command-as-string
              (explain-pause-command-record-command record))
             ;; TODO - abstraction layer?
             (gethash (explain-pause-command-record-command record)
                      explain-pause-profile--profile-statistics)))))

(defsubst explain-pause-log--send-profile-end (record)
  "Send the fact that we are ending profiling to the send pipe"
  (when explain-pause-log--send-process
    (explain-pause-log--send-dgram
     (format "(\"profile-end\" \"%s\" %s)\n"
             (explain-pause--command-as-string
              (explain-pause-command-record-command record))
             (not (eq (explain-pause-command-record-profile record) nil))))))

(defsubst explain-pause-log--send-command-exit (record)
  "Send the fact that we have finished a record to the send pipes"
  (when explain-pause-log--send-process
    (explain-pause-log--send-dgram
     (format "(\"exit\" \"%s\" \"%s\" %s %s)\n"
             (explain-pause--command-as-string
              (explain-pause-command-record-command record))
             (explain-pause--command-as-string
              (explain-pause-command-record-command
               (explain-pause-command-record-parent record)))
             (explain-pause-command-record-executing-time record)
             (when (explain-pause-command-record-profile record)
               'profile)))))

;; advices for all the things
(defun explain-pause-report-measuring-bug (where &rest args)
  "Ask the user to report a bug."
  ;; turn off everything we can
  (profiler-cpu-stop)
  (explain-pause-mode -1)

  (with-output-to-temp-buffer
      "explain-pause-mode-report-bug"
    (princ "Explain-pause-mode: please report this bug by creating a Github
issue at https://github.com/lastquestion/explain-pause-mode. Explain-pause-mode
is now _disabled_ so you can continue to hopefully use Emacs. Info follows:\n\n")
    (princ (format "explain-pause version: %s\n" explain-pause-version))
    (princ (format "emacs version: %s\n\n" emacs-version))
    (princ (format "%s\n" where))
    (dolist (arg args)
      (princ arg)
      (princ "\n"))
    (princ "\nBacktrace:\n")
    ;; emacs 27+, emacs commit 83af893fc0e7cc87c0fb0626fb48ef96e00b3f8b
    (if (fboundp 'backtrace--print-frame)
        (mapbacktrace
         (lambda (&rest args)
           (apply 'backtrace--print-frame args))
         #'explain-pause-report-measuring-bug)
      (require 'backtrace)
      (declare-function backtrace-to-string "backtrace")
      (declare-function backtrace-get-frames "backtrace")
      (princ (backtrace-to-string (backtrace-get-frames
                                   #'explain-pause-report-measuring-bug))))))

(defvar explain-pause--current-command-record nil
  "The current command records representing what we are currently
executing. This value is changed when entering / exiting `call-interactively',
and when execution contexts switch, (e.g. timer <-> command loop).")

;; most related actions here are inline subsitutions for performance reasons
(defsubst explain-pause--command-record-and-store (record)
  "Calculate the time since entry-snap of RECORD and add it to executing-time."
  (setf (explain-pause-command-record-executing-time record)
        (+ (explain-pause-command-record-executing-time record)
           (explain-pause--as-ms-exact
            (time-subtract
             (current-time)
             (explain-pause-command-record-entry-snap record))))))

(defsubst explain-pause--command-record-start-profiling (record)
  "Start profiling and record that in RECORD."
  (explain-pause-log--send-profile-start record)
  (setf (explain-pause-command-record-is-profiled record) t)
  (setf (explain-pause-command-record-under-profile record) t)
  (profiler-cpu-start explain-pause-profile-cpu-sampling-interval))

(defsubst explain-pause--command-record--save-and-stop-profiling (record)
  "Stop profiling and save the profile data for RECORD."
  ;; Note; it's a bug in the profile package that you can't call `profiler-cpu-profile'
  ;; after stopping, even though the documentation states that you can. Directly call
  ;; make-profile:
  (profiler-cpu-stop)
  ;; only bother saving the profile if it was slow:
  (when (> (explain-pause-command-record-executing-time record)
           explain-pause-slow-too-long-ms)
    (setf (explain-pause-command-record-profile record)
          (profiler-make-profile
           :type 'cpu
           :timestamp (current-time)
           :log (profiler-cpu-log))))
  (explain-pause-log--send-profile-end record))

(defsubst explain-pause--command-record-profile-p (record)
  "Should the command-record RECORD be profiled, taking into account existing
profiling conditions and nativeness? Calls
`explain-pause-profile--statistic-profile-p' as part of this determination."
  (and explain-pause-profile-enabled
       (not (explain-pause-command-record-native record))
       (not (explain-pause-command-record-under-profile record))
       (explain-pause-profile--statistic-profile-p record)))

(defsubst explain-pause--command-record-from-parent
  (current-command parent command &optional native)
  "Make a new command record from PARENT, using COMMAND, calculating all the
other values correctly in CURRENT-COMMAND context. If NATIVE is set, mark the
frame as native."
  (make-explain-pause-command-record
   :command command
   :parent parent
   :native native
   :under-profile
   (explain-pause-command-record-under-profile current-command)
   :depth
   (1+ (explain-pause-command-record-depth parent))))

(defmacro explain-pause--check-not-top-level (where &rest body)
  "Check that the `explain-pause--current-command-record' is not top level aka
`explain-pause-root-command-loop' and if it is, ask the user to report an error,
otherwise execute BODY. This is a macro to avoid execution of WHERE unless needed."
  `(if (eq explain-pause--current-command-record explain-pause-root-command-loop)
       (explain-pause-report-measuring-bug
        (format "not top level in %s" ,where)
        "current" explain-pause--current-command-record)
     ,@body))

(defmacro explain-pause--set-command-call (where record form &rest body)
  "Set `explain-pause--current-command-record' to RECORD and update it's
entry-snap to `current-time'. Profile if requested, around FORM with unwind
protect.  After, pause-and-store the RECORD, and verify that
`explain-pause--current-command-record' is still RECORD. Run BODY if so, or
`explain-pause-report-measuring-bug' otherwise."
  (declare (indent 1))
  `(progn
     (explain-pause-log--send-command-entry
      explain-pause--current-command-record
      ,record)
     (setq explain-pause--current-command-record ,record)
     (let ((should-profile (explain-pause--command-record-profile-p ,record)))
       (when should-profile
         (explain-pause--command-record-start-profiling ,record))
       (setf (explain-pause-command-record-entry-snap ,record) (current-time))
       (unwind-protect
           ,form
         (explain-pause--command-record-and-store ,record)
         (when should-profile
           (explain-pause--command-record--save-and-stop-profiling ,record))
         (explain-pause-log--send-command-exit ,record)
         (if (not (eq explain-pause--current-command-record ,record))
             (explain-pause-report-measuring-bug
              ,where
              "current" explain-pause--current-command-record
              "should be equal" ,record)
           ,@body)))))

(defsubst explain-pause--run-measure-hook (new-frame)
  "Finalize frame and send it to the measure hook"
  (setf (explain-pause-command-record-too-slow new-frame)
        (> (explain-pause-command-record-executing-time new-frame)
           explain-pause-slow-too-long-ms))
  (run-hook-with-args 'explain-pause-measured-command-hook new-frame))

(defmacro explain-pause--pause-call-unpause (where new-record-form function-form)
  "Pause current record; create a new record using NEW-RECORD-FORM;
`explain-pause--set-command-call' FUNCTION-FORM; run
`explain-pause-measured-command-hook'; unpause current record. `current-record'
is bound throughout as the current record."
  `(let ((current-record explain-pause--current-command-record))
     (explain-pause--command-record-and-store current-record)

     (let ((new-frame ,new-record-form))
       (explain-pause--set-command-call
        ,where
        new-frame
        ,function-form

        (explain-pause--run-measure-hook new-frame)

        (setf (explain-pause-command-record-entry-snap current-record)
              (current-time))

        (setq explain-pause--current-command-record current-record)))))

(defsubst explain-pause--interactive-form-needs-frame-p (form)
  "Calculate, as quickly as possible, whether this interactive form needs
a native frame."
  ;; Walk through the characters and quit as early as possible.
  (cl-loop
   for char across form
   with next-newline = nil
   do
   (cond
    (next-newline
     (when (eq char ?\n)
       (setq next-newline nil)))

    ((or (eq char ?*)
         (eq char ?^)
         (eq char ?@))
     t)

    ((or (eq char ?p)
         (eq char ?P)
         (eq char ?d)
         (eq char ?U)
         (eq char ?e)
         (eq char ?m)
         (eq char ?i)
         (eq char ?r))
     ;;TODO N
     (setq next-newline t))

    (t
     (cl-return t)))
   finally return nil))

;; `call-interactively' is never called from C code. It is called from
;; `command-execute', defined in `simple.el', which IS called from C code, from
;; `command_loop_1' and `read_char' (`keyboard.c').
;;
;; Of course `call-interactively' is called from a bazillion places in elisp too.
;;
;; When the interactive form of a function is a string, `call-interactively'
;; will call the following:
;;   completing read family:
;;   * `Fread_variable'
;;   * `Fread_non_nil_coding_system' -> calls `Fcompleting_read'
;;   * `Fread_coding_system' -> calls `Fcompleting_read'
;;   * `Fcompleting_read' -> calls elisp `completing-read-function'
;;   buffer:
;;   * `Fread_buffer' -> calls elisp `completing-read-function' OR `read-buffer-function'
;;   char family - all wait and allow timers:
;;   * `Fread_char'
;;   * `Fread_key_sequence'
;;   * `Fread_key_sequence_vector'
;;   read_minibuf family:
;;   * `read-minibuffer' (calls elisp which will call advised code)
;;   * `Fread_string' (calls `recursive_edit')
;;   directly to elisp:
;;   * `read_file_name' -> calls elisp `read-file-name'
;;   * `Qread_number' (calls elisp `read-number')
;; before calling `Qfuncall_interactively'.
;;
;; Therefore, we also advise `funcall-interactively', so we can get at the
;; time when that processing is complete.
;;
;; The call stack when this package is running looks like this:
;;
;; func
;; #<subr funcall-interactively>
;; apply(#<subr funcall-interactively>
;; funcall-interactively <- this is in the original call, unmolested so
;;                          so `called-interactive-p' can find it anyway
;; #<subr call-interactively>
;; apply(#<subr call-interactively>
;; ... <this advice>
;; around(#<subr call-interactively>
;; apply(around #<subr call-interactively>
;; call-interactively
;;
;; Peek at the interactive spec. If it is a string, then we need to push a new
;; native frame to represent the time handling the interactive spec, see above
;; for the native functions that might be called.
;;
;; This replaces the original attempt at using `pre-command-hook' and
;; `post-command-hook', which cannot guarentee matching calls (any old elisp
;; could do something dumb).
(defun explain-pause--wrap-call-interactively (original-func &rest args)
  "Advise call-interactively to track interactive execution costs and show them in
`explain-pause'."
  (let ((parent explain-pause--current-command-record)
        (target-function (car args))
        (command-frame nil)
        (extra-frame nil))

    (unless (eq parent explain-pause-root-command-loop)
      (explain-pause--command-record-and-store parent))

    ;; exclude some very special commands for performance reasons, even
    ;; before doing a string check of their form.
    ;; self-insert-command - spec 'P' - prefix
    ;; newline - spec '*P\np' - prefix
    ;; nextline, prevline - spec '^p\np' - prefix
    ;; delete-forward-char - spec 'p\nP' - prefix
    (unless (or (eq target-function #'self-insert-command)
                (eq target-function #'newline)
                (eq target-function #'next-line)
                (eq target-function #'previous-line)
                (eq target-function #'delete-forward-char))
      (let ((i-spec (cadr (interactive-form target-function))))
        (when (and (stringp i-spec)
                   (explain-pause--interactive-form-needs-frame-p i-spec))

          ;; a bunch of native code will run. we need to push a new frame to
          ;; represent that so that funcall-interactively can pop it correctly
          ;; TODO how to handle completing-read / read-buffer-function?
          (setq command-frame
                (explain-pause--command-record-from-parent
                 parent
                 parent
                 'call-interactively-interactive
                 t))

          (setq extra-frame t))))

    (unless extra-frame
      ;; no fancy stuff, so regular frame:
      (setq command-frame (explain-pause--command-record-from-parent
                           parent
                           parent
                           target-function)))

    ;; can't use set-command-call because we might have to pop two frames at once

    ;; enter command-frame
    (explain-pause-log--send-command-entry parent command-frame)
    (setq explain-pause--current-command-record command-frame)
    (setf (explain-pause-command-record-entry-snap command-frame) (current-time))

    ;; if we are regular frame, profile if needed
    (when (and (not extra-frame)
               (explain-pause--command-record-profile-p command-frame))
      (explain-pause--command-record-start-profiling command-frame))

    (unwind-protect
        (apply original-func args)
      (let ((top-frame explain-pause--current-command-record))
        ;; if there is an extra frame, the top frame is the actual command-frame
        (if extra-frame
            (cond
            ;; if we entered into funcall-interactively,
            ;; the top frame should be a frame with the command = the entry cmd
             ((and (eq (explain-pause-command-record-command top-frame)
                       target-function)
                   (eq (explain-pause-command-record-parent top-frame)
                       command-frame))

              ;; top-frame = the real frame. exit:
              (explain-pause--command-record-and-store top-frame)
              ;; if we profiled, save it:
              (when (explain-pause-command-record-is-profiled top-frame)
                (explain-pause--command-record--save-and-stop-profiling top-frame))
              (explain-pause-log--send-command-exit top-frame)
              (explain-pause--run-measure-hook top-frame)

              ;; exit the parent frame (the command-frame from this function)
              ;; since we don't bother restarting, we don't need to pause-and-store
              (explain-pause-log--send-command-exit command-frame)
              (explain-pause--run-measure-hook command-frame))
             ;; or, if we aborted out of the edit, threw, quit, whatever,
             ;; the top frame is still the call-interactively-interactive frame:
             ((eq top-frame command-frame)
              ;; this is normally done in funcall-interactively before-advice,
              ;; but instead we have to do it here:
              (explain-pause--command-record-and-store top-frame)
              (explain-pause-log--send-command-exit top-frame)
              (explain-pause--run-measure-hook top-frame))
             ;; uhoh
             (t
              (explain-pause-report-measuring-bug
               "call-interactively has extra-frame"
               "top-frame" top-frame
               "target-function" target-function)))

          ;; no extra-frame, top-frame = command-frame
          (if (not (eq top-frame command-frame))
              (explain-pause-report-measuring-bug
               "call interactively frame does not match"
               "command-frame" command-frame
               "should be equal" top-frame)
            ;; exit command-frame:
            (explain-pause--command-record-and-store command-frame)
            ;; if we profiled, save it
            (when (explain-pause-command-record-is-profiled command-frame)
              (explain-pause--command-record--save-and-stop-profiling command-frame))
            (explain-pause-log--send-command-exit command-frame)
            (explain-pause--run-measure-hook command-frame))))

      ;; restart parent
      (unless (eq parent explain-pause-root-command-loop)
        (setf (explain-pause-command-record-entry-snap parent) (current-time)))

      (setq explain-pause--current-command-record parent))))

(defun explain-pause--before-funcall-interactively (&rest args)
  "Run right before `funcall-interactively' so `explain-pause' can track how
much time the native code in `call-interatively' took."
  ;; nothing stops someone from directly calling this function.
  ;; Therefore check to see if the current-command-record is a native one
  ;; that is called 'call-interactively-interactive
  (let ((command (car args))
        (current-record explain-pause--current-command-record))
    (when (and current-record
               (explain-pause-command-record-native current-record)
               (eq (explain-pause-command-record-command current-record)
                   'call-interactively-interactive))
      ;; then we satisfy and can push a new frame for the actual function
      (explain-pause--command-record-and-store current-record)

      ;; enter the real one now, profiling if needed
      (let ((real-frame
             (explain-pause--command-record-from-parent
              current-record
              current-record
              command)))
        (explain-pause-log--send-command-entry current-record real-frame)
        (setq explain-pause--current-command-record real-frame)
        (when (explain-pause--command-record-profile-p real-frame)
          (explain-pause--command-record-start-profiling real-frame))
        (setf (explain-pause-command-record-entry-snap real-frame) (current-time))))))

(defun explain-pause--wrap-native (original-func &rest args)
  "Advise a native function. Insert a new native command record, so we can track
any calls back into elisp."
  (explain-pause--check-not-top-level
   (format "wrap-native for %s" original-func)
   (explain-pause--pause-call-unpause
    (format "wrap-native for %s" original-func)
    (explain-pause--command-record-from-parent
     current-record
     current-record
     original-func
     t)
    (apply original-func args))))

(defun explain-pause--wrap-completing-read-family (original-func &rest args)
  ;; read-command -> Fcompleting_read
  ;; read-function -> Fcompleting_read
  ;; read-variable -> Fcompleting_read - note called from `callint.c'
  ;;   `call_interactively'

  ;; completing-read ->  functions COLLECTION, PREDICATE.
  ;;   directly calls elisp completing-read-function
  ;;   called from `w32fn.c', `x-file-dialog'
  ;;   called from `coding.c', `read-non-nil-coding-system', `read-coding-system'
  ;;   called from `callint.c', `call_interactively'

  ;; this entire family of functions just calls completing-read with maybe 3
  ;; lines of related C, and then `completing-read' just directly calls
  ;; `completing-read-function'.
  ;; don't bother creating a native frame for it. Instead create a regular
  ;; frame for the `completing-read-function' _itself_
  (explain-pause--check-not-top-level
   (format "completing-read for %s" original-func)
   (explain-pause--pause-call-unpause
    (format "completing-read for %s" original-func)
    (explain-pause--command-record-from-parent
     current-record
     current-record
     completing-read-function)
    (apply original-func args))))

(defun explain-pause--wrap-read-buffer (original-func &rest args)
  "Wrap read-buffer in particular, as it calls one of two completion functions
depending on the arguments."
  (explain-pause--check-not-top-level
   "read-buffer"
   (explain-pause--pause-call-unpause
    "read-buffer"
    (explain-pause--command-record-from-parent
     current-record
     current-record
     ;; read-buffer picks based on whether `read-buffer-function' is nil
     (or read-buffer-function
         completing-read-function))
    (apply original-func args))))

;; timer or process io hooks code
(defun explain-pause--wrap-callback
    (parent-command-record original-cb &rest args)
  "Wrap a callback, so that when THIS function is called, we call the original
callback with a new command record whose parent is PARENT-COMMAND-RECORD."
  ;; This function will still be called even if the mode is off if the callback
  ;; was wrapped when the mode was on. check and exit if so:
  (if (not explain-pause-mode)
      (apply original-cb args)

    ;; the parent represents where we came from, which may or may not have
    ;; been profiled, but we are now executing in a new context - all wrappers
    ;; are either timers, process, etc.
    (explain-pause--pause-call-unpause
     (format "wrap callback for %s" original-cb)
     (explain-pause--command-record-from-parent
      current-record
      parent-command-record
      original-cb)
     (apply original-cb args))))

(defun explain-pause--generate-wrapper (parent-command-record original-callback)
  "Generate a lambda wrapper for use when we cannot pass additional parameters
ala `run-with-timer', e.g. in `make-process' and co.

PARENT-COMMAND-RECORD should describe the execution context when this wrapper
was generated. ORIGINAL-CALLBACK is the function to be wrapped."
  (lambda (&rest callback-args)
    (apply 'explain-pause--wrap-callback
           parent-command-record
           original-callback
           callback-args)))

(defun explain-pause--wrap-file-notify-add-watch (args)
  "Advise that modifies the arguments ARGS to `file-notify-add-watch' by
wrapping the callback"
  `(,@(seq-take args 2)
    ,(explain-pause--generate-wrapper
      (explain-pause--command-record-from-parent
       explain-pause--current-command-record
       explain-pause--current-command-record
       'file-notify
       t)
      (nth 2 args))))

(defun explain-pause--wrap-make-process (original-func &rest args)
  "Wrap the sentinel and process arguments inside ARGS to `make-process', if
any."
  ;; we are assuming make-process is fast enough not to subtract from current command.
  (let* ((current-record explain-pause--current-command-record)

         (process-name (plist-get args :name))
         (stderr-arg (plist-get args :stderr))

         ;; this represents the process itself
         (process-frame
          (explain-pause--command-record-from-parent
           current-record
           current-record
           process-name))

         (original-filter (plist-get args :filter))

         (wrapped-filter
          (when original-filter
            (explain-pause--generate-wrapper
             (explain-pause--command-record-from-parent
              process-frame
              process-frame
              'process-filter)
             original-filter)))

         (original-sentinel (plist-get args :sentinel))

         (wrapped-sentinel
          (when original-sentinel
            (explain-pause--generate-wrapper
             (explain-pause--command-record-from-parent
              process-frame
              process-frame
              'process-sentinel)
             original-sentinel)))

         (new-args (copy-sequence args)))

    (when wrapped-filter
      (setq new-args (plist-put new-args :filter wrapped-filter)))
    (when original-sentinel
      (setq new-args (plist-put new-args :sentinel wrapped-sentinel)))

    (let ((process (apply original-func new-args)))
      (when process
        ;; store the process frame in a process variable so later we can get at it
        ;; for new filters
        (process-put process 'explain-pause-process-frame process-frame)

        ;; store the original filters and sentinels so we can return them out,
        ;; if not nil
        (when original-filter
          (process-put process 'explain-pause-original-filter original-filter))
        (when original-sentinel
          (process-put process 'explain-pause-original-sentinel original-sentinel))

        ;; if there was a stderr argument, and the stderr argument was not a
        ;; process, then the stderr process was created in make-process
        ;; directly calling Fmake_pipe_process in C code. Pull that newly
        ;; made process out, and retroactively give it a process-frame.
        ;; note that the native call does not give it filters or anything
        ;; fancy we need to account for. (process.c)
        ;; stderr is only supported for make-process, not make-network-process
        ;; or make-pipe-process, but this value will be nil in those calls,
        ;; so we can handle all cases here.
        (when (and stderr-arg
                   (not (processp stderr-arg)))
          ;; exactly mirror the C code here
          (let* ((stderr-buffer (get-buffer-create stderr-arg))
                 (stderr-proc (get-buffer-process stderr-buffer))
                 (stderr-process-frame
                  (explain-pause--command-record-from-parent
                   current-record
                   current-record
                   (format "%s - %s" process-name stderr-arg))))

            ;; this might be nil if the process didn't start, or the buffer
            ;; didn't exist and was created, etc. let's be defensive. if the
            ;; process is nil anyway no one can find the process any other way,
            ;; so user code can't try to set process filters on it.
            (when stderr-proc
              (process-put stderr-proc
                           'explain-pause-process-frame
                           stderr-process-frame)))))

      process)))

(defun explain-pause--wrap-set-process-filter-callback (orig &rest args)
  "Advise that wraps `set-process-filter' so the callback is wrapped."
  ;; be careful to set the saved filter value AFTER the call, so if it
  ;; throws, we avoid changing it.
  (seq-let [arg-process original-callback] args
    (if (not original-callback)
        (let ((result (apply orig args)))
          (process-put arg-process 'explain-pause-original-filter nil)
          result)
      (let* ((process-frame (process-get arg-process 'explain-pause-process-frame))
             (result
              (apply orig
                     (list arg-process
                     (explain-pause--generate-wrapper
                      ;; the parent of the new record is the original process, NOT
                      ;; the caller
                      (explain-pause--command-record-from-parent
                       process-frame
                       process-frame
                       'process-filter)
                      original-callback)))))
        (process-put arg-process 'explain-pause-original-filter original-callback)
        result))))

(defun explain-pause--wrap-set-process-sentinel-callback (orig &rest args)
  "Advise that wraps `set-process-sentinel' so the callback is wrapped."
  ;; be careful to set the saved sentinel value AFTER the call, so if it
  ;; throws, we avoid changing it.
  (seq-let [arg-process original-callback] args
    (if (not original-callback)
        (let ((result (apply orig args)))
          (process-put arg-process 'explain-pause-original-sentinel nil)
          result)
      (let* ((process-frame (process-get arg-process 'explain-pause-process-frame))
             (result
              (apply orig
                     (list arg-process
                     (explain-pause--generate-wrapper
                      ;; the parent of the new record is the original process, NOT
                      ;; the caller
                      (explain-pause--command-record-from-parent
                       process-frame
                       process-frame
                       'process-sentinel)
                      original-callback)))))
        (process-put arg-process 'explain-pause-original-sentinel original-callback)
        result))))

(defun explain-pause--wrap-get-process-filter (orig &rest args)
  "Advise `process-filter' so it returns the unwrapped, original filter, so
comparisions still work."
  (let ((original-filter (process-get (car args) 'explain-pause-original-filter)))
    ;; it might be nil: a default filter, or the process has not been called with
    ;; set-process-filter with our advised callback, e.g. a long lived process
    ;; that started before the mode was activated.
    (if original-filter
        original-filter
      (apply orig args))))

(defun explain-pause--wrap-get-process-sentinel (orig &rest args)
  (let ((original-sentinel (process-get (car args) 'explain-pause-original-sentinel)))
    ;; it might be nil: a default filter, or the process has not been called with
    ;; set-process-filter with our advised callback, e.g. a long lived process
    ;; that started before the mode was activated.
    (if original-sentinel
        original-sentinel
      (apply orig args))))

(defun explain-pause--wrap-set-process-plist (args)
  "Advise set-process-plist so that command-frame information is preserved.

Explain-pause-mode sets several values in the plist, which must be preserved if
the plist is reset by user code."
  (when (car args)
    (let* ((proc (car args))
           (new-plist (cadr args))
           (existing-plist (process-plist proc)))
      (cl-loop
       for key in '(explain-pause-original-sentinel
                    explain-pause-original-filter
                    explain-pause-process-frame)
       do
       ;; only replace the key if it isn't already being set
       (unless (plist-get new-plist key)
         (plist-put new-plist key
                    (plist-get existing-plist key))))))
  args)

(defconst explain-pause--timer-frame-max-depth 64
  "The maximum depth a record chain for a timer can get.")

(defsubst explain-pause--generate-timer-parent (cb record kind)
  "Generate either a new frame for this timer callback or reuse the parent frame
if the call is recursively to ourselves.

The parent frame is only reused if it is a native frame of the right kind. In
`explain-pause--wrap-callback, the new frames uses the *current* frame at the
time of callback to decide whether profiling is on or not, so the state of
profiiling of the reused frame doesn't matter.

If the depth is too high (larger then `explain-pause--timer-frame-max-depth'
rewind the stack to the first timer of the same kind and start from there
again. This works for timers because we never unwind the parent stack in wrapper
handler. If even after doing this the depth is too high, just reroot at
emacs root."
  (or
   (when (eq (explain-pause-command-record-command record) cb)
     (let ((parent (explain-pause-command-record-parent record)))
       (when (and parent
                  (explain-pause-command-record-native parent)
                  (eq (explain-pause-command-record-command parent) kind))
         parent)))
   (when (> (explain-pause-command-record-depth record)
            explain-pause--timer-frame-max-depth)
     ;; walk back to the very first timer call
     (let ((latest-best record)
           (current record))
       (while current
         (when (and (explain-pause-command-record-native current)
                    (eq (explain-pause-command-record-command current)
                        kind))
           (setq latest-best current))
         (setq current (explain-pause-command-record-parent current)))

       (if (> (explain-pause-command-record-depth latest-best)
              explain-pause--timer-frame-max-depth)
           ;; after all that, we're still too long?!
           ;; just start from root:
           (explain-pause--command-record-from-parent
            record
            explain-pause-root-command-loop
            kind
            t)
         latest-best)))
   (explain-pause--command-record-from-parent
    record
    record
    kind
    t)))

(defun explain-pause--wrap-idle-timer-callback (args)
  "Advise that modifies the arguments ARGS to `run-with-idle-timer' by wrapping
the callback."
  `(,@(seq-take args 2)
    explain-pause--wrap-callback
    ;; make a new frame to represent the native timer, though we
    ;; don't ever increment this frame
    ,(explain-pause--generate-timer-parent
      (nth 2 args)
      explain-pause--current-command-record
      'idle-timer)
    ,@(seq-drop args 2)))

(defun explain-pause--wrap-timer-callback (args)
  "Advise that modifies the arguments ARGS to `run-with-timer' by wrapping the
callback."
  `(,@(seq-take args 2)
    explain-pause--wrap-callback
    ,(explain-pause--generate-timer-parent
      (nth 2 args)
      explain-pause--current-command-record
      'timer)
    ,@(seq-drop args 2)))

(defun explain-pause--wrap-existing-timer-list (list kind)
  "Wrap an an existing timer list, so they are setup ready for further calls."
  ;; hypothetically, we're reaching into timer internals.
  ;; YOLO!
  (dolist (timer list)
    (unless (eq (timer--function timer) 'explain-pause--wrap-callback)
      (let ((original-func (timer--function timer))
            (original-args (timer--args timer)))
        (timer-set-function timer #'explain-pause--wrap-callback
                            `(,(explain-pause--generate-timer-parent
                                original-func
                                ;; we don't know where it came from
                                explain-pause-root-command-loop
                                kind)
                              ,original-func
                              ,@original-args))))))

(defun explain-pause--wrap-existing-timers ()
  "Wrap existing timers so they are setup ready for futher calls."
  (explain-pause--wrap-existing-timer-list timer-list 'timer)
  (explain-pause--wrap-existing-timer-list timer-idle-list 'idle-timer))

(defun explain-pause--wrap-existing-processes ()
  "Wrap existing processes so they are setup ready for further calls."
  ;; be careful if we are being run again, e.g. enable-disable-enable;
  ;; check to see if the process has already been marked by us.
  (dolist (process (process-list))
    (unless (process-get process 'explain-pause-process-frame)
      (let* ((name (process-name process))
             (original-filter (process-filter process))
             (original-sentinel (process-sentinel process))
             (process-frame
              (explain-pause--command-record-from-parent
               ;; we don't really know where it was started from, use root
               explain-pause--current-command-record
               explain-pause-root-command-loop
               name)))

        (process-put process 'explain-pause-process-frame process-frame)
        ;; call set which has been advised by this point:
        (set-process-filter process original-filter)
        (set-process-sentinel process original-sentinel)))))

(defconst explain-pause--native-called-hooks
  '(post-command-hook pre-command-hook delayed-warnings-hook
                      echo-area-clear-hook post-gc-hook
                      disabled-command-function))

(defsubst explain-pause--generate-cb-wrapper (cb-func cb-type)
  "Generate a lambda wrapper for advice to wrap the function HOOK-FUNC so it
generates a frame as HOOK-LIST when called. We grab the original symbol
of HOOK-FUNC, so we can refer to the symbol if possible."
  ;; TODO perhaps dry with explain-pause--wrap-callback and
  ;; explain-pause--lambda-cb-wrapper
  (lambda (orig-func &rest args)
    (if (not explain-pause-mode)
        (apply orig-func args)

      (explain-pause--pause-call-unpause
       (format "wrap hook %s for %s" cb-func cb-type)
       ;; it would be nice to avoid this let but in cb people make them
       ;; re-entrant and just call them randomly. be defensive.
       (let ((parent
              (explain-pause--command-record-from-parent
               current-record
               ;; the parent is root because we are being called from
               ;; native code outside the command loop. TODO it's possible
               ;; we can figure out more accurately?
               explain-pause-root-command-loop
               cb-type)))
         (explain-pause--command-record-from-parent
          parent
          parent
          cb-func))
       (apply orig-func args)))))

(defun explain-pause--lambda-cb-wrapper (cb-func cb-type &rest args)
  "A named function so we can find whether we have wrapped a lambda."
  (if (not explain-pause-mode)
      (apply cb-func args)

    (explain-pause--pause-call-unpause
     (format "wrap hook (named wrapper) %s for %s" cb-func cb-type)
     ;; it would be nice to avoid this let but in hooks people make them
     ;; re-entrant and just call them randomly. be defensive.
     (let ((parent
            (explain-pause--command-record-from-parent
             current-record
             ;; TODO this might be called from non-root, too
             ;; the parent is root because we are being called from
             ;; native code outside the command loop. TODO it's possible
             ;; we can figure out more accurately?
             explain-pause-root-command-loop
             cb-type)))
       (explain-pause--command-record-from-parent
        parent
        parent
        cb-func))
     (apply cb-func args))))

;; seq-contains deprecated emacs >27
(defalias 'explain-pause--seq-contains
  (eval-when-compile
    (if (fboundp 'seq-contains-p)
        'seq-contains-p
      'seq-contains)))

(defsubst explain-pause--advice-add-cb (cb-func cb-type)
  "Add a hook-wrapper advice for CB for type TYPE, naming the
lambda advice so we can reference it later."
  (cond
   ((symbolp cb-func)
    ;; directly advisable
    (advice-add cb-func :around
                (explain-pause--generate-cb-wrapper cb-func cb-type)
                ;; it might be that a function could be in multiple hooks or callbacks
                ;; we don't want multiple advices to be called if it is called
                ;; so use a generic name
                ;; this means that only one wins and we lose information, but at least
                ;; we aren't wrong
                '((name . "explain-pause-wrap-hook")))
    cb-func)
   (t
    ;; ok, whatever it is, wrap it normally and hope for the best.
    ;; it must be "funcall"-able or run-hook will have failed anyway.
    (cond
     ((and (listp cb-func)
           (listp (nth 3 cb-func))
           ;; TODO perhaps we could do some fancy pcase stuff here.
           (equal (nth 1 (nth 3 cb-func))
                  '(function explain-pause--lambda-cb-wrapper)))
        ;; we did it already
      cb-func)
     ((and (byte-code-function-p cb-func)
           (equal (aref cb-func 1) "\xc2\xc3\xc0\xc1\x4\x24\x87")
           (explain-pause--seq-contains (aref cb-func 2)
                                        'explain-pause--lambda-cb-wrapper))
      ;; we did it already, bytecompiled
      cb-func)
     (t
      (lambda (&rest args)
        (apply #'explain-pause--lambda-cb-wrapper cb-func cb-type args)))))))

(defun explain-pause--wrap-add-hook (args)
  "Advise add-hook to advise the hook itself to add a frame when called from
native code outside command loop."
  (let ((hook-list (nth 0 args))
        (hook-func (nth 1 args)))
    (when (and (explain-pause--seq-contains
                explain-pause--native-called-hooks hook-list)
               (functionp hook-func))
      (setf (nth 1 args)
            (explain-pause--advice-add-cb hook-func hook-list))))
  args)

(defun explain-pause--wrap-remove-hook (args)
  "Advise remove-hook to wrap the hook to remove if it is a lambda, so it
can be found and removed normally."
  (let ((hook-list (nth 0 args))
        (hook-func (nth 1 args)))
    (when (and (explain-pause--seq-contains
                explain-pause--native-called-hooks hook-list)
               (functionp hook-func)
               (not (symbolp hook-func)))
      (setf (nth 1 args)
            (explain-pause--advice-add-cb hook-func hook-list))))
  args)

(defun explain-pause--wrap-existing-hooks-in-list (hook-kind hook-list)
  "Wrap existing hooks in HOOK-LIST with WRAP-FUNC."
  (cond
   ((listp hook-list)
    (cl-loop
     for hook in-ref hook-list
     do
     (when (functionp hook)
       (setf hook
             (explain-pause--advice-add-cb hook hook-kind))))
    hook-list)
   (t
    (explain-pause--advice-add-cb hook-list hook-kind))))

(defun explain-pause--wrap-existing-hooks ()
  "Wrap existing hooks in hook lists that are called from native code outside
command loop, for both the default value and all buffer local values."
  (dolist (hook-list explain-pause--native-called-hooks)
    (let ((default-list (default-value hook-list)))
      ;; the default value
      (explain-pause--wrap-existing-hooks-in-list hook-list default-list)
      ;; for each buffer
      (cl-loop
       for buffer being the buffers
       do
       (let ((local-value (buffer-local-value hook-list buffer)))
         (unless (equal local-value default-list)
           (explain-pause--wrap-existing-hooks-in-list
            hook-list
            local-value)))))))

(defun explain-pause--wrap-define-key (args)
  "Advise define key for the special keymaps when the translation is a function."
  ;; TODO also could be a keymap completely
  ;; also other kinds
  (when (functionp (nth 2 args))
    (let* ((map (car args))
           (kind
            (cond
             ((eq map key-translation-map)
              'key-translation-map)
             ((eq map function-key-map)
              'function-key-map)
             ((eq map input-decode-map)
              'input-decode-map)
             ((eq map local-function-key-map)
              'local-function-key-map)
             (t
              nil))))
      (when kind
        (setf (nth 2 args)
              (explain-pause--advice-add-cb (nth 2 args) kind)))))
  args)

(defun explain-pause--wrap-existing-special-keymap (keymap keymap-kind)
  "Walk a keymap and find all functions and wrap them."
  (when (and (symbolp keymap)
             (autoloadp (symbol-function keymap)))
    (setq keymap (autoload-do-load (symbol-function keymap) keymap)))

  (let ((map (cdr keymap)))
    (while map
      (if (keymapp map)
          ;; parent
          (explain-pause--wrap-existing-special-keymap map keymap-kind)
        ;; binding
        (let ((value (cdar map)))
          (cond
           ((keymapp value)
            (explain-pause--wrap-existing-special-keymap value keymap-kind))
           ;; TODO support cons ("string" . DEF)
           ;; support cons (MAP . char)
           ((functionp value)
            (setf (cdar map)
                  (explain-pause--advice-add-cb value keymap-kind))))))
      (setq map (cdr map)))))

(defun explain-pause--wrap-terminal-local-special-keymaps ()
  "Wrap terminal local special keymaps, and record the wrapped state."
  (explain-pause--wrap-existing-special-keymap input-decode-map 'input-decode-map)
  (explain-pause--wrap-existing-special-keymap local-function-key-map
                                               'local-function-key-map)
  (set-terminal-parameter nil 'explain-pause-keymaps-hooked t))

(defun explain-pause--wrap-existing-special-keymaps ()
  "Wrap existing functions in the special keymaps that are called from native code."
  (explain-pause--wrap-existing-special-keymap key-translation-map 'key-translation-map)
  (explain-pause--wrap-existing-special-keymap function-key-map 'function-key-map)
  ;; do all the existing terminals
  (cl-loop
   for frame in (frame-list)
   do
   (explain-pause--wrap-new-terminal-special-keymaps frame)))

(defun explain-pause--wrap-new-terminal-special-keymaps (frame)
  "When a new frame is created, wrap any special keymaps if it is a new terminal."
  (let ((terminal (frame-terminal frame)))
    (unless (terminal-parameter terminal 'explain-pause-keymaps-hooked)
      (if (eq (selected-frame)
              frame)
          (explain-pause--wrap-terminal-local-special-keymaps))
        (let ((current-frame (selected-frame)))
          (select-frame frame t)
          (explain-pause--wrap-terminal-local-special-keymaps)
          (select-frame current-frame t)))))

(eval-and-compile
  (let ((callback-family
         '(
           ;; these are functions who setup callbacks which can be wrapped.
           (run-with-idle-timer . explain-pause--wrap-idle-timer-callback)
           (run-with-timer . explain-pause--wrap-timer-callback)))
        (callback-around-family
         '(
           ;; timing callbacks, but they need around advice.
           (set-process-filter . explain-pause--wrap-set-process-filter-callback)
           (set-process-sentinel . explain-pause--wrap-set-process-sentinel-callback)))
        (make-process-family
         ;; These C functions start async processes, which raise callbacks
         ;; `filter' and `sentinel'. Wrap those.
         '(make-process
           make-pipe-process
           make-network-process))
        (native
         '(
           ;; These C functions ultimately call `read_char' which will run timers,
           ;; redisplay, and call `sit_for'.
           read-key-sequence
           read-key-sequence-vector
           read-char
           read-char-exclusive
           read-event
           ;; Menu bar function that ultimately calls `read_key_sequence' which
           ;; calls `read_char'.
           x-popup-menu
           ;; These C functions ultimately call `read_minibuf' which will call
           ;; `recursive_edit' (in C), which means they will call
           ;; `call-interactively' (which we have advised.)
           ;; read-from-minibuffer -> read_minibuf
           ;; read-string -> Fread_from_minibuffer -> read_minibuf
           ;; read-no-blanks-input -> read_minibuf
           read-from-minibuffer
           read-string
           read-no-blanks-input
           ;; recursive edit ultimately calls `command-loop' and unwinds out
           ;; either to the call site or to top level
           recursive-edit))
        (completing-read-family
         '(
           ;; These C functions ultimately call `completing_read' which will
           ;; call `completing-read-function'.
           read-command
           read-function
           read-variable
           completing-read))
        (install-attempt 0)
        (is-installed nil))

    (defun explain-pause-mode--install-hooks ()
      "Actually install hooks for `explain-pause-mode'."
      (advice-add 'call-interactively :around
                  #'explain-pause--wrap-call-interactively)
      (advice-add 'funcall-interactively :before
                  #'explain-pause--before-funcall-interactively)

      (advice-add 'add-hook :filter-args
                  #'explain-pause--wrap-add-hook)
      (advice-add 'remove-hook :filter-args
                  #'explain-pause--wrap-remove-hook)
      (advice-add 'define-key :filter-args
                  #'explain-pause--wrap-define-key)

      ;; OK, we're prepared to advise native functions and timers:
      (dolist (native-func native)
        (advice-add native-func :around
                    #'explain-pause--wrap-native))

      (dolist (completing-read-func completing-read-family)
        (advice-add completing-read-func :around
                    #'explain-pause--wrap-completing-read-family))

      (advice-add 'read-buffer :around #'explain-pause--wrap-read-buffer)

      (dolist (process-func make-process-family)
        (advice-add process-func :around
                    #'explain-pause--wrap-make-process))

      (advice-add 'process-filter :around #'explain-pause--wrap-get-process-filter)
      (advice-add 'process-sentinel :around #'explain-pause--wrap-get-process-sentinel)
      (advice-add 'set-process-plist :filter-args
                  #'explain-pause--wrap-set-process-plist)

      (dolist (callback-func callback-family)
        (advice-add (car callback-func) :filter-args (cdr callback-func)))

      (dolist (callback-func callback-around-family)
        (advice-add (car callback-func) :around (cdr callback-func)))

      (advice-add 'file-notify-add-watch :filter-args
                  #'explain-pause--wrap-file-notify-add-watch)

      (setq explain-pause--current-command-record
            explain-pause-root-command-loop)

      (add-hook 'explain-pause-measured-command-hook
                #'explain-pause-profile--profile-measured-command)

      (explain-pause--wrap-existing-processes)
      (explain-pause--wrap-existing-timers)
      (explain-pause--wrap-existing-hooks)
      (explain-pause--wrap-existing-special-keymaps)

      (add-hook 'after-make-frame-functions
                #'explain-pause--wrap-new-terminal-special-keymaps
                t) ;; append so we run after select-frame

      (when explain-pause-log--send-process
        (explain-pause-log--send-dgram
         "(\"enabled\")\n"))

      (message "Explain-pause-mode enabled."))

    (defun explain-pause-mode--try-enable-hooks ()
      "Attempt to install `explain-pause-mode' hooks."
      (unless is-installed
        (setq is-installed t)
        (setq install-attempt 0)
        (explain-pause-mode--enable-hooks)))

    (defun explain-pause-mode--enable-hooks ()
      "Install hooks for `explain-pause-mode' if it is being run at the top of the
emacs loop, e.g. not inside `call-interactively' or `sit-for' or any interleaved
timers, etc. Otherwise, try again."
      (remove-hook 'post-command-hook #'explain-pause-mode--enable-hooks)
      (condition-case err
          (if (> install-attempt 5)
              (progn
                (message "Unable to install `explain-pause-mode'. please report a bug to \
github.com/lastquestion/explain-pause-mode")
                (setq is-installed nil)
                (setq explain-pause-mode nil))
            (let ((top-of-loop t))
              ;; do not install if we are not top of loop
              ;;
              ;; this covers init.el install or command line args, because command_loop
              ;; calls post-command-hook as the first thing it does.
              (mapbacktrace (lambda (_evaled func _args _flags)
                              (unless (or (eq func 'explain-pause-mode--enable-hooks)
                                          (eq func 'explain-pause-mode--try-enable-hooks))
                                (setq top-of-loop nil)))
                            #'explain-pause-mode--enable-hooks)
              (cond
               ((not top-of-loop)
                ;; we were run via a timer, a call interactively, or startup, etc. etc. etc
                ;; install on next post-command-hook.
                ;; ignore commands until we're out of recursive edits
                (when (eq 0 (recursion-depth))
                  (setq install-attempt (1+ install-attempt)))
                ;; add ourselves to the end, so that all other hooks run. This way we can
                ;; modify the list itself without worrying about the copy native code has
                ;; in command_loop.
                (add-hook 'post-command-hook #'explain-pause-mode--enable-hooks t))
               (top-of-loop
                ;; ok, we're safe:
                (explain-pause-mode--install-hooks)))))
        (error
         (explain-pause-report-measuring-bug
          "Installation of explain-pause-mode failed"
          "error"
          err))))

    (defun explain-pause-mode--disable-hooks ()
      "Disable hooks installed by `explain-pause-mode--install-hooks'."
      (setq is-installed nil)

      (remove-hook 'after-make-frame-functions
                   #'explain-pause--wrap-new-terminal-special-keymaps)

      (advice-remove 'file-notify-add-watch
                     #'explain-pause--wrap-file-notify-add-watch)

      (dolist (callback-func callback-family)
        (advice-remove (car callback-func) (cdr callback-func)))

      (dolist (callback-func callback-around-family)
        (advice-remove (car callback-func) (cdr callback-func)))

      (advice-remove 'process-filter #'explain-pause--wrap-get-process-filter)
      (advice-remove 'process-sentinel #'explain-pause--wrap-get-process-sentinel)
      (advice-remove 'set-process-plist #'explain-pause--wrap-set-process-plist)
      (dolist (process-func make-process-family)
        (advice-remove process-func
                       #'explain-pause--wrap-make-process))

      (advice-remove 'read-buffer #'explain-pause--wrap-read-buffer)

      (dolist (completing-read-func completing-read-family)
        (advice-remove completing-read-func
                       #'explain-pause--wrap-completing-read-family))

      (dolist (native-func native)
        (advice-remove native-func
                       #'explain-pause--wrap-native))

      (advice-remove 'define-key
                     #'explain-pause--wrap-define-key)

      (advice-remove 'add-hook
                     #'explain-pause--wrap-add-hook)
      (advice-remove 'add-hook
                     #'explain-pause--wrap-remove-hook)

      (advice-remove 'call-interactively
                     #'explain-pause--wrap-call-interactively)

      (advice-remove 'funcall-interactively
                     #'explain-pause--before-funcall-interactively))))

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

This mode hooks `call-interactively', both idle and regular timers, and process
filters and sentinels.

When running interactively, e.g. run from `M-x' or similar, `explain-pause-mode'
must install itself after some time while Emacs is not doing anything."
  :global t
  :init-value nil
  :lighter " explain-pause"
  :keymap nil

  (cond
   (explain-pause-mode
    (explain-pause-mode--try-enable-hooks))
   (t
    (explain-pause-mode--disable-hooks))))

;;;###autoload
(defun explain-pause-top ()
  "Show a top-like report of commands recently ran and their runtime. Returns
the buffer."
  (interactive)

  (let ((buffer (explain-pause-top--get-default-buffer)))
      ;; this will call resize and that will refresh as width is 0
      (display-buffer buffer)
      buffer))

;;;###autoload
(defun explain-pause-log-to-socket (&optional file-socket)
  "Log the event stream to a UNIX file socket, FILE-SOCKET. If FILE-SOCKET is nil,
then the default location `explain-pause-default-log' is used. This file socket
should already exist. It might be created by `explain-pause-socket' in another
Emacs process, in which case `explain-mode-top-from-socket' will receive and
present that data. Or you can simply receive the data in any other process that
can create UNIX sockets, for example `netcat'.To turn off logging, run
`explain-pause-log-off'.

The stream is written as newline delimited elisp readable lines. See
`explain-pause-log--send-*' family of commands for the format of those objects.

Returns the process that is connected to the socket."
  (interactive)
  (unless file-socket
    (setq file-socket explain-pause-logging-default-log-location))
  (when explain-pause-log--send-process
    (explain-pause-log-off))
  (setq explain-pause-log--send-process
        (make-network-process
         :name "explain-pause-log-send"
         :family 'local
         :service file-socket
         :type 'datagram))
  explain-pause-log--send-process)

(defun explain-pause-log-off ()
  "Turn off logging of the event stream."
  (interactive)
  (when explain-pause-log--send-process
    (let ((save-process explain-pause-log--send-process))
      (setq explain-pause-log--send-process nil)
      (delete-process save-process))))

(provide 'explain-pause-log-to-socket)
(provide 'explain-pause-top)
(provide 'explain-pause-mode)

;;; explain-pause-mode.el ends here
