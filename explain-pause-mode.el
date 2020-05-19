;;; explain-pause-mode.el --- try to explain emacs pauses -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Lin Xu

;; Author: Lin Xu <lim@lastquestion.org>
;; Package-Requires: ((dash "2.12.0"))
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

;; `explain-pause-mode' is a minor mode that measures and explains when Emacs has
;; paused doing work for a long time.  Any user input during a pause is not processed
;; until it is complete.  So in other words, `explain-pause-mode' tries to explain
;; sources of user latency.  When many pauses of the same kind occur, it also
;; generates profiling reports that can be investigated immediately or sent to
;; developers.

;; Please see README.md for commentary, documentation, etc. in the repository
;; above.

;;; Code:

(require 'dash)
(require 'profiler)

;; customizable behavior
(defgroup explain-pause nil
  "Explain pauses in Emacs"
  :prefix "explain-pause-")

(defcustom explain-pause-blocking-too-long-ms 40
  "When some work in Emacs takes longer then this many milliseconds, explain-pause will tell you."
  :type 'integer)

(defcustom explain-pause-log-all-input-loop nil
  "Should all command loop executions be logged? WARNING: Very noisy!"
  :type 'boolean)

(defcustom explain-pause-log-all-timers nil
  "Should all timer executions be logged? WARNING: Very noisy!"
  :type 'boolean)

(defcustom explain-pause-log-all-process-io nil
  "Should all process filter executions be logged? WARNING: Very noisy!"
  :type 'boolean)

(defcustom explain-pause-alert-via-message t
  "Should explain-pause alert you to long pauses in the mini-buffer?"
  :type 'boolean)

(defcustom explain-pause-profile-blocking-threshold 3
  "How many times does a blocking command or function have to happen before it is profiled?"
  :type 'integer)

(defcustom explain-pause-profile-enabled t
  "Should explain-pause profile blocking work when it occurs enough times?"
  :type 'boolean)

(defcustom explain-pause-profile-cpu-sampling-interval 200000
  "The CPU sampling interval when the profiler is activated in microseconds.
The default value is 2ms."
  :type 'integer)

(defcustom explain-pause-profile-saved-profiles 5
  "The number of CPU profiles to save, after which the oldest is removed.
If you change this number, run `explain-pause-profiles-clear' to adjust
the buffer size (but you will lose the current profiles)."
  :type 'integer
  :set (lambda (symbol val)
         (set-default symbol val)
         (explain-pause-profiles-clear))
  :initialize 'custom-initialize-default)

(defun explain--as-ms-exact (time)
  "Returns the TIME object in exact milliseconds, ignoring picoseconds."
  (-let [(high-seconds low-seconds microseconds) time]
    (+ (* (+ (* high-seconds 65536) low-seconds) 1000) (/ microseconds 1000))))

;; logging functions
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

(let ((delayed-notifications '())
      (delayed-notification-timer nil))
  (defun explain--log (pause-ms str)
    "Log the message to the explain buffer at the end, optionally with a
short message to the echo area. If the minibuffer is in use, add the duration
to the delayed duration list."
    (explain--write-to-log str)
    (when explain-pause-alert-via-message
      (if (active-minibuffer-window)
          (setq delayed-notifications (-snoc delayed-notifications pause-ms))
        (explain--alert-delays pause-ms))))

  (defun explain--alert-delays-timer ()
    "display the delayed notifications, unless we're back in a minibuffer,
in which case do nothing; the next exit will reschedule."
    (unless (active-minibuffer-window)
      (explain--alert-delays delayed-notifications)
      (setq delayed-notifications '())
      (setq delayed-notification-timer nil)))

  (defun explain--delayed-logs-reset ()
    (setq delayed-notifications '())
    (when delayed-notification-timer
      (cancel-timer delayed-notification-timer)
      (setq delayed-notification-timer nil)))

  (defun explain--schedule-delayed-logs ()
    "Schedule or reschedule the delayed logs timer. Should be called when
the minibuffer is closed."
    (when delayed-notifications
      (when delayed-notification-timer
        (cancel-timer delayed-notification-timer))
      (setq delayed-notification-timer (run-with-timer 0.5 nil #'explain--alert-delays-timer)))))

(defun explain--alert-profile (commandset)
  "Write a log about the profile that just occurred for COMMANDSET, and alert it, too."
  (let ((msg
         (format "Blocking call %s was profiled! Run `explain-pause-profiles' to learn more"
                 (explain--command-set-as-string commandset))))
    (explain--write-to-log msg t)
    (message msg)))

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

(defun explain--function-as-string (cmd)
  "Generate a human readable string for a command CMD that might be bytecode.
Otherwise print it as a string."
  (if (byte-code-function-p cmd)
      (format "<bytecode> (%s, %s)" (aref cmd 2) (documentation cmd))
    (prin1-to-string cmd t)))

(defun explain--command-set-as-string (command-set)
  "Format a COMMAND-SET as a human readable string.
A command set is a list of commands that represent the context that lead to the
blocking execution (or we think so, anyway)."
  (mapconcat
   #'explain--function-as-string
   command-set ", "))

(defun explain--alert-delays (ms-or-array)
  "Display an alert message of duration(s) MS-OR-ARRAY."
  (let ((ms-str
         (if (listp ms-or-array)
             (mapconcat #'prin1-to-string ms-or-array ", ")
           (prin1-to-string ms-or-array))))
    (message "Emacs blocked for %s ms - check *explain-pause-log*" ms-str)))

(defun explain--log-pause (diff sit-wait-ms command-set log-current-buffer buffer-difference)
  "Log the pause to the log.

DIFF is the ms duration of the pause.
SIT-WAIT-MS is the ms duration of any sit-fors.
COMMAND-SET is the command-set that paused.
if LOG-CURRENT-BUFFER or BUFFER-DIFFERENCE are not nil, they are logged.  These are buffer objects."
  ;; use sprintf, it's probably faster (...eh)
  (let ((sit-wait-str
         (if (> sit-wait-ms 0)
             (format " (sit-for %s ms)" sit-wait-ms)
           ""))
        (buffer-difference-str
         (if buffer-difference
             (format " (new buffers [%s])" (explain--buffers-as-string buffer-difference))
           ""))
        (current-buffer-str
         (if log-current-buffer
             (format " [%s]" (explain--buffer-as-string))
           ""))
        (commandset-str (explain--command-set-as-string command-set)))

    (explain--log diff
                  (format "%d ms%s - %s%s%s\n"
                          diff
                          sit-wait-str
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
         (-each (ring-elements profiles)
           (lambda (profile)
             (-let (((time-stamp diff command-set profile) profile))
               ;; TODO maybe a nicer table or something? There's only a handful of items though.
               (insert (format "Slow profile report\n  Time: %s\n  Command: %s\n  Duration: %d ms\n\n"
                               (current-time-string time-stamp)
                               (explain--command-set-as-string command-set)
                               diff))
               (insert-text-button "[ View profile ]"
                                   'action #'explain--profile-report-click-profile
                                   'profile profile)
               (insert "\n\n")))))))
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
                   explain-pause-profile-blocking-threshold)))))

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
     (sit-for-wait 0)
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
    (setq sit-for-wait 0)
    (when (explain--profile-p commands)
      (explain--start-profiling))
    (setq before-command-snap (current-time)))

  (defun explain--exit-command (now-snap command-set)
    "Finish running a measurement loop."
    (let* ((diff (- (explain--as-ms-exact (time-subtract now-snap before-command-snap))
                    sit-for-wait))
           (too-long (and (> diff explain-pause-blocking-too-long-ms)
                          (not (explain--excluded-command-p command-set))))
           (was-profiled profiling-command))

      (when was-profiled
        (let ((profile (explain--save-and-stop-profiling)))
          ;; only save the profile if it was worth it
          (when too-long
            (explain--store-profile now-snap diff command-set profile))))

      (when (or too-long
                explain-pause-log-all-input-loop)
        (explain--log-pause diff sit-for-wait command-set t
                            (-difference (buffer-list) before-buffer-list))

        ;; only increment if it was actually too long, not if it was overriden
        (when too-long
          (explain--increment-profile command-set))

        (when was-profiled
          (explain--alert-profile command-set)))))

  (defun explain--precommand-hook ()
    (setq before-buffer-list (buffer-list))
    (explain--enter-command (list real-this-command)))

  (defun explain--postcommand-hook ()
    (when executing-command
      (explain--exit-command (current-time) executing-command)
      (unless mini-buffer-enter-stack
        ;; if the minibuffer stack is empty, we can expect that after this loop
        ;; completes, we can display stuff. schedule the delayed notifications:
        (explain--schedule-delayed-logs))
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
                                       (minibuffer-contents-no-properties))))
         ;; pop off the command that started this minibuffer to begin with
         (exiting-minibuffer-command (pop mini-buffer-enter-stack))
         (prev-command-set (cons real-this-command exiting-minibuffer-command))
         (next-command-set (cons minibuffer-command exiting-minibuffer-command)))

      (explain--exit-command now-snap prev-command-set)
      (explain--enter-command next-command-set)))

  ;; it would be nice to wrap only the args, but we actually need to know exactly
  ;; how long we waited for...
  (defun explain--wrap-sit-for (original-sit-for &rest args)
    "Advise sit-for and measure how long we actually sat for. Increment
the current sit-for-time with this value."
    (let ((before-snap (current-time)))
      (apply original-sit-for args)
      (let ((diff (explain--as-ms-exact (time-subtract nil before-snap))))
        (setq sit-for-wait (+ diff sit-for-wait)))))

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
      ;; accept-process-output or sit-wait'ed, which is why we need to save/restore
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
                 (too-long (> diff explain-pause-blocking-too-long-ms)))

            (when was-profiled
              (let ((profile (explain--save-and-stop-profiling)))
                (when too-long
                  (explain--store-profile now-snap diff command-set profile))))

            (setq executing-command original-execution-command)

            (when (or too-long
                      (symbol-value diff-override))
              (explain--log-pause diff 0 command-set nil nil)

              (when too-long
                (explain--increment-profile command-set))

              (when was-profiled
                (explain--alert-profile command-set)))))))))

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
  (-let* (((&plist :filter original-filter :sentinel original-sentinel) args)
          (new-args (-clone args))
          (wrapped-filter (explain--generate-wrapper (explain--generate-command-set 'process-filter) original-filter))
          (wrapped-sentinel (explain--generate-wrapper (explain--generate-command-set 'sentinel-filter) original-sentinel)))
    (when original-filter
      (setq new-args (plist-put new-args :filter wrapped-filter)))
    (when original-sentinel
      (setq new-args (plist-put new-args :sentinel wrapped-sentinel)))
    new-args))

(defun explain--wrap-set-process-filter-callback (args)
  "Advise that modifies the arguments ARGS to `process-filter' by wrapping the callback."
  (-let* (((arg-process original-callback) args)
          (wrapped-filter (explain--generate-wrapper (explain--generate-command-set 'process-filter) original-callback)))
    (list arg-process wrapped-filter)))

(defun explain--wrap-set-process-sentinel-callback (args)
  "Advise that modifies the arguments ARGS to `process-sentinel' by wrapping the callback."
  (-let* (((arg-process original-callback) args)
          (wrapped-filter (explain--generate-wrapper (explain--generate-command-set 'sentinel-filter) original-callback)))
    (list arg-process wrapped-filter)))

(defun explain--wrap-idle-timer-callback (args)
  "Advise that modifies the arguments ARGS to `run-with-idle-timer' by wrapping the callback."
  (-insert-at 2 #'explain--measure-idle-timer-callback args))

(defun explain--wrap-timer-callback (args)
  "Advise that modifies the arguments ARGS to `run-with-timer' by wrapping the callback."
  (let ((original-callback (nth 2 args)))
    (if (eq original-callback #'explain--alert-delays-timer)
        args
      (-insert-at 2 #'explain--measure-timer-callback args))))

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
      ((hooks '((pre-command-hook . explain--precommand-hook)
                (post-command-hook . explain--postcommand-hook)
                (minibuffer-setup-hook . explain--enter-minibuffer)
                (minibuffer-exit-hook . explain--exit-minibuffer)))
       (advices '((run-with-idle-timer . explain--wrap-idle-timer-callback)
                  (run-with-timer . explain--wrap-timer-callback)
                  (set-process-filter . explain--wrap-set-process-filter-callback)
                  (set-process-sentinel . explain--wrap-set-process-sentinel-callback))))
  (cond
   (explain-pause-mode
    (explain--command-loop-reset)
    (explain--delayed-logs-reset)
    (dolist (hook hooks)
      (add-hook (car hook) (cdr hook)))
    (dolist (advice advices)
      (advice-add (car advice) :filter-args (cdr advice)))
    (advice-add 'sit-for :around #'explain--wrap-sit-for)
    (dolist (func '(make-process make-pipe-process make-network-process))
      (advice-add func :filter-args #'explain--wrap-make-process-sentinel-filter-callback)))
   (t
    (dolist (func '(make-process make-pipe-process make-network-process))
      (advice-remove func #'explain--wrap-make-process-sentinel-filter-callback))
    (dolist (advice advices)
      (advice-remove (car advice) (cdr advice)))
    (dolist (hook hooks)
      (remove-hook (car hook) (cdr hook)))))))

(provide 'explain-pause-mode)

;;; explain-pause-mode.el ends here
