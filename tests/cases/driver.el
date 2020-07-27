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

;;; Driver for integration tests that run emacs in a tmux terminal
;;; and drive full keyboard input.

;; expects only to be run once. half the code runs in the test, half
;; in the tested emacs.

;; test driver code:
(require 'seq)

(setq event-stream nil)

(setq stream-logs (getenv "STREAMLOGS"))
(setq wait-keys (getenv "WAITKEYS"))
(setq verbose (getenv "VERBOSE"))

(defun log-socket-filter (process string)
  "Accept socket log input from the tested emacs and save it to
the stream buffer, and also parse it into the event-stream (which
is in reverse order.) When `exit-test-quit-emacs' is found, set
exit-command in the session."
  (let ((session (process-get process :session)))
    (when (and verbose
               (nth 5 session))
      (message "message %s but already quit" string))

    ;; unless we already quit...
    (unless (nth 5 session)
      (with-current-buffer (process-get process :socket-buffer)
        (insert string))

      (when stream-logs
        (princ (format "[%s] - %s" (current-time) string)))

      (let* ((event (read string))
             (command (nth 1 event)))

        (push event event-stream)

        (cond
         ((and command
               (string-match "exit-test" command)
               (not (equal "exit-test-quit-emacs-check" command)))
          (message "... emacs terminated: %s" command)
          (setf (nth 5 session) command))
         ((equal "enabled" (nth 0 event))
          (message "...mode enabled")
          (setf (nth 6 session) t)))))))

;; utility functions for walking the event stream, which is assumed
;; to be in correct order (reversed from event-stream global)
(defun find-ptr (list pred)
  "Find the ptr to the first passing pred."
  (find-ptr-between (cons list nil) pred))

(defun span-between (span pred-start pred-end)
  "Find the span inside the span with PRED-START and PRED-END."
  (let* ((start (find-ptr-between span pred-start))
         (end (find-ptr-between (cons (cdr start) (cdr span)) pred-end)))
    (cons start end)))

(defun found-span-p (span)
  "Is the span not empty?"
  (and (car span)
       (cdr span)))

(defun span (head pred-start pred-end)
  "Find the span with the first passing PRED-START and first
passing PRED-END after that found PRED-START in ( start . end ),
inclusively."
  (span-between (cons head nil) pred-start pred-end))

(defun find-ptr-between (span pred)
  "Find the ptr to pred between the span, inclusively."
  (let ((head (car span))
        (end (cddr span))) ; one past
    (if (catch 'place
          (while (not (eq head end))
            (when (funcall pred (car head))
              (throw 'place t))
            (setq head (cdr head)))
          nil)
        head
      nil)))

(defun get-value (stream valname)
  "Get the value of valanme that was set by the first 'value' event in stream"
  (nth 2 (car (find-ptr stream (find-by "value" valname)))))

(defun get-value-between (span valname)
  "Get the value of valname that was set by a `value` event
in span."
  (nth 2 (car (find-ptr-between span (find-by "value" valname)))))

(defun find-enabled (stream)
  "Find the enable log."
  (find-ptr stream (lambda (x)
                     (equal (nth 0 x) "enabled"))))

(defun find-by (type command)
  "Create a PRED to find a event by TYPE for COMMAND."
  (lambda (x)
    (and (equal (nth 0 x) type)
         (equal (nth 1 x) command))))

(defun span-func (head command)
  "Find the first span that matches an enter/exit pair for
COMMAND."
  (span head
        (find-by "enter" command)
        (find-by "exit" command)))

(defun span-func-between (span command)
  "Find the first span inside span that matches an enter/exit
pair for COMMAND."
  (span-between span
        (find-by "enter" command)
        (find-by "exit" command)))

(defun print-span (span)
  "Print a span."
  (let ((ptr (car span))
        (end (cddr span)))
    (while (not (eq ptr end))
      (message "%s" (car ptr))
      (setq ptr (cdr ptr)))))

(defun print-stream-buffer (session)
  (with-current-buffer (nth 3 session)
    (princ (buffer-string))))

;; simple test asserts.
(defmacro message-assert (test-form msg)
  "print MSG, and a check mark if passed or x if failed. Set the
value `passed' to 1 if it fails. `passed' is expected to be used
as the args to `kill-emacs' by the tester. THIS DOES NOT STOP
EXECUTION by throwing (compare to `cl-assert', etc.)"
  `(let ((passing ,test-form))
     (message "\e[%sm%s %s\e[0m"
              (if passing
                  32
                31)
              ,msg
              (if passing
                  "✓"
                "✗"))
     (when (not passing)
       (setq passed 1))))

(defmacro message-assert-not (test-form msg)
  `(message-assert (not ,test-form) ,msg))

(defun exit-measured-time (record)
  "Return the measured-time from the exit record."
  (nth 3 record))

(defun start-test (&optional filename emacs-args boot-function)
  "Start emacs to test.

Unless filename is given, use the file that defines `before-test'.
Unless emacs-args is given, defaults to loading explain-pause via
`-l`.
Unless boot-function is given, calls `setup-test-boot' inside
that emacs via `-f`.
Unless boot-function is given, after startup finishes,
`eval-expr' runs `before-test'.

Returns the session to be used by later commands. Assumes this
emacs is running at root of the project."

  (unless filename
    (setq filename (symbol-file 'before-test)))

  (unless emacs-args
    (setq emacs-args `("-nw" "-Q" ;; no window, no init
                      "-l"
                      ,(getenv "TARGET")
                      "-l"
                      "./tests/cases/driver.el"
                      )))

  (let* ((emacs-binary (expand-file-name invocation-name invocation-directory))
         (name (file-name-base filename))
         (socket-filename (concat (file-name-directory filename)
                                  name
                                  "-socket.sock"))
         (socket-buffer (format "%s-socket" name))
         (buffer (get-buffer-create socket-buffer))
         (session
          (list
           name
           nil ;; PID
           socket-filename
           socket-buffer
           nil ;; socket process
           nil ;; dead or not
           nil ;; started or not
           ))
         (boot-args (or boot-function
                        '("-f" "setup-test-boot"))))

    (setenv "SOCKET" socket-filename)

    (message "Starting subemacs for test %s with target %s"
             filename
             (getenv "TARGET"))

    ;; in case the previous tests crashed early
    (ignore-errors (delete-file socket-filename))

    (let* ((socket-process
            (make-network-process
             :name socket-buffer
             :type 'datagram
             :server t
             :family 'local
             :buffer nil
             :service socket-filename
             :filter 'log-socket-filter
             :plist `(:socket-buffer
                      ,socket-buffer
                      :session
                      ,session)))
           (args `("tmux" nil ,name nil "new-session" "-d"
                   "-n" ,name ;; name the session
                   "-P" "-F" "\"#{pane_pid}\"" ;; get us the pid for later
                   ,emacs-binary
                   ,@emacs-args
                   "-l"
                   ,filename
                   ,@boot-args))
           (exit-code nil))

      (when verbose
        (message "%s" args))

      (setq exit-code (apply 'call-process args))

      (with-current-buffer name
        (cond
         ((eq exit-code 0)
          (let ((pid-string
                 (buffer-substring-no-properties 2
                                                 (- (point-max) 2))))
            ;; ok
            (message "Started successfully... PID: %d" (string-to-number pid-string))
            (setf (nth 1 session) (string-to-number pid-string))
            (setf (nth 4 session) socket-process)
            ;; run setup
            (unless boot-function
              (eval-expr session "(before-test)"))
            session))
         (t
          ;; no good
          (message "Failed to start:\n%s" (buffer-string))
          (delete-process socket-process)
          (ignore-errors (delete-file socket-filename))
          (kill-emacs 1)))))))

(let ((spin 0)
      (values ["\e[D-"
               "\e[D/"
               "\e[D|"
               "\e[D\\"]))

  (defun spinner ()
    (send-string-to-terminal (aref values spin))
    (setq spin (% (1+ spin) 4))))

(defun wait-until-ready (session)
  "Wait until the mode is enabled."
  (message "Waiting until ready...")
  (let ((proc (nth 4 session)))
    (while (not (nth 6 session))
      (spinner)
      (accept-process-output)))
  (message "...ready"))

(defun wait-until-dead (session)
  "Wait until the session is dead."
  (let ((proc (nth 4 session)))
    (message "Waiting until dead...")
    (while (not (nth 5 session))
      (spinner)
      (accept-process-output))
    ;; child died
    (message "... finished.\nwaiting for PID to die:")
    ;; sometimes there is left over input.
    ;; this only work on *nix
    (if (getenv "NODIE")
        (read-from-minibuffer "Quit?")
      (let ((living t))
        (while living
          (if (eq
               (call-process "ps" nil nil nil "-p"
                             (number-to-string (nth 1 session)))
               1)
              ;; dead
              (setq living nil)
            ;; alive
            (spinner)
            (sleep-for 0.1)))))

    (message "done\n")
    ;; delete the network process
    (delete-process proc)
    ;; delete the socket files
    (ignore-errors (delete-file (nth 2 session)))

    (if (equal (nth 5 session) "exit-test-debugger-invoked")
        (progn
          (message "\e[31mtest failed in debugger ✗\e[0m")
          (kill-emacs 1))
      (finish-test session))))

(defun session-socket-buffer (session)
  "Get the name of the socket buffer in SESSION."
  (nth 3 session))

(defun convert-to-hex (keys)
  (seq-reduce
   (lambda (accum key)
     (cond
      ((eq key 'enter)
       (append accum (list "0D")))
      ((eq key 'escape)
       (append accum (list "1B")))
      (t
       (append accum
               (mapcar (lambda (char)
                         (format "%x" char))
                       key)))))
   keys '()))

(defun convert-to-special (key)
  (cond
   ((eq key 'f10)
    "f10")
   ((eq key 'quit)
    "C-g")))

(defun convert-to-str (keys)
  (mapconcat (lambda (key)
               (cond
                ((eq key 'enter)
                 "<ENTER>")
                ((eq key 'escape)
                 "<ESC>")
                (t
                 key)))
             keys ""))

(defun send-key-stream (session args)
  "Actually send a key stream to tmux."
  (let ((proc (make-process
               :name "tmux"
               :buffer nil
               :command `("tmux" "send-keys" "-t"
                          ,(car session)
                          ,@args)
               :connection 'pipe)))
    (while (process-live-p proc)
      (when verbose
        (spinner))
      (accept-process-output))))

(defun send-key (session &rest KEYS)
  "Send KEYS to tmux session created by `start-test'"
  (when wait-keys
    (read-from-minibuffer (format "send %s..." KEYS)))
  (if verbose
      (message "%s %s"
               (convert-to-str KEYS)
               (convert-to-hex KEYS))
    (send-string-to-terminal "."))
  (send-key-stream session
                   (cons "-H" (convert-to-hex KEYS))))

(defun send-special-key (session special-key)
  "Send either f10 or ctrl-g."
  (when wait-keys
    (read-from-minibuffer (format "send %s..." special-key)))
  (if verbose
      (message "%s %s" special-key (convert-to-special special-key))
    (send-string-to-terminal "."))
  (send-key-stream session (cons (convert-to-special special-key) nil)))

(defun m-key-string (session key command)
  (send-key session 'escape key)
  (let ((keys (seq-partition command 5)))
    (seq-doseq (key keys)
      (send-key session key)))
  (send-key session 'enter))

(defun m-x-run (session command)
  "M-x run a command and press enter."
  (m-key-string session "x" command))

(defun eval-expr (session expr)
  "M-: eval-expression expr and press enter."
  (m-key-string session ":" expr))

(defun call-after-test (session)
  "Call after-test inside the emacs in SESSION by sending SIGUSR1, which
it is assumed `test-setup' has trapped."
  (if (getenv "PAUSEATTACH")
      (message "Pausing to allow attach...")
    (when verbose
      (message "sending sigusr1 to %s" (nth 1 session)))
    (signal-process (nth 1 session) 'sigusr1)))

(defun send-signal (session signal)
  "Send signal to session."
  (when verbose
    (message "[%s] - signal %s" (current-time) signal))
  (signal-process (nth 1 session) signal))

;; inside tested code functions
(defun quit-with-record (record)
  (send-exit-record record)
  (unless (getenv "NODIE")
    (kill-emacs 1)))

(defun check-buffers ()
  "Find any buffer with backtrace or explain-pause-mode-report-bug or
if messages buffer has error message."
  (cl-loop
   for buffer being the buffers
   do
   (let ((name (buffer-name buffer)))
     (when (or (string-match-p "backtrace" name)
               (string-match-p "explain-pause-mode-report-bug" name)
               (string-match-p "Warnings" name))
       (quit-with-record "exit-test-debugger-invoked"))))

  (with-current-buffer (messages-buffer)
    (goto-char 0)
    (unless (re-search-forward "Debug on Error enabled globally" nil t)
      (quit-with-record "exit-test-debug-on-error-off"))

    (when (re-search-forward "error" nil t)
      (quit-with-record "exit-test-message-error"))))


(defun send-value (name val)
  "Send the name/value pair to the event log. Run only inside tested code."
  (explain-pause-log--send-dgram
   (format "(\"value\" \"%s\" %s)\n"
           name
           (prin1-to-string val))))

(defun send-exit-record (why)
  "Send an emergency exit record (needed if explain-pause didn't install)."
  (explain-pause-log--send-dgram
   (format "(\"enter\" \"%s\")\n" why)))

(defun exit-test-quit-emacs-check ()
  (interactive)
  "Call after-test, and then close and quit emacs. Run by SIGUSR1."
  ;; assumed defined in test file
  (after-test)
  (check-buffers)
  (call-interactively 'exit-test-quit-emacs))

(defun exit-test-quit-emacs ()
  (interactive)
  (send-exit-record "exit-test-unclean")
  (unless (getenv "NODIE")
    (explain-pause-log-off)
    (kill-emacs)))

(defun exit-test-debugger-invoked ()
  (interactive)
  (check-buffers)
  (send-exit-record "exit-test-unclean")
  (unless (getenv "NODIE")
    (explain-pause-log-off)
    (kill-emacs)))

(defun setup-test-boot ()
  "Setup-test and then start the mode."
  (setup-test)
  (explain-pause-mode))

(defun setup-test ()
  "Trap SIGUSR1 so we can call `after-test' inside this
emacs. Connect to the logging socket. If the debugger starts, log
into the event stream and then quit."
  (add-hook 'debugger-mode-hook
            (lambda ()
              (call-interactively 'exit-test-debugger-invoked)))
  (toggle-debug-on-error)

  (define-key special-event-map [sigusr1] 'exit-test-quit-emacs-check)

  (explain-pause-log-to-socket (getenv "SOCKET"))

  (when (getenv "VERBOSE")
    (advice-add 'explain-pause-log--send-dgram :before
                (lambda (&rest args)
                  (let ((inhibit-message t))
                    (message "send dgram %s" args))))))
