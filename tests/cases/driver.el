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
(setq event-stream nil)

(defun log-socket-filter (process string)
  "Accept socket log input from the tested emacs and save it to
the stream buffer, and also parse it into the event-stream (which
is in reverse order.) When `exit-test-quit-emacs' is found, set
exit-command in the session."
  (with-current-buffer (process-get process :socket-buffer)
    (insert string))

  ;;TODO (message string)

  (let* ((event (read string))
         (command (nth 1 event)))

    (push event event-stream)

    (when (or
           (equal "exit-test-quit-emacs" command)
           (equal "exit-test-debugger-invoked" command))
      (message "... emacs terminated: %s" command)
      (setf (nth 5 (process-get process :session)) command))))

;; utility functions for walking the event stream, which is assumed
;; to be in correct order (reversed from event-stream global)
(defun find-ptr (list pred)
  "Find the ptr to the first passing pred."
  (find-ptr-between (cons list nil) pred))

(defun span (head pred-start pred-end)
  "Find the span with the first passing PRED-START and first
passing PRED-END after that found PRED-START in ( start . end ),
inclusively."
  (let* ((start (find-ptr head pred-start))
         (end (find-ptr start pred-end)))

    (cons start end)))

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

(defun get-value-between (span valname)
  "Get the value of valname that was set by a `value` event
in span."
  (nth 2 (car (find-ptr-between span (find-by "value" valname)))))

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
  ;; TODO would be nice to have color.
  `(let ((passing ,test-form))
     (message "%s %s"
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

(defun start-test (&optional filename)
  "Start emacs, loading explain-pause and FILENAME, and run
`before-test' inside that emacs from that file. If FILENAME is
nil, use the file that defined the function
`before-test'. Returns the name of the session to be used by
later commands. Assumes this emacs is running at root of the
project."

  (unless filename
    (setq filename (symbol-file 'before-test)))

  (message "Starting subemacs for test %s" filename)

  (let* ((name (file-name-base filename))
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
           )))

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
           (exit-code
            (call-process "tmux" nil name nil "new-session" "-d"
                          "-n" name
                          "-P" "-F" "\"#{pane_pid}\""
                          "emacs" "-nw" "-Q"
                          "-l"
                          ;; TODO maybe make this calculate the paths..?
                          "./explain-pause-mode.el"
                          "-l"
                          "./tests/cases/driver.el"
                          "-l"
                          filename
                          "--eval"
                          (format "(setq socket-filename \"%s\")" socket-filename)
                          "-f"
                          "setup-test")))

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
            (eval-expr session "(before-test)")
            session))
         (t
          ;; no good
          (message "Failed to start:\n%s" (buffer-string))
          (delete-process socket-process)
          (ignore-errors (delete-file socket-filename))
          (kill-emacs 1)))))))

(defun wait-until-dead (session)
  "Wait until the session is dead."
  (while (not (nth 5 session))
    (accept-process-output (nth 4 session)))
  ;; child died
  (message "... test finished")
  (delete-process (nth 4 session))
  (ignore-errors (delete-file (nth 2 session)))
  (if (equal (nth 5 session) "exit-test-debugger-invoked")
      (progn
        (message "test failed in debugger ✗")
        (kill-emacs 1))
    (finish-test session)))

(defun session-socket-buffer (session)
  "Get the name of the socket buffer in SESSION."
  (nth 3 session))

(defun send-key (session &rest KEYS)
  "Send KEYS to tmux session created by `start-test'"
  (apply 'call-process "tmux" nil nil nil "send-keys" "-t" (car session) KEYS))

(defun m-x-run (session command)
  "M-x run a command and press enter."
  (send-key session "Escape" (concat "x" command) "Enter"))

(defun eval-expr (session expr)
  "M-: eval-expression expr and press enter."
  (send-key session "Escape" (concat ":" expr) "Enter"))

(defun call-after-test (session)
  "Call after-test inside the emacs in SESSION by sending SIGUSR1, which
it is assumed `test-setup' has trapped."
  (signal-process (nth 1 session) 'sigusr1))

;; inside tested code functions
(defun send-value (name val)
  "Send the name/value pair to the event log. Run only inside tested code."
  (process-send-string
   explain-pause-log--send-process
   (format "(\"value\" \"%s\" %s)\n"
           name
           (prin1-to-string val))))

(defun exit-test-quit-emacs ()
  (interactive)
  "Call after-test, and then close and quit emacs. Run by SIGUSR1."
  ;; assumed defined in test file
  (after-test)
  (explain-pause-log-off)
  (kill-emacs))

(defun exit-test-debugger-invoked ()
  (interactive)
  (explain-pause-log-off)
  (kill-emacs))

(defun setup-test ()
  "Trap SIGUSR1 so we can call `after-test' inside this
emacs. Start explain-pause and connect to the logging socket. This must be
called as the last thing in `before-test'. If the debugger starts, log
into the event stream and then quit."
  (add-hook 'debugger-mode-hook
            (lambda ()
              (call-interactively 'exit-test-debugger-invoked)))
  (toggle-debug-on-error)

  (define-key special-event-map [sigusr1] 'exit-test-quit-emacs)
  (explain-pause-log-to-socket socket-filename)
  (explain-pause-mode))
