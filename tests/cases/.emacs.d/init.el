(setq auto-save-list-file-prefix nil)
(load "explain-pause-mode.el")
(load "./tests/cases/driver.el")
(setup-test)
(explain-pause-mode)
(when (getenv "TESTREAD")
  (read-event "in init" nil 0.1))

