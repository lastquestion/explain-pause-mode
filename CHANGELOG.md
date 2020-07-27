## Changelog

### 0.2 (unreleased)
#### Major changes
* Measure via `call-interactively` instead of hooking `pre` and `post` command hooks.
* Remove `explain-pause-log` and `explain-pause-profile`. Their functionality is replaced by `explain-pause-top`.
* Add `explain-pause-top`.
* Add two user notification levels, `developer` and `normal`. The default is `normal`. In `normal`, `explain-pause-mode` notifies the user occasionally about slow commands. In `developer`, it always notifies the user immediately.
* Add custom faces for `explain-pause-top`.
* Measure code run in `post-command-hook`, `pre-command-hooks`, `post-gc-hook`, and a few other rarer hooks.

#### New custom variables
* `explain-pause-top-auto-refresh-interval`
* `explain-pause-top-click-profile-action`
* `explain-pause-profile-slow-threshold`
* `explain-pause-profile-enabled`
* `explain-pause-alert-normal-interval`
* `explain-pause-alert-normal-minimum-count`

#### Bug fixes
* Fix `magit` commit over TRAMP ([#46](https://github.com/lastquestion/explain-pause-mode/issues/46))
* Fix `treemacs` nil dereference in process-filter ([#44](https://github.com/lastquestion/explain-pause-mode/issues/44))
* Fix `explain-pause-top` crash when an item already exists on first render ([#40](https://github.com/lastquestion/explain-pause-mode/issues/))
* Improve how `explain-pause-top` shows lambdas and closures ([#34](https://github.com/lastquestion/explain-pause-mode/issues/))
* Fix `magit` commit locally sometimes breaking when `explain-pause` attempts to profile while already profiling ([#26](https://github.com/lastquestion/explain-pause-mode/issues/))
* Fix `emacsclient -nw` not working ([#50](https://github.com/lastquestion/explain-pause-mode/issues/50))
* Fix `kill-buffer` not being recorded when quitting out of command ([#58](https://github.com/lastquestion/explain-pause-mode/issues/58))
* Fix `flyspell` not working because of `post-command-hook` ([#54](https://github.com/lastquestion/explain-pause-mode/issues/54))
* Fix `notmuch` not working because `make-process` can create `stderr` processes ([#64](https://github.com/lastquestion/explain-pause-mode/issues/64))
* Fix `flymake` not working because `post-command-hook` did not handle compiled lambdas ([#71](https://github.com/lastquestion/explain-pause-mode/issues/71))
* Fix `disabled-command` not working because `command-execute` calls `disabled-command-function` directly ([#73](https://github.com/lastquestion/explain-pause-mode/issues/73))
* Fix `circe` not working because `set-process-plist` lost process command frame information ([#79](https://github.com/lastquestion/explain-pause-mode/issues/79))
* Fix button-clicks in emacs 26 not working because input-translation keymaps are not handled ([#84](https://github.com/lastquestion/explain-pause-mode/issues/84))
