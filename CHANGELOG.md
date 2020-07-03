## Changelog

### 0.2 (unreleased)
#### Major changes
* Measure via `call-interactively` instead of hooking `pre` and `post` command hooks.
* Remove `explain-pause-log` and `explain-pause-profile`. Their functionality is replaced by `explain-pause-top`.
* Add `explain-pause-top`.
* Add two user notification levels, `developer` and `normal`. The default is `normal`. In `normal`, `explain-pause-mode` notifies the user occasionally about slow commands. In `developer`, it always notifies the user immediately.
* Add custom faces for `explain-pause-top`.

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
