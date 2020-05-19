## explain-pause-mode

### How to use
```
(explain-pause-mode t)
```

`explain-pause-mode` is very lightweight; you can leave it running all the time. Or, if you prefer, you can turn it on and off when you notice Emacs acting slow.

When something takes a while (default: over **40ms**; this is configurable), you'll see the message:

```
Emacs blocked for ...ms - check *explain-pause-log*
```

At this point, you can check the buffer `*explain-pause-log*` to see what was slow and the information gathered. If the slowness occurs a number of times (configurable, see customizable varables) `explain-pause-mode` will take a sampling
profile and create a text report you can use to diagnose or send to package developers.

When a profile is gathered, you'll see the message:

```
Blocking call ... was profiled! Run `explain-pause-profiles' to learn more
```

You can then run `explain-pause-profiles` to navigate and view the profile report.

### What's the point of this?

We would all like emacs to respond faster to our input, in other words, to have the lowest feasible latency. Latency under 100ms is a good goal to achieve, (though in reality humans can easily detect latency in the 40ms range).

However, often the reason why emacs does not quickly respond to user input has less to do with the emacs core and much more to do with the exact running situation - the combination of packages, files open, user commands, etc.

The goal of this package is to provide an always-running watchdog that attempts (relatively well) to measure when code is synchronously (\*) working for a long time.

It attemps to intelligently capture information into a log that ought to be useful for developers and users to diagnose and fix why emacs was slow. (\*)

With enough bug reports and data, I hope that togther we can improve the latency of emacs and the package ecosystem.

(\*) sychronous means "code executing without ever yielding to the input-command loop"

(\*) slow means "as a user, I pressed a key but emacs did not register or service that request quickly, and I became frustrated"

### How does this thing work?
#### What's a pause, anyway?
Let's define "pauses" first. When a user attempts to enter input to emacs via the input-command loop, e.g. they press a key, or click the mouse, the user expects that the response to that action occurrs immediately.

A "pause" occurs when emacs is already busy doing _something else_, and is unable to process and return to the "wait for user input" state immediately. For example, this might manifest to a user as "I pressed C-x b but nothing happened for a moment".

The terminology here is stolen from "GC pause", which is a common concern in most garbage collected execution environments, where in naive implementations of garabage collection, the entire application is paused.

A "pause" might not actually affect a user. If it happens while the user is thinking, or not typing, or reading, then it is not noticeable. This is why this package is not called "explain-slow", but "explain-pause".

However, if developers drive pause times down as low as possible, the likelihood a user will request action during a pause becomes very small. Thus, the user experiences an extremely low latency system - never waiting for emacs to respond.

#### Measuring pauses
Measuring pauses from within user-space is possible. At first, one might expect that execution environment support is required (and for perfect pause detection, this is true.) However, extremely useful data can still be collected from within
the environment itself.

Much of what I attempt here is based on experiences doing similar pause detection in Javascript runtimes. In fact, JS is quite similar to elisp running in emacs in many ways, including the concurrent execution model.

One of the easiest ways of measuring pauses is to use a timer that schedules itself every N ms, and measures how long it took before it actually was allowed to execute. If the real wall clock time is much later then the requested time, "something" executed for so long that latency was impacted.

This is one of the common mechanisms for measuring all sorts of platform level interruptions. However, it does not easily provide a way to see "what" actually causing that delay.

Luckily, unlike generic mechanisms to measure latency and pauses, we only need to instrument and understand emacs. Therefore, we can take advantage of context knowledge and do a much better job.

The basic strategy of this package is to attempt to instrument every transition from "waiting for input" state to "elisp execution state". (This could perhaps be achieved in a cleaner mechanism in the C layer of emacs, but in reality,
elisp is extremely powerful and covers almost all the interesting use cases, and a package is much easier to install and distribute then a diff to the C core of emacs...)

There are 3 major transition points:
* from the input-command loop into some command;
* from some external input (process, socket, etc.) into some handler;
* from a timer (idle or regular) into some command

This package uses advises on core C elisp functions by modifying their callbacks to wrap a timer around the actual original callback.

This package also installs a pre and post command hook to time the execution of the command run during the input-command loop.

Next, when execution of those 3 kinds handlers take longer then a defined amount of time, this package records the information. If this occurs more then a certain number of times, the next time that entry occurs, a sampling profiler (the
builtin profiler) is executed, and the result collected.

### Caveats, Q&A, etc. etc.

Yes, I know that I am advising native C functions, and that if those were called from within C itself, the advise would not execute. Luckily, based on my (brief?) exploration of the C core of emacs, these functions aren't called from C.

There is special handling for sit-for, as it acts like an interrupt that allows transition back to the input-loop. But I assume that people only call it from handlers out of the input-command loop, and not from timers or process handlers. The documentation says people shouldn't do the latter...

There is special handling for minibuffers. The minibuffer itself can be slow (especially complex completion code). But that is also distinguished from the action that occurs once the minibuffer is closed.

I might have missed other transition functions. File a bug report or contribute back!

Yes, it is possible that a hook or advise might install itself extremely early and before this package is installed. I'm not sure how likely that is to happen in real life, yet, though.

### Contribute
It's quite possible I either wrote terrible elisp or forgot various transition functions. Please help me improve my elisp and this package and contribute PRs if you feel so inclined.

Also, you can check the issues in Github if you'd like ideas on what to improve on.
