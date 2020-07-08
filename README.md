## explain-pause-mode
[See what's in development in the project board](https://github.com/lastquestion/explain-pause-mode/projects/1) | 
[Feature requests, bug reports, issues](https://github.com/lastquestion/explain-pause-mode/issues) |
[Changelog](https://github.com/lastquestion/explain-pause-mode/blob/master/CHANGELOG.md)

*v0.2* is the current development release. See [vNext](#vnext) for more details on what's coming next.

### How to use
`explain-pause-mode` is very lightweight; you can leave it running all the time. Turn it on after requiring it:
```elisp
(explain-pause-mode)
```

`explain-pause-top` shows a `top`-like view of the things Emacs has been doing:

<img src="https://raw.githubusercontent.com/lastquestion/explain-pause-mode/master/top.gif" height="338px">

### Installing
There are 2 ways right now to install. ([Melpa coming soon](https://github.com/lastquestion/explain-pause-mode/issues/49)!)

#### Straight.el
If you are using [straight.el](https://github.com/raxod502/straight.el), then you can use a normal receipe to install:
```elisp
(straight-use-package
 '(explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode"))
(explain-pause-mode)
```

You might be using [use-package](https://github.com/jwiegley/use-package) with [straight.el](https://github.com/raxod502/straight.el), then you can install and enable at the same time:
```elisp
(use-package explain-pause-mode
  :straight (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode")
  :config
  (explain-pause-mode))
```

#### From sources
If you are interested in contributing, you might want to directly install from sources. Just `git clone` and then `make install`. `make install` will bytecompile and install the sources to a path you specify, which should be on your emacs load path. It is important to bytecompile and optimize `explain-pause-mode` for normal use, as the package hooks into many operations that happen on every command or keypress.

### vNext
*v0.3* major features roadmap: 
  * [memory tracking and GC pauses](https://github.com/lastquestion/explain-pause-mode/issues/27)
  * [show commands per buffer](https://github.com/lastquestion/explain-pause-mode/issues/48)


### Contribute
Feel free to take a look at the issues board, development is tracked openly on it and the the project board. 
