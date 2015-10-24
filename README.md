# state

This library allows you to switch back and forth between predefined
workspaces. It allows you for example to press <kbd>C-c s g</kbd> to
switch to `gnus` in fullscreen. When you are done with `gnus` your can
switch back to where you were by pressing the same keystroke.
Similarly, you can take a quick peek at your `*Messages*` buffer by
pressing <kbd>s-s m</kbd> and return to where you were by pressing the
same keystroke.

## Installation

Make sure the file `state.el` is in your load path and put the
following in your `.emacs`:
```lisp
(require 'state)
(state-global-mode 1)
```

## Simple buffer switching

To define a new state, you need to use the macro `state-define-state`.
The simplest switch is a switch to a buffer. For example the
`*Messages*` buffer:
```lisp
(state-define-state
 message
 :key "m"
 :switch "*Messages*")
```
The first argument to `state-define-state` is a unique symbol
identifying the state. The rest is a property list. For a simple
buffer switching state we have to specify the key to press to switch
to that buffer and the name of the buffer we want to switch to. We
could have specified the path of a file as well. Pressing <kbd>C-c s
m</kbd> switches to the `*Messages*` buffer. Pressing it again
switches back to where you were.

## General case

If your workspace is not a simple file or if you want a different
behaviour when switching to it, you have to specify yourself several
properties.
- The `:in` property is used to charaterize the state and should
  return a non-nil value if we are currently in this workspace and nil
  otherwise.
- The `:exist` property tells if the workspace has been created. We no
  longer need to call the create property.
- The `:create` property is used to create your workspace if it does
  not exist already. For example, if there is no `gnus` buffer for the
  `gnus` state.
- The `:switch` property is performing the actual switch.
- The `:bound` property is used to make a state only accessible from
  another state. It is useful for example to have dedicated terminal
  buffer for a project.

## Examples

Some examples taken from my [emacs configuration](https://github.com/thisirs/dotemacs):
- A simple switch to the `*scratch*` buffer:
```lisp
(state-define-state
 scratch
 :key "s"
 :switch "*scratch*")
```
- To my timeline with `twittering-mode`:
```lisp
(state-define-state
 twit
 :key "t"
 :in (and (require 'twittering-mode nil t) (twittering-buffer-p))
 :switch twit)
```
- Switching to my `init-*.el` files:
```lisp
(state-define-state
 emacs
 :key "e"
 :in "~/.emacs.d/init"
 :create (find-file "~/.emacs.d/init.el"))
```
- Switching to a terminal dedicated to the `emacs` state:
```lisp
(state-define-state
 emacs-term
 :key "z"
 :bound emacs
 :exist (get-buffer "*ansi-term (dotemacs)*")
 :in (equal (buffer-name) "*ansi-term (dotemacs)*")
 :switch (state-switch-buffer-other-window "*ansi-term (dotemacs)*")
 :create (ansi-term "/bin/zsh" "ansi-term (dotemacs)"))
```
- Switching to a general purpose terminal:
```lisp
(state-define-state
 term
 :key "z"
 :exist (get-buffer "*ansi-term*")
 :in (equal (buffer-name) "*ansi-term*")
 :switch (state-switch-buffer-other-window "*ansi-term*")
 :create (ansi-term "/bin/zsh"))
```
- Switching to `gnus`:
```lisp
(state-define-state
 gnus
 :key "g"
 :in (memq major-mode
           '(message-mode
             gnus-group-mode
             gnus-summary-mode
             gnus-article-mode))
 :create gnus)
```
- For ERC users, switching to ERC and cycling through ERC buffers by
pressing <kbd>i</kbd> repeatedly:
```lisp
(state-define-state
 erc
 :key "i"
 :in (memq (current-buffer)
           (erc-buffer-list))
 :switch (erc-start-or-switch 1)
 :keep (erc-track-switch-buffer 0))
```
with `erc-start-or-switch` being
```lisp
(defun erc-start-or-switch (arg)
  "Connect to ERC, or switch to last active buffer"
  (interactive "P")
  (if (and (get-buffer "irc.freenode.net:6667")
           (erc-server-process-alive (get-buffer "irc.freenode.net:6667")))
      (erc-track-switch-buffer 1)
    (when (or arg (y-or-n-p "Start ERC? "))
      (erc :server "irc.freenode.net"
           :port 6667
           :nick "thisirs"
           :password (secrets-get-secret "Default" "NickServ")))))
```
- A context-aware switch to associated repl. Pressing <kbd>C-c s j</kbd>
in an emacs-lisp file switches to `ielm`. Same for `MATLAB`, `python`
and `ruby` files.
```lisp
(defmacro state-define-repl (name key buffer-name from create)
  `(state-define-state
    ,name
    :bound ,from
    :key ,key
    :exist (get-buffer ,buffer-name)
    :in (equal (buffer-name) ,buffer-name)
    :switch (state-switch-buffer-other-window ,buffer-name)
    :create (progn
              (switch-to-buffer-other-window (current-buffer))
              ,create)))

(state-define-repl elisp-repl "j" "*ielm*" (eq major-mode 'emacs-lisp-mode) (ielm))
(state-define-repl matlab-repl "j" "*MATLAB*" (eq major-mode 'matlab-mode) (matlab-shell))
(state-define-repl python-repl "j" "*Python*" (eq major-mode 'python-mode) (run-python "/usr/bin/python2.7"))
(state-define-repl ruby-repl "j" "*ruby*" (eq major-mode 'ruby-mode) (inf-ruby))
```
