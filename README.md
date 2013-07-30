# state

This library allows you to switch back and forth between predefined
workspaces. For example, pressing <kbd>s-s g</kbd> switches to `gnus`
in fullscreen. When you are done with `gnus` your can switch back to
where you were by pressing the same keystroke or <kbd>s-s o</kbd>.
Similarly, you can take a quick peek at your `*Messages*` buffer by
pressing <kbd>s-s m</kbd> and return to where you were by pressing the
same keystroke.

## Installation

Make sure the file `state.el` is in your load path and put the
following in your `.emacs`:
```lisp
(require 'state)
(state-install-bindings)
```

## Customizations

The workspaces are listed in the variable `state-alist`. The simplest
way to specify a workspace is to define, for example:
```lisp
(emacs
 ((key . "e")
  (switch . "~/.emacs.d/init.el")))
```
In this case, pressing <kbd>s-s e</kbd> switches to your `init.el` as
if you were opening it with `find-file` and pressing <kbd>s-s e</kbd> once
again brings you back.

In the case your workspace is not a simple file, you can specify it
with the `state-p` keyword. For example, the `gnus` workspace mentioned
earlier is defined as follows:
```lisp
(gnus
 (key . "g")
 (state-p . (memq major-mode
                  '(message-mode
                    gnus-group-mode
                    gnus-summary-mode
                    gnus-article-mode)))
(switch . (wconf-fullscreen 'gnus (gnus))))
```
`state-p` is a function or a form indicating when we are in `gnus`.
The `switch` keyword specifies a function or a form that performs the
switch. In this case, we use a helper function that stores the window
configuration for us and make sure we are in fullscreen after switching.

## License

Copyright (C) 2013 Sylvain Rousseau <thisirs at gmail dot com>

Author: Sylvain Rousseau <thisirs at gmail dot com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
