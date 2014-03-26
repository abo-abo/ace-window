# ace-window

**GNU Emacs package for selecting a window to switch to**

## What and why

I'm sure you're aware of `other-window` command. While it's great for
two windows, it quickly loses it's value when there are more windows:
you need to call it many times, and since it's not easily predictable,
you have to check each time if you're in the window that you wanted.

Another approach is to use `windmove-left`, `windmove-up` etc.  These
are fast and predictable. Their disadvantage is that they need 4 key
bindings.  The default ones are shift+arrows, which are hard to reach.

This package aims to take the speed and predictability of `windmove`
and pack it into a single key binding, similar to `other-window`.

## Setup

Just assign `ace-window` to a short key binding, as switching windows
is a common task. I suggest **M-p** as it's short and not bound in the
default Emacs.

## Usage

When there are two windows, `ace-window` will call `other-window`.  If
there are more, each window will have its first character highlighted.
Pressing that character will switch to that window.  Note that unlike
`ace-jump-mode`, the point position will not be changed: it's the same
behavior as that of `other-window`.

The windows are ordered top-down, left-to-right. This means that if
you remember your window layouts, you can switch windows without even
looking at the leading char.  For instance, the top left window will
always be `1`.

## Customization

Aside from binding `ace-window`, maybe you'd like to customize
`aw-keys` - the sequence of leading characters for each window:

    (global-set-key (kbd "M-p") 'ace-window)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

`aw-keys` are 0-9 by default, which is reasonable, but in the setup
above, the keys are on the home row.
