# ace-window

**GNU Emacs package for selecting a window to switch to**

## What and why

I'm sure you're aware of `other-window` command. While it's great for
two windows, it quickly loses its value when there are more windows:
you need to call it many times, and since it's not easily predictable,
you have to check each time if you're in the window that you wanted.

Another approach is to use `windmove-left`, `windmove-up`, etc.  These
are fast and predictable. Their disadvantage is that they need 4 key
bindings.  The default ones are shift+arrows, which are hard to reach.

This package aims to take the speed and predictability of `windmove`
and pack it into a single key binding, similar to `other-window`.

## Setup

Just assign `ace-window` to a short key binding, as switching windows
is a common task. I suggest <kbd>M-p</kbd>, as it's short and not
bound in the default Emacs.

## Usage

When there are two windows, `ace-window` will call `other-window`.  If
there are more, each window will have its first character highlighted.
Pressing that character will switch to that window.  Note that, unlike
`ace-jump-mode`, the point position will not be changed: it's the same
behavior as that of `other-window`.

The windows are ordered top-down, left-to-right. This means that if
you remember your window layouts, you can switch windows without even
looking at the leading char.  For instance, the top left window will
always be `1`.

`ace-window` works across multiple frames, as you can see from the
[in-action gif](http://oremacs.com/download/ace-window.gif).

## Swap and delete window

- You can swap windows by calling `ace-window` with a prefix argument <kbd>C-u</kbd>.

- You can delete the selected window by calling `ace-window` with a double prefix argument, i.e. <kbd>C-u C-u</kbd>.

## Change the action midway

You can also start by calling `ace-window` and then decide to switch the action to `delete` or `swap` etc.  By default the bindings are:

- <kbd>x</kbd> - delete window
- <kbd>m</kbd> - swap (move) window
- <kbd>c</kbd> - split window fairly, either vertically or horizontally
- <kbd>v</kbd> - split window vertically
- <kbd>b</kbd> - split window horizontally
- <kbd>n</kbd> - select the previous window
- <kbd>i</kbd> - maximize window (select which window)
- <kbd>o</kbd> - maximize current window

In order for it to work, these keys *must not* be in `aw-keys` and you have to have `aw-dispatch-always` set to `t`.

## Customization
Aside from binding `ace-window`:

    (global-set-key (kbd "M-p") 'ace-window)

maybe you'd like the following customizations:

### `aw-keys`
`aw-keys` - the sequence of leading characters for each window:

    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

`aw-keys` are 0-9 by default, which is reasonable, but in the setup
above, the keys are on the home row.

### `aw-scope`
The default one is `global`, which means that `ace-window` will work
across frames. If you set this to `frame`, `ace-window` will offer you
the windows only on current frame.

### `aw-background`

By default, `ace-window` temporarily sets a gray background and
removes color from available windows in order to make the
window-switching characters more visible. This is the behavior
inherited from `ace-jump-mode`.

This behavior might not be necessary, as you already know the locations
where to look, i.e. the top-left corners of each window.
So you can turn off the gray background with:

    (setq aw-background nil)

### `aw-dispatch-always`

When non-nil, `ace-window` will issue a `read-char` even for one window.
This will make `ace-window` act differently from `other-window` for one
or two windows. This is useful to change the action midway
and execute other action other than the *jump* default.
By default is set to `nil`

### `aw-dispatch-alist`

This is the list of actions that you can trigger from `ace-window` other than the
*jump* default.
By default is:

    (defvar aw-dispatch-alist
    '((?x aw-delete-window " Ace - Delete Window")
        (?m aw-swap-window " Ace - Swap Window")
        (?n aw-flip-window)
        (?c aw-split-window-fair " Ace - Split Fair Window")
        (?v aw-split-window-vert " Ace - Split Vert Window")
        (?b aw-split-window-horz " Ace - Split Horz Window")
        (?i delete-other-windows " Ace - Maximize Window")
        (?o delete-other-windows))
    "List of actions for `aw-dispatch-default'.")

If the pair key-action is followed by a string, then `ace-window` will be
invoked again to be able to select on which window you want to select the
action. Otherwise the current window is selected.
