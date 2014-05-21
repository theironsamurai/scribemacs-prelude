Scribemacs - v0.4
==================

Scribemacs is my personal configuration for [Emacs Prelude](https://github.com/bbatsov/prelude) designed with the following in mind:

1. Prose & Poetry writing
2. Web development
3. Functional programming in Clojure & Haskell
4. Portability *(easily installed/updated on my multiple computers and servers)*

Prelude already covers most of that, but to kick it up a notch, I've added some new keybindings, functionality, and altered some of the defaults.

If you're my doppelganger, you might be interested in giving it a go -- or using some of the ideas for your own config.

*NOTE: While I use this on top of Prelude, most of it would work alongside just about any Emacs configuration with a little tweaking.*

## Installation

Scribemacs is meant to be added ON TOP of Emacs + Prelude.

Simply clone this repository, and move the ```scribemacs.el``` file into your ```~/.emacs.d/personal``` folder, which is Prelude's default personal-config folder.

Restart Emacs. It will begin auto-loading all the new shit. Done!

## Features & Non-Features

- Distraction free (text centered) mode
- Word-wrap everywhere *(I told you this was for writers!)*
- Toggle on/off just about everything with new 'hot keys'
- UTF8 everywhere *(with easy lambda, function, and arrow insertion)*
- CUA keys work again
- X-clipboard settings improved
- Auto-loading of some extra packages and themes *(default theme is Cyperpunk)*
- Auto-complete on by default *(via Company-mode)*
- Smex, baby!
- Arrow keys work again :-)
- turn off white-space highlighting
- transient mark mode on
- delete selection mode on
- Clojure REPL fixes

... etc...

## Keymap

In addition to giving you back your CUA keys ```C-c,C-v,C-z```, Scribemacs also gives you a ton of new fancy-pantsy key bindings (and associated functionalities) to make writing and manipulating text even easier.

### Navigation

Keybindings        | Description
-------------------|-------------------------------------------------------
<kbd>C-u</kbd>     | Page Up
<kbd>C-i</kbd>     | Page Down


### Editing & Writerly things

Keybindings        | Description
-------------------|-------------------------------------------------------
<kbd>M-1</kbd>     | Word count (and character, etc)
<kbd>M-2</kbd>     | Speck Check (as you type) on/off
<kbd>M-3</kbd>     | Double Space on/off
<kbd>M-4</kbd>     | Auto-complete (via Company-mode) on/off
<kbd>C-c 1</kbd>   | Set cursor type (bar vs hbar) toggle *(default is bar)*
<kbd>C-c 2</kbd>   | Smart Parenthesis Strict mode *(default: on)*
<kbd>C-c 3</kbd>   | Highlight line mode *(default: off)*
<kbd>C-c 4</kbd>   | Word Wrap (visual-line-mode) - *(default: on)*

### Sneaky Tricks

 Keybindings        | Description
:-------------------|:-------------------------------------------------------
<kbd>C-TAB</kbd>   | Check spelling of word at point (cursor)
<kbd>C-e</kbd>     | Backwards kill word
<kbd>C-"="</kbd>   | Increase text size *(control + equals sign)*
<kbd>C-"-"</kbd>   | Decrease text size *(control + minus sign)*


### GUI Manipulation

 Keybindings        | Description
--------------------|----------------------------------------------------
<kbd>C-9</kbd>     | Select previous window
<kbd>C-0</kbd>     | Select next window
<kbd>f2</kbd>      | Split window vertically (top/bottom)
<kbd>f3</kbd>      | Split window horizontally (left/right)
<kbd>f4</kbd>      | Alternate between vertical & horizontal split windows
<kbd>f5</kbd>      | Delete ALL OTHER windows, exept the one you are IN
<kbd>f6</kbd>      | Delete ONLY the window the cursor is in NOW
<kbd>f7</kbd>      | Line numbers on/off
<kbd>f8</kbd>      | Scroll bar on/off
<kbd>f9</kbd>      | Menu bar on/off
<kbd>M-f9</kbd>    | Tool bar menu on/off
<kbd>f11</kbd>     | Full Screen *(this is already in Prelude, but people forget about it.)*
<kbd>M-f11</kbd>   | Center-text-mode (distraction free)

### Special Character Insert

These are particularly nice for Math and Haskell -- thanks @Bodil!

Keybindings        | Description
-------------------|-------------------------------------------------------
<kbd>M-l</kbd>     | Lambda λ
<kbd>M-f</kbd>     | Function ƒ
<kbd>M--</kbd>     | Arrow →



## Support/Help

None.

This is my personal configuration. If all of this was a cake: Emacs would be the ingredients; Prelude would be the recipe, cake, and icing; Scribemacs is the sprinkles.

In other words, make this your own. Have fun. And get to writing.
