# Scribemacs: Emacs 4 Writers v0.2

**Scribemacs** is a set of defaults and keybindings to make a *prose writers* life easier (and more intuitive) in Emacs.

It runs perfectly fine on a Vanilla Emacs install, as it only leverages what is already there. (The only exception is an optional set of keybindings to go with *Flyspell* if you have it installed.)

### The Point Of Scribemacs

Emacs comes with TONS - literally around 1,000 - different key bindings (which are a subset of the nearly 8,000 different functions and functionalities). 

The WHOLE POINT of using Emacs as a prose editor is to be able to leverage this power.

The only downside is that Emacs was never designed with the needs of the writer FIRST -- of course not, it's a coders editor. 

However, for geeky writers and programmers who enjoy getting their write-on, there is good news! It isn't that hard to make Emacs kick butt as a prose-writers tool.

Scribemacs is a single, short, .el file that will make any writer feel right at home - *assuming, of course, that they were hit with the geek-stick at some point during their youth.*

## Installation

You can either just copy/paste the contents of the ```scribemacs.el``` file into your ```.emacs``` file (or whereever you keep your configurations).

OR, you can clone this repo, and move the file into a directory where you have your configurations. Easy peasy.

### Prelude Users

If you are a Prelude user (or would like to be), and you'd like to take things a step further, then you can install my [Prelude Nemesis]() "plugin" which has Scribemacs already installed. This comes with extra goodies for Clojure and other stuff.

## Features & Non-Features

*Most of these can be toggled on/off via the new key bindings below.*

- Word-wrap (visual-line-mode) is on globally, by default
- Copy/paste works like you're used to (C-c, C-v)
- Word-count displays in mini-buffer via key binding
- Full screen mode
- Distraction free mode
- Menu bar toggle
- Toolbar toggle
- line numbers toggle
- Cursor style is "hbar", a thin horizontal line (toggle for box style)
- Cursor blinks
- Highlight-line-mode off by default (easy toggle on)
- Double space toggle (more like 1.5 space, lol)

## New Key Bindings

- ```<f11>``` = Full-screen toggle
- ```M-<f11>```  = Focus-mode toggle, ala Writeroom/Darkroom/Pyroom (see below)
- ```<f9>```  = Menu-bar-mode toggle (default: on)
- ```M-<f9>``` = Tool-bar toggle
- ```<f8>```  = Scroll-bar toggle (default: on)
- ```<f7>```  = Line numbers toggle, linum-mode (defualt: off)
- ```C-c 1``` = Word-count, displays in mini-buffer
- ```C-c 2```   = Double Space toggle (default: Off)
- ```C-c 3``` = Cursor-style toggle (default: hbar; option: box)
- ```C-c 4``` = Highlight-line-mode toggle (default: on)
- ```C-c 5``` = Word-wrap toggle, vl-mode (default: on *globally*)
- ```C +```   = Text-scale increase (Control and equals/plus key)
- ```C -```   = Text-scale decrease (Control and minus key)

If you're new to Emacs, "C" is the "Control key", and "M" is the "alt" key. You'll be using these often, lol.

## "Focus" Mode

Scribemacs does NOT come with a "one-click" focus-mode. Instead, the goal is to maintain ultimate *modularity*, just like Emacs.

Thankfully, it only takes TWO clicks :-)

How it works is like this:

- Click *f11* to make Emacs full screen (covers everything)
- Click *M-f11* to make the fringe HUGE ;-) this centers the text on screen
- If you only have one window open before clicking *f11*, it will do both at the same time (the first time).

*For obvious reasons, big fringe looks hideous when you have more than one window open!*

Note: the menu, tool bar, scroll bar, and line numbers are all off by default. But, you can turn each of them on/off to match your style.

*KNOWN BUG: Fringe mode toggles on/off sometimes when switching buffers, or going into dir-mode.*

## (Optional) Spell Check

If you install *Flyspell*, then uncomment the lines at the top of the ```scribemacs.el``` file and get:

- ```M-2``` = Spell Check toggle (default: ON)
- ```C-tab``` = Check spelling of word at cursor 

*(note: this is already on in Prelude-Nemesis.)*

## Contribute

If you'd like to help me with this project that would be much appreciated. I know many others would love to set Emacs up as a kick-butt, productive, powerhouse Prose-Writers editor.

## Licence: GPLv2

Copyright (c) 2014, Nick Horton
