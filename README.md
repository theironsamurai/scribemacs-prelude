# Scribemacs: Emacs 4 Writers v0.1 - BETA

**Scribemacs** is an Emacs "overlay" that makes default the things a *prose-writer* might find essential and/or nice.

Everything from default word-wrapping and spell check, to a full-screen, "no distractions" mode, to a nicer looking cursor. (See below for details.)

Defaults that make sense to a writer.

It is *highly* modular. Each piece can be toggled ON/OFF with its own key binding -- in the spirit of Emacs. 

So while Scribemacs is quite oppinionated to suit my personal tastes as a writer, you can use it as a base and make it your own quite easily.

## Preliminaries

### Word(s) of Warning: Expectations & Who This Is For

The *entire purpose* of Scribemacs is to make Emacs kick more ass for a *Writer*... not necessarily a programmer.

Scribemacs IS opinionated.

It "takes over" your defaults and changes quite a few of them.

The defaults it chooses are geared towards the various 'text-modes' in Emacs (markdown, Org, Latex, etc), rather than programming language modes.

It also assumes you are like me. Afterall, I created this to make MY Emacs act the way I want it to. If you have different interests, goals, tastes, then maybe you'll want to alter the defaults and/or use something else entirely.

Again, everything is "toggle-able". So, of course, you can be a programmer and use this if you like the defaults.

**My "avatar" user is a writer who is quite geeky, rather than a programmer who is also a writer.**

*Scribemacs is very much still in the beta-testing phase. But, totally usable. I use it every day* :-)

### Not A "Minimal" Writing Editor

Many people are interested in minimal writing environments. That's understandable, given that distraction-free wins over destraction-full.

But, there is a big difference between a one-trick-pony, distraction-free, minimal text editor -- and a powerful, multi-trick-pony, back-from-the-future editor that INCLUDES a distraction-free MODE.

Scribemacs includes multiple options for minimal writing environments, distraction-free options, etc.  But, it doesn't limit you to them, or even in how you set yours up.

More over, each of these options (and more) can be toggled on/off via the keybindings below.

- Do you want totally distraction-free, text-centered, no-menu, no-scroll-bar, no-tabs, no-line-numbers, etc? You got it.
- Do you want minimal, but still have the scroll bar? You can do that, too.
- How about full screen (cover whole screen), but still have menu, scroll bar, line numbers, and tabs? Yep.

It's all up to you. Welcome to the Emacs lifestyle :-)

If you actually want a minimal text editor, I would suggest a typewriter, or sheet of paper with a quill. There are times when those options ARE the best options. I won't deny it!

But, when I'm using my computer... I want the power of a computer behind me, even if I AM using a minimal writing mode in that moment.

I assume anyone wanting to use Scribemacs feels the same way. 

### Dependencies: Melpa

Obviously, you need Emacs.

You'll also need the *Melpa Package Repository* installed. If you don't already, add this to your init file, or whereever you store your customizations.

~~~~
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )
~~~~

Once you've got that, all other dependencies will be checked by Scribemacs, and auto-installed if you don't already have them. *(still in beta!)*

These include:

- Flyspell (spell-checker)
- Dired-details
- Dired-details+

As well as a few others, and a handful of themes.

### (Optional) Emacs Prelude

I *strongly* favor using [Emacs Prelude]() alongside Scribemacs.

It will automatically install a ton of extra, hyper-useful, features that you'd likely install eventually anyway.

It's what I use, so that clearly influences the direction I am taking the Scribemacs project -- as a "meta-alt" distribrution.

That said, my goal is to make sure Scribemacs is totally independent, and will work fine on a "vanilla", default, Emacs so that any prose writer can use it without needing to get all fancy.

However, my own purpose in using Emacs -- of all things -- to write prose in is so that I have a *powerful* editor that can do all kinds of crazy shit standard "prose editors" simply can't do.

Prelude is a meta-Emacs distribution that takes the idea of power and runs with it, all while "modernizing" it a bit.

If you go this route -- Emacs + Prelude + Scribemacs = Awesome -- then, you might also like to include my [Prelude-Plus-Pack](https://github.com/theironsamurai/prelude-plus-pack) and *Prelude-Plugin-Loader* which I use to take it even further.

These last two are being built to compliment -- rather than clash with -- Scribemacs.


## Installation

### Lazy Method

Copy/paste the contents of the ```scribemacs.el``` file into your ```.emacs``` file, or into a file in your ```.emacs.d``` folder.

Restart Emacs.

It will check if you have the necessary dependencies. If not, it will install them, so this could take a few minutes.

### Slightly Less Lazy

Clone this repo to your computer

~~~~
git clone https://github.com/theironsamurai/scribemacs.git
~~~~

- Move the ```scribemacs.el``` file inside of it to whereever you keep your custom configurations
- Restart Emacs

### As Prelude "Plugin"

If you are a Prelude user as I am, then you can "install" this as a *Prelude plugin*. Just take my ```scribemacs.el``` file, and place it within your ```~/.emacs.d/personal/``` folder.

Prelude automatically runs any elisp file within that folder.


OR
Start by installing my [Prelude-Plugin-Loader](https://github.com/theironsamurai/prelude-plugin-loader), then follow the instructions.

You may want to also install my [Prelude-Plus-Pack](https://github.com/theironsamurai/prelude-plus-pack) which makes this environment even more friendly -- not just for a prose-writer, but also for Clojure.



## Features & Non-Features

*Most of these can be toggled on/off via the new key bindings below.*

- Word-wrap (visual-line-mode) is on globally, by default
- Spell-checker on by default
- Copy/paste works like you're used to (C-c, C-v)
- Word-count displays in mini-buffer via key binding
- Full screen mode
- Distraction free mode
- Menu toggles on and off
- line numbers toggle on and off
- Cursor style is "hbar", a thin horizontal line
- Cursor blinks
- Highlight-line-mode off by default (easy toggle on)
- Cleaner dired-mode via Dired-details and Dired-details+
- Tabs, like a browser (default: off)
- Double space toggle (more like 1.5 space, lol)

*Note: most of this stuff is already IN Emacs and/or Prelude. But, the point of Scribemacs is to make it easier to use, and/or set these as defaults -- as 1st class citizens -- rather than options only used on occation.*


## New Key Bindings

- ```<f11>``` = Full-screen toggle
- ```M-<f11>```  = Focus-mode toggle, ala Writeroom/Darkroom/Pyroom
- ```<f9>```  = Menu-bar-mode toggle (default: on)
- ```<f8>```  = Scroll-bar toggle (default: on)
- ```<f7>```  = Line numbers toggle, linum-mode (defualt: off)
- ```M-<f7>``` = Tabbar mode toggle (default: off)
- ```C-c 1``` = Word-count, displays in mini-buffer
- ```C-c 2``` = Spell check toggle, Flyspell (defualt: on)
- ```C-c 3``` = Cursor-style toggle (default: hbar; option: box)
- ```C-c 4``` = Highlight-line-mode toggle (default: on)
- ```C-c 5``` = Word-wrap toggle, vl-mode (default: on *globally*)
- ```C-=```   = Text-scale increase (Control and equals/plus key)
- ```C--```   = Text-scale decrease (Control and minus key)
- ```M-2```   = Double Space toggle (default: Off)
- ```C-tab``` = Check spelling of word at cursor

## Contribute

If you'd like to help me with this project that would be much appreciated. I know many others would love to set Emacs up as a kick-butt, productive, powerhouse Prose-Writers editor.

## Licence: GPLv2

Copyright (c) 2014, Nick Horton
