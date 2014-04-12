# Scribemacs: Emacs 4 Writers


**Scribemacs** is an Emacs "overlay" that makes default the things a non-fiction prose-writer would find essential and/or nice -- assuming this "avatar" is me :-)

Everything from default word-wrapping and spell check, to a full-screen, "no distractions" mode, to smart auto-complete, to a nicer looking cursor. (See below for details.)

It is *highly* modular. Each piece can be toggled ON/OFF with its own key binding -- in the spirit of Emacs.

So while Scribemacs is quite oppinionated to suit my personal tastes as a writer, you can use it as a base and make it your own quite easily.

## Word(s) of Warning: Expectations & Who This Is For

The *entire purpose* of Scribemacs is to make Emacs kick more ass for a *Writer*... not necessarily a programmer.

Scribemacs IS opinionated.

It "takes over" your defaults and changes quite a few of them.

The defaults it chooses are geared towards the various 'text-modes' in Emacs (markdown, Org, Latex, etc), rather than programming language modes.

It also assumes you are like me. Afterall, I created this to make MY Emacs act the way I want it to. If you have different interests, goals, tastes, then maybe you'll want to alter the defaults and/or use something else entirely.

Again, everything is "toggle-able". So, of course, you can be a programmer and use this if you like the defaults. But, my "avatar" user is a writer who is quite geeky, rather than a programmer who is also a writer.

*Scribemacs is very much still in the beta-testing phase. But, totally usable. I use it every day* :-)

## Dependencies: Melpa

Obviously, you need Emacs.

You'll also need the *Melpa Package Repository* installed. If you don't already, add this to your init file, or whereever you store this.

~~~~
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )
~~~~

Once you've got that, all other dependencies will be checked by Scribemacs, and auto-installed if you don't already have them. *(still in beta!)*

These include:

- Flyspell
- Dired-details
- Dired-details+
- Smex
- Auto-complete-mode

As well as a few themes (see below).

## (Optional) Emacs Prelude

I *strongly* favor using [Emacs Prelude]() alongside Scribemacs.

That will automatically install a ton of extra, hyper-useful, features that you'd likely install eventually anyway.

It's what I use, so that clearly influences the direction I am taking the Scribemacs project -- as a "meta" distribrution.

That said, my goal is to make sure Scribemacs is totally independent, and will work fine on a "vanilla", default, Emacs so that any prose writer can use it without needing to get all fancy.

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

If you are a Prelude user as I am, then you can "install" this as a *Prelude plugin*.

Start by installing my [Prelude-Plugin-Installer](), then follow the instructions.

You may want to also install my [Prelude-Plus-Pack]() which makes this environment even more friendly -- not just for a prose-writer, but also for Clojure.

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
- Highlight-line-mode on by default (easy toggle off)
- Cleaner dired-mode via Dired-details and Dired-details+


*Note: most of this stuff is already IN Emacs. But, the point of Scribemacs is to make it easier to use, and/or set these as defaults -- as 1st class citizens -- rather than options only used on occation.*

**Themes**

I've set the default theme to *Gandalf*, which is a very nice white-background theme that looks particularly nice in Markdown-mode.

Scribemacs will auto-install a collection of themes on first-start-up, if they aren't already. Again, the assumption is that you are a writer. Most writers I know love to change the look of their writing environment often -- and easily.

Among the themes auto-installed are the classic "solarized" themes, sublime-text themes, and my personal favorite dark theme: cyberpunk.

## New Key Bindings

- ```<f11>``` = Full-screen toggle
- ```<f9>```  = Focus-mode toggle, ala Writeroom/Darkroom/Pyroom
- ```<f8>```  = Menu-bar-mode toggle (default: on)
- ```<f7>```  = Line numbers toggle, linum-mode (defualt: off)
- ```C-c 1``` = Word-count, displays in mini-buffer
- ```C-c 2``` = Spell check toggle, Flyspell (defualt: on)
- ```C-c 3``` = Cursor-style toggle (default: hbar; option: box)
- ```C-c 4``` = Highlight-line-mode toggle (default: on)
- ```C-c 5``` = Word-wrap toggle, vl-mode (default: on *globally*)
- ```C-c 6``` = Auto-complete-mode (default: on)



