Prelude Scribemacs - v0.3
======================

Emacs Prelude for prose writers, web designers, and dablers in functional programming -- in other words. If you are my dual from another dimension, you might find this usefull :-)

This is my *personal* config, that adds a number of new defaults, key bindings, packages, themes, and such. I use mine primarily for Prose writing, web-dev, and functional programming in Clojure and Haskell. 

*This is still very much in beta. But if you'd like to snag some of these ideas for your own config, I hope they come in handy.*

## Installation

As the name implies, Prelude-Scribemacs is meant to be added to your ```~/.emacs.d/personal``` folder, which is Prelude's default personal-config folder. 

To make this a bit more modular -- like a plugin -- I literally create a folder in the personal dir called "plugins", then clone this repo inside of that one, then tell prelude where to find it.

### Step 1

How I do it is I make another folder inside of that one:

~~~~
~/.emacs.d/personal/plugins/
~~~~

Open a terminal. Then git clone this repository inside of that folder:

~~~~
 cd ~/.emacs.d/personal/plugins/
 git clone https://github.com/theironsamurai/prelude-scribemacs.git
~~~~

(Optional) While you are at it, you can clone my Scribemacs project as a plugin as well:

~~~~
 git clone https://github.com/theironsamurai/scribemacs.git
~~~~

### Step 2

Now, you'll need to tell Prelude to look for the plugin.

If you don't yet have a personal .el file (or more) in your personal folder, then now is a good time to make one.

Create a file called ```prelude-plugin-loader.el```. (You can name it anything you want, of course.)

Copy this code inside of it:

~~~~
;; tell Prelude to add the "plugins" folder to
;; it's load path. 

(defvar prelude-personal-plugins-dir
  (concat prelude-personal-dir "/plugins/"))
(let ((default-directory prelude-personal-plugins-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; Load Prelude-scribemacs plugin 

(add-to-list 'load-path "plugins/scribemacs")
(load "scribemacs.el")
~~~~

### Step 3

Restart Emacs!

## Features & Non-Features

*Note: the following isn't complete -- it doesn't reflect all the changes. I'll update this readme soon, don't worry.*

- Auto-load key packages
- Auto-load some nice themes
- Auto-complete on by default
- Smex with "C-t"
- Arrow keys work again :-)
- turn off white-space highlighting
- tabs like a web browser (when you want them)
- transient mark mode on
- delete selection mode on
- Clojure REPL fixes
- 

## Key Bindings

- ```M-2``` = Toggle Flyspell auto check off/on
- ```C-tab``` = Spell check word at cursor
- ```C-t``` = Smex
- ```M-3``` = Toggle smartparens-strict-mode (default: on)
- ```C-c 6``` = Toggle real-global-auto-complete-mode
- ```M-<f7>``` = Toggle tabbar mode (default: off)

*Note: Scribemacs adds even more for setting window full screen, distraction free modes, CUA mode, visual line mode always, toggling on/off the menu, toolbar, line numbers, etc.*
