;;; scribe-mode.el -- Emacs minor mode for Prose Writers!
;;
;;  Copyright (c) 2014, Nick Horton
;;
;;  Aka: The Iron Samurai
;;  Main Blog: TheIronSamurai.com
;;  Geek Blog: SapienGames.com
;;
;;  License: GPLv2
;;
;;; This is not a part of GNU Emacs
;;
;;; Commentary:
;;
;;
;;   This is Prelude4Writers!
;;
;;    A simple customization to Emacs Prelude
;;    designed specifically to help make
;;    prose writing easier in Emacs.
;;
;;
;;
;;; ----------------- GOAL --------------------------------
;; 	1. Make prose writing as elegant, simple, and fun
;;         as it is in Pyroom, Writeroom, or WriteMonkey...
;;
;; 	2. With the added beauty of Sublime-Text-like themes,
;;         and their syntax highlighting for markdown,
;;         latex, etc...
;;
;; 	3. And the shear power of Emacs + Prelude.
;;
;;    That's 3 for the price of one!
;;; -------------------------------------------------------

;;; Code:

;;; ----------------------------------------------------------
;;; ----------------- Prelude Disabling ----------------------
;;; ----------------------------------------------------------
;;
;;  I know... Why am I using Prelude if I'm just going around
;;  disabling functions from it! For the same reasons a
;;  sculpter takes a beautiful hunk of marble
;;  and then starts chipping away at it until it's looking
;;  just as they want it to. Prelude is near perfect for our needs,
;;  it simply wasn't designed for a prose writer.
;;    These little tweaks make all the difference.
;;
;;; ----------------------------------------------------------


;; Prelude Disable: White Space Highlighting

;; (setq prelude-whitespace nil)


;; Prelude Disable: Arrow Keys Off

;; (setq guru-warn-only t)


;;; --------------------  Dired-Details+ ------------------------
;;
;;    You need to install this via package manager first. Then,
;;    come back and uncomment this line:
;;
;;; ------------------------------------------------------------


; (require 'dired-details+)


;;; -------------------------------------------------------------
;;; ------------------ Visual Tweaks ----------------------------
;;; -------------------------------------------------------------


;;  Set Margins

(setq-default left-margin-width 5 right-margin-width 5)


;;  Remove Fringe

(set-fringe-mode 0)


;;  Visual Line Mode default in all text buffers

(add-hook 'text-mode-hook 'visual-line-mode)

;;  Turn off hl-line-mode (it highlights white-space when on)

(global-hl-line-mode -1)


;;  Turn on Blinking Cursor

(blink-cursor-mode 1)


;;; -----------------------------------------------------------
;;; ------------------ Usability Tweaks -----------------------
;;; -----------------------------------------------------------

;;  CUA default - cut/copy/paste like "normal"

(cua-mode 1)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)


;;  Word Count (on demand!)

(defalias 'word-count 'count-words)


;;; -------------LOAD Config Files--------------------

;; defaults: turning modes on, and requiring them, etc
(add-to-list 'load-path "config/")
(load "scribe-package-loader.el")
(load "scribe-theme-loader.el")
(load "scribe-defaults.el")
(load "scribe-bindings.el")
(load "scribe-focus-mode.el")



(provide 'scribe-mode)

;;; scribe-mode.el ends here
