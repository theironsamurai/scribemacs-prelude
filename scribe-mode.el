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
;;   This is a relatively simple minor mode for Emacs
;;   designed with prose-writing in mind.
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
;; 	3. And the shear power of Emacs!
;;
;;    That's 3 for the price of one!
;;; -------------------------------------------------------

;;; Code:


;;; -------------LOAD Config Files--------------------

;; defaults: turning modes on, and requiring them, etc
(add-to-list 'load-path "scribe-config/")
(load "scribe-package-loader.el")
; (load "scribe-theme-loader.el")
(load "scribe-defaults.el")
(load "scribe-bindings.el")
(load "scribe-focus-mode.el")
; (load "scribe-prelude.el") ;; only necessary if you are using Prelude


(provide 'scribe-mode)

;;; scribe-mode.el ends here
