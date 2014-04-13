;;; scribemacs.el -- Emacs minor mode for Prose Writers!
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

;;; Commentary:

;;; ----------------- GOAL --------------------------------
;;      1. Make prose writing as elegant, simple, and fun
;;         as it is in Pyroom, Writeroom, or WriteMonkey...
;;
;;      2. With the added beauty of Sublime-Text-like themes,
;;         and their syntax highlighting for markdown,
;;         latex, etc...
;;
;;      3. And the shear power of Emacs!
;;
;;    That's 3 for the price of one!
;;; -------------------------------------------------------

;;; Code:

;; --- Dependencies
;;
;;  !important!
;;  These require that you have the "Melpa"
;;  package repository set up for use.
;;  See the README for help.
;;  Trust me, you want it anyway :-)


(defvar sweet-packages '(dired-details
                         dired-details+
                         smex
                         flyspell
                         tabbar
                         ))

(dolist (p sweet-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'flyspell)
(require 'dired-details)
(require 'dired-details+)
(require 'ido)
(require 'smex)
(require 'tabbar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Theme Loader

(defvar sweet-themes '(gandalf-theme
                       cyberpunk-theme
                       sublime-themes
                       solarized-theme
                         ))

(dolist (p sweet-themes)
  (when (not (package-installed-p p))
    (package-install p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  --- Word Wrap
;;  Toggle: "C-c 5"
;;
;;  Default is ON in ALL buffers!

(global-visual-line-mode 1)
(global-set-key (kbd "C-c 5")
                '(lambda()(interactive)
                   (visual-line-mode 'toggle)
                   (message "Word Wrap!")))

;;  --- Ispell Word
;;  Key: "<C-tab>"
(global-set-key (kbd "<C-tab>") 'ispell-word)

;;  --- Highlight Line Mode:
;;  Toggle: "C-c 4"
(global-hl-line-mode -1)

(global-set-key (kbd "C-c 4")
                '(lambda()(interactive)
                   (hl-line-mode 'toggle)
                   (message "Highlight current line!")))


;;  --- Cursor
;;  Toggle: "C-c 3"
;;
;;  Switch between a thin horizontal line
;;  and a block-style cursor.
;;  Default is thin-line

(blink-cursor-mode 1)

(setq-default cursor-type 'hbar)

(add-hook 'text-mode-hook '(setq cursor-type 'hbar))

(global-set-key (kbd "C-c 3")
                '(lambda()(interactive)
                   (if (eq cursor-type 'box)
                       (setq cursor-type 'hbar)
                     (setq cursor-type 'box))))

;;  --- Spell Check
;;  Toggle: "C-c 2"
;;
;;  Checks spelling while you type and highlights
;;  the mistakes.

(flyspell-mode 1)
(global-set-key (kbd "C-c 2")
                '(lambda()(interactive)
                   (flyspell-mode 'toggle)
                   (message "Spell Check!")))


;;  --- CUA: copy, paste
;;  Making Emacs fit in a little better.
;;  "C-c" is cut, "C-v" is paste.
;;  Also, makes it work with other programs
;;  like your web browser, for copying between
;;  them.
;;
;;  Warning: it IS a little wonky because
;;  of how Emacs deals with it's "killing".

(cua-mode 1)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)


;;  --- Word Count
;;  Toggle: "C-c 1"

(defalias 'word-count 'count-words)
(global-set-key (kbd "C-c 1") 'word-count)


;;  --- Double Space
;;  Toggle: "M-2"
;;  Default: Off

(defun toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height."
  (interactive)
  (if (eq line-spacing nil)
      (setq-default line-spacing 0.4) ; add 0.5 height between lines
    (setq-default line-spacing nil)   ; no extra heigh between lines
    )
  (redraw-display))

(global-set-key (kbd "M-2") 'toggle-line-spacing)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Menu
;; Toggle: "f9"


(global-set-key (kbd "<f9>")
                '(lambda()(interactive)
                   (menu-bar-mode 'toggle)
                   (message "Menu Toggle!")))


;;  --- Scroll Bar
;;  Toggle: "f8"
;;  Default: Off

(scroll-bar-mode -1)
(global-set-key (kbd "<f8>")
                '(lambda()(interactive)
                   (scroll-bar-mode 'toggle)
                   (message "Scroll Bar Toggle")))


;; --- Line Numbers
;; Toggle: "f7"
;; Default: Off

(global-set-key (kbd "<f7>")
                '(lambda()(interactive)
                   (global-linum-mode 'toggle)
                   (message "Line Numbers Toggle!")))


;; --- Full Screen
;; Toggle: "f11"
;; Covers the entire screen and turns off all menu/sidebar/etc
;; and centers text
;;
;; WARNING: doesn't allow you to center text when Scribe "helper"
;; is active. AKA, when you toggle ON <f9> it gets screwy.

(defun toggle-fullscreen () ;; thanks to Ivan Kanis
  "Toggle full screen on X11."
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(global-set-key [f11] 'toggle-fullscreen)

;;;  -------------------------------------------------------------
;;;  ---------------------FOCUS MODE------------------------------
;;;  -------------------------------------------------------------


;;; --- "Focus Mode" (Fringe-style)
;;  Default: Off
;;  Toggle: "M-f11"
;;
;; This is a great focus mode by Bzg. You can read
;; his blog post here:
;;
;;     http://bzg.fr/emacs-strip-tease.html
;;
;; I have made only minor modifications.

;;; --- BEGIN BZG's WORK

(defvar bzg-big-fringe-mode nil)
(define-minor-mode bzg-big-fringe-mode
  "Minor mode to use big fringe in the current buffer."
  :init-value nil
  :global t
  :variable bzg-big-fringe-mode
  :group 'editing-basics
  (if (not bzg-big-fringe-mode)
      (set-fringe-style nil)
    (set-fringe-mode
     (/ (- (frame-pixel-width)
           (* 85 (frame-char-width)))
        2))))

;; Default is OFF
(bzg-big-fringe-mode -1)

;; Toggle Fringe-Focus: "f9" function key
(global-set-key (kbd "M-<f11>")
                '(lambda()(interactive)
                   (bzg-big-fringe-mode 'toggle)
                   (message "Big Fringe!")))


;; To activate the fringe by default and deactivate it when windows
;; are split vertically, uncomment this:
 (add-hook 'window-configuration-change-hook
           (lambda ()
             (if (delq nil
                       (let ((fw (frame-width)))
                         (mapcar (lambda(w) (< (window-width w) fw))
                                 (window-list))))
                 (bzg-big-fringe-mode 0)
               (bzg-big-fringe-mode 1))))

;; To get rid of the indicators in the fringe, uncomment this:
; (mapcar (lambda(fb) (set-fringe-bitmap-face fb 'org-hide))
;        fringe-bitmaps)


;;; --- END bzg's work


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;  ---text scale increase/decrease
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; --- Tab Bar Mode
;; Toggle: "M-f7"

(tabbar-mode -1)

(global-set-key (kbd "M-<f7>")
                '(lambda()(interactive)
                   (tabbar-mode 'toggle)
                   (message "Tabbar mode")))


;;; ---------------------------------------------------------

(provide 'scribemacs)

;;; scribemacs.el ends here
