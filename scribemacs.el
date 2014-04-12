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
;;
;;; Commentary:
;;
;;
;;   This is a relatively simple minor mode for Emacs
;;   designed with prose-writing in mind.
;;
;;
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

;; first, load in dependencies
; (add-to-list 'load-path' "/scribe-config/")
; (load "scribe-automargin.el")
; (load "scribe-prelude.el")
; (load "scribe-package-loader.el")

;;  Visual Line Mode default in all text buffers

(add-hook 'text-mode-hook 'visual-line-mode)

;;  Turn off hl-line-mode (it highlights white-space when on)

(global-hl-line-mode -1)

;; turn on scroll bar

(scroll-bar-mode 1)

;;  Turn on Blinking Cursor

(blink-cursor-mode 1)

;; Use a minimal cursor
(setq-default cursor-type 'hbar)

;;  CUA default - cut/copy/paste like "normal"

(cua-mode 1)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

;;  Word Count (on demand!)

(defalias 'word-count 'count-words)

;; require ido

(require 'ido)

;;; ---------------- Toggling ON/OFF -----------------------------

;; Menu toggle ON/OFF with <f9> function key

(global-set-key (kbd "<f8>")
                '(lambda()(interactive)
                   (menu-bar-mode 'toggle)
                   (message "Menu Toggle!")))

;; Line numbers on left toggle ON/OFF with <f7>

(global-set-key (kbd "<f7>")
                '(lambda()(interactive)
                   (global-linum-mode 'toggle)
                   (message "Line Numbers Toggle!")))


;; toggle full screen Emacs session with <f11> function key
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




;;; --- "Focus Mode", aka, bzg-big-fringe-mode
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

;; Now activate this global minor mode
(bzg-big-fringe-mode 1)

(global-set-key (kbd "<f9>")
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

;; To get rid of the indicators in the fringe
; (mapcar (lambda(fb) (set-fringe-bitmap-face fb 'org-hide))
;        fringe-bitmaps)

;;; --- END bzg's work


;;; --- More Key Bindings


;; text scale increase/decrease
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)


(provide 'scribemacs)

;;; scribemacs.el ends here
