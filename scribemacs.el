;;; scribemacs.el --- Emacs for writers!
;;
;; Copyright (c) 2014 Nick Horton
;;
;; Author: Nick Horton
;; URL: http://sapiengames.com
;; Version: 0.2.0.0
;; Keywords: awesome, scribe, emacs, samurai

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Modular defaults to make your writing life easier!

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;;;  --- (OPTIONAL) SPELL CHECK -----------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;  You must have "Flyspell" installed!
;;
;;  Then, come back and uncomment the following lines :-)
;;  
;;  This will give you 2 new key bindings, and turn Spell check on
;;  by default.
;;  You can turn auto-spell-check OFF with "M-2" and "C-tab" will
;;  check the spelling of the word your cursor is on. :-)

;(global-set-key (kbd "<C-tab>") 'ispell-word)
;(flyspell-mode 1)
;(global-set-key (kbd "M-2")
;                '(lambda()(interactive)
;                   (flyspell-mode 'toggle)
;                   (message "Spell Check!")))


;;;  --- DISPLAY SETTINGS -----------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; --- Line Numbers (f7)

(global-set-key (kbd "<f7>")
                '(lambda()(interactive)
                   (global-linum-mode 'toggle)
                   (message "Line Numbers Toggle!")))

;; Add vertical line to right of line-numbers
(setq linum-format "%6d \u2502\ ")


;;  --- Scroll Bar (f8)

(scroll-bar-mode -1)
(global-set-key (kbd "<f8>")
                '(lambda()(interactive)
                   (scroll-bar-mode 'toggle)
                   (message "Scroll Bar Toggle")))

;; --- Menu (f9)

(global-set-key (kbd "<f9>")
                '(lambda()(interactive)
                   (menu-bar-mode 'toggle)
                   (message "Menu Toggle!")))

;;  --- Tool Bar (M-f9)

(global-set-key (kbd "M-<f9>")
                '(lambda()(interactive)
                   (tool-bar-mode 'toggle)
                   (message "Menu Toggle!")))


;;  --- Full Screen (f11)

(defun toggle-fullscreen () ;; thanks to Ivan Kanis
  "Toggle full screen on X11."
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(global-set-key [f11] 'toggle-fullscreen)


;;  --- Fringe Focus Mode (M-f11)

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

;; Toggle Fringe-Focus: "M-f11" function key
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





;;;  --- TEXT SETTINGS --------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  --- Defaults For Modernization
;;  Includes CUA mode, usuability tweaks, etc
(require 'ido)
(cua-mode 1)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(ido-mode t)
(delete-selection-mode t)
(transient-mark-mode t)

;;  --- Undo Tree (C-x u)

; (use-package undo-tree
; :init (global-undo-tree-mode))

;;  ---text scale increase/decrease (C +/=)(C -)

(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)


;;  --- Word Count (C-c 1)

(defalias 'word-count 'count-words)
(global-set-key (kbd "C-c 1") 'word-count)


;;  --- Double Space (C-c 2)

(defun toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height."
  (interactive)
  (if (eq line-spacing nil)
      (setq-default line-spacing 0.4) ; add 0.4 height between lines
    (setq-default line-spacing nil)   ; no extra heigh between lines
    )
  (redraw-display))

(global-set-key (kbd "C-c 2") 'toggle-line-spacing)


;;  --- Cursor (C-c 3)

(blink-cursor-mode 1)
(setq-default cursor-type 'hbar)
(global-set-key (kbd "C-c 3")
                '(lambda()(interactive)
                   (if (eq cursor-type 'box)
                       (setq cursor-type 'hbar)
                     (setq cursor-type 'box))))


;;  --- Highlight Line Mode (C-c 4)

(global-hl-line-mode -1)

(global-set-key (kbd "C-c 4")
                '(lambda()(interactive)
                   (hl-line-mode 'toggle)
                   (message "Highlight current line!")))


;;  --- Word Wrap (C-c 5)

(global-visual-line-mode 1)
(global-set-key (kbd "C-c 5")
                '(lambda()(interactive)
                   (visual-line-mode 'toggle)
                   (message "Word Wrap!")))

(provide 'scribemacs)

;;; scribemacs.el ends here
