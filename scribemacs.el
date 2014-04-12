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

;;  Set Margins

                                        ; (setq-default left-margin-width 5)

;;  Remove Fringe

(set-fringe-mode 15)

;;  Visual Line Mode default in all text buffers

(add-hook 'text-mode-hook 'visual-line-mode)

;;  Turn off hl-line-mode (it highlights white-space when on)

(global-hl-line-mode -1)

;; turn on scroll bar

(scroll-bar-mode 1)

;;  Turn on Blinking Cursor

(blink-cursor-mode 1)

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

;; Line numbers on left toggle ON/OFF with C-c f9

(global-set-key (kbd "<f7>")
                '(lambda()(interactive)
                   (linum-mode 'toggle)
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


;;; --- Toggle Focus Mode.


(defun toggle-minimal-mode (fs)
  (interactive "P")
  (defun fullscreen-margins nil
    (if (and (window-full-width-p) (not (minibufferp)))
        (set-window-margins nil (/ (- (frame-width) 80) 2) (/ (- (frame-width) 80) 2))
      (mapcar (lambda (window) (set-window-margins window nil nil)) (window-list))))

  (cond (menu-bar-mode
         (menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode 1)
;         (set-frame-height nil (+ (frame-height) 4))
         (if fs (progn (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                                              '(1 "_NET_WM_STATE_FULLSCREEN" 0))
                       (add-hook 'window-configuration-change-hook 'fullscreen-margins))))
        (t (menu-bar-mode 1) (tool-bar-mode -1) (scroll-bar-mode 1)
           (when (frame-parameter nil 'fullscreen)
             (remove-hook 'window-configuration-change-hook 'fullscreen-margins)
             (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                                    '(0 "_NET_WM_STATE_FULLSCREEN" 0))
             (set-window-buffer (selected-window) (current-buffer)))
           (set-frame-width nil (assoc-default 'width default-frame-alist)))))

(global-set-key [f9] 'toggle-minimal-mode)


;;; --- More Key Bindings


;; text scale increase/decrease
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)


(provide 'scribemacs)

;;; scribemacs.el ends here
