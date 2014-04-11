;;; prose-mode.el -- Emacs For Prose Writers!
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



;;; ---------------Toggle Full-Screen Minimal mode----------------------
;;
;;   Comes with these (standard) key-bindings:
;;
;; 	1. (f11) toggles on/off menus and other silly stuff
;;
;; 	2. (C-u f11) toggles true minimal mode with centered text (ish!)
;;
;;
;;  Credit: I'm pretty sure I got this off the EmacsWiki, but I can't
;;          honestly remember at this point.
;;
;;  NOTE:  You can change the default "full screen" width by
;;         altering the numbers in the 5th line of the code below.
;;         IF you want wider text, change the numbers after
;;                    (frame-width) 80
;;         to something bigger, like:
;;                    (frame-width) 110
;;         Or, make it smaller, if you want it skinnier!
;;         It's up to you.
;;         Do that for each one.
;;; --------------------------------------------------------------------

(defun toggle-minimal-mode (fs)
  (interactive "P")
  (defun fullscreen-margins nil
    (if (and (window-full-width-p) (not (minibufferp)))
        (set-window-margins nil (/ (- (frame-width) 80) 2) (/ (- (frame-width) 80) 2))
      (mapcar (lambda (window) (set-window-margins window nil nil)) (window-list))))

  (cond (menu-bar-mode
         (menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode 1)
         (set-frame-height nil (+ (frame-height) 4))
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

(global-set-key [f11] 'toggle-minimal-mode)








;;; ---------------------------------------------
;;;            END CUSTOMIZE
;;;----------------------------------------------


(provide 'prose-mode)

;;; prelude4writers.el ends here
