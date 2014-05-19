;; scribemacs.el -- a packages installer for Emacs Prelude
;;
;;  Copyright (c) 2014, Nick Horton
;;
;;  This is not a part of GNU Emacs
;;
;;
;;; Commentary:
;;
;;  Keepin' it simple

;;; Code:

;;;  ----------------------------------------------------
;;;  ---------------PRELUDE DISABLE----------------------
;;;  ----------------------------------------------------

;;  --- Prelude Disable
;;  White Space Highlighting

(setq prelude-whitespace nil)

;;  --- Prelude Disable
;;  Arrow Keys Off

(setq guru-warn-only t)


;;; ----------------------------------------------
;;; ---------------CORE SHIT----------------------
;;; ----------------------------------------------

(ido-mode t)
(cua-mode 1)
(blink-cursor-mode 1)
(global-hl-line-mode -1)
(global-visual-line-mode 1)
(flyspell-mode 1)
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(savehist-mode t)
(setq-default save-place-globally t)

;; UTF-8 EVERYWHERE!
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Asking for "yes or no" is silly in the day and
;; age of text-speak
(defalias 'yes-or-no-p 'y-or-n-p)



;;; -------------------------------------------
;;; ----------------PACKAGES-------------------
;;; -------------------------------------------


;;  --- Auto-Load Packages
;;
;;  this loads the packs we want to add
;;  Prelude has most of what we want. But, these
;;  additions are quite nice.

(prelude-require-packages '(dired-details+
                            smex
                            auto-complete
                            tabbar
                            smart-mode-line
                         ))

;; --- Require
;; Now, these force their use:
(require 'smex)
(require 'dired-details+)
(require 'ido)
(require 'auto-complete)
(setq sml/theme 'dark)
(sml/setup)

;;; ------------------------------------------------
;;; ------------- THEMES ---------------------------
;;; ------------------------------------------------

(prelude-require-packages '(calmer-forest-theme
                            cyberpunk-theme
                            dakrone-theme
                            django-theme
                            flatland-theme
                            gandalf-theme
                            grandshell-theme
                            gruvbox-theme
                            heroku-theme
                            monokai-theme
                            mustang-theme
                            occidental-theme
                            organic-green-theme
                            purple-haze-theme
                            soft-charcoal-theme
                            soft-morning-theme
                            soft-stone-theme
                            solarized-theme
                            sublime-themes
                    ))


;;; --------------------------------------------------
;;; ------------------USER INTERFACE------------------
;;; --------------------------------------------------


;; -- FUNCTION: My Toggle Window Split
;;
;; hat tip: Stecker Halter
;; http://steckerhalter.co.vu/steckemacs.html

(defun my-toggle-window-split ()
  "Window splitting vertically or horizontally by toggle."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))


;;;  --- Center-Mode (M-f11) -------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; by Pavel Panchekha

(provide 'center-text)

(define-minor-mode center-text-mode
  "Toggles center-text mode

With no argument, this command toggles the mode.
A positive argument turns the mode on.
A negative argument turns the mode off.

Center-text-mode causes Emacs to display the buffer contents
centered in a fixed-size column in the middle of the window.

Center-text-mode tries to recompute the margins necessary to
center the text with the given width every time the window size
changes.  However, Emacs doesn't always call the redisplay
handler.  In this case, you can force a redisplay with [C-x w],
or you can pass [C-x w] a prefix argument to change the column
width.
"

  :init-value nil
  :lighter " Center"
  :keymap `((,(kbd "C-x w") . center-text-set-width))
  :after-hook (center-text-mode-helper))

(defcustom center-text-default-width 80
  "The default width of the text column in center-text-mode"

  :type 'integer
  :group center-text-mode)

(defun center-text-mode-helper ()
  "Internal to center-text-mode"

  (if center-text-mode
      (center-text-mode-enable)
    (center-text-mode-disable)))

(defun center-text-mode-enable ()
  "Internal to center-text-mode"

  (make-local-variable 'center-text-width)
  (if (not (boundp 'center-text-width))
      (setf center-text-width center-text-default-width))

  (make-local-variable 'center-text-old-bg)
  (if (not (boundp 'center-text-old-bg))
      (setf center-text-old-bg (face-background 'fringe)))

  (set-face-background 'fringe (face-background 'default))
  
  (add-to-list 'window-size-change-functions
               'center-text-redisplay-helper)
   
  (center-text-center center-text-width))

(defun center-text-redisplay-helper (frame)
  "Internal to center-text-mode"

  (loop for window in (window-list frame)
        do
        (with-selected-window window
          (if (and center-text-mode
                   (boundp 'center-text-width))
              (center-text-center center-text-width))))

  (switch-to-buffer (current-buffer)))
            

(defun center-text-mode-disable ()
  "Internal to center-text-mode"

  (set-window-fringes nil nil nil)
  (if (boundp 'center-text-old-bg)
      (set-face-background 'fringe center-text-old-bg))
  (makunbound 'center-text-width)
  (makunbound 'center-text-old-bg)
  (setf window-size-change-functions
        (cl-remove 'center-text-redisplay-helper
                   window-size-change-functions
                   :count 1)))

(defun center-text-set-width (width)
  "Set the width of the main text column in center-text-mode.

  Set the width to WIDTH characters, which must be a positive integer.
  If WIDTH is 1, instead simply re-computes the margins.  This is
  sometimes necessary because Emacs doesn't properly fire window-size
  change hooks."

  (interactive "p")

  (if (= width 1)
      ; Then we're probably the non-prefix version, because no one
      ; wants a width-1 column of text.
      (center-text-center center-text-width)
    (center-text-center width)
    (setf center-text-width width)))


(defun center-text-center (max-chars)
  "Expand or shrink the fringe until the text width is
MAX-CHARS characters or fewer characters wide or less"

  (while (not (or (= (window-width) max-chars)
                  (and (< (window-width) max-chars)
                       (= (car (window-fringes)) 0))))
    (let ((char-width
           (* (aref (font-info (face-font 'default)) 2) (/ 1.0 12.0) 6.0))
          (current-chars
           (window-width))
          (current-fringe
           (cons (car (window-fringes)) (cadr (window-fringes)))))
      (let* ((excess-chars (- current-chars max-chars))
             (excess-width (* excess-chars char-width))
             (deficit-margin (floor (/ excess-width 2)))
             (left-fringe (max 0 (+ (car current-fringe) deficit-margin)))
             (right-fringe (max 0 (+ (cdr current-fringe) deficit-margin))))
        (set-window-fringes nil left-fringe right-fringe)))))



;;; --------------------------------------------------
;;; ------------------EDITING-------------------------
;;; --------------------------------------------------

;;  --- Double Space

(defun toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height."
  (interactive)
  (if (eq line-spacing nil)
      (setq-default line-spacing 0.4) ; add 0.4 height between lines
    (setq-default line-spacing nil)   ; no extra heigh between lines
    )
  (redraw-display))

;;; -- Word Count alias
(defalias 'word-count 'count-words)



;;; --------------------------------------------------
;;; ---------------LANG :: CLOJURE--------------------
;;; --------------------------------------------------

;; Fix nrepl/cider not killing buffers/processes

(defun my-nrepl-jack-in ()
  (interactive)
  (dolist (buffer (buffer-list))
    (when (string-prefix-p "*nrepl" (buffer-name buffer))
      (kill-buffer buffer)))
  (nrepl-jack-in nil))



;;; --------------------------------------------------
;;; ---------------KEY BINDINGS-----------------------
;;; --------------------------------------------------

;;; -- EDITING


;;; -- (M-'s)
(global-set-key (kbd "M-4") 'company-mode)
(global-set-key (kbd "M-3") 'smartparens-strict-mode)
(global-set-key (kbd "M-2") 'flyspell-mode)

(global-set-key (kbd "<C-tab>") 'ispell-word)
(global-set-key (kbd "C-e") 'backward-kill-word)

;;; -- (C-c's)
(global-set-key (kbd "C-c 5") 'visual-line-mode)
(global-set-key (kbd "C-c 4") 'hl-line-mode)
(global-set-key (kbd "C-c 3")
                '(lambda()(interactive)
                   (if (eq cursor-type 'box)
                       (setq cursor-type 'hbar)
                     (setq cursor-type 'box))))
(global-set-key (kbd "C-c 2") 'toggle-line-spacing)
(global-set-key (kbd "C-c 1") 'word-count)                     


;;  ---Smex
(global-set-key (kbd "C-t") 'smex)

;;  ---text scale increase/decrease (C +/=)(C -)
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)


;; Undo to C-z like a muggle; Android kbd doesn't do C-_
(global-set-key (kbd "C-z") 'undo)

;; Some bindings for special characters
(global-set-key (kbd "M-l") (lambda () (interactive) (insert "\u03bb"))) ;lambda
(global-set-key (kbd "M-f") (lambda () (interactive) (insert "\u0192"))) ;function
(global-set-key (kbd "M--") (lambda () (interactive) (insert "\u2192"))) ;arrow



;; -- WINDOW SPLIT KEY BINDINGS

(global-set-key (kbd "C-9") (lambda () (interactive) (select-window (previous-window))))
(global-set-key (kbd "C-0") (lambda () (interactive) (select-window (next-window))))
(global-set-key (kbd "<f2>") 'split-window-vertically)
(global-set-key (kbd "<f3>") 'split-window-horizontally)
(global-set-key (kbd "<f4>") 'my-toggle-window-split)
(global-set-key (kbd "<f5>") 'delete-other-windows)
(global-set-key (kbd "<f6>") 'delete-window)


;; --- Line Numbers (f7)

(global-set-key (kbd "<f7>")
                '(lambda()(interactive)
                   (global-linum-mode 'toggle)
                   (message "Line Numbers Toggle!")))

;; Add vertical line to right of line-numbers
(setq linum-format "%6d ")


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


(global-set-key (kbd "M-<f11>")
                '(lambda()(interactive)
                   (center-text-mode 'toggle)
                   (message "Center Text Mode Toggle")))



;; END

(provide 'scribemacs)

;;; scribemacs.el ends here
