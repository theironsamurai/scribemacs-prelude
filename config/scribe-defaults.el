;;; scribe-defaults.el

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

;; scribe-defaults.el ends here
