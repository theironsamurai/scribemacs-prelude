;;; scribemacs-editing.el -- basic editing functionality

;;; Code:



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

;; Undo to C-z like a muggle; Android kbd doesn't do C-_
(global-set-key (kbd "C-z") 'undo)

;; Some bindings for special characters
(global-set-key (kbd "M-l") (lambda () (interactive) (insert "\u03bb"))) ;lambda
(global-set-key (kbd "M-f") (lambda () (interactive) (insert "\u0192"))) ;function
(global-set-key (kbd "M--") (lambda () (interactive) (insert "\u2192"))) ;arrow

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

;;  --- Backward Kill Word (C-e)

(global-set-key (kbd "C-e") 'backward-kill-word)






;;;  --- SPELL CHECK -----------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  You can turn auto-spell-check OFF with "M-2" and "C-tab" will
;;  check the spelling of the word your cursor is on. :-)

(global-set-key (kbd "<C-tab>") 'ispell-word)
(flyspell-mode 1)
                                        ; (global-set-key (kbd "M-2")
                                        ;                 '(lambda()(interactive)
                                        ;                    (flyspell-mode 'toggle)
                                        ;                    (message "Spell Check Toggle")))

(global-set-key (kbd "M-2") 'flyspell-mode)



(provide 'scribemacs-editing)

;;; scribemacs-editing.el ends here
