;;; scribe-bindings.el

;;; code:

;; text scale increase/decrease
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; C-t for toggling smex
(define-key global-map (kbd "C-t") 'smex)
