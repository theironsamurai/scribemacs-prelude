;;; scribe-focus-mode.el -- focus mode like Pyroom



;;; Code:

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


;; Scribe "helper" mode: turn on menu, scroll-bar, line numbers
;; toggle ON/OFF with <f9> function key

(global-set-key (kbd "<f9>") ; F7 - Activate auto-fill-mode, flyspell-mode and abbrev-mode
                '(lambda()(interactive)
                   (menu-bar-mode 'toggle)
                   (scroll-bar-mode 'toggle)
                   (linum-mode 'toggle)
                   (message "Toggle menu scroll line numbers")))

;; toggle full screen Emacs session with <f11> function key
;; Covers the entire screen and turns off all menu/sidebar/etc
;; and centers text
;;
;; WARNING: doesn't allow you to center text when Scribe "helper"
;; is active. AKA, when you toggle ON <f9> it gets screwy. 

(defun toggle-fullscreen () ;; thanks to Ivan Kanis
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(global-set-key [f11] 'toggle-fullscreen)

;; scribe-focus-mode ends here
