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

(defun toggle-scribe-focus-mode (fs)
  (interactive "P")
  (defun fullscreen-margins nil
    (if (and (window-full-width-p) (not (minibufferp)))
        (set-window-margins nil (/ (- (frame-width) 80) 2) (/ (- (frame-width) 80) 2))
      (mapcar (lambda (window) (set-window-margins window nil nil)) (window-list))))

  (cond (menu-bar-mode
         (menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode 1)
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

(global-set-key [f11] 'toggle-scribe-focus-mode)



;; scribe-focus-mode ends here
