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

;;;  -----------------------------------------------
;;;  --- LOAD SUBFOLDERS & FILES -------------------
;;;  -----------------------------------------------

(add-to-list 'load-path "plugins/scribemacs/config/")
(load "scribemacs-settings.el")
;; (load "scribemacs-tabbar.el")

(add-to-list 'load-path "plugins/scribemacs/lang-packs/")
(load "scribemacs-clojure.el")




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

;; END

(provide 'scribemacs)

;;; scribemacs.el ends here
