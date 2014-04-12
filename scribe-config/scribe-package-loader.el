;;; scribe-package-loader.el -- loads & requires dependencies

;;; commentary:

;;; code:

(defvar sweet-packages '(dired-details
                         dired-details+
                         smex
                         flyspell
                    ))

(dolist (p sweet-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'dired-details)
(require 'dired-details+)
(require 'ido)
(require 'smex)

;;; scribe-package-loader.el ends here
