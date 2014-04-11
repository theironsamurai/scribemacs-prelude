;;; scribe-package-loader.el -- loads packages

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

;;; scribe-package-loader.el ends here
