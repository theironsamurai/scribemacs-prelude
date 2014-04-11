;;; scribe-package-loader.el

(defvar sweet-packages '(dired-details
                         dired-details+
                         smex
                         flyspell
                    ))

(dolist (p sweet-packages)
  (when (not (package-installed-p p))
    (package-install p)))
