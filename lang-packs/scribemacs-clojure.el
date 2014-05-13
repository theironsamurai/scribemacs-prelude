;;; scribemacs-clojure.el --- defaults for Clojure

;;; Code:

;; Fix nrepl/cider not killing buffers/processes

(defun my-nrepl-jack-in ()
  (interactive)
  (dolist (buffer (buffer-list))
    (when (string-prefix-p "*nrepl" (buffer-name buffer))
      (kill-buffer buffer)))
  (nrepl-jack-in nil))

;;; scribemacs-clojure.el ends here
