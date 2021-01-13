;;
;; Window size - maximize it, hide the button bar
;;
(cond ((display-graphic-p)
       (tool-bar-mode -1)
       (toggle-frame-maximized)))

;; Don't open new frames for each file open externally.
(setq ns-pop-up-frames nil)
(setq pop-up-frames nil)

(when (eq system-type 'darwin)
  ;; Use my favorite font
  (set-face-attribute 'default nil :family "Source Code Pro")
  ;; Update title bar to assume dark mode
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  )

(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort abort-recursive-edit
                                      exit-minibuffer keyboard-quit))
          (invert-face 'mode-line)
          (run-with-timer 0.1 nil 'invert-face 'mode-line))))

(require 'all-the-icons)
;; (all-the-icons-install-fonts)
(straight-use-package
 'spaceline 
 :config (straight-use-package 'spaceline-all-the-icons 
                               :config (spaceline-all-the-icons-theme)))
