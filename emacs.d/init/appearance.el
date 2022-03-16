;;
;; Window size - maximize it, hide the button bar
;;
(cond ((display-graphic-p)
       (tool-bar-mode -1)))
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

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
;; (use-package
;;  spaceline
;;  :config (straight-use-package 'spaceline-all-the-icons 
;;                                :config (spaceline-all-the-icons-theme)))
(use-package doom-modeline
  :defines doom-modeline-buffer-encoding doom-modeline-buffer-file-name-style doom-modeline-mode doom-modeline-project-detection doom-modeline-vcs-max-length
  :ensure t
  :config
  (setq doom-modeline-buffer-encoding 'nondefault)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (setq doom-modeline-project-detection 'projectile)
  (setq doom-modeline-vcs-max-length 36)
  :init
  (doom-modeline-mode 1))
