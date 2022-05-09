;;
;; Window size - maximize it, hide the button bar
;;
(cond ((display-graphic-p)
       (tool-bar-mode -1)))
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; Don't open new frames for files
(setq ns-pop-up-frames nil)
(setq pop-up-frames nil)

(when (eq system-type 'darwin)
  ;; Use my favorite font
  (set-face-attribute 'default nil :family "Source Code Pro")
  ;; Update title bar to assume dark mode
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  )

;; Use a visual bell, except when Ctrl-G
(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort abort-recursive-edit
                                      exit-minibuffer keyboard-quit))
          (invert-face 'mode-line)
          (run-with-timer 0.1 nil 'invert-face 'mode-line))))

(require 'all-the-icons)
;; (all-the-icons-install-fonts)
(use-package
 spaceline
 :config (straight-use-package 'spaceline-all-the-icons 
                               :config (spaceline-all-the-icons-theme)))
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (load-theme (cond (my/alternate-desktop 'doom-spacegrey) (t 'doom-zenburn)) t)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  ;; (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; (use-package doom-modeline
;;   :defines doom-modeline-buffer-encoding doom-modeline-buffer-file-name-style doom-modeline-mode doom-modeline-project-detection doom-modeline-vcs-max-length doom-themes-padded-modeline doom-modeline-major-mode-icon
;;   :ensure t
;;   :config
;;   (setq doom-modeline-buffer-encoding 'nondefault)
;;   (setq doom-modeline-buffer-file-name-style 'relative-from-project)
;;   (setq doom-modeline-project-detection 'projectile)
;;   (setq doom-modeline-vcs-max-length 36)
;;   (setq doom-modeline-major-mode-icon nil)
;;   (setq doom-themes-padded-modeline t)
;;   :init
;;   (doom-modeline-mode 1))

(use-package solaire-mode
  :init
  (solaire-global-mode +1))
