(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-and-a-half-arguments (list "--ignore-dir=build" "--ignore-dir=tools"))
 '(ack-and-a-half-project-root-file-patterns
   (quote
    ("\\`.git\\'" "\\`.bzr\\'" "\\`_darcs\\'" "\\`.hg\\'")))
 '(ack-prompt-for-directory (quote unless-guessed))
 '(ag-reuse-buffers t)
 '(fill-column 80)
 '(global-auto-revert-mode t)
 '(initial-buffer-choice t)
 '(load-home-init-file t t)
 '(magit-log-arguments (quote ("-n256" "--decorate")))
 '(package-selected-packages
   (quote
    (lsp-ui lsp-mode string-inflection terraform-mode dumb-jump helm-ag ag helm-projectile helm projectile prettier-js flycheck-flow company-flow use-package flymake-ruby aggressive-indent rjsx-mode zenburn-theme yasnippet yaml-mode web-mode textmate smex scss-mode rainbow-delimiters python-mode protobuf-mode markdown-mode magit js2-mode irony highlight-symbol highlight-indentation haml-mode groovy-mode git-gutter+ flycheck fill-column-indicator editorconfig cursor-chg csharp-mode company coffee-mode)))
 '(show-trailing-whitespace t)
 '(speedbar-frame-parameters
   (quote
    ((minibuffer)
     (width . 40)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 0))))
 '(speedbar-frame-plist
   (quote
    (minibuffer nil width 40 border-width 0 internal-border-width 0 unsplittable t default-toolbar-visible-p nil has-modeline-p nil menubar-visible-p nil default-gutter-visible-p nil)))
 '(tool-bar-mode nil)
 '(web-mode-enable-control-block-indentation nil)
 '(web-mode-indent-style 1)
 '(whitespace-style (quote (trailing tab-mark))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(trailing-whitespace ((((class color) (background light)) (:background "gray89"))))
 '(whitespace-trailing ((t (:background "medium sea green" :foreground "yellow" :weight bold)))))

(setq x-select-enable-clipboard t)
(cond ((fboundp 'x-selection-value) (setq interprogram-paste-function 'x-selection-value))
      ((fboundp 'x-cut-buffer-or-selection-value) (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)))
