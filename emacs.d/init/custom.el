(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-and-a-half-arguments (list "--ignore-dir=build" "--ignore-dir=tools"))
 '(ack-and-a-half-project-root-file-patterns '("\\`.git\\'" "\\`.bzr\\'" "\\`_darcs\\'" "\\`.hg\\'"))
 '(ack-prompt-for-directory 'unless-guessed)
 '(ag-reuse-buffers t)
 '(custom-safe-themes
   '("333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "a0be7a38e2de974d1598cf247f607d5c1841dbcef1ccd97cded8bea95a7c7639" "6c9cbcdfd0e373dc30197c5059f79c25c07035ff5d0cc42aa045614d3919dab4" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "d6603a129c32b716b3d3541fc0b6bfe83d0e07f1954ee64517aa62c9405a3441" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "4a8d4375d90a7051115db94ed40e9abb2c0766e80e228ecad60e06b3b397acab" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "711efe8b1233f2cf52f338fd7f15ce11c836d0b6240a18fffffc2cbd5bfe61b0" "ea5822c1b2fb8bb6194a7ee61af3fe2cc7e2c7bab272cbb498a0234984e1b2d9" "08a27c4cde8fcbb2869d71fdc9fa47ab7e4d31c27d40d59bf05729c4640ce834" "aaa4c36ce00e572784d424554dcc9641c82d1155370770e231e10c649b59a074" default))
 '(fill-column 80)
 '(flycheck-javascript-flow-args nil)
 '(global-auto-revert-mode t)
 '(initial-buffer-choice t)
 '(load-home-init-file t t)
 '(magit-log-arguments '("-n256" "--decorate"))
 '(package-selected-packages
   '(lsp-ui lsp-mode string-inflection terraform-mode dumb-jump helm-ag ag helm-projectile helm projectile prettier-js flycheck-flow company-flow use-package flymake-ruby aggressive-indent rjsx-mode zenburn-theme yasnippet yaml-mode web-mode textmate smex scss-mode rainbow-delimiters python-mode protobuf-mode markdown-mode magit js2-mode irony highlight-symbol highlight-indentation haml-mode groovy-mode git-gutter+ flycheck fill-column-indicator editorconfig cursor-chg csharp-mode company coffee-mode))
 '(show-trailing-whitespace t)
 '(speedbar-frame-parameters
   '((minibuffer)
     (width . 40)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 0)))
 '(speedbar-frame-plist
   '(minibuffer nil width 40 border-width 0 internal-border-width 0 unsplittable t default-toolbar-visible-p nil has-modeline-p nil menubar-visible-p nil default-gutter-visible-p nil))
 '(tool-bar-mode nil)
 '(web-mode-enable-control-block-indentation nil)
 '(web-mode-indent-style 1)
 '(whitespace-style '(trailing tab-mark)))


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
