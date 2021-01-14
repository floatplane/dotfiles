;;
;; Make sure local packages can be found
;;
(add-to-list 'load-path (expand-file-name "packages" user-emacs-directory))

;;
;; Before loading packages, make sure they don't poop everywhere
;;
(require 'no-littering)

;;
;; Bootstrap code for Straight
;;
(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;
;; Load packages.
;;
(dolist (p
         `( ;; backquote: quote the list, but allow substitution of evaluated forms within it
            ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html#Backquote
           ag
           aggressive-indent
           all-the-icons
           ;; auto-complete
           ;; auto-complete-clang-async
           better-defaults
           coffee-mode
           company
           company-flow
           counsel
           csharp-mode
           cursor-chg
           doom-themes
           dumb-jump
           editorconfig
           fill-column-indicator
           flycheck
           flycheck-flow
           flymake-ruby
           git-gutter+
           git-link
           groovy-mode
           haml-mode
           helm
           helm-ag
           helm-projectile
           highlight-indentation
           highlight-symbol
           ido-completing-read+
           irony
           ivy
           js2-mode
           lsp-mode
           lsp-ui
           magit
           markdown-mode
           neotree
           prettier-js
           projectile
           ,'(fzf :type git :host github :repo "dbalatero/fzf.el") ;; comma breaks out of backquote, allows us to make this a quoted element
           protobuf-mode
           python-mode
           rainbow-delimiters
           rjsx-mode
           robe
           scss-mode
           smex
           spaceline
           spaceline-all-the-icons
           string-inflection
           swiper
           terraform-mode
           textmate
           treemacs
           use-package
           web-mode
           yaml-mode
           yasnippet
           zenburn-theme
   ))
  (straight-use-package p))

;; (straight-use-package '(fzf :type git :host github :repo "dbalatero/fzf.el"))
