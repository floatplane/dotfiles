;;
;; Make sure local packages can be found
;;
(defun path-relative-to-parent(p)
  (expand-file-name (concat (file-name-directory (or load-file-name buffer-file-name)) "../" p)))

(add-to-list 'load-path (path-relative-to-parent "packages"))

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
       (path-relative-to-parent "straight/repos/straight.el/bootstrap.el"))
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
           add-node-modules-path
           ag
           aggressive-indent
           all-the-icons
           amx
           ;; auto-complete
           ;; auto-complete-clang-async
           bazel
           better-defaults
           cc-mode
           coffee-mode
           company
           company-flow
           counsel
           csharp-mode
           cursor-chg
           doom-themes
           dumb-jump
           editorconfig
           enh-ruby-mode
           eslint-fix
           fill-column-indicator
           flycheck
           flycheck-flow
           flymake-ruby
           git-gutter+
           git-link
           groovy-mode
           haml-mode
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
           ripgrep
           rjsx-mode
           robe
           s ;; string manipulation
           scala-mode
           scss-mode
           smex
           solaire-mode
           spaceline
           spaceline-all-the-icons
           string-inflection
           swiper
           terraform-mode
           textmate
           treemacs
           typescript-mode
           use-package
           web-mode
           yaml-mode
           yasnippet
           zenburn-theme
   ))
  (straight-use-package p))

;; (straight-use-package '(fzf :type git :host github :repo "dbalatero/fzf.el"))
