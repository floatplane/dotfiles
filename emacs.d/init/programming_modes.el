;;
;; Source control
;;
(use-package magit
  :defines magit-file-section-map magit-hunk-section-map
  :bind (("C-c m s" . magit-status)
         :map magit-hunk-section-map
         ("RET" . magit-diff-visit-file-other-window)
         :map magit-file-section-map
         ("RET" . magit-diff-visit-file-other-window)))

;; This is probably what breaks git in the modeline
(setq vc-handled-backends (delq 'Git vc-handled-backends))

(use-package git-link
  :defines git-link-open-in-browser git-link-commit-remote-alist git-link-remote-alist git-link-default-branch
  :config
  (setq git-link-open-in-browser t)
  (setq git-link-default-branch "master")
  ;; sets up stripe git enterprise as a git-link handler
  (add-to-list 'git-link-remote-alist
               '("git\\.corp\\.stripe\\.com" git-link-github))
  (add-to-list 'git-link-commit-remote-alist
               '("git\\.corp\\.stripe\\.com" git-link-commit-github))
  ;; binds git-link to f2 for either current line or active region
  (global-set-key [f2] 'git-link))

(use-package rainbow-delimiters
  :defines rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package add-node-modules-path)

(use-package web-mode
  :defines web-mode-comment-formats web-mode-comment-style web-mode-enable-auto-quoting web-mode-enable-auto-indentation
  :mode "\\.phtml\\'"
  :mode "\\.tpl\\.php\\'"
  :mode "\\.jsp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.erb\\'"
  :mode "\\.mustache\\'"
  :mode "\\.djhtml\\'"
  :mode "\\.html?\\'"
  :mode "\\.mako\\'"
  :config
  (setq rainbow-delimiters-mode nil)
  (whitespace-mode 0)
  (setq web-mode-enable-auto-indentation nil)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-comment-style 2) ;; server-side comments rather than HTML comments
  (add-to-list 'web-mode-comment-formats '("css" . "//" ))
  (add-to-list 'web-mode-comment-formats '("jsx" . "//" ))
  (add-to-list 'web-mode-comment-formats '("javascript" . "//" ))
)

(use-package haml-mode :mode "\\.haml\\'")

(use-package scss-mode
  :defines scss-compile-at-save
  :mode "\\.scss\\'"
  :config (setq scss-compile-at-save nil))

(use-package coffee-mode :mode "\\.coffee\\'")

(use-package csharp-mode :mode "\\.cs\\'")

(use-package markdown-mode
  :mode "\\.markdown\\'"
  :mode "\\.md\\'")

(use-package yaml-mode :mode "\\.ya?ml$")

(use-package cc-mode
  ;; Default .h files to C++ mode.
  :mode ("\\.h\\'" . c++-mode)
  ;; Make .mel files use C++ mode.
  :mode ("\\.mel\\'" . c++-mode)
  ;; Make UnrealScript files use C++ mode.
  :mode ("\\.uc\\'" . c++-mode)
  ;; Make .m and .mm files use objective-c mode.
  :mode ("\\.m\\'" . objc-c-mode)
  :mode ("\\.mm\\'" . objc-c-mode))

;; Ruby alist
(use-package enh-ruby-mode
  :mode "\\.rb\\'"
  :mode "\\.rake\\|Rakefile$"
  :interpreter "ruby")
(use-package flymake-ruby
  :after (enh-ruby-mode)
  :hook (ruby-mode-hook . flymake-ruby-load))

(use-package robe
  :disabled
  :after (company enh-ruby-mode)
  :hook (ruby-mode-hook . robe-mode)
  :config
  (setq company-dabbrev-downcase nil)
  (push 'company-robe company-backends))

;; Question: can I have multiple use-package decls? Seems like yes: https://github.com/jwiegley/use-package/issues/662

;; (require 'lsp-mode)
;; (lsp-define-stdio-client
;;  lsp-ruby
;;  "ruby"
;;  (lambda () "/Users/bsharon/stripe/pay-server/")
;;  '("pay" "exec" 
;;    "scripts/bin/typecheck" "--lsp" "-v" 
;;    "--statsd-host=127.0.0.1" "--statsd-prefix=ruby_typer.payserver.mydev"))

(use-package js2-mode :mode "\\.js\\|\\.es\\|\\.json$")

(use-package typescript-mode :mode "\\.tsx?$")

(use-package python-mode
  :defines python-indent
  :mode "\\.pyw?\\|SConstruct\\|SConscript\\|BUCK$"

  :hook (python-mode-hook . (lambda ()
                              (let ((filename (buffer-file-name)))
                                ;; Enable buck mode for the appropriate files
                                (when (and filename
                                           (string-match "/BUCK.*" filename))
                                  (setq python-indent 2))))))

;; This is a local package, can't use use-package
(require 'capnp-mode)
(add-to-list 'auto-mode-alist '("\\.capnp\\'" . capnp-mode))

(use-package bazel-mode
  :mode "/BUILD\\(\\..*\\)?\\'"
  :mode "/WORKSPACE\\'"
  :mode "\\.\\(BUILD\\|WORKSPACE\\|bzl\\)\\'")

;;
;; Shell stuff
;;
(setq ansi-color-names-vector ; better contrast colors
      ["black" "red4" "green4" "yellow4"
       "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook '(lambda () (toggle-truncate-lines 1)))

(setq comint-prompt-read-only t)
(setq compilation-scroll-output t)

(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

;; Get Stripe stuff
(cond ((file-exists-p "~/stripe/stripemacs/stripemacs.el")
       (defvar devbox-machine "qa-mydev--02df3f12b2a77dbd7.northwest.stripe.io")
       (defvar stripe-username "bsharon")
       (load "~/stripe/stripemacs/stripemacs.el")))
