(require 'magit)
(setq vc-handled-backends (delq 'Git vc-handled-backends))
;; (setq auto-revert-check-vc-info t)
(define-key magit-hunk-section-map (kbd "RET") 'magit-diff-visit-file-other-window)
(define-key magit-file-section-map (kbd "RET") 'magit-diff-visit-file-other-window)
;; (setq magit-commit-show-diff nil
;;       magit-revert-buffers 1)

;; used for quickly opening github links to line or region
;; configured to open in a browser right away
(straight-use-package 'git-link
  :config
  (setq git-link-open-in-browser t)
  (setq git-link-default-branch "master")
  )

;; sets up stripe git enterprise as a git-link handler
(eval-after-load 'git-link
  '(progn
    (add-to-list 'git-link-remote-alist
      '("git\\.corp\\.stripe\\.com" git-link-github))
    (add-to-list 'git-link-commit-remote-alist
      '("git\\.corp\\.stripe\\.com" git-link-commit-github))))

;; binds git-link to f2 for either current line or active region
(global-set-key [f2] 'git-link)

(require 'dumb-jump)
(dumb-jump-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'add-node-modules-path)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mako\\'" . web-mode))
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq rainbow-delimiters-mode nil)
  (setq whitespace-mode 0)
  (setq web-mode-enable-auto-indentation nil)
  (setq web-mode-enable-auto-quoting nil)
  (add-to-list 'web-mode-comment-formats '("css" . "//" ))
  (add-to-list 'web-mode-comment-formats '("jsx" . "//" ))
  (add-to-list 'web-mode-comment-formats '("javascript" . "//" ))
  (add-node-modules-path)
  (message "Entering web mode")
)
(add-hook 'web-mode-hook  'my-web-mode-hook)
(setq web-mode-comment-style 2) ;; server-side comments rather than HTML comments

(require 'haml-mode)

(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)

(require 'coffee-mode)

(require 'csharp-mode)

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;; Default .h files to C++ mode.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Make .mel files use C++ mode.
(add-to-list 'auto-mode-alist '("\\.mel\\'" . c++-mode))

;; Make UnrealScript files use C++ mode.
(add-to-list 'auto-mode-alist '("\\.uc\\'" . c++-mode))

;; Make .m and .mm files use objective-c mode.
;; (require 'objc-c-mode)
;; (add-to-list 'auto-mode-alist '("\\.m\\'" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

;; Actually, make .m files use Octave for now
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Ruby alist
(require 'enh-ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\|Rakefile$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(progn
     (setq company-dabbrev-downcase nil)
     (push 'company-robe company-backends)))

;; (require 'lsp-mode)
;; (lsp-define-stdio-client
;;  lsp-ruby
;;  "ruby"
;;  (lambda () "/Users/bsharon/stripe/pay-server/")
;;  '("pay" "exec" 
;;    "scripts/bin/typecheck" "--lsp" "-v" 
;;    "--statsd-host=127.0.0.1" "--statsd-prefix=ruby_typer.payserver.mydev"))

;; Javscript alist
(setq auto-mode-alist (cons '("\\.js\\|\\.es\\|\\.json$" .
                              js2-mode) auto-mode-alist))
(require 'js2-mode)
(add-hook 'js2-mode-hook 'add-node-modules-path)

(setq auto-mode-alist (cons '("\\.ts\\|\\.tsx\\$" .
                              typescript-mode) auto-mode-alist))
(require 'typescript-mode)
(add-hook 'typescript-mode-hook 'add-node-modules-path)

;; Python editing stuff
(setq auto-mode-alist (cons '("\\.pyw?\\|SConstruct\\|SConscript\\|BUCK$" .
                              python-mode) auto-mode-alist))
;; (setq interpreter-mode-alist (cons '("python" . python-mode)
;;                                    interpreter-mode-alist))
(require 'python-mode)
(add-hook 'python-mode-hook
      (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable buck mode for the appropriate files
              (when (and filename
             (string-match "/BUCK.*" filename))
        (setq python-indent 2)))))

;; Capnproto
(require 'capnp-mode)
(add-to-list 'auto-mode-alist '("\\.capnp\\'" . capnp-mode))

;; Bazel
(require 'bazel-mode)
(add-to-list 'auto-mode-alist '("/BUILD\\(\\..*\\)?\\'" . bazel-mode))
(add-to-list 'auto-mode-alist '("/WORKSPACE\\'" . bazel-mode))
(add-to-list 'auto-mode-alist '("\\.\\(BUILD\\|WORKSPACE\\|bzl\\)\\'" . bazel-mode))


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

(require 're-builder)
(setq reb-re-syntax 'string)

;; Get Stripe stuff
(cond ((file-exists-p "~/stripe/stripemacs/stripemacs.el")
       (defvar devbox-machine "qa-mydev--02df3f12b2a77dbd7.northwest.stripe.io")
       (defvar stripe-username "bsharon")
       (load "~/stripe/stripemacs/stripemacs.el")))

