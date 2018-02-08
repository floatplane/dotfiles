(require 'magit)
(setq vc-handled-backends (delq 'Git vc-handled-backends))
(setq magit-commit-show-diff nil
      magit-revert-buffers 1)

(require 'dumb-jump)
(dumb-jump-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

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
  (message "Entering web mode")
)
(add-hook 'web-mode-hook  'my-web-mode-hook)
(setq web-mode-comment-style 2) ;; server-side comments rather than HTML comments

(require 'haml-mode)

(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)

(require 'coffee-mode)
;; Support Streamline.js compiled CoffeeScript
(add-to-list 'auto-mode-alist '("\\._coffee\\'" . coffee-mode))

(require 'csharp-mode)

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;; Default .h files to C++ mode.
(setq auto-mode-alist (cons '("\\.h\\'" . c++-mode) auto-mode-alist))

;; Make .mel files use C++ mode.
(setq auto-mode-alist (cons '("\\.mel\\'" . c++-mode) auto-mode-alist))

;; Make UnrealScript files use C++ mode.
(setq auto-mode-alist (cons '("\\.uc\\'" . c++-mode) auto-mode-alist))

;; Make .m and .mm files use objective-c mode.
;; (require 'objc-c-mode)
;; (setq auto-mode-alist (cons '("\\.m\\'" . objc-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mm\\'" . objc-mode) auto-mode-alist))

;; Actually, make .m files use Octave for now
(setq auto-mode-alist (cons '("\\.m\\'" . octave-mode) auto-mode-alist))

;; Ruby alist
(setq auto-mode-alist (cons '("\\.rake\\|Rakefile$" .
                              ruby-mode) auto-mode-alist))
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

;; Javscript alist
(setq auto-mode-alist (cons '("\\.js\\|\\.es\\|\\.json$" .
                              js2-mode) auto-mode-alist))
(require 'js2-mode)

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

