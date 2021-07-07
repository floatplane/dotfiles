;; Fix some insanity
(require 'better-defaults)

;; Try speeding up some things by not forcing external commands to initialize
;; zsh
(setq shell-file-name "/bin/zsh")

;; I want unique names for different buffers to start with the file name,
;; so autocomplete works better.
(setq uniquify-buffer-name-style 'post-forward)

;; Make sure emacsclient can connect to a running instance
(require 'server)
(unless (server-running-p)
  (server-start))

;; On OS X, make sure we come to the foreground when needed
(when (featurep 'ns)
  (defun ns-raise-emacs ()
    "Raise Emacs."
    (ns-do-applescript "tell application \"Emacs\" to activate"))
(defun ns-raise-emacs-with-frame (frame)
    "Raise Emacs and select the provided frame."
    (with-selected-frame frame
      (when (display-graphic-p)
        (ns-raise-emacs))))
(add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)
(when (display-graphic-p)
    (ns-raise-emacs)))

;; Reindent as you type. I think I like this.
;; Never mind. Too aggressive on existing code.
;; (global-aggressive-indent-mode 1)
;; (add-to-list 'aggressive-indent-excluded-modes 'web-mode)

(require 'git-gutter+)
(global-git-gutter+-mode)

(require 'editorconfig)
(setq editorconfig-get-properties-function
      'editorconfig-core-get-properties-hash)
(editorconfig-mode 1)

;; M-y and M-n for yes/no responses
(require 'quick-yes)

;; Don't ask if I really want to open a symlink to a VC-controlled file; just
;; open the file.
(setq vc-follow-symlinks t)

;; Always auto-revert.
(global-auto-revert-mode t)

(setq case-fold-search t)
(setq current-language-environment "Latin-1")
(setq default-input-method "latin-1-prefix")

;;
;; Set up windows-like shifted motion keys and other goodness.
;; Allow shifted movement keys to modify current region.
;;
(cua-mode)
;; (delete-selection-mode 1)
;; (setq transient-mark-mode t)
(setq cua-enable-cua-keys nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; org-mode
(require 'org)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(setq org-log-done t)
(setq org-startup-indented t)
(setq org-catch-invisible-edits 'smart)

;;
;; Whitespace
;;
;; Define a function to disable tabs, and set the tab width
;; to something sensible.  These variables are buffer
;; local, so we have to set them in a mode hook.
(defun fix-tabs ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4))
(add-hook 'text-mode-hook 'fix-tabs)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default fill-column 100)
(defun sensible-indentation ()
  "Make all the indentation settings sensible"
  (interactive)
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq c-basic-offset 4))
(setq tab-always-indent "complete")

(require 'whitespace)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-use-filename-at-point nil)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

;; Not sure I'm convinced by ivy mode
;; (require 'ivy)
;; (require 'swiper)
;; (require 'counsel)
;; (ivy-mode 1)
;; (setq ivy-use-virtual-buffers t)
;; (setq enable-recursive-minibuffers t)
;; (global-set-key "\C-s" 'swiper)
;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
;; (global-set-key (kbd "<f6>") 'ivy-resume)
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c k") 'counsel-ag)
;; (global-set-key (kbd "C-x l") 'counsel-locate)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;; (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; Code folding based on indentation
;; https://www.emacswiki.org/emacs/HideShow#toc5
(defun toggle-selective-display (column)
      (interactive "P")
      (set-selective-display
       (or column
           (unless selective-display
             (1+ (current-column))))))
(global-set-key (kbd "C-\\") 'toggle-selective-display)

(defun apostrophe ()
    (interactive)
  (insert "’"))
(global-set-key (kbd "C-'") 'apostrophe)

;; Enable winner-mode
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Automatically clean up old buffers
(require 'midnight)

(require 'amx)
(amx-mode)

(require 'tramp)
(setq tramp-default-method "scp")

;; display filename in title bar
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; Don't backup files into the tree; put them all somewhere safe
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/etc/backups"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

(require 'desktop)
(setq desktop-save t)
(desktop-save-mode 1)

(require 'string-inflection)

;;
;; Project navigation
;;
(setq fzf/files-source "custom")
(setq fzf/files-source-custom-command "fd")
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-x f") 'fzf-projectile)
(define-key projectile-mode-map (kbd "C-c f") 'projectile-ripgrep)
(define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

