;; Fix some insanity
(require 'better-defaults)

;; Make sure emacsclient can connect to a running instance
(require 'server)
(unless (server-running-p)
  (server-start))

;; Reindent as you type. I think I like this.
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'web-mode)

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
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

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

(require 'whitespace)

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
    '(("." . "~/.emacs.d/.backups"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

