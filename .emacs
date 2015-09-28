;;
;; External packages.
;;
(defvar packages-list
  '(
    ;; auto-complete
    ;; auto-complete-clang-async
    coffee-mode
    company
    csharp-mode
    cursor-chg
    fill-column-indicator
    flycheck
    haml-mode
    highlight-indentation
    highlight-symbol
    irony
    js2-mode
    magit
    markdown-mode
    protobuf-mode
    python-mode
    rainbow-delimiters
    scss-mode
    smex
    textmate
    web-mode
    yaml-mode
    yasnippet
    zenburn-theme
    )
  "List of packages needs to be installed at launch")
;
;; Window size
;;
(cond ((display-graphic-p)
       (tool-bar-mode -1)
       (set-frame-height (selected-frame) 40)
       (set-frame-width (selected-frame) 132)
       (set-frame-position (selected-frame) 0 0)))

;; Disable Ctrl-Z minimization/suspension of emacs.
(global-set-key [C-z] nil)

;; On OS X we don't inherit the same path as terminal programs would, so read in PATH from .bashrc
;; http://emacswiki.org/emacs/EmacsApp
(defun set-exec-path-from-shell-PATH ()
  "Sets the exec-path to the same value used by the user shell"
  (let ((path-from-shell
         (replace-regexp-in-string
          "[[:space:]\n]*$" ""
          (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
 
;; call function now
(if (not (getenv "TERM_PROGRAM"))
    (set-exec-path-from-shell-PATH))
;; Make sure exec-path matches our environment's path
(setq exec-path (split-string (getenv "PATH") path-separator))


; Define a function to disable tabs, and set the tab width
; to something sensible.  These variables are buffer
; local, so we have to set them in a mode hook.
(defun fix-tabs ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4))

(defun toggle-tabs ()
  "Toggle the value of indent-tabs-mode (buffer-local)"
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode)))

(defun yes-tabs (tab-width-value)
  "Enable tabs for indent, set tab-width to the given value"
  (interactive)
  (progn (setq indent-tabs-mode t)
         (setq tab-width tab-width-value)))

(defun no-tabs ()
  "Disable tabs for indent"
  (interactive)
  (setq indent-tabs-mode nil))

; When would I not want this?
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

; Add the home directory's emacs directory to the load path.
(setq load-path (cons (expand-file-name "~/emacs") load-path))
(setq load-path (cons (expand-file-name "~/emacs/use-package") load-path))

; Require packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


(require 'cl)
(defun has-package-not-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))
(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))

;; Load theme, if themes supported
(cond ((boundp 'custom-theme-load-path)
       (load-theme 'zenburn t)))

;; M-y and M-n for yes/no responses
(require 'quick-yes)

;; Don't ask if I really want to open a symlink to a VC-controlled file; just
;; open the file.
(setq vc-follow-symlinks t)

;; yasnippets
(require 'yasnippet)
(yas-global-mode 1)

;; Company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Irony mode.
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; Irony mode.
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)
(eval-after-load 'flycheck
  '(add-to-list 'flycheck-checkers 'irony))

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(require 'textmate)
(add-hook 'textmate-mode-hook
          (lambda () (progn (message "Textmate mode activated")
                            (setq textmate-mode-keymap (cdr (assoc 'textmate-mode minor-mode-map-alist)))
                            (define-key textmate-mode-keymap [(meta up)] nil)
                            (define-key textmate-mode-keymap [(meta down)] nil)
                            (define-key textmate-mode-keymap [(meta shift up)] nil)
                            (define-key textmate-mode-keymap [(meta shift down)] nil))))
(textmate-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; From https://bitbucket.org/durin42/dotfiles/src/tip/.elisp/settings/50.localfuncs.el#cl-9:
;; advice to prevent ido from using flex matches on huge lists of files
;; with enough characters typed and blocking for an absurd amount of time
;; after a stupid typo
;; The value af-ido-flex-fuzzy-limit is the maximum value of the product
;; of the number of characters the user has entered and the number of
;; options in the ido list.
;; The default value was determined experimentally and seemed to be
;; the boundary of sane wait times when this was written.
(defvar af-ido-flex-fuzzy-limit (* 2000 5))
(defadvice ido-set-matches-1 (around my-ido-set-matches-1 activate)
  "Conditionally disable flex matching if the list is huge.

This is useful because when the ido list is huge, ido flex matching
spends an eternity in a regex if you make a typo."
  (let ((ido-enable-flex-matching (< (* (length (ad-get-arg 0)) (length ido-text))
                                     af-ido-flex-fuzzy-limit)))
    ad-do-it))

;; ; Save the desktop periodically
;; (setq desktop-dirname "~/.emacs.desktop.dir/")
;; (setq desktop-load-locked-desktop nil)
;; (require 'desktop)
;; (add-hook 'desktop-not-loaded-hook
;; 		  (lambda () (progn (desktop-save-mode-off)
;; 							(message "Disabling desktop saving"))))
;; (setq desktop-save t)
;; (add-hook 'auto-save-hook (lambda () (desktop-save-in-desktop-dir)))
;; (desktop-save-mode 1)

;; Enable winner-mode
(when (fboundp 'winner-mode)
      (winner-mode 1))

; Automatically clean up old buffers
(require 'midnight)

(require 'smex)
(add-hook 'after-init-hook 'smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq rainbow-delimiters-mode nil)
  (setq whitespace-mode 0)
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

(load "actionscript-mode-connors.el")
(require 'actionscript-mode)
(add-hook 'actionscript-mode-hook 'sensible-indentation)

(require 'csharp-mode)

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(setq global-auto-revert-mode 1)
(require 'whitespace)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/emacs/ac-dict")
;; (ac-config-default)

(require 'tramp)
(setq tramp-default-method "scp")

;; (require 'fill-column-indicator)
;; (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode t)))
;; (global-fci-mode 1)

; (add-hook 'before-save-hook 'delete-trailing-whitespace) <-- this is a little too prone to making changes

; display filename in title bar
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

; Don't backup files into the tree; put them all somewhere safe
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs_backups"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;;
;; Shell stuff
;;
(setq ansi-color-names-vector ; better contrast colors
      ["black" "red4" "green4" "yellow4"
       "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(add-hook 'shell-mode-hook '(lambda () (toggle-truncate-lines 1)))
(setq comint-prompt-read-only t)

;
; fire up cedet
;
; (setq semantic-load-turn-everything-on t)
;; Load CEDET
; (load "cedet/common/cedet.el")
;; Enabling SEMANTIC minor modes.  See semantic/INSTALL for more ideas.
; (semantic-load-enable-code-helpers)

;
; fire up ecb
;
; (cond (xemacsp (require 'ecb)
;              (ecb-activate)))

;
; Set up bash as the shell
;; (setq process-coding-system-alist '(("bash" . undecided-unix)))
;; (setq shell-file-name "bash")
;; (setenv "SHELL" shell-file-name)
;; (setq explicit-shell-file-name shell-file-name)
;; ;;
;; ;; This removes unsightly ^M characters that would otherwise
;; ;; appear in the output of java applications.
;; ;;
;; (add-hook 'comint-output-filter-functions
;;        'comint-strip-ctrl-m)

;
; Stuff I borrowed from Josh
;

(defun open-file-complement ()
  "Open complementary header or source file"
  (interactive)
  (let* ((fn (file-name-nondirectory (buffer-file-name)))
         (base (file-name-sans-extension fn))
         (ext (file-name-extension fn))

         (try-open (lambda (ext) (if (file-exists-p (concat base "." ext))
                                     (find-file (concat base "." ext))
                                   ())))

         ; try-open-2 is the supercharged version that looks for
         ; complementing files relative to the current location
         (try-open-2 (lambda (ext)
                       (let* ((srcfile (file-truename (concat "../src/" base "." ext)))
                              (incfile (file-truename (concat "../inc/" base "." ext)))
                              (extfile (file-truename (concat base "." ext))))
                         ;;                      (message "srcfile: `%s'" srcfile)
                         ;;                      (message "incfile: `%s'" incfile)
                         ;;                      (message "extfile: `%s'" extfile)
                         ;;                      ())))
                         (cond ((file-exists-p extfile) (find-file extfile))
                               ((file-exists-p srcfile) (find-file srcfile))
                               ((file-exists-p incfile) (find-file incfile))
                               (t nil))))))
    (if (or (equal ext "h") (equal ext "hh"))
        (cond ((funcall try-open-2 "cpp"))
              ((funcall try-open-2 "cc"))
              ((funcall try-open-2 "c"))
              ((funcall try-open-2 "C"))
              ((funcall try-open-2 "m"))
              ((funcall try-open-2 "mm"))
              (t (message "Couldn't find a complement to header `%s'" fn)))
      (if (or (equal ext "cpp")
              (equal ext "cc")
              (equal ext "c")
              (equal ext "C")
              (equal ext "m")
              (equal ext "mm")
              )
          (cond ((funcall try-open-2 "h"))
                ((funcall try-open-2 "hh"))
                (t (message "Couldn't find a complement to source `%s'" fn)))
        (message "Duh?  What kind of file is `%s'?" fn)))))

(add-hook 'text-mode-hook 'fix-tabs)

;; Default .h files to C++ mode.
(setq auto-mode-alist (cons '("\\.h\\'" . c++-mode) auto-mode-alist))

;; Make .mel files use C++ mode.
(setq auto-mode-alist (cons '("\\.mel\\'" . c++-mode) auto-mode-alist))

;; Make UnrealScript files use C++ mode.
(setq auto-mode-alist (cons '("\\.uc\\'" . c++-mode) auto-mode-alist))

;; Make .m and .mm files use objective-c mode.
; (require 'objc-c-mode)
(setq auto-mode-alist (cons '("\\.m\\'" . objc-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.mm\\'" . objc-mode) auto-mode-alist))

;; Ruby alist
(setq auto-mode-alist (cons '("\\.rake\\|Rakefile$" .
                              ruby-mode) auto-mode-alist))

;; Javscript alist
(setq auto-mode-alist (cons '("\\.js\\|\\.json$" .
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

;
; Tweak the key maps
;
(defun my-prev-error ()
  "Goto the previous error in the compilation buffer"
  (interactive)
  (next-error -1)
)

(setq my-grep-history nil)
(setq my-grep-dir-history nil)
(setq my-grep-default-dir "~/src")
(defun my-interactive-grep (search-string search-directory)
                               "Grep for a symbol in the specified directory"
                               (interactive (let* ((default-dir (cond ((car my-grep-dir-history))
                                                                      (t my-grep-default-dir))))
                                              (list (read-string "Grep for: "
                                                                 (symbol-near-point)
                                                                 'my-grep-history)
                                                    (read-string "In directory: " default-dir 'my-grep-dir-history))))
                               (grep (concat "grep -nr \""
                                             search-string "\" " search-directory )))

(define-key global-map '[(control tab)] 'swbuff-switch-to-next-buffer)
(define-key global-map '[(control shift tab)] 'swbuff-switch-to-previous-buffer)
(define-key global-map '[(control f3)] 'my-interactive-grep)
(define-key global-map '[f4] 'next-error)
(define-key global-map '[(shift f4)] 'my-prev-error)
(define-key global-map '[(control pause)] 'kill-compilation)
(define-key global-map '[(control break)] 'kill-compilation)
(define-key global-map '[f1]  (lambda () (interactive) (manual-entry (current-word))))

(global-set-key '[(home)] 'move-beginning-of-line)
(global-set-key '[(end)] 'move-end-of-line)
(global-set-key '[(kp-delete)] 'delete-char)

;; (setq mac-command-key-is-meta t)
;; (setq x-alt-keysym 'meta)
;; (setq x-super-keysym 'meta)


(defun mac-toggle-max-window ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                           'fullboth)))

(define-key global-map [(meta return)] 'mac-toggle-max-window)

(setq compilation-scroll-output t)
;; Recognize jshint errors
;; (add-to-list 'compilation-error-regexp-alist-alist
;;              '(jshint "^\\([^:]+\\): line \\([0-9]+\\), col \\([0-9]+\\)" 1 2 3))
;; (add-to-list 'compilation-error-regexp-alist 'jshint)

(require 'server)
(unless (server-running-p)
  (server-start))

;; (defun ns-raise-emacs ()
;;   (ns-do-applescript "tell application \"Emacs\" to activate"))
;; (add-hook 'server-visit-hook 'ns-raise-emacs)

;; (speedbar)

(setq grep-command "grep -nr")
; (setq compile-command "c:\\ad\\snicket\\bin\\build.bat winpc debug code")
(setq compile-command "rake lint")
(define-key global-map '[f7] 'compile)

(defun switch-to-kexp ()
  "Set things up for kexp"
  (interactive)
  (setq my-grep-history nil)
  (setq my-grep-dir-history nil)
  (setq my-grep-default-dir "/Users/brians/projects/kexp")
  (setq compile-command "cd /Users/brians/projects/kexp ; osascript build.scpt;")
  (define-key global-map '[f7] 'compile)
)

(setq case-fold-search t)
(setq current-language-environment "Latin-1")
(setq default-input-method "latin-1-prefix")
;; (turn-on-font-lock)
;; (global-font-lock-mode t)

;
; Set up windows-like shifted motion keys and other goodness.
; Allow shifted movement keys to modify current region.
; CUA Package from http://www.cua.dk/cua.html
;
(cua-mode 'emacs)
(delete-selection-mode 1)
(setq transient-mark-mode t)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(require 're-builder)
(setq reb-re-syntax 'string)
+
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ack-and-a-half-arguments (list "--ignore-dir=build" "--ignore-dir=tools"))
 '(ack-and-a-half-project-root-file-patterns (quote ("\\`.git\\'" "\\`.bzr\\'" "\\`_darcs\\'" "\\`.hg\\'")))
 '(ack-prompt-for-directory (quote unless-guessed))
 '(fill-column 80)
 '(global-auto-revert-mode t)
 '(global-whitespace-mode t)
 '(initial-buffer-choice t)
 '(load-home-init-file t t)
 '(ns-pop-up-frames nil)
 '(show-trailing-whitespace t)
 '(speedbar-frame-parameters (quote ((minibuffer) (width . 40) (border-width . 0) (menu-bar-lines . 0) (tool-bar-lines . 0) (unsplittable . t) (left-fringe . 0))))
 '(speedbar-frame-plist (quote (minibuffer nil width 40 border-width 0 internal-border-width 0 unsplittable t default-toolbar-visible-p nil has-modeline-p nil menubar-visible-p nil default-gutter-visible-p nil)))
 '(tool-bar-mode nil)
 '(whitespace-style (quote (trailing tab-mark))))


(when (eq system-type 'darwin)
      (set-face-attribute 'default nil :family "Source Code Pro"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "apple" :family "Source Code Pro"))))
 '(trailing-whitespace ((((class color) (background light)) (:background "gray89"))))
 '(whitespace-trailing ((t (:background "medium sea green" :foreground "yellow" :weight bold)))))

(setq x-select-enable-clipboard t)
(cond ((fboundp 'x-selection-value) (setq interprogram-paste-function 'x-selection-value))
      ((fboundp 'x-cut-buffer-or-selection-value) (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)))

