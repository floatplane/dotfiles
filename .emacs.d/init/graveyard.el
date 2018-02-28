;;
;; Collection of stuff that was useful at one point, but not currently
;;

;; (require 'neotree)
;; (defun neotree-project-dir ()
;;   "Open NeoTree using the git root."
;;   (interactive)
;;   (let ((project-dir (projectile-project-root))
;;         (file-name (buffer-file-name)))
;;     (neotree-toggle)
;;     (if project-dir
;;         (if (neo-global--window-exists-p)
;;             (progn
;;               (neotree-dir project-dir)
;;               (neotree-find file-name)))
;;       (message "Could not find git project root."))))
;; (global-set-key [f8] 'neotree-project-dir)

;; yasnippets
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; Company mode
;; (require 'company)
;; (add-hook 'after-init-hook 'global-company-mode)

;; Capnp
;; (require 'capnp-mode)
;; (add-to-list 'auto-mode-alist '("\\.capnp\\'" . capnp-mode))

;; ;; Irony mode.
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-irony))
;; ;; (optional) adds CC special commands to `company-begin-commands' in order to
;; ;; trigger completion at interesting places, such as after scope operator
;; ;;     std::|
;; (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; ;; Irony mode.
;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)

;; Flycheck
;; (require 'flycheck)
;; (add-hook 'after-init-hook 'global-flycheck-mode)
;; (eval-after-load 'flycheck
;;   '(add-to-list 'flycheck-checkers 'irony))

;; rtags
;; (require 'rtags)
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-rtags))
;; (rtags-enable-standard-keybindings)
;; (setq rtags-completions-enabled t)
;; (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
;; (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
;; (define-key c-mode-base-map (kbd "C-<") 'rtags-location-stack-back)
;; (define-key c-mode-base-map (kbd "C->") 'rtags-location-stack-forward)
;; (define-key c-mode-base-map (kbd "C-v") 'rtags-find-virtuals-at-point)
;; (define-key c-mode-base-map (kbd "M-i") 'rtags-imenu)


;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
;; (defun my-irony-mode-hook ()
;;   (define-key irony-mode-map [remap completion-at-point]
;;     'irony-completion-at-point-async)
;;   (define-key irony-mode-map [remap complete-symbol]
;;     'irony-completion-at-point-async))
;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; (require 'textmate)
;; (add-hook 'textmate-mode-hook
;;           (lambda () (progn (message "Textmate mode activated")
;;                             (setq textmate-mode-keymap (cdr (assoc 'textmate-mode minor-mode-map-alist)))
;;                             (define-key textmate-mode-keymap [(meta up)] nil)
;;                             (define-key textmate-mode-keymap [(meta down)] nil)
;;                             (define-key textmate-mode-keymap [(meta shift up)] nil)
;;                             (define-key textmate-mode-keymap [(meta shift down)] nil))))
;; (textmate-mode)

;; (require 'ido)
;; (ido-mode t)
;; (setq ido-enable-flex-matching t)

;; ;; From https://bitbucket.org/durin42/dotfiles/src/tip/.elisp/settings/50.localfuncs.el#cl-9:
;; ;; advice to prevent ido from using flex matches on huge lists of files
;; ;; with enough characters typed and blocking for an absurd amount of time
;; ;; after a stupid typo
;; ;; The value af-ido-flex-fuzzy-limit is the maximum value of the product
;; ;; of the number of characters the user has entered and the number of
;; ;; options in the ido list.
;; ;; The default value was determined experimentally and seemed to be
;; ;; the boundary of sane wait times when this was written.
;; (defvar af-ido-flex-fuzzy-limit (* 2000 5))
;; (defadvice ido-set-matches-1 (around my-ido-set-matches-1 activate)
;;   "Conditionally disable flex matching if the list is huge.

;; This is useful because when the ido list is huge, ido flex matching
;; spends an eternity in a regex if you make a typo."
;;   (let ((ido-enable-flex-matching (< (* (length (ad-get-arg 0)) (length ido-text))
;;                                      af-ido-flex-fuzzy-limit)))
;;     ad-do-it))

;; (require 'smex)
;; (add-hook 'after-init-hook 'smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; ;; This is your old M-x.
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(load "actionscript-mode-connors.el")
(require 'actionscript-mode)
(add-hook 'actionscript-mode-hook 'sensible-indentation)

;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/emacs/ac-dict")
;; (ac-config-default)

;; (require 'fill-column-indicator)
;; (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode t)))
;; (global-fci-mode 1)

;; (add-hook 'before-save-hook 'delete-trailing-whitespace) <-- this is a little too prone to making changes

;;
;; fire up cedet
;;
;; (setq semantic-load-turn-everything-on t)
;; Load CEDET
;; (load "cedet/common/cedet.el")
;; Enabling SEMANTIC minor modes.  See semantic/INSTALL for more ideas.
;; (semantic-load-enable-code-helpers)

;;
;; fire up ecb
;;
;; (cond (xemacsp (require 'ecb)
;;              (ecb-activate)))

;;
;; Set up bash as the shell
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

;;
;; Stuff I borrowed from Josh
;;

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
    (if (or (equal ext "h") (equal ext "hh") (equal ext "hpp"))
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
                ((funcall try-open-2 "hpp"))
                (t (message "Couldn't find a complement to source `%s'" fn)))
        (message "Duh?  What kind of file is `%s'?" fn)))))

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

(global-set-key [(control tab)] 'swbuff-switch-to-next-buffer)
(global-set-key [(control shift tab)] 'swbuff-switch-to-previous-buffer)

;; (define-key global-map '[(control pause)] 'kill-compilation)
;; (define-key global-map '[(control break)] 'kill-compilation)
;; (define-key global-map '[f1]  (lambda () (interactive) (manual-entry (current-word))))
;; (define-key c-mode-base-map '[(control super up)] 'open-file-complement)

;; (setq mac-command-key-is-meta t)
;; (setq x-alt-keysym 'meta)
;; (setq x-super-keysym 'meta)

(setq grep-command "grep -nr")
;; (setq compile-command "c:\\ad\\snicket\\bin\\build.bat winpc debug code")
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

;; (turn-on-font-lock)
;; (global-font-lock-mode t)
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

;; Recognize jshint errors
;; (add-to-list 'compilation-error-regexp-alist-alist
;;              '(jshint "^\\([^:]+\\): line \\([0-9]+\\), col \\([0-9]+\\)" 1 2 3))
;; (add-to-list 'compilation-error-regexp-alist 'jshint)

;; (defun ns-raise-emacs ()
;;   (ns-do-applescript "tell application \"Emacs\" to activate"))
;; (add-hook 'server-visit-hook 'ns-raise-emacs)

;; (speedbar)
