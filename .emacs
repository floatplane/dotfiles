;
;; Window size
;;
(set-frame-height (selected-frame) 40)
(set-frame-width (selected-frame) 132)
(set-frame-position (selected-frame) 0 0)

;; Disable Ctrl-Z minimization/suspension of emacs.
(global-set-key [C-z] nil)

;  Define a function to (formerly) disable tabs, and set the tab width
;  to something sensible.  These variables are buffer
; local, so we have to set them in a mode hook.
(defun fix-tabs ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4))

; Add the home directory's emacs directory to the load path.
(setq load-path (cons (expand-file-name "~/emacs") load-path))

;; Window colors
;; (load "color-theme-solarized.el")
;; (color-theme-solarized-light)

; Define a "jump-to-column" function, we'll install that through
; customize to be another XFF keyword
(defun jump-to-column (dest-col)
  "Move to the specified column.  Column is in the VC sense, where a tab
counts as n columns, rather than 1.  Column numbers are 1-based."
  (interactive "nColumn: ")
  (let* ((line (buffer-substring (point-at-bol) (point-at-eol)))
         (col 1)
         (index 0)
         )
    ; (message "line = %s, index = %d, col = %d" line index col)
    (while (> dest-col col)
        (progn (cond ((char-equal ?\t (aref line index))
                      (setq col (+ col tab-width))
                      (setq col (- col (mod (- col 1) tab-width))))
                     (t (setq col (+ col 1))))
               (setq index (+ index 1))))
               ; (message "line = %s, index = %d, col = %d" line index col)))
    ; (message "index = %d, col = %d" index col)
    (goto-char (+ (point-at-bol) index))))

;
; Set up buffer switching through ctrl-tab/ctrl-shift-tab.
; Package from http://perso.wanadoo.fr/david.ponce/more-elisp.html
;
(load "swbuff.el")
(require 'swbuff)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

; Save the desktop periodically
(require 'desktop)
(setq desktop-dirname "~/.emacs.desktop.dir/")
(setq desktop-save t)
(add-hook 'auto-save-hook (lambda () (desktop-save-in-desktop-dir)))
(desktop-save-mode 1)

; Automatically clean up old buffersx
(require 'midnight)

(load "smex.el")
(require 'smex)
(add-hook 'after-init-hook 'smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(load "anything.el")
(load "rcodetools.el")
(load "anything-rcodetools.el")
(require 'anything)
(require 'rcodetools)
(require 'anything-rcodetools)

(load "nxhtml/autostart.el")

(load "haml-mode.el")
(require 'haml-mode)

(when (file-exists-p "~/emacs/rinari/rinari.el")
  (load "rinari/rinari.el")
  (require 'rinari)
)

(load "actionscript-mode-connors.el")
(require 'actionscript-mode)

(load "csharp-mode.el")
(require 'csharp-mode)

(setq global-auto-revert-mode 1)
(require 'whitespace)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/emacs/ac-dict")
(ac-config-default)

(require 'tramp)
(setq tramp-default-method "scp")

;; Replace $RSENSE_HOME with the directory where RSense was installed in full path
;; Example for UNIX-like systems
;; (setq rsense-home "/home/tomo/opt/rsense-0.2")
;; or
;; (setq rsense-home (expand-file-name "~/opt/rsense-0.2"))
;; Example for Windows
;; (setq rsense-home "C:\\rsense-0.2")
;; (setq rsense-home "/opt/rsense-0.3")
;; (add-to-list 'load-path (concat rsense-home "/etc"))
;; (require 'rsense)
;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (add-to-list 'ac-sources 'ac-source-rsense-method)
;;             (add-to-list 'ac-sources 'ac-source-rsense-constant)))

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
; Set up windows-like shifted motion keys and other goodness.
; Allow shifted movement keys to modify current region.
; CUA Package from http://www.cua.dk/cua.html
;
(cua-mode t)

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

;; Python editing stuff
(setq auto-mode-alist (cons '("\\.pyw?\\|SConstruct\\|SConscript$" .
                              python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;; Lua editing stuff
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("lua" . lua-mode)
                                   interpreter-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(set-variable 'lua-indent-level 4)
(set-variable 'lua-default-application "/ad/build/winpc/release/lua/lua.exe")

;
; Tweak the key maps
;
(defun my-prev-error ()
  "Goto the previous error in the compilation buffer"
  (interactive)
  (next-error -1)
)

;; GNU Emacs doesn't have symbol-near-point apparently
;; stolen from browse-cltl2.el, and in turn:
;; stolen from XEmacs 19.15 syntax.el
(if (not (fboundp (function symbol-near-point)))
    (defun symbol-near-point ()
      "Return the first textual item to the nearest point."
      (interactive)
      ;;alg stolen from etag.el
      (save-excursion
        (if (not (memq (char-syntax (preceding-char)) '(?w ?_)))
            (while (not (looking-at "\\sw\\|\\s_\\|\\'"))
              (forward-char 1)))
        (while (looking-at "\\sw\\|\\s_")
          (forward-char 1))
        (if (re-search-backward "\\sw\\|\\s_" nil t)
            (regexp-quote
             (progn (forward-char 1)
                    (buffer-substring (point)
                                      (progn (forward-sexp -1)
                                             (while (looking-at "\\s'")
                                               (forward-char 1))
                                             (point)))))
          nil))))

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

(tool-bar-mode -1)
(define-key global-map [(alt return)] 'mac-toggle-max-window)

(setq compilation-scroll-output t)

;
; Fire up gnuserv when running in Emacs - otherwise we need to use
; XEmacs' winclient and XFF.el
;
;; (cond ((and (not xemacsp) (not aquamacsp))
;; 	(require 'gnuserv)
;; 	(setq gnuserv-frame (selected-frame))
;; 	(gnuserv-start)))
(server-start)
;; (defun ns-raise-emacs ()
;;   (ns-do-applescript "tell application \"Emacs\" to activate"))
;; (add-hook 'server-visit-hook 'ns-raise-emacs)

;; (speedbar)

(setq grep-command "grep -nr")
; (setq compile-command "c:\\ad\\snicket\\bin\\build.bat winpc debug code")

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
(setq transient-mark-mode t)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
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
 '(whitespace-style (quote (trailing tab-mark))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(trailing-whitespace ((((class color) (background light)) (:background "gray89"))))
 '(whitespace-trailing ((t (:background "medium sea green" :foreground "yellow" :weight bold)))))


;; (autoload 'ack-same "full-ack" nil t)
;; (autoload 'ack "full-ack" nil t)
;; (autoload 'ack-find-same-file "full-ack" nil t)
;; (autoload 'ack-find-file "full-ack" nil t)
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when (file-exists-p "~/.emacs.d/elpa/package.el")
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;; Load google-specific packages, if they exist
(if (file-exists-p "~/emacs/.emacs.google.el")
    (load-file "~/emacs/.emacs.google.el"))
