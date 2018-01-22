;; On OS X we don't inherit the same path as terminal programs would, so read in PATH from the shell
;; http://emacswiki.org/emacs/EmacsApp
;; TODO: I think there's a built-in now

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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

;; Load the rest of the init scripts
(dolist (initfile '("package"
                    "editor"
                    "appearance"
                    "theme"
                    "programming_modes"
                    "keys"))
  (let ((path (expand-file-name (concat "~/.emacs.d/" initfile ".el") )))
    (load-file path)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
