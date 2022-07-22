;; On OS X we don't inherit the same path as terminal programs would, so read in PATH from the shell
;; http://emacswiki.org/emacs/EmacsApp
;; TODO: I think there's a built-in now
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

(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (progn
	  (message "Native comp is available")
      ;; Using Emacs.app/Contents/MacOS/bin since it was compiled with
      ;; ./configure --prefix="$PWD/nextstep/Emacs.app/Contents/MacOS"
      ;; Append to path to give priority to values from exec-path-from-shell-initialize.
      (add-to-list 'exec-path (concat invocation-directory (file-name-as-directory "bin")) t)
	  (setenv "LIBRARY_PATH" (concat (getenv "LIBRARY_PATH")
                                     (when (getenv "LIBRARY_PATH")
                                       ":")
				                     ;; This is where Homebrew puts libgccjit libraries.
                                     (car (file-expand-wildcards
                                           (expand-file-name
                                            (concat
                                             ;; brew --prefix returns either "/usr/local" or "/opt/homebrew"
                                             (file-name-as-directory (string-trim (shell-command-to-string "brew --prefix")))
                                             "opt/libgccjit/lib/gcc/*"))))))
	  ;; Only set after LIBRARY_PATH can find gcc libraries.
	  (setq comp-deferred-compilation t)
      (setq comp-speed 3))
  (message "Native comp is *not* available"))

(let
    ((initpath (concat (file-name-directory load-file-name) (file-name-as-directory "init"))))

  ;; Load the rest of the init scripts
  (dolist (initfile '("package.el"
                      "editor.el"
                      "appearance.el"
                      "programming_modes.el"
                      "keys.el"))
    (let ((path (expand-file-name initfile initpath)))
      (load-file path)))

  (setq custom-file (expand-file-name (concat initpath "custom.el")))
  (load custom-file)

  )
