;; Add the package directories to the load path.
(dolist (dir '(".emacs.d/packages/use-package" ".emacs.d/packages"))
  (let ((path (expand-file-name (concat "~/" dir))))
    (setq load-path (cons path load-path))
    (message (format "Added %s to load-path" path))))

;;
;; External packages.
;;
(defvar packages-list
  '(
    ;; auto-complete
    ;; auto-complete-clang-async
    ag
    aggressive-indent
    coffee-mode
    company
    counsel
    csharp-mode
    cursor-chg
    doom-themes
    editorconfig
    fill-column-indicator
    flycheck
    flymake-ruby
    git-gutter+
    groovy-mode
    haml-mode
    highlight-indentation
    highlight-symbol
    irony
    ivy
    js2-mode
    magit
    markdown-mode
    neotree
    protobuf-mode
    python-mode
    rainbow-delimiters
    robe
    scss-mode
    smex
    swiper
    textmate
    web-mode
    yaml-mode
    yasnippet
    zenburn-theme
    )
  "List of packages needs to be installed at launch")

;; Set up package archives
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Look for missing packages, and install them
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

