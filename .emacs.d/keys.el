;; Disable Ctrl-Z minimization/suspension of emacs.
(global-set-key [(control z)] nil)

;;
;; Tweak the key maps
;;
(global-set-key (kbd "C-c C-f") 'projectile-ag) ; 'my-interactive-grep)

(global-set-key '[(home)] 'move-beginning-of-line)
(global-set-key '[(end)] 'move-end-of-line)
(global-set-key '[(kp-delete)] 'delete-char)

;; For OS X Emacs from https://github.com/railwaycat/homebrew-emacsmacport, un-swap alt & meta
(cond ((boundp 'mac-command-modifier)
       (setq mac-command-modifier mac-option-modifier)
       (setq mac-option-modifier 'meta)))

(defun mac-toggle-max-window ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                         'fullboth)))

(define-key global-map [(meta return)] 'mac-toggle-max-window)

