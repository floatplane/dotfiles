;; Disable Ctrl-Z minimization/suspension of emacs.
(global-set-key [(control z)] nil)

;;
;; Tweak the key maps
;;
(global-set-key (kbd "C-c C-f") 'projectile-ag) ; 'my-interactive-grep)

(global-set-key (kbd "C-.") 'dumb-jump-go)
(global-set-key (kbd "C-,") 'dumb-jump-back)

(global-set-key '[(home)] 'move-beginning-of-line)
(global-set-key '[(end)] 'move-end-of-line)
(global-set-key '[(kp-delete)] 'delete-char)

(global-set-key (kbd "C-c s t") 'string-inflection-cycle) ;; t for toggle
(global-set-key (kbd "C-c s c") 'string-inflection-lower-camelcase)
(global-set-key (kbd "C-c s C") 'string-inflection-lower-camelcase)
(global-set-key (kbd "C-c s s") 'string-inflection-underscore)  ;; s for snake case

(defun my-prev-error ()
  "Goto the previous error in the compilation buffer"
  (interactive)
  (next-error -1)
  )

(define-key global-map '[f4] 'next-error)
(define-key global-map '[(shift f4)] 'my-prev-error)

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

