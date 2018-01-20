;;
;; Window size - maximize it, hide the button bar
;;
(cond ((display-graphic-p)
       (tool-bar-mode -1)
       (toggle-frame-maximized)))

;; Don't open new frames for each file open externally.
(setq ns-pop-up-frames nil)
(setq pop-up-frames nil)

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Source Code Pro"))

