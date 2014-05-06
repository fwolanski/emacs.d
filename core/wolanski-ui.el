;;; wolanski-ui.el

;;; Code:

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(blink-cursor-mode 0)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; make the fringe (gutter) smaller, and remove the right & left fringe
;; truncation indication

(add-to-list 'default-frame-alist '(left-fringe  . 8))
(add-to-list 'default-frame-alist '(right-fringe  . 1))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " " (:eval (if (buffer-file-name)
                                      (abbreviate-file-name (buffer-file-name))
                                    "%b"))))
;; git-gutter
(require 'diff-hl)
(global-diff-hl-mode t)

;; rainbow delims
(global-rainbow-delimiters-mode t)

;; underline errors
(require 'flycheck)
(setq flycheck-highlighting-mode nil)

;; Color theme
(load-theme 'blue-dark t)
;; Color theme everywhere.
(setq color-theme-is-global t)

;; disable visual bell
(setq visible-bell t)

;; don't wrap lines
(setq-default truncate-lines t)

;; show-paren-mode: subtle highlighting of matching parens (global-mode)
(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

;; highlight the current line
(global-hl-line-mode +1)

(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; Linum format to avoid graphics glitches in fringe
(setq linum-format "%4d ")

;; make the mode line a bit better and add some color
(require 'smart-mode-line)
(if after-init-time (sml/setup)
  (add-hook 'after-init-hook 'sml/setup))

(setq sml/override-theme nil)

;; make the file name a little shorter
(setq sml/shorten-directory t)
(setq sml/name-width 10)

;; make sure some buffers always take the bottom seat
(require 'popwin)
(popwin-mode 1)
(setq helm-popwin
      '(("*Helm Find Files*" :height 20)
        ("^\*helm.+\*$" :regexp t :height 20)))
(setq popwin:special-display-config
                     (append helm-popwin
                             popwin:special-display-config))

(provide 'wolanski-ui)
;;; wolanski-ui.el ends here
