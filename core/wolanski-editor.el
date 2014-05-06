;;; wolanski-editor.el

;;; Code:

;; two space instead of tabs
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 2)            ;; but maintain correct appearance
(setq-default fill-column 80)

(setq indent-line-function 'insert-tab)

;; delete the selection with a keypress
(delete-selection-mode t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; smart pairing for all
(electric-pair-mode t)

;; diminish keeps the modeline tidy
(require 'diminish)

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defun wolanski-auto-save-command ()
  (when (and  buffer-file-name
             (buffer-modified-p (current-buffer)))
    (save-buffer)))

(defadvice switch-to-buffer (before save-buffer-now activate)
  (wolanski-auto-save-command))
(defadvice other-window (before other-window-now activate)
  (wolanski-auto-save-command))
(defadvice windmove-up (before other-window-now activate)
  (wolanski-auto-save-command))
(defadvice windmove-down (before other-window-now activate)
  (wolanski-auto-save-command))
(defadvice windmove-left (before other-window-now activate)
  (wolanski-auto-save-command))
(defadvice windmove-right (before other-window-now activate)
  (wolanski-auto-save-command))

(add-hook 'mouse-leave-buffer-hook 'wolanski-auto-save-command)

;; note - this should be after volatile-highlights is required
;; add the ability to copy and cut the current line, without marking it
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))


;; flyspell-mode does spell-checking on the fly as you type
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))

(defun wolanski-enable-flyspell ()
  (when (executable-find ispell-program-name)
    (flyspell-mode +1)))


;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(require 'expand-region)

;; shorter aliases for ack-and-a-half commands
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
(require 'midnight)

;; abbrev config
(add-hook 'text-mode-hook 'abbrev-mode)
(diminish 'abbrev-mode)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; whitespace-mode config
(require 'whitespace)
(setq whitespace-style '(face tabs empty trailing))
(diminish 'whitespace-mode)

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

;; sensible undo
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; enable winner-mode to manage window configurations
(winner-mode +1)

(require 'auto-complete)
(require 'auto-complete-config)
(require 'smartparens)
(require 'smartparens-config)
(diminish 'auto-complete-mode)

;;set all coding systems to utf-8
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;;disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(setq utf-translate-cjk-mode nil)

;; apply syntax highlighting to all buffers
(global-font-lock-mode t)

;; no overlay in smartparens
(setq sp-highlight-pair-overlay nil)

;; Autocomplete defaults
;; ESC to get out of autocomplete menu
(ac-config-default)
(define-key ac-completing-map (kbd "ESC") 'ac-stop)
(setq ac-delay 0.1
      ac-auto-show-menu 0.2
      ac-quick-help-delay 2.5
      ac-ignore-case nil
      ac-limit 20)

(setq-default ac-sources '(ac-source-imenu
                           ac-source-words-in-buffer
                           ac-source-words-in-same-mode-buffers
                           ac-source-dictionary
                           ac-source-filename))

;; Show matching parens immediately.
(show-paren-mode t)
(setq show-paren-delay 0)

;; enable live word counts
(require 'wc-mode)
;; only show the number of words
(setq wc-modeline-format "[%tw]")

(provide 'wolanski-editor)
