;;; wolanski-keybindings.el

;;; Code:

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Start proced in a similar manner to dired
;; (global-set-key (kbd "C-x p") 'proced)

;; always use smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is the old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; a complement to the zap-to-char command, that doesn't eat up the target character
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;; opposite of fill-paragraph
(define-key global-map (kbd "C-q") 'unfill-paragraph)

(global-set-key '[M-up] 'wolanski/search-word-backward)
(global-set-key '[M-down] 'wolanski/search-word-forward)

(defadvice search-for-keyword (around wolanski activate)
  "Match in a case-insensitive way."
  (let ((case-fold-search t))
    ad-do-it))

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp
                 isearch-string
               (regexp-quote isearch-string))))))

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)


(setq term-bind-key-alist
      (list (cons "C-c C-c" 'term-interrupt-subjob)
            (cons "C-p" 'previous-line)
            (cons "C-n" 'next-line)
            (cons "M-f" 'term-send-forward-word)
            (cons "M-b" 'term-send-backward-word)
            (cons "C-c C-j" 'term-line-mode)
            (cons "C-c C-k" 'term-char-mode)
            (cons "M-DEL" 'term-send-backward-kill-word)
            (cons "M-d" 'term-send-forward-kill-word)
            (cons "<C-left>" 'term-send-backward-word)
            (cons "<C-right>" 'term-send-forward-word)
            (cons "C-r" 'term-send-reverse-search-history)
            (cons "M-p" 'term-send-raw-meta)
            (cons "M-y" 'term-send-raw-meta)
            (cons "C-y" 'term-send-raw)))

(require 'evil-indent-textobject)
(require 'evil)

(evil-mode 1)

;;; prevent evil mode in some places
(loop for (mode . state) in '((inferior-emacs-lisp-mode     . emacs)
                              (pylookup-mode                . emacs)
                              (comint-mode                  . emacs)
                              (ebib-entry-mode              . emacs)
                              (ebib-index-mode              . emacs)
                              (ebib-log-mode                . emacs)
                              (gtags-select-mode            . emacs)
                              (eshell-mode                  . emacs)
                              (shell-mode                   . emacs)
                              (term-mode                    . emacs)
                              (bc-menu-mode                 . emacs)
                              (magit-branch-manager-mode    . emacs)
                              (semantic-symref-results-mode . emacs)
                              (rdictcc-buffer-mode          . emacs)
                              (deft-mode                    . emacs)
                              (erc-mode                     . normal))
      do (evil-set-initial-state mode state))

;; comments
(define-key evil-normal-state-map ",c" 'comment-dwim-line)
(define-key evil-visual-state-map ",c" 'comment-dwim)

;; custom movement commands
(define-key evil-normal-state-map (kbd "[") (lambda ()
                    (interactive)
                    (previous-line 10)
                    (evil-scroll-line-up 10)))

(define-key evil-normal-state-map (kbd "]") (lambda ()
                      (interactive)
                      (next-line 10)
                      (evil-scroll-line-down 10)))

(define-key evil-normal-state-map "-" 'er/expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; spell and synonyms
(global-set-key (kbd "C-x s") 'ispell-word)
;; (define-key evil-normal-state-map ",s" 'ispell-word)
(define-key evil-normal-state-map (kbd "C-x S") 'synonyms-no-read)
;; (define-key evil-normal-state-map ",S" 'synonyms-no-read)

;; helm find files
(global-set-key (kbd "C-x f") 'helm-find-files)
;; (define-key evil-normal-state-map ",f" 'helm-find-files)

;; helm mini files
(global-set-key (kbd "C-x o") 'helm-mini)
;; (define-key evil-normal-state-map ",o" 'helm-mini)

;; fullscreen
;; (define-key evil-normal-state-map (kbd "C-S-f") 'toggle-frame-fullscreen)

(define-key evil-normal-state-map (kbd "M-`") 'ns-next-frame)
(global-set-key (kbd "M-`") 'ns-next-frame)

;; beginning and end of line
(define-key evil-normal-state-map "L" 'evil-end-of-line)
(define-key evil-motion-state-map "H" 'evil-beginning-of-line)

;; buffers
(global-set-key (kbd "C-x b") 'ibuffer)
;; (define-key evil-normal-state-map (kbd "C-b") 'ibuffer)
;; (define-key evil-normal-state-map (kbd ", b") 'ibuffer)

;; undo
(global-set-key (kbd "C-x u") 'undo-tree-visualize)
;; (define-key evil-normal-state-map (kbd ", u") 'undo-tree-visualize)

;; find file in project
(global-set-key (kbd "C-x p") 'projectile-find-file)
;; (define-key evil-normal-state-map (kbd ", p") 'projectile-find-file)

;; quickly switch from a project
(global-set-key (kbd "C-x C-p") 'projectile-switch-project)
(global-set-key (kbd "C-x M-p") 'projectile-switch-project)

;; find text in project
(global-set-key (kbd "C-x a") 'projectile-ack)
;; (define-key evil-normal-state-map (kbd ", a") 'projectile-ack)


;; ansi-term
(global-set-key (kbd "C-x t") 'multi-term)
;; (define-key evil-normal-state-map (kbd ", t") 'multi-term)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
;; (define-key evil-normal-state-map (kbd ", g") 'magit-status)

;; deft
(global-set-key (kbd "C-x n") 'deft)
;; (define-key evil-normal-state-map (kbd ", n") 'deft)

;; lights
(global-set-key (kbd "C-x l") 'wolanski-big-fringe-mode)
;; (define-key evil-normal-state-map (kbd ", n") 'deft)



;; evil overrides
(define-key evil-visual-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

;; scrolling overrides
(define-key evil-normal-state-map (kbd "C-<down>") 'scroll-up)
(define-key evil-normal-state-map (kbd "C-<up>") 'scroll-down)

;; other conveniences
;; (define-key evil-normal-state-map (kbd ", k") 'kill-this-buffer)
;; (define-key evil-normal-state-map (kbd ", q") 'delete-window)
;; (define-key evil-normal-state-map (kbd ", v") 'split-window-horizontally)
;; (define-key evil-normal-state-map (kbd ", =") 'balance-windows)


(global-set-key (kbd "C-SPC") #'ace-jump-line-mode)
(global-set-key (kbd "S-SPC") #'ace-jump-word-mode)

;; escape cancels everything like in vim
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(define-key evil-visual-state-map ">" (defun angel-indent ()
   (interactive)
   (evil-shift-right (region-beginning) (region-end))
   (evil-normal-state)
   (evil-visual-restore)))

(define-key evil-visual-state-map "<" (defun angel-unindent ()
   (interactive)
   (evil-shift-left (region-beginning) (region-end))
   (evil-normal-state)
   (evil-visual-restore)))

(provide 'wolanski-keybindings)

;;; wolanski-keybindings.el ends here
