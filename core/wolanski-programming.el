;;; wolanski-programming.el

;;; Code:

(defun wolanski-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun wolanski-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

;; delete the trailing whitespace
(defun prog-delete-trailing-whitespace ()
  (when (derived-mode-p 'prog-mode)
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'prog-delete-trailing-whitespace)

;; also keep it decent
(defun wolanski-enable-whitespace ()
    (add-hook 'before-save-hook 'whitespace-cleanup nil t)
    (whitespace-mode +1))

(add-hook 'prog-mode-hook 'wolanski-enable-whitespace)

;; draw a thin line at column 80
(require 'fill-column-indicator)

;; show the name of the current function definition in the modeline
(require 'which-func)
(setq which-func-modes t)
(which-function-mode 1)

(defun wolanski-prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  ;; (when (executable-find ispell-program-name)
  ;;   (flyspell-prog-mode))
  (wolanski-enable-whitespace)
  (fci-mode +1)
  (flycheck-mode +1)
  (auto-complete-mode +1)
  (wolanski-local-comment-auto-fill)
  (wolanski-add-watchwords))

(setq wolanski-prog-mode-hook 'wolanski-prog-mode-defaults)

(add-hook 'prog-mode-hook (lambda ()
                            (run-hooks 'wolanski-prog-mode-hook)))

(provide 'wolanski-programming)
;;; packages.el ends here
