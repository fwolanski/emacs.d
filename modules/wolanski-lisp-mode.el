;;; wolanski-lisp-mode.el

;;; Code:

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
       (prettify-symbols-mode +1)
       (paredit-mode +1)
       (evil-paredit-mode +1)))
(provide 'wolanski-lisp-mode)

;;; wolanski-lisp-mode.el ends here
