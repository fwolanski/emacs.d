;;; wolanski-org-mode.el

;;; Code:


;; open text files in org mode too!
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))

;; Org-mode settings
;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; syntax highlighting in buffers
(setq org-src-fontify-natively t)
(setq org-replace-disputed-keys t)



(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (R . t)
   (sh . t)))

(add-hook 'org-mode-hook 'wolanski-text-hook)

(provide 'wolanski-org-mode)
;;; wolanski-org-mode.el ends here
