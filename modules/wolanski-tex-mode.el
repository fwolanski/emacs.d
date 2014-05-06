;;; wolanski-tex-mode.el

;;; Code:

;; (eval-after-load "tex"
;;   '(add-to-list 'TeX-command-list
;;                 '("XeLaTeX" "xelatex -interaction=nonstopmode %s"
;;                   TeX-run-command t t :help "Run xelatex") t)
;;   (setq TeX-command-default "XeLaTeX")
;;   (setq TeX-save-query nil)
;;   (setq TeX-show-compilation t))

(setq latex-run-command "xelatex")

(add-hook 'LaTeX-mode-hook 'wolanski-text-hook)

(provide 'wolanski-tex-mode)

;;; wolanski-tex-mode.el ends here
