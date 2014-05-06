;;; wolanski-js-mode.el

;;; Code:

;; (require 'js2-mode)

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

(eval-after-load 'js2-mode
  '(progn
     (defun wolanski-js-mode-defaults ()
       ;; electric-layout-mode doesn't play nice with js-mode
       (flycheck-mode -1)
       ;; (electric-layout-mode -1)
       ;; (smartparens-mode 1)
       (setq js2-basic-offset 2)
       (setq js2-highlight-level 3)
       (setq-default js2-global-externs '("module" "require" "jQuery" "$" "_" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))

       )
     (setq wolanski-js-mode-hook 'wolanski-js-mode-defaults)
     (add-hook 'js2-mode-hook (lambda () (run-hooks 'wolanski-js-mode-hook)))))

(provide 'wolanski-js-mode)

;;; wolanski-js-mode.el ends here
