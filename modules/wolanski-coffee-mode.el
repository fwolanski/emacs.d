;;; wolanski-coffee-mode.el

;;; Code:

;; (require 'programming)

(require 'coffee-mode)

(require 'auto-complete)

(eval-after-load 'coffee-mode
  '(progn
     (defun wolanski-coffee-mode-defaults ()
       ;; CoffeeScript uses two spaces.
       ;; (set (make-local-variable 'tab-width) 2)

       ;; If you don't have js2-mode
       (setq coffee-js-mode 'js2-mode)
       (custom-set-variables '(coffee-tab-width 2))
       (setq evil-shift-width 2)
       (auto-complete-mode t)
       ;; (defadvice coffee-indent-line (after wants-indent activate)
       ;;   (when (< (current-indentation) (coffee-previous-indent))
       ;;     (let ((tabs-needed (- (/ (coffee-previous-indent) coffee-tab-width) 1)))
       ;;       (when (> tabs-needed 0)
       ;;         (insert-tab tabs-needed))))
       ;;   (when(coffee-line-wants-indent)
       ;;     (insert-tab 1)))
       )
     (setq wolanski-coffee-mode-hook 'wolanski-coffee-mode-defaults)
     (add-hook 'coffee-mode-hook (lambda () (run-hooks 'wolanski-coffee-mode-hook)))
     ))

(provide 'wolanski-coffee-mode)

;;; wolanski-coffee-mode.el ends here
