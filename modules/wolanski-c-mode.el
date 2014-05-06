;;; wolanski-c-mode.el

;;; Code:

(defun wolanski-c-mode-common-defaults ()
  (setq c-basic-offset 2))

;; this will affect all modes derived from cc-mode, like
;; java-mode, php-mode, etc
(add-hook 'c-mode-common-hook 'wolanski-c-mode-common-defaults)

(provide 'wolanski-c-mode)

;;; wolanski-c-mode.el ends here
