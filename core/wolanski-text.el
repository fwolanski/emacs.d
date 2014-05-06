;;; wolanski-text.el

;;; Code:


(defun wolanski-text-hook ()
  (progn
    (wolanski-enable-flyspell)
    (auto-fill-mode 1)
    (longlines-mode 1)
    (remove-hook 'before-save-hook 'delete-trailing-whitespace)
    (abbrev-mode 1)
    (wc-mode)))

(provide 'wolanski-text)
;;; wolanski-text.el ends here
