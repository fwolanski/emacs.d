;;; wolanski-php-mode.el

;;; Code:

;; (require 'php-mode)

(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

(evil-declare-key 'normal php-mode-map "K" 'php-search-documentation)

(provide 'wolanski-php-mode)

;;; wolanski-php-mode.el ends here
