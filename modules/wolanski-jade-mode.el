;;; wolanski-jade-mode.el

;;; Code:

(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

(provide 'wolanski-jade-mode)

;;; wolanski-jade-mode.el ends here
