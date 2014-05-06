;;; wolanski-clojure-mode.el

;;; Code:

(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

(require 'evil-paredit)

(defun esk-pretty-fn ()
  (font-lock-add-keywords nil `(("(\\(\\<defn\\>\\)"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1)
                                                           "\u03bb"
                                                           'decompose-region)))))))

(eval-after-load 'clojure-mode
  '(progn
     (defun wolanski-clojure-mode-defaults ()
       (paredit-mode +1)
       (evil-paredit-mode +1)
       (esk-pretty-fn)
       )
     (setq wolanski-clojure-mode-hook 'wolanski-clojure-mode-defaults)
     (add-hook 'clojure-mode-hook (lambda () (run-hooks 'wolanski-clojure-mode-hook)))
     ))

(eval-after-load "paredit"
  '(diminish 'paredit-mode " π"))

(eval-after-load 'nrepl
  '(progn
     (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

     (defun wolanski-nrepl-mode-defaults ()
       (subword-mode +1))

     (setq wolanski-nrepl-mode-hook 'wolanski-nrepl-mode-defaults)

     (add-hook 'nrepl-mode-hook (lambda ()
                                  (run-hooks 'wolanski-nrepl-mode-hook)))))

(provide 'wolanski-clojure-mode)

;;; wolanski-clojure-mode.el ends here
