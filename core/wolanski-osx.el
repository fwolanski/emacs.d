;;; wolanski-osx.el

;;; Code:

;; On OS X Emacs doesn't use the shell PATH if it's not started from
;; the shell. Let's fix that:
(require 'exec-path-from-shell)

(exec-path-from-shell-initialize)

;; It's all in the Meta
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)

;; this is better
(setq ns-use-native-fullscreen nil)

(let ((path (shell-command-to-string ". ~/.bash_profile; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))

;; (keyboard-translate ?\M-, ?\C-x)

(define-key key-translation-map (kbd "M-,") (kbd "C-x"))

;; see http://stackoverflow.com/questions/4179230/is-there-a-trick-to-debugging-a-spin-in-emacs-elisp-in-a-flymake-module/9885264#9885264
(setq flymake-gui-warnings-enabled nil)

(provide 'wolanski-osx)
;;; wolanski-osx.el ends here
