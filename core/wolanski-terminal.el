;;; wolanski-terminal.el

;;; Code:

(require 'eshell)
(require 'em-smart)

(setq eshell-directory-name (expand-file-name "eshell" savefile-dir))

(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(setq eshell-cmpl-cycle-completions nil
      eshell-save-history-on-exit t
      eshell-buffer-shorthand t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

;; regular terminal
;; allow tabs in termainal
(add-hook 'term-mode-hook (lambda()
                (yas-minor-mode -1)
                (define-key term-raw-map (kbd "C-y") 'term-paste)))

(require 'multi-term)
(setq multi-term-program "/bin/bash")


;; Use Emacs terminfo, not system terminfo
;; you may need to add this if you use Cocoa Emacs or Carbon Emacs
;; tic -o ~/.terminfo /Applications/Emacs.app/Contents/Resources/etc/e/eterm-color.ti
(setq system-uses-terminfo nil)

(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" savefile-dir))

;; utf-8 in terminal
(defadvice ansi-term (after advise-ansi-term-coding-system)
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)

(add-hook 'term-mode-hook
  (lambda()
    (hl-line-mode -1)))

;;;###autoload
;; (defun really-kill-eterm-buffer
;;    (let ((inhibit-read-only t)) (kill-this-buffer)))

;; (eval-after-load 'esh-opt
;;   '(progn
;;      (require 'em-prompt)
;;      (require 'em-term)
;;      (require 'em-cmpl)
;;      (setenv "PAGER" "cat")
;;      (set-face-attribute 'eshell-prompt nil :foreground "turquoise1")
;;      (add-hook 'eshell-mode-hook ;; for some reason this needs to be a hook
;;                '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol)))
;;      (setq eshell-cmpl-cycle-completions nil)

;;      ;; TODO: submit these via M-x report-emacs-bug
;;      (add-to-list 'eshell-visual-commands "ssh")
;;      ;; (add-to-list 'eshell-visual-commands "tail")
;;      (add-to-list 'eshell-command-completions-alist
;;                   '("gunzip" "gz\\'"))
;;      (add-to-list 'eshell-command-completions-alist
;;                   '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))))

;; (setq eshell-directory-name eshell-folder)
;; ;; (setq eshell-aliases-file "alias")

;; ;;;###autoload
;; (defun eshell/cdp ()
;;   "Change directory to the project's root."
;;   (eshell/cd (locate-dominating-file default-directory ".git")))

;; ;; these two haven't made it upstream yet
;; ;;;###autoload
;; (when (not (functionp 'eshell/find))
;;   (defun eshell/find (dir &rest opts)
;;     (find-dired dir (mapconcat (lambda (arg)
;;                                  (if (get-text-property 0 'escaped arg)
;;                                      (concat "\"" arg "\"")
;;                                    arg))
;;                                opts " "))))

;; ;;;###autoload
;; (when (not (functionp 'eshell/rgrep))
;;   (defun eshell/rgrep (&rest args)
;;     "Use Emacs grep facility instead of calling external grep."
;;     (eshell-grep "rgrep" args t)))

;; ;;;###autoload
;; (defun eshell/extract (file)
;;   (let ((command (some (lambda (x)
;;                          (if (string-match-p (car x) file)
;;                              (cadr x)))
;;                        '((".*\.tar.bz2" "tar xjf")
;;                          (".*\.tar.gz" "tar xzf")
;;                          (".*\.bz2" "bunzip2")
;;                          (".*\.rar" "unrar x")
;;                          (".*\.gz" "gunzip")
;;                          (".*\.tar" "tar xf")
;;                          (".*\.tbz2" "tar xjf")
;;                          (".*\.tgz" "tar xzf")
;;                          (".*\.zip" "unzip")
;;                          (".*\.Z" "uncompress")
;;                          (".*" "echo 'Could not extract the file:'")))))
;;     (eshell-command-result (concat command " " file))))

;; (defface esk-eshell-error-prompt-face
;;   '((((class color) (background dark)) (:foreground "red" :bold t))
;;     (((class color) (background light)) (:foreground "red" :bold t)))
;;   "Face for nonzero prompt results"
;;   :group 'eshell-prompt)

;; (add-hook 'eshell-after-prompt-hook
;;           (defun esk-eshell-exit-code-prompt-face ()
;;             (when (and eshell-last-command-status
;;                        (not (zerop eshell-last-command-status)))
;;               (let ((inhibit-read-only t))
;;                 (add-text-properties
;;                  (save-excursion (beginning-of-line) (point)) (point-max)
;;                  '(face esk-eshell-error-prompt-face))))))

;; (defun esk-eshell-in-dir (&optional prompt)
;;   "Change the directory of an existing eshell to the directory of the file in
;;   the current buffer or launch a new eshell if one isn't running.  If the
;;   current buffer does not have a file (e.g., a *scratch* buffer) launch or raise
;;   eshell, as appropriate.  Given a prefix arg, prompt for the destination
;;   directory."
;;   (interactive "P")
;;   (let* ((name (buffer-file-name))
;;          (dir (cond (prompt (read-directory-name "Directory: " nil nil t))
;;                     (name (file-name-directory name))
;;                     (t nil)))
;;          (buffers (delq nil (mapcar (lambda (buf)
;;                                       (with-current-buffer buf
;;                                         (when (eq 'eshell-mode major-mode)
;;                                           (buffer-name))))
;;                                     (buffer-list))))
;;          (buffer (cond ((eq 1 (length buffers)) (first buffers))
;;                        ((< 1 (length buffers)) (ido-completing-read
;;                                                 "Eshell buffer: " buffers nil t
;;                                                 nil nil (first buffers)))
;;                        (t (eshell)))))
;;     (with-current-buffer buffer
;;       (when dir
;;         (eshell/cd (list dir))
;;         (eshell-send-input))
;;       (end-of-buffer)
;;       (pop-to-buffer buffer))))


(provide 'wolanski-terminal)
;;; wolanski-terminal.el ends here
