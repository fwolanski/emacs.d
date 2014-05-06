;;; wolanski-functions.el

;;; Code:

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


(defun comment-dwim-line (&optional arg)
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun esk-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

;;; It is the opposite of fill-paragraph. Takes a multi-line paragraph and makes
;;; it into a single line of text.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun wolanski-dired-create-directory ()
  (interactive)
  (let ((dir-name-name (read-from-minibuffer "Directory name: ")))
  (let ((dir-name (concat dired-directory dir-name-name)))
  (let ((ok (progn (if (file-exists-p dir-name) (message (concat "Directory " dir-name " already exists."))) (not (file-exists-p dir-name)))))
    (when ok
      (make-directory dir-name)
      (dired-do-redisplay))))))

(defun wolanski-dired-create-file ()
  (interactive)
  (let ((file-name-name (read-from-minibuffer "File name: ")))
  (let ((file-name (concat dired-directory file-name-name)))
  (let ((ok (progn (if (file-exists-p file-name) (message (concat "File " file-name " already exists."))) (not (file-exists-p file-name)))))
    (when ok
      (find-file file-name)
      (set-buffer-modified-p t)
      (save-buffer))))))

(defun wolanski/search-word-backward ()
  "Find the previous occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-backward "w_")
    (goto-char
     (if (re-search-backward (concat "\\_<" (current-word) "\\_>") nil t)
         (match-beginning 0)
       cur))))

(defun wolanski/search-word-forward ()
  "Find the next occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-forward "w_")
    (goto-char
     (if (re-search-forward (concat "\\_<" (current-word) "\\_>") nil t)
         (match-beginning 0)
       cur))))

;; A small minor mode to use a big fringe
(defvar wolanski-big-fringe-mode nil)
(define-minor-mode wolanski-big-fringe-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable wolanski-big-fringe-mode
  :group 'editing-basics
  (if (not wolanski-big-fringe-mode)
    (progn
      (set-fringe-mode '(8 . 1))
      (custom-set-faces '(fringe ((t (:background "#001821"))))))
    (progn
      (set-fringe-mode
       (/ (- (frame-pixel-width)
             (* 100 (frame-char-width)))
          2))
      (custom-set-faces '(fringe ((t (:background "#000000"))))))))

(provide 'wolanski-functions)
;;; wolanski-functions.el ends here
