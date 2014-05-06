;;; wolanski-env.el

;;; Code:

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; saveplace remembers your location in a file when saving files
(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" savefile-dir))
;; activate it for all buffers
(setq-default save-place t)

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" savefile-dir))

(savehist-mode +1)

;; save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" savefile-dir)
      recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)

;; (require 'desktop)

;; Automatically save and restore sessions only in graphical emacs.
;; (setq desktop-dirname             savefile-dir
;;       desktop-base-file-name      "emacs.desktop"
;;       desktop-base-lock-name      "lock"
;;       desktop-path                (list desktop-dirname)
;;       desktop-save                t
;;       desktop-files-not-to-save   "^$" ;reload tramp paths
;;       desktop-load-locked-desktop nil)

;; (when (display-graphic-p)
;;   (desktop-save-mode 1))

;; tramp, for sudo access
(require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
(setq tramp-default-method "ssh")
(setq password-cache-expiry nil)

;; ido-mode
(require 'ido)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file (expand-file-name "ido.hist" savefile-dir)
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1)
(ido-mode +1)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode +1)
;; (ido-everywhere +1)
;; auto-completion in minibuffer
(icomplete-mode +1)

(require 'smex)
;; Use Smex for recent M-x commands a la ido.
(smex-initialize)

;; flx is a fuzzy finder that does with ido
(require 'flx-ido)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

(set-default 'imenu-auto-rescan t)

;; group ibuffer list by version controled directory by default
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))
;; bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" savefile-dir)
      bookmark-save-flag 1)

;; load yasnippet
;; (require 'yasnippet)
;; (add-to-list 'yas-snippet-dirs snippets-dir)
;; (yas-global-mode 1)
;; (diminish 'yas-minor-mode)


;; projectile is a project management mode
(require 'projectile)
(setq projectile-cache-file (expand-file-name  "projectile.cache" savefile-dir))
(projectile-global-mode t)
(diminish 'projectile-mode )

(require 'helm-misc)
(require 'helm-projectile)

(defun helm-prelude ()
  "Preconfigured `helm'."
  (interactive)
  (condition-case nil
      (if (projectile-project-root)
          ;; add project files and buffers when in project
          (helm-other-buffer '(helm-c-source-projectile-files-list
                               helm-c-source-projectile-buffers-list
                               helm-c-source-buffers-list
                               helm-c-source-recentf
                               helm-c-source-buffer-not-found)
                             "*helm prelude*")
        ;; otherwise fallback to helm-mini
        (helm-mini))
    ;; fall back to helm mini if an error occurs (usually in projectile-project-root)
    (error (helm-mini))))

;; A global minor mode that enables selection of windows according to numbers
;; with the C-x C-j prefix.  Another mode, window-number-meta-mode enables the
;; use of the M- prefix.
(require 'window-number)
(window-number-mode 1)
(window-number-meta-mode 1)

(setq abbrev-file-name abbrev-file)
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

(setq synonyms-file thesaurus-file)
;; (setq synonyms-cache-file  <name & location of your cache file>)
(require 'synonyms)

(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(require 'anzu)
(global-anzu-mode +1)
(diminish 'anzu-mode)

;; ;; note taking software like notational velocity
(require 'deft)
(setq deft-extension "txt")
(setq deft-directory "~/Google Drive/Documents/Notes")
(setq deft-text-mode 'org-mode)
(setq deft-use-filename-as-title t)

;; vkill is a process manager
(require 'vkill)

;; artbollocks-mode.el is an emacs minor mode for avoiding cliche and bad
;; grammar when writing about art.
(require 'artbollocks-mode)

(setq artbollocks-weasel-words-regex
      (concat "\\b" (regexp-opt
                     '("one of the"
                       "should"
                       "just"
                       "sort of"
                       "a lot"
                       "probably"
                       "maybe"
                       "perhaps"
                       "I think"
                       "really"
                       "pretty"
                       "nice"
                       "action"
                       "utilize"
                       "leverage") t) "\\b"))


(provide 'wolanski-env)
