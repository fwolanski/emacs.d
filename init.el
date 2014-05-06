;;; init.el

;;; Code:

(defvar root-dir (file-name-directory load-file-name))
(defvar core-dir (expand-file-name "core" root-dir))
(defvar modules-dir (expand-file-name  "modules" root-dir))
(defvar theme-dir (expand-file-name  "themes/" root-dir))
(defvar eshell-folder (expand-file-name  "eshell" root-dir))

(defvar thesaurus-file (expand-file-name  "dict/mthesaur.txt" root-dir))
(defvar abbrev-file (expand-file-name  "dict/abbrev-defs.el" root-dir))

(defvar vendor-dir (expand-file-name "vendor" root-dir))
(defvar snippets-dir (expand-file-name "snippets" root-dir))
(defvar savefile-dir (expand-file-name "savefile" root-dir))
(defvar modules-file (expand-file-name "modules.el" root-dir))


(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

(add-to-list 'load-path core-dir)
(add-to-list 'load-path modules-dir)
(add-to-list 'load-path vendor-dir)
(add-to-list 'custom-theme-load-path theme-dir)

(require 'cask "~/.Cask/cask.el")
(cask-initialize)
;; (require 'pallet)

(require 'wolanski-functions)
(require 'wolanski-ui)
(require 'wolanski-editor)
(require 'wolanski-env)
(require 'wolanski-keybindings)
(require 'wolanski-programming)
(require 'wolanski-text)
(require 'wolanski-erc)

;; OSX specific settings
(when (eq system-type 'darwin)
  (require 'wolanski-osx))

;; this needs to go after the osx stuff to ensure the correct path is set
(require 'wolanski-terminal)

;; the modules
(require 'wolanski-c-mode)
(require 'wolanski-coffee-mode)
(require 'wolanski-js-mode)
(require 'wolanski-php-mode)
(require 'wolanski-clojure-mode)
(require 'wolanski-css-mode)
(require 'wolanski-html-mode)
(require 'wolanski-markdown-mode)
(require 'wolanski-jade-mode)
(require 'wolanski-lisp-mode)
(require 'wolanski-org-mode)
(require 'wolanski-math-mode)
(require 'wolanski-tex-mode)

;; get some speed and prevent over-eager garbage collection
(setq gc-cons-threshold 20000000)
