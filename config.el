;; -----------------------------
;; ----- Config / Packages -----
;; -----------------------------

;; ----- Native Comp Settings
(setq warning-minimum-level :error)

;; ----- Backup Files
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)

;; ----- Error & Warnings
;; disable bell
(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
;; define warning function
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

;; ----- Fonts
(add-to-list 'default-frame-alist '(font . "Hack Nerd Font 16"))

;; ----- Use Package 
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; ----- Exec Path From Shell 
(use-package exec-path-from-shell 
  :ensure t)
;; get the shell paths first
(exec-path-from-shell-initialize)

;; ----- Evil Mode
;; undo for evil mode
(use-package undo-fu 
  :ensure t) 
(use-package evil 
  :ensure t
  :init
  ;; set undo to undo-fu before loading
  (setq evil-undo-system 'undo-fu))
(evil-mode 1)

;; ----- Smart Parens
(use-package smartparens
  :ensure t)
(require 'smartparens-config)

;; ----- General
(use-package general 
  :ensure t
  :config
  (general-evil-setup t))

;; ----- NeoTree
(use-package neotree
  :ensure t)

;; ----- Org Roam
(use-package org-roam
  :ensure t
  :config
  (require 'org-roam-utils)
  :bind (:map org-mode-map
              ("C-M-i" . completion-at-point)))
;; directory where notes are stored
(setq org-roam-directory (file-truename "~/org"))
(org-roam-db-autosync-mode)
;; line wrapping
(add-hook 'org-mode-hook #'visual-line-mode)
;; org shortcuts
(require 'org-tempo)

;; ----- Rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; ----- Which Key
(use-package which-key 
  :ensure t)
(which-key-mode)

;; ----- Theme(s)
;; vscode dark theme
(use-package vscode-dark-plus-theme 
  :ensure t)
;; (load-theme 'vscode-dark-plus t)
;; Gruv
(use-package gruvbox-theme
  :ensure t)
(load-theme 'gruvbox-dark-soft t)

;; hiding toolbar
(tool-bar-mode -1)

;; syntax highlighting
(use-package htmlize
  :ensure t)

;; line numbers
(global-display-line-numbers-mode)
(add-hook 'org-mode-hook
	  (lambda ()
	    (display-line-numbers-mode -1)))

;; ----- TreeSitter
(use-package tree-sitter
  :ensure t)
(use-package tree-sitter-langs
  :ensure t)

;; ----- V Term
(setq shell-file-name "/bin/zsh")
(use-package vterm 
  :ensure t)
(setq vterm-kill-buffer-on-exit t)
(add-hook 'vterm-mode-hook
	  (lambda ()
	    (display-line-numbers-mode -1)))

(provide 'config)
;;; config.el ends here
