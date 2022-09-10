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
  ;; set to nil for use with evil collection
  (setq evil-want-keybinding nil)
  ;; set undo to undo-fu before loading
  (setq evil-undo-system 'undo-fu))
(evil-mode 1)
;; evil collections for use with dired
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

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
  ;; for exporting
  (require 'org-roam-export)
  :bind (:map org-mode-map
              ("C-M-i" . completion-at-point)))
;; directory where notes are stored
(setq org-roam-directory (file-truename "~/org"))
(org-roam-db-autosync-mode)
;; line wrapping
(add-hook 'org-mode-hook #'visual-line-mode)
;; org shortcuts
(require 'org-tempo)

;; custom function for resetting org links for export
(defun reset-roam-links ()
    "Reset links in org when it can't export."
    (interactive)
    (progn
      (org-roam-db-clear-all)
      (org-roam-db-sync)
      (org-id-update-id-locations)
      (org-roam-update-org-id-locations)))
  
;; ----- Rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; ----- Which Key
(use-package which-key 
  :ensure t)
(which-key-mode)

;; ----- Theme(s)
;; startup
(setq inhibit-startup-screen t)
;; solarized
(use-package solarized-theme
  :ensure t)
(load-theme 'solarized-selenized-light t)
;; monokai
(use-package monokai-theme
  :ensure t)
;; (load-theme 'monokai t)
;; vscode dark theme
(use-package vscode-dark-plus-theme 
  :ensure t)
;; (load-theme 'vscode-dark-plus t)
;; Gruv
(use-package gruvbox-theme
  :ensure t)
;; (load-theme 'gruvbox-dark-soft t)

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
