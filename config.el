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

;; ----- LSP -----
;; ----- lsp-mode
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((rust-mode . lsp)
         (js-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))		; global enable
;; use flycheck by default
(setq flycheck-standard-error-navigation t)

;; Lsp UI
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(setq lsp-ui-sideline-show-diagnostics t)

;; Helm
(use-package helm
  :ensure t)
;; remap chords to helm
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)				; global enable
(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

;; ----- Company
(use-package company
  :ensure t)
(add-hook 'after-init-hook 'global-company-mode)
;; ---------------

;; ----- Go
(use-package go-mode
  :ensure t)
;; lang specific hooks
(add-hook 'go-mode-hook #'tree-sitter-hl-mode)
(add-hook 'go-mode-hook #'smartparens-mode)

;; ----- JS
;; lang specific hooks
(add-hook 'js-mode-hook #'tree-sitter-hl-mode)
(add-hook 'js-mode-hook #'smartparens-mode)
;; run org blocks
(require 'ob-js)

;; ----- Rust
(use-package rust-mode
  :ensure t)
;; org src support
(use-package ob-rust
  :ensure t)

;; lang specific hooks
(add-hook 'rust-mode-hook #'tree-sitter-hl-mode)
(add-hook 'rust-mode-hook #'smartparens-mode)

;; ----- Zig
(use-package zig-mode
  :ensure t)

;; ----- Org Config
;; better indentation
(add-hook 'org-mode-hook
          'org-indent-mode)
;; load babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)
   (rust . t)))

;; ---------------------
;; ----- Functions -----
;; ---------------------

(defun custom-js-format-buffer ()
  "Formats a js buffer using the deno formatter."
  (interactive)
  (let ((xfpath (buffer-file-name)))
    (if xfpath
        (progn
          (save-buffer)
          (shell-command
           (format "deno fmt %s"
                   (shell-quote-argument xfpath)))
          (revert-buffer t t t))
      (user-error "Current buffer must be a file"))))

;; ----- All Modes 
(general-define-key
 :states 'normal
 :prefix "SPC"
 ;; buffer management
 "b s" '(switch-to-buffer :which-key "switch to buffer")
 "b k" '(kill-buffer-and-window :which-key "kill buffer and window")
 "b K" '(kill-some-buffers :which-key "kill some buffers")
 ;; compiling
 "c c" '(compile :which-key "compile")
 ;; Helm 
 "." '(helm-find-files :which-key "helm-find-files")
 ;; Neotree
 "n" '(neotree-toggle :which-key "neotree toggle")
 ;; Org Roam
 "r f" '(org-roam-node-find :which-key "org roam node find"))

;; ----- Evil Bindings
;; Yanking to end of line
(general-define-key
 :states 'normal
 "Y" (kbd "y$"))

;; ----- Center Searching 
;; search next
(defun search-next-center-evil ()
  (interactive)
    (evil-search-next)
    (evil-scroll-line-to-center
      (line-number-at-pos (point))))
;; search previous 
(defun search-prev-center-evil () 
  (interactive)
    (evil-search-previous)
    (evil-scroll-line-to-center
      (line-number-at-pos (point))))
;; remap
(general-define-key
 :states 'normal
 "n" 'search-next-center-evil)
(general-define-key
 :states 'normal
 "N" 'search-prev-center-evil)
;; ---------------------- 

;; ----- LSP
;; nvim like bindings
(general-define-key
 :states 'normal
 :keymaps 'lsp-ui-mode-map
 ;; hover
 "K" '(lsp-ui-doc-show :which-key "lsp ui doc show")
 ;; finding def / references
 "gd" '(lsp-ui-peek-find-definitions :which-key "lsp ui peak find definitions")
 "gr" '(lsp-ui-peek-find-references :which-key "lsp ui peak find references"))
;; formatting 
(general-define-key
 :states '(normal visual)
 :keymaps 'lsp-mode-map
 :prefix "SPC"
 "f b" '(lsp-format-buffer :which-key "lsp format buffer")
 "f r" '(lsp-format-region :which-key "lsp format region"))

;; ----- Neotree
;; evil bindings
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

;; ----- JS  
(general-define-key
 :states 'normal
 :keymaps 'js-mode-map
 :prefix "TAB"
 "f" '(custom-js-format-buffer :which-key "custom-js-format-buffer"))

;; ----- Org Roam
(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 :prefix "SPC"
 "r i" '(org-roam-node-insert :which-key "org roam node insert")
 "r b" '(org-roam-buffer-toggle :which-key "org roam buffer toggle"))

;; ----- Smartparens
;; ()
(sp-pair "(" ")" :wrap "C-(")
;; {}
(sp-pair "{" "}" :wrap "C-{")
;; []
(sp-pair "[" "]" :wrap "M-[")

;; ----- V Term
(general-define-key
 :states 'normal
 :prefix "SPC"
 "v" '(vterm-other-window :which-key "vterm-other-window"))
