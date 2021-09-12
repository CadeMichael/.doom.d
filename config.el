(require 'package)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package exec-path-from-shell :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; package used for undoing
(use-package undo-fu :ensure t)
(use-package evil
    :ensure t ;; install evil if not installed
    :init     ;; tweak evil's configuration before loading
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    (setq evil-undo-system 'undo-fu)
    (evil-mode))
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package general
   :ensure t
   :config
   (general-evil-setup t))

(use-package org-bullets
    :ensure t
	:init
	(add-hook 'org-mode-hook (lambda ()
			    (org-bullets-mode 1))))

(use-package org-tempo
       :ensure nil)

(use-package which-key
:ensure t
:init
        (setq which-key-side-window-location 'bottom
                which-key-sort-order #'which-key-key-order-alpha
                which-key-sort-uppercase-first nil
                which-key-add-column-padding 1
                which-key-max-display-columns nil
                which-key-min-display-lines 6
                which-key-side-window-slot -10
                which-key-side-window-max-height 0.25
                which-key-idle-delay 0.8
                which-key-max-description-length 25
                which-key-allow-imprecise-window-fit t
                which-key-separator " --> " ))
(which-key-mode)

(use-package vertico
 :ensure t
 :init
 (vertico-mode)

  ; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
)

(use-package vterm
    :ensure t)
(setq shell-file-name "/bin/zsh" ;; this will be different for linux and mac machines
    vterm-max-scrollback 5000)

(use-package magit :ensure t)
(use-package git-gutter :ensure t :config (global-git-gutter-mode +1))

(use-package ztree :ensure t)
(general-define-key
 :states '(normal)
 :keymaps 'override
 :prefix "SPC"
 "z" '(ztree-dir :which-key "open ztree directory view"))
(setq ztree-dir-move-focus t)

(general-define-key
 :states '(normal)
 :keymaps 'ztree-mode-map
 "H" 'ztree-dir-widen-to-parent
 "L" 'ztree-dir-narrow-to-dir
 "o" 'ztree-perform-action)
(define-key ztree-mode-map (kbd "C-h") 'ztree-dir-toggle-show-filtered-files)
(define-key ztree-mode-map (kbd "C-z d") 'ztree-dir-open-dired-at-point)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package lsp-haskell :ensure t)
(use-package lsp-mode
  :ensure t
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (go-mode . lsp-deferred)
  (js-mode . lsp-deferred)
  (haskell-mode . lsp-deferred)
  (lua-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :config
  (define-key lsp-mode-map (kbd "C-l C-l") lsp-command-map))

(use-package lsp-ui :ensure t)

(use-package company
  :ensure t
  :bind ("C-SPC" . company-mode)
  :config (setq lsp-completion-provider :capf))
(use-package yasnippet :ensure t)
(add-hook 'prog-mode-hook
        '(lambda ()
           (yas-minor-mode)))
;;(add-hook 'after-init-hook 'global-company-mode) not a fan of it in every mode

(setq company-minimum-prefix-length 1
      company-idle-delay 0.0) ;; default is 0.2
(setq company-selection-wrap-around t)

(use-package projectile :ensure t :bind
  (("C-c p f" . projectile-find-file)))
(projectile-mode +1)
;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package helm-lsp :ensure t)
(use-package helm :ensure t
  :config (helm-mode)(require 'helm-config))

(use-package haskell-mode :ensure t)

(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t))

(general-define-key
 :states '(normal)
 :keymaps 'haskell-mode-map
 :prefix "SPC"
 "c l" '(haskell-process-load-or-reload :which-key "load current file")
 "'" '(haskell-interactive-bring :which-key "interactive bring")
 "c t" '(haskell-process-do-type :which-key "process do type")
 "c i" '(haskell-process-do-info :which-key "process do info")
 "c SPC c" '(haskell-process-cabal-build :which-key "cabal build")
 "c k" '(haskell-interactive-mode-clear :which-key "interactive mode clear")
 "c c" '(haskell-process-cabal :which-key "process cabal"))
;; managing imports
(define-key haskell-mode-map (kbd "<f8>") 'haskell-navigate-imports)

(use-package lua-mode :ensure t)

(use-package go-mode :ensure t)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

(use-package geiser-guile :ensure t)

(use-package markdown-mode :ensure t :config (add-hook 'markdown-mode-hook 'flyspell-mode))

(use-package ess :ensure t)
(require 'ess-site)
(setq ess-use-flymake nil)

(add-hook 'ess-r-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-c C-r d") #'ess-rdired)))

(add-hook 'ess-rdired-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "C-c C-r d") #'kill-buffer-and-window)))
;; so I don't have to remap the standard bindings
(evil-set-initial-state 'ess-rdired-mode 'emacs)

(use-package poly-markdown :ensure t)
(use-package poly-R :ensure t)
(require 'poly-markdown)
(require 'poly-R)

;; MARKDOWN

(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;; R modes

(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq make-backup-files nil) 
(setq auto-save-default nil)

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)
(setq display-line-numbers-type 'relative)

;;basic theme that is on every installation
(load-theme 'wombat t)

;;(use-package gruvbox-theme :ensure t)
;;(load-theme 'gruvbox-dark-soft t)

;; (use-package nord-theme :ensure t)
;; (load-theme 'nord t)

;; (use-package dracula-theme :ensure t)
;; (load-theme 'dracula t)

(use-package all-the-icons :ensure t)
(set-face-attribute 'default nil :font "Fira Code 14")

(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-directory "~/org/"
      org-hide-emphasis-markers t
      org-bullets-bullet-list '("●" "○" "◆" "◇"))
(setq org-src-preserve-indentation nil
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)

(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
    org-edit-src-content-indentation 0)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((scheme . t)
   (lua . t)
   (R . t)
   ))

(require 'ob-js)
(add-to-list 'org-babel-load-languages '(js . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))

(nvmap :states 'normal :keymaps 'override :prefix "SPC"
  "SPC"   '(counsel-M-x :which-key "M-x")
  "c c"   '(compile :which-key "Compile")
  "c C"   '(recompile :which-key "Recompile")
  "h r r" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key "Reload emacs config")
  "t t"   '(toggle-truncate-lines :which-key "Toggle truncate lines")
  ;; File manipulation
  "."     '(find-file :which-key "Find file")
  "f s"   '(save-buffer :which-key "Save file")
  "f C"   '(copy-file :which-key "Copy file")
  "f D"   '(delete-file :which-key "Delete file")
  "f R"   '(rename-file :which-key "Rename file")
  ;; Vterm
  "v v"   '(vterm-other-window :which-key "Open Vterm"))

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
