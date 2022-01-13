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

(setq shell-file-name "/bin/zsh") ;; this will be different for linux and mac machines
;;(setq shell-file-name "/bin/bash") ;; this will be different for linux and mac machines
(use-package vterm
  :ensure t)
(setq vterm-kill-buffer-on-exit t)
(general-define-key
 :states '(normal)
 :keymaps 'override
 :prefix "SPC"
 "v" '(vterm-other-window :which-key "open vterm in other window"))
(add-hook 'vterm-mode-hook
          #'(lambda ()
             (local-set-key (kbd "C-c q") #'kill-buffer-and-window)))

(use-package magit :ensure t)
(use-package git-gutter :ensure t :config (global-git-gutter-mode +1))

(use-package ztree :ensure t)
(general-define-key
 :states '(normal)
 :keymaps 'override
 :prefix "SPC"
 "z" '(ztree-dir :which-key "open ztree directory view"))
(setq ztree-dir-move-focus t)
(add-hook 'ztree-mode-hook
          #'(lambda ()
             (local-set-key (kbd "C-c q") #'kill-buffer-and-window)))

(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))

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

(use-package lsp-mode
  :ensure t
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (go-mode . lsp-deferred)
  (js-mode . lsp-deferred)
  ;;(C\*l-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  :config
  (define-key lsp-mode-map (kbd "C-l C-l") lsp-command-map))

(use-package lsp-ui :ensure t)

(use-package company
  :ensure t
  :bind ("C-SPC" . company-mode)
  :config (setq lsp-completion-provider :capf))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "Tab") nil)
  (define-key company-active-map (kbd "<tab>") nil))

(setq company-minimum-prefix-length 1
      company-idle-delay 0.0) ;; default is 0.2
(setq company-selection-wrap-around t)
(setq lsp-ui-doc-show-with-cursor nil)

(use-package yasnippet :ensure t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snips"))
  ;; preventing weird indenting 
  (setq yas-indent-line 'fixed)
  (yas-global-mode 1))

(use-package projectile :ensure t)
(projectile-mode +1)
;; Recommended keymap prefix on Windows/Linux
(general-define-key
 :states '(normal)
 :prefix "SPC"
 "p" '(projectile-command-map :which-key "projectile command map")
 "p f" '(projectile-find-file :which-key "projectile find file"))

(use-package helm-lsp :ensure t)
(use-package helm :ensure t
  :config (helm-mode)(require 'helm-config))
(use-package helm-projectile :ensure t :config (helm-projectile-on))

(use-package haskell-mode :ensure t)
(use-package lsp-haskell :ensure t)
(require 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

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
(general-define-key
 :states '(visual)
 :keymaps 'lua-mode-map
 :prefix "SPC"
 "r" '(lua-send-region :which-key "send region"))

(use-package go-mode :ensure t)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package rustic :ensure t)
(setq rustic-lsp-server 'rust-analyzer)
(setq rustic-lsp-client 'lsp-mode)

(use-package nodejs-repl :ensure t)
  (add-hook 'js-mode-hook
          (lambda ()
            (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
            (define-key js-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
            (define-key js-mode-map (kbd "C-c C-c") 'nodejs-repl-send-buffer)
            (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
            (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)))
(general-define-key
 :states '(visual)
 :keymaps 'js-mode-map
 :prefix "SPC"
 "r" '(nodejs-repl-send-region :which-key "send region"))

(use-package svelte-mode
  :ensure t
  :config (add-hook 'svelte-mode-hook #'lsp-deferred))
(use-package web-mode :ensure t)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(use-package lsp-java :ensure t :config (add-hook 'java-mode-hook #'lsp-deferred))
(use-package dap-mode :ensure t :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

(use-package geiser-guile :ensure t)

(use-package racket-mode :ensure t) 
(general-define-key
 :states '(normal)
 :keymaps 'racket-mode-map
 :prefix "SPC"
 "\\" '(racket-insert-lambda :which-key "insert lambda"))
(general-define-key
 :states '(visual)
 :keymaps 'racket-mode-map
 :prefix "SPC"
 "r" '(racket-send-region :which-key "insert lambda"))

(use-package flycheck-clj-kondo :ensure t)
(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))
(use-package cider :ensure t)
(general-define-key
 :states '(visual)
 :keymaps 'cider-mode-map
 :prefix "SPC"
 "r" '(cider-eval-region :which-key "send region"))
;;(setq cider-lein-parameters "repl :headless :host localhost")

(use-package smartparens :ensure t)
(require 'smartparens-config)
(sp-pair "\<" nil :actions :rem)
(add-hook 'racket-mode-hook #'smartparens-mode)
(add-hook 'clojure-mode-hook #'smartparens-mode)
(add-hook 'scheme-mode-hook #'smartparens-mode)
(add-hook 'rustic-mode-hook #'smartparens-mode)
(add-hook 'go-mode-hook #'smartparens-mode)
(add-hook 'lua-mode-hook #'smartparens-mode)
(add-hook 'lisp-interaction-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(general-define-key
   :states '(normal)
   :keymaps 'smartparens-mode-map
   :prefix "SPC"
   ">" '(sp-up-sexp :which-key "up sexp")
   "<" '(sp-down-sexp :which-key "down sexp")
   "{" '(sp-backward-barf-sexp :whick-key "Barf backward")
   "}" '(sp-forward-barf-sexp :which-key "Barf forward")
   "(" '(sp-backward-slurp-sexp :whick-key "Slurp backward")
   ")" '(sp-forward-slurp-sexp :which-key "Slurp forward")
   "^" '(sp-join-sexp :which-key "join sexp")
   "+" '(sp-absorb-sexp :which-key "join sexp")
   "|" '(sp-split-sexp :which-key "split sexp"))

(general-define-key
 :states '(normal)
 :keymaps 'geiser-mode-map
 :prefix "SPC"
 "l f" '(geiser-load-file :which-key "load file")
 "\\" '(geiser-insert-lambda :which-key "insert lambda"))
(general-define-key
 :states '(visual)
 :keymaps 'geiser-mode-map
 :prefix "SPC"
 "r" '(geiser-eval-region :which-key "insert lambda"))

(use-package markdown-mode :ensure t :config (add-hook 'markdown-mode-hook 'flyspell-mode))

(use-package ess :ensure t)
(require 'ess-site)
(setq ess-use-flymake nil)

(add-hook 'ess-r-mode-hook
	  #'(lambda ()
	     (local-set-key (kbd "C-c C-r d") #'ess-rdired)))

(add-hook 'ess-rdired-mode-hook
	  #'(lambda ()
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

(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "cheat-sh.el")

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq make-backup-files nil) 
(setq auto-save-default nil)

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)
;;(setq display-line-numbers-type 'relative)

;;basic theme that is on every installation
;;(load-theme 'wombat t)

;;(use-package gruvbox-theme :ensure t)
;;(load-theme 'gruvbox-dark-soft t)

(use-package dracula-theme :ensure t)
(load-theme 'dracula t)

(use-package smart-mode-line :ensure t)
(setq sml/theme 'respectful)
(setq sml/no-confirm-load-theme t)
(setq sml/shorten-modes t)
(sml/setup)

(setq tab-line-new-button-show nil)  
(setq tab-line-close-button-show nil)

(use-package all-the-icons :ensure t)
(add-to-list 'default-frame-alist '(font . "SauceCodePro Nerd Font 14"))

(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-directory "~/org/"
      org-hide-emphasis-markers t
      org-bullets-bullet-list '("●" "○" "◆" "◇"))
(setq org-src-preserve-indentation nil)
(use-package htmlize :ensure t) ;; allows for syntax highlighting on exports

(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
    org-edit-src-content-indentation 0)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((scheme . t)
   (lua . t)
   (R . t)
   (haskell . t)
   ))

(require 'ob-js)
(add-to-list 'org-babel-load-languages '(js . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))

(nvmap :states 'normal :keymaps 'override :prefix "SPC"
  "c c"   '(compile :which-key "Compile")
  "c C"   '(recompile :which-key "Recompile")
  "h r r" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key "Reload emacs config")
  "t t"   '(toggle-truncate-lines :which-key "Toggle truncate lines")
  "t l"   '(tab-line-mode :which-key "tab line mode")
  ;; buffers
  "b"     '(switch-to-buffer :which-key "switch to buffer")
  ;; File manipulation
  "."     '(find-file :which-key "Find file")
  "f s"   '(save-buffer :which-key "Save file")
  "f C"   '(copy-file :which-key "Copy file")
  "f D"   '(delete-file :which-key "Delete file")
  "f R"   '(rename-file :which-key "Rename file")
  "k k"   '(kill-buffer-and-window :which-key "kill and klose")
  ;; cht sheet
  "c h"   '(cheat-sh :which-key "open cheat sheet lookup"))

;; better yanking 
(define-key evil-normal-state-map
            (kbd "Y")
            (kbd "y$"))

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(setq warning-minimum-level 'error)
