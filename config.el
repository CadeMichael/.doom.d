(require 'package) 
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/")) 
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
;; using http instead might stop hangs or having the following
;; (custom-set-variables
;;  '(gnutls-algorithm-priority "normal:-vers-tls1.3"))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
	"straight/repos/straight.el/bootstrap.el"
	user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil) ;; prevents loading before init.el

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package exec-path-from-shell :ensure t)
(exec-path-from-shell-initialize)

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

(require 'org-tempo)
;; src block short cuts
(add-to-list
 'org-structure-template-alist '("el" . "src elisp"))

(use-package org-preview-html
  :ensure t
  :config
  (setq org-preview-html-viewer 'xwidget))

(use-package org-roam
  :ensure t
  :config
  (require 'org-roam-utils)
  :bind (:map org-mode-map
              ("C-M-i" . completion-at-point)))
;; directory where notes are stored
(setq org-roam-directory (file-truename "~/org"))
(org-roam-db-autosync-mode)

;; kemappings
(general-define-key
 :states '(normal)
 :keymaps 'org-mode-map
 :prefix "SPC"
 "r i" '(org-roam-node-insert :which-key "node insert")
 "r b" '(org-roam-buffer-toggle :which-key "buffer toggle")
 "P" '(org-preview-html-mode :which-key "Org Preview HTML"))

(setq org-log-done t)
(setq org-agenda-files '("~/.schedule"))

;; show key binding function
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

(setq shell-file-name "/bin/zsh") ;; this will be different for linux and mac machines
(use-package vterm
  :ensure t)
(setq vterm-kill-buffer-on-exit t)
(general-define-key
 :states '(normal)
 :keymaps 'override
 :prefix "SPC"
 "v" '(vterm-other-window :which-key "open vterm in other window"))
;; keybind for closing vterm
(add-hook 'vterm-mode-hook
          #'(lambda ()
              (local-set-key (kbd "C-c q") #'kill-buffer-and-window)))

;; Git Integration
(use-package magit
  :ensure t)
;; highlight line where there are changes
(use-package git-gutter :ensure t :config (global-git-gutter-mode +1))

(use-package treemacs
  :ensure t
  :config
  (setq treemacs-is-never-other-window t)
  (general-define-key
   :states '(normal)
   :keymaps 'override
   :prefix "SPC"
   "N" '(treemacs :which-key "treemacs")
   "n" '(treemacs-select-window :which-key "treemacs")))

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

;; hide and show dot files
(use-package dired-hide-dotfiles
  :ensure t
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H"  'dired-hide-dotfiles-mode))

;; to prevent persistent dired buffers getting buried
;; dired-single kills old buffers
(use-package dired-single :ensure t)

(use-package dired 
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom
  ((when
       (string= system-type "gnu/linux") ;mac os ls sucks
     (dired-listing-switches "-agho --group-directories-first")))
  :config
  (evil-collection-define-key
    'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

;; if two dired buffers are open and you go to copy,
;; location will default to other dired buffer
(setq dired-dwim-target t)
;; might need (require 'dired-x)

;; file icons
(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
;; ensure that standard error handling ports to flycheck
(setq flycheck-standard-error-navigation t)

;; more IDE like features with LSP
(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :commands lsp-ui-doc-hide
  :init
  (setq lsp-ui-doc-enable t)
  :config
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
  (general-nmap
    :keymap 'lsp-ui-mode-map
    "gD" '(lsp-ui-peek-find-definitions :which-key "peek definitions")
    "gr" '(lsp-ui-peek-find-references :which-key "peek references")
    "TAB" '(lsp-ui-doc-focus-frame :which-key "lsp ui doc focus")
    "K" '(lsp-ui-doc-show :which-key "lsp ui doc show")))


;; lsp mode
(use-package lsp-mode
  :ensure t
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  ;; golang
  (go-mode . lsp-deferred)
  ;; javascript
  (js-mode . lsp-deferred)
  ;; svelte 
  (web-mode . lsp-deferred)
  ;; haskell
  (haskell-mode .lsp-deferred)
  :commands (lsp lsp-deferred)
  :bind-keymap ("C-SPC" . lsp-command-map)
  :bind (:map lsp-mode-map
	      ("C-SPC c" . helm-lsp-code-actions)))

;; blurry icons on mac
(when (string= "darwin" system-type)
  (setq lsp-headerline-breadcrumb-icons-enable nil))

;; dap mode - helpful with dart
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package company
  :ensure t
  :bind ("C-<tab>" . company-mode)
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

(use-package projectile
  :ensure t
  :custom ((projectile-completion-system 'helm))
  :config (projectile-mode))
;; Recommended keymap prefix on Windows/Linux
(general-define-key
 :states '(normal)
 :prefix "SPC"
 "p" '(projectile-command-map :which-key "projectile command map")
 "p f" '(projectile-find-file :which-key "projectile find file"))

(use-package rainbow-delimiters :ensure t)
(add-hook 'org-mode-hook #'rainbow-delimiters-mode)
(add-hook 'racket-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'web-mode-hook #'rainbow-delimiters-mode)
(add-hook 'go-mode-hook #'rainbow-delimiters-mode)
(use-package aggressive-indent :ensure t)
(add-hook 'racket-mode-hook #'aggressive-indent-mode)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'web-mode-hook #'aggressive-indent-mode)

(use-package helm-lsp :ensure t)
(use-package helm
  ;; recommended to use straight
  :straight t 				 
  :bind
  (("M-x" . helm-M-x))
  (("C-x C-f" . helm-find-files))
  :config
  (helm-mode 1)
  (require 'helm-config))

(use-package lsp-dart
  :ensure t
  :hook
  (dart-mode . lsp-deferred)
  (dart-mode . hs-minor-mode))
;; hover for running apps
(use-package hover
  :ensure t
  :after dart-mode
  :init (hover-minor-mode 1))
(setq hover-hot-reload-on-save t)

;; Assuming usage with dart-mode
(use-package dart-mode
  :custom
  (dart-sdk-path (concat (getenv "HOME") "/flutter/bin/cache/dark-sdk/")
   dart-format-on-save t))
;; keybindings, using d as primary key
(general-def
  :states 'normal
  :keymaps 'dart-mode-map
  :prefix "SPC"
  "d o" '(lsp-dart-show-flutter-outline :which-key "show flutter outline")
  "d r" '(lsp-dart-run :which-key "dart run")
  "d h r" '(lsp-dart-dap-flutter-hot-reload :which-key "hot reload")
  "d h R" '(lsp-dart-dap-flutter-hot-restart :which-key "hot restart")
  "d h h" '(hover-run-or-hot-reload :which-key "hover run or hot reload")
  "d p" '(lsp-dart-pub-get :which-key "dart pub get"))

(use-package haskell-mode :ensure t)
(use-package lsp-haskell :ensure t)
(require 'lsp-haskell)

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
 "c t" '(haskell-process-do-type :which-key "process do type")
 "c i" '(haskell-process-do-info :which-key "process do info")
 "c SPC c" '(haskell-process-cabal-build :which-key "cabal build")
 "c k" '(haskell-interactive-mode-clear :which-key "interactive mode clear")
 "c c" '(haskell-process-cabal :which-key "process cabal"))
;; managing imports
(define-key haskell-mode-map (kbd "<f8>") 'haskell-navigate-imports)

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

(use-package web-mode :ensure t)
(setq web-mode-enable-auto-pairing t)
;; html support 
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; css support
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
;; svelte support
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
(setq web-mode-engines-alist
      '(("svelte" . "\\.svelte\\'")))
(eval-after-load "web-mode"
  '(setq web-mode-enable-auto-expanding t))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))
;;; support for hy
(use-package hy-mode
  :ensure t)
(use-package ob-hy
  :ensure t)

(use-package geiser-guile :ensure t)

(use-package racket-mode :ensure t) 
;; org mode src block support
(use-package ob-racket
  :after org
  :config
  (add-hook 'ob-racket-pre-runtime-library-load-hook
	      #'ob-racket-raco-make-runtime-library)
  :straight (ob-racket
	       :type git :host github :repo "hasu/emacs-ob-racket"
	       :files ("*.el" "*.rkt")))

(general-define-key
 :states '(normal)
 :keymaps 'racket-mode-map
 :prefix "SPC"
 "\\" '(racket-insert-lambda :which-key "insert lambda"))

(general-define-key
 :states '(visual)
 :keymaps 'racket-mode-map
 :prefix "SPC"
 "r" '(racket-send-region :which-key "send region"))

(use-package slime :ensure t)
(setq inferior-lisp-program "sbcl")

;; error handling / linting
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

(use-package smartparens :ensure t)
(require 'smartparens-config)
(sp-pair "\<" nil :actions :rem) ;don't use with < from html 
(add-hook 'clojure-mode-hook #'smartparens-mode)
(add-hook 'css-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'lisp-interaction-mode-hook #'smartparens-mode)
(add-hook 'lua-mode-hook #'smartparens-mode)
(add-hook 'go-mode-hook #'smartparens-mode)
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'racket-mode-hook #'smartparens-mode)
(add-hook 'rustic-mode-hook #'smartparens-mode)
(add-hook 'scheme-mode-hook #'smartparens-mode)
(add-hook 'svelte-mode-hook #'smartparens-mode)
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
   "+" '(sp-absorb-sexp :which-key "absorb sexp")
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
 "r" '(geiser-eval-region :which-key "eval region"))

(use-package markdown-mode :ensure t
  :config
  (add-hook 'markdown-mode-hook 'flyspell-mode)) ;make sure spelling is alright

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

;; R mode
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

(use-package cheat-sh :ensure t)

;;(menu-bar-mode 0)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq make-backup-files nil) 
(setq auto-save-default nil)

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)
(add-hook 'org-mode-hook
          (lambda () (display-line-numbers-mode -1)))
(add-hook 'vterm-mode-hook
          (lambda () (display-line-numbers-mode -1)))
(setq display-line-numbers-type 'relative)

(use-package dracula-theme :ensure t)
(load-theme 'dracula t)

(use-package smart-mode-line :ensure t)
(setq sml/theme 'respectful)
(setq sml/no-confirm-load-theme t)
(setq sml/shorten-modes t)
(sml/setup)

(use-package centaur-tabs
  :ensure t
  :config
  (setq
   centaur-tabs-set-icons t
   centaur-tabs-gray-out-icons 'buffer
   centaur-tabs-set-modified-marker t
   centaur-tabs-modified-marker ""
   centaur-tabs-height 32))
;; font specific config
(if (string= "gnu/linux" system-type)
    (centaur-tabs-change-fonts "Monoid Nerd Font" 160)
  (centaur-tabs-change-fonts "Fira Code Nerd Font" 160))

(use-package tree-sitter :ensure t)
(use-package tree-sitter-langs :ensure t)
(require 'tree-sitter)
(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package all-the-icons :ensure t)
(when (string= system-type "gnu/linux")
  (add-to-list 'default-frame-alist '(font . "Monoid Nerd Font 18")))
(when (string= system-type "darwin")       
  (add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font 14")))

(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-startup-banner 3)
  (setq dashboard-set-init-info nil)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items
           '((projects . 10)
            (recents . 5))))
;; startup the dashboard 
(dashboard-setup-startup-hook)

(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-directory "~/org/"
        org-hide-emphasis-markers t
        org-bullets-bullet-list '("●" "○" "◆" "◇"))
(setq org-src-preserve-indentation nil)
;; allows for syntax highlighting on exports
(use-package htmlize :ensure t)

(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
    org-edit-src-content-indentation 0)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((scheme . t)
   (lua . t)
   (R . t)
   (hy . t)
   (lisp . t)
   (js . t)
   (racket . t)
   (python . t)
   (haskell . t)))
;; basic conf
(push '("conf-unix" . conf-unix) org-src-lang-modes)

(require 'color)
;; src blocks
(set-face-attribute 'org-block nil :foreground nil
                    :inherit '(fixed-pitch))
;; code
(set-face-attribute 'org-code nil
                    :inherit '(shadow fixed-pitch))
;; darken blocks
(set-face-attribute 'org-block nil :background
                    (color-darken-name
                     (face-attribute 'default :background) 3))
;; block lines
(set-face-attribute 'org-block-begin-line nil :background
                    (color-darken-name
                     (face-attribute 'default :background) 3))

(require 'ob-js)
(add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))

(general-def :states 'normal :keymaps 'override :prefix "SPC"
  "a"     '(org-agenda :which-key "org-agenda")
  "c c"   '(compile :which-key "Compile")
  "c C"   '(recompile :which-key "Recompile")
  "h r r" '((lambda ()
              (interactive)
              (load-file "~/.emacs.d/init.el"))
            :which-key "Reload emacs config")
  "t t"   '(toggle-truncate-lines :which-key "Toggle truncate lines")
  "t l"   '(centaur-tabs-mode :which-key "tab line mode")
  "<left>"   '(centaur-tabs-backward :which-key "tab backward")
  "<right>"  '(centaur-tabs-forward :which-key "tab forward")
  ;; buffers
  "b"     '(switch-to-buffer :which-key "switch to buffer")
  ;; File manipulation
  "."     '(helm-find-files :which-key "Find file")
  "f s"   '(save-buffer :which-key "Save file")
  "f C"   '(copy-file :which-key "Copy file")
  "f D"   '(delete-file :which-key "Delete file")
  "r f"   '(org-roam-node-find :which-key "find node")
  "f R"   '(rename-file :which-key "Rename file")
  "k b"   '(kill-buffer-and-window :which-key "kill and close current")
  "k s"   '(kill-some-buffers :which-key "kill some buffers")
  ;; cht sheet
  "c h"   '(cheat-sh :which-key "open cheat sheet lookup"))

;; better yanking 
(general-nmap "Y" (kbd "y$"))

;; better searching
;; centers each result
(general-nmap "n" 'search-next-center-evil)
(defun search-next-center-evil () 
  (interactive)
    (evil-search-next)
    (evil-scroll-line-to-center
      (line-number-at-pos (point))))

(general-nmap "N" 'search-prev-center-evil)
(defun search-prev-center-evil () 
  (interactive)
    (evil-search-previous)
    (evil-scroll-line-to-center
      (line-number-at-pos (point))))

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(setq warning-minimum-level 'error)
