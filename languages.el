;; ---------------------
;; ----- Languages -----
;; ---------------------

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

(provide 'languages)
;;; languages.el ends here
