;; Load Literate Config File 
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("efcecf09905ff85a7c80025551c657299a4d18c5fcfedd3b2f2b6287e4edd659" "d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "b4e786d88aeb48bce6c3b93a72d50e9c98966b759b2b09d837ea93e25acb8cc2" "0ed28b0694dd2c7a2407598e63650a8562b9e833a1a136ee74790a74d3776d3b" default))
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(enh-ruby enh-ruby-mode php-mode ob-julia-vterm julia-vterm lua-mode ob-clojure parinfer-rust-mode parinifer-rust-mode cider clojure-mode julia-mode typescript-mode svelte-mode yasnippet web-mode poly-mode emmet-mode dracula-theme doom-modeline projectile magit git-gutter monokai-theme monokai evil-collection gruvbox-theme rainbow-delimiters smartparens ob-rust flycheck el evil undo-fu which-key vterm use-package general exec-path-from-shell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
