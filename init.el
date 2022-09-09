;; ----- Config / Packages 
(load-file (expand-file-name "config.el" user-emacs-directory))
;; ----- Languages
(load-file (expand-file-name "languages.el" user-emacs-directory))
;; ----- Keybindings
(load-file (expand-file-name "keybindings.el" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b4e786d88aeb48bce6c3b93a72d50e9c98966b759b2b09d837ea93e25acb8cc2" "0ed28b0694dd2c7a2407598e63650a8562b9e833a1a136ee74790a74d3776d3b" default))
 '(package-selected-packages
   '(gruvbox-theme rainbow-delimiters smartparens ob-rust flycheck el evil undo-fu which-key vterm use-package general exec-path-from-shell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
