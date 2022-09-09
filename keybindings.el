;; -----------------------
;; ----- Keybindings -----
;; -----------------------

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
;; ** 
(sp-pair "*" "*" :wrap "M-*")

;; ----- V Term
(general-define-key
 :states 'normal
 :prefix "SPC"
 "v" '(vterm-other-window :which-key "vterm-other-window"))

(provide 'keybindings)
;;; keybindings.el ends here
