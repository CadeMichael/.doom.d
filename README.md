# Emacs Configuration 

- minimal elisp config for emacs, might move to org in future
- main goal of this config is to be for JS and note taking almost exclusively 
- need to install *use-package* manually for this to work
- Getting the Config into emacs
```scm
;; Load Literate Config File 
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file
    (expand-file-name "~/.emacs.d/config.org")))
```

# unused config

```scm
;; ----- Ruby
(general-define-key
 :states 'normal
 :keymaps 'enh-ruby-mode-map
 :prefix "SPC"
 "l f" '(ruby-load-file :which-key "ruby load file")
 "r s" '(rails-run-server :which-key "rails run server")
 "r c" '(rails-start-console :which-key "rails start console")
 "r r" '(rails-run-command :which-key "rails run command")
 "r t" '(rails-test-file :which-key "rails test file"))

;; ----- Elixir
;; inf 
(general-define-key
 :keymaps 'inf-elixir-mode-map
 :prefix "C-c"
 "C-z" '(previous-multiframe-window :which-key "other window"))
;; elixir
(general-define-key
 :keymaps 'elixir-mode-map
 "C-<return>" '(inf-elixir-send-line :which-key "send line"))
(general-define-key
 :keymaps 'elixir-mode-map
 :prefix "C-c"
 "C-c" '(inf-elixir-send-buffer :which-key "elixir inf send buffer")
 "C-z" '(elixir-inf-switch :which-key "elixir inf switch"))

;; ----- Go
(general-define-key
 :states 'normal
 :keymaps 'go-mode-map
 :prefix "SPC"
 "g r" '(go-run-file :which-key "go run file"))

;; ----- JS
(general-define-key
 :states 'normal
 :keymaps 'js-mode-map
 :prefix "TAB"
 "f" '(custom-js-format-buffer :which-key "custom-js-format-buffer"))
;; js
(define-key js-mode-map (kbd "C-c C-z") 'js-repl-toggle)
(define-key js-mode-map (kbd "C-c C-b") 'js-send-buffer)
(define-key js-mode-map (kbd "C-c C-r") 'js-send-region)
;; js repl
(define-key js-comint-mode-map (kbd "C-c C-z") 'js-repl-toggle)
(define-key js-comint-mode-map (kbd "C-c C-c") 'js-comint-reset-repl)
;; finding go.mod
(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))
(cl-defmethod project-root ((project (head go-module)))
  (cdr project))
(add-hook 'project-find-functions #'project-find-go-module)

;; finding bb.edn
(defun project-find-bb-edn (dir)
  (when-let ((root (locate-dominating-file dir "bb.edn")))
    (cons 'bb-edn root)))
(cl-defmethod project-root ((project (head bb-edn)))
  (cdr project))
(add-hook 'project-find-functions #'project-find-bb-edn)
;; JS formatter (better than lsp)
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
*** Go
#+begin_src elisp
;; ------------------
;; ----- Golang -----
;; ------------------

(defun go--run-cmd (cmd &optional args)
  "Use compile command to execute a go CMD with ARGS if given."
    (shell-command (concat "go" " " cmd " " args)))

(defun go-run-file ()
  "Run current file"
  (interactive)
  (go--run-cmd "run" (buffer-file-name)))

(defun go-build ()
  "Build go project"
  (interactive)
  (go--run-cmd "build"))

;; -----------------------
#+end_src
*** JS
#+begin_src elisp
;; ----- JS Repl
(defun js-repl-toggle ()
  "Toggle repl or js file."
  (interactive)
  (if
      (eq major-mode 'js-comint-mode)
      (previous-multiframe-window)
    (js-comint-start-or-switch-to-repl)))
#+end_src
*** python
#+begin_src elisp
(defun python-unittest-file ()
  (interactive)
  (let ((default-directory (project-root (project-current)))
        (fname (buffer-file-name)))
    (compilation-start
     (format "python -m unittest %s"
             (shell-quote-argument fname)))))
#+end_src
*** Ruby & Rails
#+begin_src elisp
;; ------------------------
;; ----- Ruby & Rails -----
;; ------------------------

;; Load ruby file into repl
(defun ruby-load-file ()
  "open eshell with ruby loaded in"
  (interactive)
  (let
      ((file buffer-file-name)
       (term-buf (vterm)))
    (switch-to-buffer (other-buffer term-buf))
    (switch-to-buffer-other-window term-buf)
    (with-current-buffer term-buf
      (vterm--goto-line -1)
      (vterm-send-string (format " irb -r %s" file))
      (vterm-send-return))))

(defun send-region-to-irb ()
  (interactive)
  (let (
        (str 
         (buffer-substring-no-properties
          (region-beginning)
          (region-end)))
        (term-buf (vterm-other-window)))
    (with-current-buffer term-buf
      (vterm--goto-line -1)
      (vterm-send-return)
      (vterm-send-string  str)
      (vterm-send-return))))

;; ---- Rails Functions ----
(defun is-rails-project? ()
  "Using projectile determine if file is in a rails project."
  (if (eq nil (projectile-project-root))
      (progn (user-error "not in project") nil)
    (if (eq nil
	    (executable-find (format "%sbin/rails" (projectile-project-root))))
	(progn (user-error "not rails project") nil)
      t)))

(defun rails-command-send (args)
  "Send given [ARGS] to `compilation-start' so project rails can run it."
  (let ((rails-exe
	 (format "%sbin/rails %s" (projectile-project-root) args)))
    (async-shell-command
     (format "cd %s && %s" (projectile-project-root) rails-exe)
     "*Rails Command*")
    (pop-to-buffer "*Rails Command*")
    (evil-normal-state)))

(defun rails-run-server ()
  "Start rails server with `rails-command-send' function."
  (interactive)
  (if (is-rails-project?)
      (progn 
	(message "running rails server...")
	(rails-command-send "s"))
    nil))

(defun rails-run-command ()
  "Get user input and use `compilation-start' to run it."
  (interactive)
  (if (is-rails-project?)
      (rails-command-send (read-string "Rails => "))
    nil))

(defun rails-test-file ()
  "If the current file is in a RoR project use the project rails to test it."
  (interactive)
  (if (is-rails-project?)
      (let ((rails-exe
	     (format "%sbin/rails" (projectile-project-root)))
	    (b-name (format "===TEST[%s]===" (buffer-name)))
	    (test-file (buffer-file-name)))
	(with-output-to-temp-buffer b-name
	  (shell-command
	   (format "%s test %s" rails-exe test-file) b-name))
	(pop-to-buffer b-name))
    nil))

(defun rails-start-console ()
  "Start rails console using a new vterm buffer named accordingly."
  (interactive)
  (if (is-rails-project?)
      (let ((rails-exe
	     (format "%sbin/rails c" (projectile-project-root)))
	    (term-buf
	     (vterm (concat "*" "rails console" "*"))))
	(message "starting rails console...")
	(switch-to-buffer (other-buffer term-buf))
	(switch-to-buffer-other-window term-buf)
	(with-current-buffer term-buf
	  (vterm--goto-line -1)
	  (vterm-send-string rails-exe)
	  (vterm-send-return)))
    nil))
;; -------------------------
#+end_src
*** Elixir
#+begin_src elisp
;; ----- REPL management
(defun elixir-inf-switch ()
  "switch to inf elixir window"
  (interactive)
  (let ((bufs (mapcar #'buffer-name (buffer-list))))
	  (elixir-inf-helper bufs)))

(defun elixir-inf-helper (lis)
  "find terminal and switch to term buffer"
  (cond
   ((eq '() lis)
    (inf-elixir-set-repl))
   ((string= (car lis) "*Inf-Elixir*")
    (switch-to-buffer-other-window (car lis)))
   (t
    (elixir-inf-helper (cdr lis)))))
#+end_src
;; ----- Svelte
(use-package svelte-mode
  :ensure t)

;; ----- Zig
(use-package zig-mode
  :ensure t
  :hook
  (zig-mode . tree-sitter-hl-mode))
;; ----- Ruby
(require 'ob-ruby)
(use-package enh-ruby-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))

;; ----- Elixir
(use-package elixir-mode
  :ensure t
  :hook
  (elixir-mode . inf-elixir-minor-mode))
;; inferior repl mode
(use-package inf-elixir
  :ensure t)

;; ----- Go
(use-package go-mode
  :ensure t
  :hook
  (go-mode . tree-sitter-hl-mode))

;; ----- HTML / Vue
;; use C-j to expand
(use-package emmet-mode
  :ensure t
  :hook
  (web-mode . emmet-mode))
;; Web Mode for better html editing
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

;; ----- JS /TS
;; lang specific hooks
(add-hook 'js-mode-hook #'tree-sitter-hl-mode)
;; run org blocks
(require 'ob-js)
;; TS setup 
(use-package typescript-mode
  :ensure t
  :hook
  (typescript-mode . tree-sitter-hl-mode))
;; repl integration
(use-package js-comint
  :ensure t)
```
