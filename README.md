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
