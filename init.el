;; Enable to start emacs from anywhere
;; emacs -q -l ~/path/to/somewhere/init.el
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; Manage packages for each emacs version
(let ((versioned-dir (locate-user-emacs-file emacs-version)))
  (setq el-get-dir (expand-file-name "el-get" versioned-dir)
        package-user-dir (expand-file-name "elpa" versioned-dir)))

;; Install el-get
(add-to-list 'load-path (expand-file-name "el-get" el-get-dir))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; Install use-package
(el-get-bundle jwiegley/use-package)

;; Set use-package to empty command unless use-package is installed
(unless (require 'use-package nil t)
  (defmacro use-package (&rest args)))

;; Package list from github to be installed by el-get
(el-get-bundle winterTTr/ace-jump-mode)
(el-get-bundle auto-complete/auto-complete)
(el-get-bundle flycheck/flycheck)
(el-get-bundle magit/git-modes)
(el-get-bundle emacs-helm/helm
  :autoloads "helm-autoloads"
  :build (("make")))
(el-get-bundle tom-tan/hlinum-mode)
(el-get-bundle magit/magit)
(el-get-bundle magnars/multiple-cursors.el)
(el-get-bundle ProofGeneral/PG)
(el-get-bundle Fanael/rainbow-delimiters)
(el-get-bundle Fuco1/smartparens)
(el-get-bundle k-talo/volatile-highlights.el)
(el-get-bundle bbatsov/zenburn-emacs)

;; Initialize package
(use-package package
  :init
  (setq gnutls-verify-error t)
  :config
  (package-initialize))

;; Package list from elpa to be installed by el-get
(el-get-bundle elpa:markdown-mode)
(el-get-bundle elpa:smooth-scroll)
(el-get-bundle elpa:undohist)
(el-get-bundle elpa:undo-tree)

;; Global key bindings
(use-package bind-key
  :config
  (bind-key* "C-h" 'backward-delete-char-untabify)
  (bind-key* "C-x C-b" 'bs-show)
  (bind-key "C-t" (lambda () (interactive)
                    (backward-char) (transpose-chars 1)))
  (bind-key "C-u" (lambda () (interactive) (kill-line 0)))
  (bind-key* "C-x n" (lambda () (interactive) (other-window 1)))
  (bind-key* "C-x p" (lambda () (interactive) (other-window -1)))
  (bind-key "C-\\" nil))

;; Disable start page
(setq inhibit-startup-message t)
;; yes/no -> y/n
(defalias 'yes-or-no-p 'y-or-n-p)
;; Costomizable initial scratch message
(setq initial-scratch-message
      (concat initial-scratch-message ""))
;; Enable to delete region
(delete-selection-mode 1)
;; Enable minibuffer completion
(icomplete-mode 1)
;; Show trailing space
(setq-default show-trailing-whitespace t)
;; Use space as indent
(setq-default indent-tabs-mode nil)
;; Show (row,column) number
(column-number-mode 1)
;; Highlight parens
(show-paren-mode 1)
;; Suppress question about following symlinks to version controlled files
(setq vc-follow-symlinks t)

;; Initialize ProofGeneral
;; (async-shell-command (concat "make -C " (expand-file-name "PG" el-get-dir)))
(use-package proof-site
  :init
  (load (expand-file-name "PG/generic/proof-site" el-get-dir))
  :config
  (setq coq-compile-before-require t)
  (setq coq-compile-parallel-in-background t)
  (setq proof-electric-terminator-enable t)
  (setq proof-three-window-mode-policy 'hybrid))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zenburn-theme volatile-highlights undohist undo-tree smooth-scroll smartparens rainbow-delimiters multiple-cursors markdown-mode magit hlinum helm gitignore-mode gitconfig-mode flycheck bind-key auto-complete ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
