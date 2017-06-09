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
(el-get-bundle toroidal-code/cycle-themes.el)
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

;; package
(use-package package
  :init
  (setq gnutls-verify-error t)
  :config
  (package-initialize)
  )

;; Package list from elpa to be installed by el-get
(el-get-bundle elpa:markdown-mode)
(el-get-bundle elpa:smooth-scroll)
(el-get-bundle elpa:undohist)
(el-get-bundle elpa:undo-tree)

;; Global key bindings
(defun my-transpose-char() (interactive) (backward-char) (transpose-chars 1))
(defun my-transpose-word() (interactive) (backward-word) (transpose-words 1))
(defun my-next-window() (interactive) (other-window 1))
(defun my-prev-window() (interactive) (other-window -1))
(use-package bind-key
  :config
  (bind-key* "C-h" 'backward-delete-char-untabify) ; from help-command
  (bind-key* "C-t" 'my-transpose-char)             ; from transpose-chars
  (bind-key* "C-u" 'backward-kill-sentence)        ; from universal-argument
  (bind-key* "C-x n" 'my-next-window)
  (bind-key* "C-x p" 'my-prev-window)
  (bind-key* "C-x C-b" 'bs-show)        ; from list-buffers
  (bind-key* "C-S-<down>" 'enlarge-window)
  (bind-key* "C-S-<left>" 'shrink-window-horizontally)
  (bind-key* "C-S-<right>" 'enlarge-window-horizontally)
  (bind-key* "C-S-<up>" 'shrink-window)
  (bind-key* "M-h" 'help-for-help)      ; from mark-paragraph
  (bind-key* "M-t" 'my-transpose-word)  ; from transpose-words
  (bind-key* "M-`" 'menu-bar-open)      ; from tmm-menubar
  (bind-key* "C-\\" nil)                ; from toggle-input-method
  )

;; cycle themes
(use-package cycle-themes
  :init
  (setq cycle-themes-theme-list '(zenburn manoj-dark tsdh-dark default))
  (setq custom-known-themes (append '(user changed) cycle-themes-theme-list))
  (add-hook 'cycle-themes-after-cycle-hook
            (lambda () (interactive)
              (message "Themes = %S" custom-enabled-themes)))
  :config
  (cycle-themes-mode)
  (custom-set-faces
   '(default ((t :background "unspecified-bg"))))
  )

;; proof general
;; (async-shell-command (concat "make -C " (expand-file-name "PG" el-get-dir)))
(use-package proof-site
  :init
  (load (expand-file-name "PG/generic/proof-site" el-get-dir))
  :config
  (setq coq-compile-before-require t)
  (setq coq-compile-parallel-in-background t)
  (setq proof-electric-terminator-enable t)
  (setq proof-three-window-mode-policy 'hybrid)
  )

;; undo tree
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  )

;; Load separate custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
