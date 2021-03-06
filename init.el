;; Enable to start emacs from anywhere
;; emacs -q -l ~/path/to/somewhere/init.el
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; Manage packages for each emacs version
(defvar my-emacs-version-dir (locate-user-emacs-file emacs-version))
(setq el-get-dir (expand-file-name "el-get" my-emacs-version-dir)
      package-user-dir (expand-file-name "elpa" my-emacs-version-dir))

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
(el-get-bundle skeeto/emacs-memoize)
(el-get-bundle flycheck/flycheck)
(el-get-bundle magit/git-modes)
(el-get-bundle tom-tan/hlinum-mode)
(el-get-bundle DarwinAwardWinner/ido-completing-read-plus)
(el-get-bundle creichert/ido-vertical-mode.el)
(el-get-bundle magit/magit)
(el-get-bundle jrblevin/markdown-mode)
(el-get-bundle magnars/multiple-cursors.el)
(el-get-bundle Fanael/rainbow-delimiters)
(el-get-bundle magnars/s.el)
(el-get-bundle Fuco1/smartparens)
(el-get-bundle nonsequitur/smex)
(el-get-bundle ocaml/tuareg)
(el-get-bundle bbatsov/zenburn-emacs)

;; package
(use-package package
  :init
  (let* ((protocol (if (gnutls-available-p) "https://" "http://"))
         (elpa (concat protocol "elpa.gnu.org/packages/"))
         (melpa (concat protocol "melpa.org/packages/"))
         (marmalade (concat protocol "marmalade-repo.org/packages/")))
    (add-to-list 'package-archives (cons "elpa" elpa))
    (add-to-list 'package-archives (cons "melpa" melpa))
    (add-to-list 'package-archives (cons "marmalade" marmalade)))
  :config
  (package-initialize)
  )

;; Package list from elpa to be installed by el-get
(el-get-bundle elpa:smooth-scroll)
(el-get-bundle elpa:undo-tree)

;; Global key bindings
(defun my-backward-kill-line() (interactive) (kill-line 0))
(defun my-next-window() (interactive) (other-window 1))
(defun my-prev-window() (interactive) (other-window -1))
(defun my-reload-init-file() (interactive) (load-file user-init-file))
(defun my-transpose-char() (interactive) (backward-char) (transpose-chars 1))
(defun my-transpose-word() (interactive) (backward-word) (transpose-words 1))
(use-package bind-key
  :config
  (bind-key* "C-h" 'backward-delete-char-untabify) ; from help-command
  (bind-key* "C-t" 'my-transpose-char)             ; from transpose-chars
  (bind-key* "C-u" 'my-backward-kill-line)         ; from universal-argument
  (bind-key* "C-x n" 'my-next-window)
  (bind-key* "C-x p" 'my-prev-window)
  (bind-key* "C-x C-b" 'bs-show)        ; from list-buffers
  (bind-key* "C-S-<down>" 'enlarge-window)
  (bind-key* "C-S-<left>" 'shrink-window-horizontally)
  (bind-key* "C-S-<right>" 'enlarge-window-horizontally)
  (bind-key* "C-S-<up>" 'shrink-window)
  (bind-key* "M-h" 'help-for-help)      ; from mark-paragraph
  (bind-key* "M-t" 'my-transpose-word)  ; from transpose-words
  (bind-key* "M-R" 'my-reload-init-file)
  (bind-key* "M-`" 'menu-bar-open)      ; from tmm-menubar
  (bind-key* "C-\\" nil)                ; from toggle-input-method
  )

;; ace jump mode
(use-package ace-jump-mode
  :config
  (setq ace-jump-mode-move-keys
        (append "asdfghjkl;qwertyuiopzxcvbnm,./" nil))
  (setq ace-jump-word-mode-use-query-char nil)
  :bind
  ("M-:" . ace-jump-char-mode)
  ("M-@" . ace-jump-word-mode)
  ("M-g" . ace-jump-line-mode)
  )

;; auto complete
(use-package auto-complete
  :config
  (ac-config-default)
  )

;; cl lib
(use-package cl-lib)

;; cycle themes
(defun my-print-current-theme()
  (interactive) (message "Themes = %S" custom-enabled-themes))
(use-package cycle-themes
  :init
  (let ((white-list '())
        (black-list '())
        (available-list (custom-available-themes)))
    (setq cycle-themes-theme-list
          (cl-union (cl-set-difference available-list black-list)
                    white-list)))
  (setq custom-known-themes (append '(user changed) cycle-themes-theme-list))
  (add-hook 'cycle-themes-after-cycle-hook 'my-print-current-theme)
  :config
  (cycle-themes-mode)
  )

;; flycheck
(use-package flycheck
  )

;; hlinum
(use-package hlinum
  :init
  (add-hook 'linum-mode-hook 'hlinum-activate)
  )

;; ido-completing-read+
(use-package ido-completing-read+
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (setq ido-enable-flex-matching t)
  (ido-ubiquitous-mode 1)
  )

;; ido vertical mode
(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  )

;; markdown mode
(use-package markdown-mode
  :commands
  (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "markdown")
  )

;; proof general
(let* ((pg-dir (expand-file-name "PG" my-emacs-version-dir))
       (clone-command
        (concat "git clone https://github.com/ProofGeneral/PG " pg-dir))
       (make-command (concat "make -C " pg-dir)))
  (cond ((not (file-exists-p pg-dir))
         (async-shell-command clone-command))
        ((not (file-exists-p
               (expand-file-name "generic/proof-site.elc" pg-dir)))
         (async-shell-command make-command))
        (t
         (use-package proof-site
           :init
           (load (expand-file-name "generic/proof-site" pg-dir))
           :config
           (setq coq-compile-before-require t)
           (setq coq-compile-parallel-in-background t)
           (setq proof-electric-terminator-enable t)
           (setq proof-three-window-mode-policy 'hybrid)
           ))))

;; rainbow delimiters
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

;; smex
(use-package smex
  :config
  (smex-initialize)
  :bind
  ("M-x" . smex)
  ("M-X" . execute-extended-command)
  )

;; smartparens
(use-package smartparens
  :config
  (use-package smartparens-config)
  (smartparens-global-mode 1)
  )

;; smooth scroll
(use-package smooth-scroll
  :config
  (smooth-scroll-mode t)
  )

;; tuareg
(use-package tuareg)

;; undo tree
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  )

;; zenburn theme
(use-package zenburn-theme)

;; Load optional .emacs if exists
(let ((dotemacs (expand-file-name ".emacs" user-emacs-directory)))
  (when (file-exists-p dotemacs)
    (load-file dotemacs)))

;; Load separate custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))
