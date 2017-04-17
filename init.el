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

;; C-b C-t
(defun my-transpose-chars()
  (interactive)
  (backward-char)
  (transpose-chars 1))
;; C-u as in terminal
(defun my-cancel-line()
  (interactive)
  (kill-line 0))
;; Next pane
(defun my-next-window()
  (interactive)
  (other-window 1))
;; Prev pane
(defun my-prev-window()
  (interactive)
  (other-window -1))

;; Global key bindings
(use-package bind-key
  :config
  (bind-key* "C-h" 'backward-delete-char-untabify)
  (bind-key* "C-x C-b" 'bs-show)
  (bind-key "C-t" 'my-transpose-chars)
  (bind-key "C-u" 'my-cancel-line)
  (bind-key* "C-x n" 'my-next-window)
  (bind-key* "C-x p" 'my-prev-window)
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
;; Show line number
(global-linum-mode 1)
;; Show (row,column) number
(column-number-mode 1)
;; Highlight parens
(show-paren-mode 1)
